#include <Rcpp.h>
#include <RcppParallel.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>

using Rcpp::IntegerMatrix;
using Rcpp::IntegerVector;
using Rcpp::List;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using RcppParallel::RMatrix;
using RcppParallel::RVector;
using RcppParallel::Worker;

namespace {

struct SortedValues {
  std::vector<int> empirical_order;
  std::vector<int> synthetic_order;
  std::vector<int> synthetic_offsets;
};

SortedValues build_sorted_values(
    const NumericVector& empirical,
    const NumericMatrix& synthetic) {
  const int observations = empirical.size();
  const int panels = synthetic.ncol();
  SortedValues sorted;
  sorted.empirical_order.reserve(observations);
  for (int row = 0; row < observations; ++row) {
    if (std::isfinite(empirical[row])) {
      sorted.empirical_order.push_back(row);
    }
  }
  std::stable_sort(
      sorted.empirical_order.begin(),
      sorted.empirical_order.end(),
      [&empirical](int left, int right) {
        return empirical[left] < empirical[right];
      });

  sorted.synthetic_offsets.reserve(panels + 1);
  sorted.synthetic_offsets.push_back(0);
  for (int panel = 0; panel < panels; ++panel) {
    std::vector<int> order;
    order.reserve(observations);
    for (int row = 0; row < observations; ++row) {
      if (std::isfinite(synthetic(row, panel))) {
        order.push_back(row);
      }
    }
    std::stable_sort(
        order.begin(),
        order.end(),
        [&synthetic, panel](int left, int right) {
          return synthetic(left, panel) < synthetic(right, panel);
        });
    sorted.synthetic_order.insert(
        sorted.synthetic_order.end(), order.begin(), order.end());
    sorted.synthetic_offsets.push_back(sorted.synthetic_order.size());
  }
  return sorted;
}

double weighted_total(
    const std::vector<int>& order,
    int begin,
    int end,
    const RVector<int>& clip_index,
    const RMatrix<int>& draw_counts,
    int draw) {
  double total = 0.0;
  for (int position = begin; position < end; ++position) {
    total += draw_counts(draw, clip_index[order[position]]);
  }
  return total;
}

template <typename ValueAccessor>
double weighted_median(
    const std::vector<int>& order,
    int begin,
    int end,
    const RVector<int>& clip_index,
    const RMatrix<int>& draw_counts,
    int draw,
    double total,
    ValueAccessor value) {
  if (!(total > 0.0)) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  const long long integer_total = static_cast<long long>(std::llround(total));
  const long long lower_rank = (integer_total + 1LL) / 2LL;
  const long long upper_rank = integer_total / 2LL + 1LL;
  long long cumulative = 0LL;
  double lower = std::numeric_limits<double>::quiet_NaN();
  double upper = std::numeric_limits<double>::quiet_NaN();
  for (int position = begin; position < end; ++position) {
    const int row = order[position];
    cumulative += draw_counts(draw, clip_index[row]);
    if (!std::isfinite(lower) && cumulative >= lower_rank) {
      lower = value(row);
    }
    if (cumulative >= upper_rank) {
      upper = value(row);
      break;
    }
  }
  return (lower + upper) / 2.0;
}

double weighted_wasserstein(
    const RVector<double>& empirical,
    const RMatrix<double>& synthetic,
    const std::vector<int>& empirical_order,
    const std::vector<int>& synthetic_order,
    int synthetic_begin,
    int synthetic_end,
    const RVector<int>& clip_index,
    const RMatrix<int>& draw_counts,
    int draw,
    int panel,
    double empirical_total,
    double synthetic_total) {
  int empirical_position = 0;
  int synthetic_position = synthetic_begin;
  double empirical_cumulative = 0.0;
  double synthetic_cumulative = 0.0;
  double area = 0.0;
  const int empirical_end = empirical_order.size();

  while (empirical_position < empirical_end ||
         synthetic_position < synthetic_end) {
    const double empirical_value = empirical_position < empirical_end
        ? empirical[empirical_order[empirical_position]]
        : std::numeric_limits<double>::infinity();
    const double synthetic_value = synthetic_position < synthetic_end
        ? synthetic(synthetic_order[synthetic_position], panel)
        : std::numeric_limits<double>::infinity();
    const double support = std::min(empirical_value, synthetic_value);

    while (empirical_position < empirical_end &&
           empirical[empirical_order[empirical_position]] == support) {
      const int row = empirical_order[empirical_position];
      empirical_cumulative += draw_counts(draw, clip_index[row]);
      ++empirical_position;
    }
    while (synthetic_position < synthetic_end &&
           synthetic(synthetic_order[synthetic_position], panel) == support) {
      const int row = synthetic_order[synthetic_position];
      synthetic_cumulative += draw_counts(draw, clip_index[row]);
      ++synthetic_position;
    }

    const double next_empirical = empirical_position < empirical_end
        ? empirical[empirical_order[empirical_position]]
        : std::numeric_limits<double>::infinity();
    const double next_synthetic = synthetic_position < synthetic_end
        ? synthetic(synthetic_order[synthetic_position], panel)
        : std::numeric_limits<double>::infinity();
    const double next_support = std::min(next_empirical, next_synthetic);
    if (std::isfinite(next_support)) {
      const double cdf_difference =
          empirical_cumulative / empirical_total -
          synthetic_cumulative / synthetic_total;
      area += std::abs(cdf_difference) * (next_support - support);
    }
  }
  return area;
}

class BootstrapWorker : public Worker {
 public:
  BootstrapWorker(
      const NumericVector& empirical,
      const NumericMatrix& synthetic,
      const IntegerVector& clip_index,
      const IntegerVector& variant_index,
      const IntegerMatrix& draw_counts,
      const SortedValues& sorted,
      double scale,
      NumericMatrix& output)
      : empirical_(empirical),
        synthetic_(synthetic),
        clip_index_(clip_index),
        variant_index_(variant_index),
        draw_counts_(draw_counts),
        empirical_order_(sorted.empirical_order),
        synthetic_order_(sorted.synthetic_order),
        synthetic_offsets_(sorted.synthetic_offsets),
        scale_(scale),
        output_(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    const int panels = synthetic_.ncol();
    const int variants = output_.ncol();
    for (std::size_t draw = begin; draw < end; ++draw) {
      std::vector<double> sums(variants, 0.0);
      std::vector<int> counts(variants, 0);
      const double empirical_total = weighted_total(
          empirical_order_, 0, empirical_order_.size(), clip_index_,
          draw_counts_, draw);
      const double empirical_median = weighted_median(
          empirical_order_, 0, empirical_order_.size(), clip_index_,
          draw_counts_, draw, empirical_total,
          [this](int row) { return empirical_[row]; });
      for (int panel = 0; panel < panels; ++panel) {
        const int synthetic_begin = synthetic_offsets_[panel];
        const int synthetic_end = synthetic_offsets_[panel + 1];
        const double synthetic_total = weighted_total(
            synthetic_order_, synthetic_begin, synthetic_end, clip_index_,
            draw_counts_, draw);
        const double synthetic_median = weighted_median(
            synthetic_order_, synthetic_begin, synthetic_end, clip_index_,
            draw_counts_, draw, synthetic_total,
            [this, panel](int row) { return synthetic_(row, panel); });
        const double wasserstein = weighted_wasserstein(
            empirical_, synthetic_, empirical_order_, synthetic_order_,
            synthetic_begin, synthetic_end, clip_index_, draw_counts_, draw,
            panel, empirical_total, synthetic_total);
        const double median_difference =
            std::abs(synthetic_median - empirical_median);
        const double discrepancy =
            std::max(median_difference, wasserstein) / scale_;
        const int variant = variant_index_[panel];
        if (!std::isfinite(discrepancy) || variant < 0 || variant >= variants) {
          output_(draw, 0) = std::numeric_limits<double>::quiet_NaN();
          return;
        }
        sums[variant] += discrepancy;
        ++counts[variant];
      }
      for (int variant = 0; variant < variants; ++variant) {
        output_(draw, variant) = counts[variant] > 0
            ? sums[variant] / counts[variant]
            : std::numeric_limits<double>::quiet_NaN();
      }
    }
  }

 private:
  const RVector<double> empirical_;
  const RMatrix<double> synthetic_;
  const RVector<int> clip_index_;
  const RVector<int> variant_index_;
  const RMatrix<int> draw_counts_;
  const std::vector<int>& empirical_order_;
  const std::vector<int>& synthetic_order_;
  const std::vector<int>& synthetic_offsets_;
  const double scale_;
  RMatrix<double> output_;
};

}  // namespace

// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::export]]
NumericMatrix lomm11_bootstrap_metric_group_cpp(
    NumericVector empirical,
    NumericMatrix synthetic,
    IntegerVector clip_index,
    IntegerVector variant_index,
    IntegerMatrix draw_counts,
    double scale,
    int variant_count) {
  if (empirical.size() != synthetic.nrow() ||
      clip_index.size() != synthetic.nrow() ||
      variant_index.size() != synthetic.ncol() ||
      draw_counts.nrow() < 1 || draw_counts.ncol() < 2 ||
      !std::isfinite(scale) || scale <= 0.0 || variant_count < 2) {
    Rcpp::stop("Paired bootstrap inputs have incompatible dimensions or scale.");
  }
  for (int row = 0; row < clip_index.size(); ++row) {
    if (clip_index[row] < 0 || clip_index[row] >= draw_counts.ncol()) {
      Rcpp::stop("Clip indices are outside the bootstrap design.");
    }
  }
  for (int panel = 0; panel < variant_index.size(); ++panel) {
    if (variant_index[panel] < 0 || variant_index[panel] >= variant_count) {
      Rcpp::stop("Variant indices are outside the factorial design.");
    }
  }
  for (int draw = 0; draw < draw_counts.nrow(); ++draw) {
    int total = 0;
    for (int clip = 0; clip < draw_counts.ncol(); ++clip) {
      if (draw_counts(draw, clip) < 0) {
        Rcpp::stop("Bootstrap clip multiplicities must be non-negative.");
      }
      total += draw_counts(draw, clip);
    }
    if (total != draw_counts.ncol()) {
      Rcpp::stop("Every bootstrap draw must resample exactly n clips.");
    }
  }

  const SortedValues sorted = build_sorted_values(empirical, synthetic);
  if (sorted.empirical_order.empty()) {
    Rcpp::stop("Empirical metric values contain no finite observations.");
  }
  NumericMatrix output(draw_counts.nrow(), variant_count);
  BootstrapWorker worker(
      empirical, synthetic, clip_index, variant_index, draw_counts,
      sorted, scale, output);
  RcppParallel::parallelFor(0, draw_counts.nrow(), worker, 1);
  for (int row = 0; row < output.nrow(); ++row) {
    for (int column = 0; column < output.ncol(); ++column) {
      if (!std::isfinite(output(row, column))) {
        Rcpp::stop("A bootstrap discrepancy is non-finite.");
      }
    }
  }
  return output;
}
