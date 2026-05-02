#!/usr/bin/env Rscript

options(run_extract_pipeline = FALSE)

do.call(
  Sys.setenv,
  list(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    BLIS_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1"
  )
)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0L) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("run_method_benchmark.R", mustWork = TRUE)
}
repo_root <- dirname(script_path)

source(file.path(repo_root, "extract.r"))

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

quiet_eval <- function(expr) {
  out_depth <- sink.number()
  msg_depth <- sink.number(type = "message")
  out_conn <- textConnection("bench_quiet_output", "w", local = TRUE)
  msg_conn <- textConnection("bench_quiet_messages", "w", local = TRUE)

  cleanup <- function() {
    while (sink.number(type = "message") > msg_depth) {
      try(sink(type = "message"), silent = TRUE)
    }
    while (sink.number() > out_depth) {
      try(sink(), silent = TRUE)
    }
    try(close(msg_conn), silent = TRUE)
    try(close(out_conn), silent = TRUE)
  }
  on.exit(cleanup(), add = TRUE)

  sink(out_conn)
  sink(msg_conn, type = "message")

  force(expr)
}

with_time_limit <- function(limit_sec, expr) {
  if (!is.finite(limit_sec) || is.na(limit_sec) || limit_sec <= 0) {
    return(force(expr))
  }

  setTimeLimit(cpu = Inf, elapsed = limit_sec, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  force(expr)
}

run_with_hard_timeout <- function(limit_sec, expr) {
  if (!is.finite(limit_sec) || is.na(limit_sec) || limit_sec <= 0) {
    return(force(expr))
  }

  kill_process_tree <- function(pid) {
    if (!is.finite(pid) || is.na(pid)) {
      return(invisible(NULL))
    }
    child_out <- try(
      system2("pgrep", c("-P", as.character(pid)), stdout = TRUE, stderr = FALSE),
      silent = TRUE
    )
    if (!inherits(child_out, "try-error") && length(child_out) > 0L) {
      child_pids <- suppressWarnings(as.integer(child_out))
      child_pids <- child_pids[is.finite(child_pids)]
      for (child_pid in child_pids) {
        kill_process_tree(child_pid)
      }
    }
    try(tools::pskill(pid, tools::SIGTERM), silent = TRUE)
    Sys.sleep(0.2)
    try(tools::pskill(pid, tools::SIGKILL), silent = TRUE)
    invisible(NULL)
  }

  job <- parallel::mcparallel(
    {
      limit_blas_threads()
      tryCatch(
        list(ok = TRUE, value = force(expr)),
        error = function(e) list(ok = FALSE, message = conditionMessage(e))
      )
    },
    silent = TRUE
  )

  child_pid <- job$pid
  on.exit({
    if (is.finite(child_pid) && !is.na(child_pid)) {
      kill_process_tree(child_pid)
    }
  }, add = TRUE)

  out <- parallel::mccollect(job, wait = FALSE, timeout = limit_sec)

  if (is.null(out)) {
    kill_process_tree(child_pid)
    parallel::mccollect(job, wait = FALSE)
    stop(sprintf("Timed out after %.0f seconds.", limit_sec), call. = FALSE)
  }

  result <- out[[1]]
  child_pid <- NA_integer_

  if (!is.list(result) || !isTRUE(result$ok)) {
    stop(result$message %||% "Timed child process failed.", call. = FALSE)
  }

  result$value
}

available_methods <- c("dynEGA", "graphicalVAR", "gimme", "pca")
method_labels <- c(
  dynEGA = "DynEGA",
  graphicalVAR = "GraphicalVAR",
  gimme = "GIMME",
  pca = "PCA"
)

parse_bool_env <- function(name, default = FALSE) {
  raw <- Sys.getenv(name, if (default) "1" else "0")
  tolower(raw) %in% c("1", "true", "yes", "y")
}

parse_int_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) {
    return(as.integer(default))
  }

  value <- suppressWarnings(as.integer(raw))
  if (!is.finite(value) || is.na(value)) {
    return(as.integer(default))
  }
  as.integer(value)
}

parse_num_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) {
    return(as.numeric(default))
  }

  value <- suppressWarnings(as.numeric(raw))
  if (!is.finite(value) || is.na(value)) {
    return(as.numeric(default))
  }
  as.numeric(value)
}

parse_num_csv_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) {
    return(as.numeric(default))
  }

  values <- suppressWarnings(as.numeric(trimws(strsplit(raw, ",", fixed = TRUE)[[1]])))
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(as.numeric(default))
  }
  values
}

parse_int_csv_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) {
    return(as.integer(default))
  }

  values <- suppressWarnings(as.integer(trimws(strsplit(raw, ",", fixed = TRUE)[[1]])))
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(as.integer(default))
  }
  as.integer(values)
}

parse_chr_csv_env <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) {
    return(as.character(default))
  }

  values <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    return(as.character(default))
  }
  as.character(values)
}

env_is_set <- function(name) {
  nzchar(trimws(Sys.getenv(name, "")))
}

parse_num_csv_env_first <- function(names, default) {
  for (name in names) {
    if (env_is_set(name)) {
      return(parse_num_csv_env(name, default))
    }
  }
  as.numeric(default)
}

parse_int_csv_env_first <- function(names, default) {
  for (name in names) {
    if (env_is_set(name)) {
      return(parse_int_csv_env(name, default))
    }
  }
  as.integer(default)
}

parse_int_env_first <- function(names, default) {
  for (name in names) {
    if (env_is_set(name)) {
      return(parse_int_env(name, default))
    }
  }
  as.integer(default)
}

limit_blas_threads <- function() {
  vars <- c(
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "BLIS_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "NUMEXPR_NUM_THREADS"
  )
  values <- as.list(rep("1", length(vars)))
  names(values) <- vars
  do.call(Sys.setenv, values)
}

parse_methods_env <- function() {
  raw <- trimws(Sys.getenv("BENCHMARK_METHODS", ""))
  if (!nzchar(raw)) {
    return(c("dynEGA", "graphicalVAR", "gimme"))
  }

  methods <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  methods <- methods[nzchar(methods)]
  invalid <- setdiff(methods, available_methods)
  if (length(invalid) > 0L) {
    stop(
      "Unknown methods in BENCHMARK_METHODS: ",
      paste(invalid, collapse = ", "),
      call. = FALSE
    )
  }
  unique(methods)
}

required_packages_for_methods <- function(methods) {
  pkg_map <- list(
    dynEGA = c("EGAnet", "clue"),
    graphicalVAR = c("graphicalVAR", "igraph", "clue"),
    gimme = c("gimme", "igraph", "clue"),
    pca = c("psych", "clue")
  )
  unique(unlist(pkg_map[methods], use.names = FALSE))
}

assert_required_packages <- function(methods) {
  required <- required_packages_for_methods(methods)
  missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0L) {
    stop(
      "Missing required package(s): ",
      paste(missing, collapse = ", "),
      ". Install them before running the benchmark.",
      call. = FALSE
    )
  }
}

expression_factor_archetypes <- data.frame(
  factor_archetype = c(
    "smooth_slow",
    "moderate_bounded",
    "reactive_bounded",
    "volatile_bounded"
  ),
  eta = c(-0.30, -0.55, -0.85, -1.10),
  zeta = c(-1.70, -1.00, -0.65, -0.30),
  sigma_q = c(0.12, 0.30, 0.45, 0.65),
  stringsAsFactors = FALSE
)

build_expression_mode_scenarios <- function() {
  modes <- list(
    low_single_stable = list(
      scenario_family = "low_single_stable",
      scenario_mode = "low_single_stable",
      sigma_scale = 0.50,
      active_gain = 0.35,
      background_gain = 0.05,
      dwell_min_frac = 1.00,
      dwell_max_frac = 1.00,
      switching = FALSE
    ),
    high_single_stable = list(
      scenario_family = "high_single_stable",
      scenario_mode = "high_single_stable",
      sigma_scale = 1.00,
      active_gain = 1.00,
      background_gain = 0.08,
      dwell_min_frac = 1.00,
      dwell_max_frac = 1.00,
      switching = FALSE
    ),
    high_switch_stable = list(
      scenario_family = "high_switch_stable",
      scenario_mode = "high_switch_stable",
      sigma_scale = 1.00,
      active_gain = 1.00,
      background_gain = 0.08,
      dwell_min_frac = 0.20,
      dwell_max_frac = 0.45,
      switching = TRUE
    ),
    high_switch_volatile = list(
      scenario_family = "high_switch_volatile",
      scenario_mode = "high_switch_volatile",
      sigma_scale = 1.25,
      active_gain = 1.25,
      background_gain = 0.08,
      dwell_min_frac = 0.03,
      dwell_max_frac = 0.10,
      switching = TRUE
    )
  )

  q2_grid <- c(`2d` = 1.5, `3d` = 2.5, `4d` = 4)
  scenarios <- vector("list", length(modes) * length(q2_grid))
  scenario_id <- 1L

  for (mode_name in names(modes)) {
    mode <- modes[[mode_name]]

    for (q2 in q2_grid) {
      n_factors <- map_n_factors_scalar(q2)
      factor_rows <- expression_factor_archetypes[seq_len(n_factors), , drop = FALSE]
      sigma_q <- factor_rows$sigma_q * mode$sigma_scale

      scenarios[[scenario_id]] <- list(
        scenario = paste0(mode_name, "_", n_factors, "d"),
        scenario_family = mode$scenario_family,
        scenario_mode = mode$scenario_mode,
        scenario_nfac = as.integer(n_factors),
        Q2 = q2,
        eta = factor_rows$eta,
        zeta = factor_rows$zeta,
        sigma_q = sigma_q,
        factor_archetypes = factor_rows$factor_archetype,
        active_gain = mode$active_gain,
        background_gain = mode$background_gain,
        dwell_min_frac = mode$dwell_min_frac,
        dwell_max_frac = mode$dwell_max_frac,
        switching = mode$switching
      )
      scenario_id <- scenario_id + 1L
    }
  }

  scenarios
}

build_benchmark_config <- function(smoke = FALSE) {
  methods <- parse_methods_env()
  variant_smoke <- parse_bool_env("BENCHMARK_VARIANT_SMOKE", default = FALSE)
  dry_run <- parse_bool_env("BENCHMARK_DRY_RUN", default = FALSE)
  available_cores <- parallel::detectCores(logical = TRUE)
  if (!is.finite(available_cores) || is.na(available_cores)) {
    available_cores <- 1L
  }
  available_cores <- as.integer(available_cores)

  scenario_list <- build_expression_mode_scenarios()

  if (smoke) {
    TimeP_vals <- parse_int_csv_env("BENCHMARK_VARIANT_TIMEP", 100L)
    N_vals <- parse_int_csv_env_first(c("BENCHMARK_VARIANT_N", "BENCHMARK_VARIANT_N_VIDEOS"), 10L)
    NVAR_vals <- parse_int_csv_env_first(c("BENCHMARK_VARIANT_NVAR", "BENCHMARK_VARIANT_P"), 3L)
    Error_vals <- parse_num_csv_env_first(c("BENCHMARK_VARIANT_ERROR", "BENCHMARK_VARIANT_BASE_ERR"), 0.025)
    Q_vals <- parse_num_csv_env("BENCHMARK_VARIANT_Q", 0.75)
    Loadings_vals <- parse_num_csv_env("BENCHMARK_VARIANT_LOADINGS", 0.6)
    reps <- parse_int_env("BENCHMARK_VARIANT_REPS", 1L)
    dynEGA_ncores <- 1L
    pca_parallel_iter <- 2L
    graphical_var_timeout_sec <- 240
    gimme_timeout_sec <- parse_num_env("BENCHMARK_GIMME_TIMEOUT_SEC", 420)
  } else {
    TimeP_vals <- parse_int_csv_env("BENCHMARK_FULL_TIMEP", c(100L, 300L))
    N_vals <- parse_int_csv_env_first(c("BENCHMARK_FULL_N", "BENCHMARK_FULL_N_VIDEOS"), c(50L, 100L))
    NVAR_vals <- parse_int_csv_env_first(c("BENCHMARK_FULL_NVAR", "BENCHMARK_FULL_P"), c(3L, 4L))
    Error_vals <- parse_num_csv_env_first(c("BENCHMARK_FULL_ERROR", "BENCHMARK_FULL_BASE_ERR"), c(0.01, 0.05))
    Q_vals <- parse_num_csv_env("BENCHMARK_FULL_Q", 0.75)
    Loadings_vals <- parse_num_csv_env("BENCHMARK_FULL_LOADINGS", 0.6)
    reps <- parse_int_env("BENCHMARK_FULL_REPS", 10L)
    dynEGA_ncores <- 8L
    pca_parallel_iter <- 20L
    graphical_var_timeout_sec <- parse_num_env("BENCHMARK_GRAPHICALVAR_TIMEOUT_SEC", Inf)
    gimme_timeout_sec <- parse_num_env("BENCHMARK_GIMME_TIMEOUT_SEC", 3600)
  }

  TimeP_vals <- as.integer(TimeP_vals[is.finite(TimeP_vals) & TimeP_vals > 0L])
  N_vals <- as.integer(N_vals[is.finite(N_vals) & N_vals > 0L])
  NVAR_vals <- as.integer(NVAR_vals[is.finite(NVAR_vals) & NVAR_vals > 0L])
  Error_vals <- as.numeric(Error_vals[is.finite(Error_vals) & Error_vals >= 0])
  Q_vals <- as.numeric(Q_vals[is.finite(Q_vals) & Q_vals > 0])
  Loadings_vals <- as.numeric(Loadings_vals[is.finite(Loadings_vals) & Loadings_vals > 0])
  if (length(TimeP_vals) == 0L ||
      length(N_vals) == 0L ||
      length(NVAR_vals) == 0L ||
      length(Error_vals) == 0L ||
      length(Q_vals) == 0L ||
      length(Loadings_vals) == 0L) {
    stop("Revised benchmark grid has an empty factor level set.", call. = FALSE)
  }

  scenario_filter <- parse_chr_csv_env("BENCHMARK_SCENARIOS", character())
  if (length(scenario_filter) > 0L) {
    keep <- vapply(
      scenario_list,
      function(scenario) scenario$scenario %in% scenario_filter,
      logical(1)
    )
    scenario_list <- scenario_list[keep]
    if (length(scenario_list) == 0L) {
      stop(
        "BENCHMARK_SCENARIOS did not match any configured scenarios.",
        call. = FALSE
      )
    }
  }

  max_workers <- max(1L, available_cores - 1L)
  default_parallel_workers <- if (smoke) {
    15L
  } else {
    8L
  }
  parallel_workers <- min(
    max_workers,
    parse_int_env("BENCHMARK_WORKERS", default_parallel_workers)
  )

  list(
    methods = methods,
    scenarios = scenario_list,
    TimeP_vals = TimeP_vals,
    N_vals = N_vals,
    NVAR_vals = NVAR_vals,
    Error_vals = Error_vals,
    Q_vals = Q_vals,
    Loadings_vals = Loadings_vals,
    base_err_vals = Error_vals,
    reps = reps,
    p = NVAR_vals[[1]],
    N_videos = N_vals[[1]],
    dt = NA_real_,
    integrator = "linear_exact",
    n_embed = 25L,
    dynEGA_ncores = dynEGA_ncores,
    pca_parallel_iter = pca_parallel_iter,
    graphical_var_timeout_sec = graphical_var_timeout_sec,
    gimme_timeout_sec = gimme_timeout_sec,
    dynEGA_n_embed_time_scaled = !env_is_set("BENCHMARK_DYN_N_EMBED"),
    dynEGA_n_embed_candidates = {
      candidates <- sort(unique(parse_int_csv_env(
        "BENCHMARK_DYN_N_EMBED",
        c(5L, 10L, 15L)
      )))
      candidates <- candidates[is.finite(candidates) & candidates >= 3L]
      if (length(candidates) == 0L) {
        stop("BENCHMARK_DYN_N_EMBED must contain at least one integer >= 3.", call. = FALSE)
      }
      as.integer(candidates)
    },
    dynEGA_variant_algorithms = parse_chr_csv_env(
      "BENCHMARK_DYN_ALGORITHMS",
      c("leiden", "louvain", "walktrap")
    ),
    dynEGA_variant_uni_methods = parse_chr_csv_env(
      "BENCHMARK_DYN_UNI_METHODS",
      c("expand", "LE", "louvain")
    ),
    graphical_var_community_algorithms = parse_chr_csv_env(
      "BENCHMARK_GRAPHICALVAR_COMMUNITY_ALGORITHMS",
      "louvain"
    ),
    gimme_community_algorithms = parse_chr_csv_env(
      "BENCHMARK_GIMME_COMMUNITY_ALGORITHMS",
      "louvain"
    ),
    gimme_ar = parse_bool_env("BENCHMARK_GIMME_AR", default = TRUE),
    gimme_standardize = parse_bool_env("BENCHMARK_GIMME_STANDARDIZE", default = TRUE),
    gimme_var = parse_bool_env("BENCHMARK_GIMME_VAR", default = TRUE),
    gimme_hybrid = parse_bool_env("BENCHMARK_GIMME_HYBRID", default = FALSE),
    gimme_stop_crit = Sys.getenv("BENCHMARK_GIMME_STOP_CRIT", "model fit"),
    gimme_group_correct = Sys.getenv("BENCHMARK_GIMME_GROUP_CORRECT", "Bonferroni Group"),
    gimme_indiv_correct = Sys.getenv("BENCHMARK_GIMME_INDIV_CORRECT", "Bonferroni"),
    gimme_alpha = parse_num_env("BENCHMARK_GIMME_ALPHA", 0.05),
    gimme_groupcutoff = parse_num_env("BENCHMARK_GIMME_GROUPCUTOFF", 0.75),
    gimme_subcutoff = parse_num_env("BENCHMARK_GIMME_SUBCUTOFF", 0.75),
    gimme_rmsea_cutoff = parse_num_env("BENCHMARK_GIMME_RMSEA_CUTOFF", 0.05),
    gimme_srmr_cutoff = parse_num_env("BENCHMARK_GIMME_SRMR_CUTOFF", 0.05),
    gimme_nnfi_cutoff = parse_num_env("BENCHMARK_GIMME_NNFI_CUTOFF", 0.95),
    gimme_cfi_cutoff = parse_num_env("BENCHMARK_GIMME_CFI_CUTOFF", 0.95),
    gimme_n_excellent = parse_int_env("BENCHMARK_GIMME_N_EXCELLENT", 2L),
    gimme_dir_prop_cutoff = parse_num_env("BENCHMARK_GIMME_DIR_PROP_CUTOFF", 0),
    available_cores = available_cores,
    parallel_workers = parallel_workers,
    graphical_var_workers = min(
      max_workers,
      parse_int_env("BENCHMARK_GRAPHICALVAR_WORKERS", if (smoke) 2L else parallel_workers)
    ),
    gimme_workers = min(
      max_workers,
      parse_int_env("BENCHMARK_GIMME_WORKERS", if (smoke) 1L else parallel_workers)
    ),
    smoke = smoke,
    smoke_profile = if (smoke) "revision_first_benchmark_smoke" else "revision_first_benchmark",
    variant_smoke = variant_smoke,
    dry_run = dry_run,
    scenario_filter = scenario_filter,
    progress = parse_bool_env("BENCHMARK_PROGRESS", default = FALSE),
    checkpoint_each = parse_bool_env("BENCHMARK_CHECKPOINT_EACH", default = FALSE),
    resume = parse_bool_env("BENCHMARK_RESUME", default = FALSE),
    out_dir = Sys.getenv(
      "BENCHMARK_OUT_DIR",
      if (smoke) "benchmark_outputs_smoke" else "benchmark_outputs"
    )
  )
}

build_scenario_grid <- function(cfg) {
  rows <- list()
  row_id <- 1L

  for (scenario in cfg$scenarios) {
    for (TimeP in cfg$TimeP_vals) {
      for (N in cfg$N_vals) {
        for (NVAR in cfg$NVAR_vals) {
          for (Error in cfg$Error_vals) {
            for (Q in cfg$Q_vals) {
              for (Loadings in cfg$Loadings_vals) {
                for (rep_idx in seq_len(cfg$reps)) {
                  rows[[row_id]] <- data.frame(
                    scenario = scenario$scenario,
                    scenario_family = scenario$scenario_family,
                    scenario_mode = scenario$scenario_mode,
                    scenario_nfac = as.integer(scenario$scenario_nfac),
                    Q2 = scenario$Q2,
                    N = as.integer(N),
                    TimeP = as.integer(TimeP),
                    NVAR = as.integer(NVAR),
                    Error = as.numeric(Error),
                    Q = as.numeric(Q),
                    Loadings = as.numeric(Loadings),
                    base_err = as.numeric(Error),
                    rep = rep_idx,
                    stringsAsFactors = FALSE
                  )
                  rows[[row_id]]$eta <- list(as.numeric(scenario$eta))
                  rows[[row_id]]$zeta <- list(as.numeric(scenario$zeta))
                  rows[[row_id]]$sigma_q <- list(as.numeric(scenario$sigma_q) * (as.numeric(Q) / 0.75))
                  rows[[row_id]]$factor_archetypes <- list(as.character(scenario$factor_archetypes))
                  rows[[row_id]]$active_gain <- as.numeric(scenario$active_gain)
                  rows[[row_id]]$background_gain <- as.numeric(scenario$background_gain)
                  rows[[row_id]]$dwell_min_frac <- as.numeric(scenario$dwell_min_frac)
                  rows[[row_id]]$dwell_max_frac <- as.numeric(scenario$dwell_max_frac)
                  rows[[row_id]]$switching <- isTRUE(scenario$switching)
                  rows[[row_id]]$dt <- ifelse(as.integer(TimeP) == 100L, 0.4, 0.25)
                  row_id <- row_id + 1L
                }
              }
            }
          }
        }
      }
    }
  }

  grid <- dplyr::bind_rows(rows)
  grid$seed_base <- 200000L + seq_len(nrow(grid))
  grid
}

filter_gimme_bounded_grid <- function(scenario_grid) {
  scenario_grid[
    scenario_grid$N == 50L &
      scenario_grid$TimeP == 100L &
      scenario_grid$NVAR == 3L &
      abs(scenario_grid$Error - 0.025) < 1e-12 &
      abs(scenario_grid$Q - 0.75) < 1e-12 &
      abs(scenario_grid$Loadings - 0.6) < 1e-12,
    ,
    drop = FALSE
  ]
}

build_gimme_bounded_grid <- function(cfg) {
  old <- cfg
  old$N_vals <- 50L
  old$TimeP_vals <- 100L
  old$NVAR_vals <- 3L
  old$Error_vals <- 0.025
  old$Q_vals <- 0.75
  old$Loadings_vals <- 0.6
  old$base_err_vals <- 0.025
  build_scenario_grid(old)
}

flatten_scenario_grid_for_csv <- function(scenario_grid) {
  out <- scenario_grid
  list_cols <- names(out)[vapply(out, is.list, logical(1))]
  for (col in list_cols) {
    out[[col]] <- vapply(
      out[[col]],
      function(x) paste(as.character(x), collapse = ";"),
      character(1)
    )
  }
  out
}

build_method_specs <- function(cfg) {
  specs <- list()
  spec_id <- 1L

  add_spec <- function(family, label, algorithm = NA_character_,
                       uni_method = NA_character_, n_embed = NA_integer_,
                       n_embed_candidates = NULL,
                       variant = "default") {
    specs[[spec_id]] <<- list(
      method = if (identical(variant, "default")) {
        family
      } else {
        paste0(family, "__", variant)
      },
      method_family = family,
      method_label = label,
      community_algorithm = algorithm,
      uni_method = uni_method,
      n_embed = n_embed,
      n_embed_candidates = n_embed_candidates
    )
    spec_id <<- spec_id + 1L
  }

  if ("dynEGA" %in% cfg$methods) {
    add_spec(
      family = "dynEGA",
      label = method_labels[["dynEGA"]],
      algorithm = "louvain",
      uni_method = "expand",
      n_embed = NA_integer_,
      n_embed_candidates = cfg$dynEGA_n_embed_candidates,
      variant = "baseline"
    )

    if (cfg$variant_smoke) {
      for (algorithm in cfg$dynEGA_variant_algorithms) {
        for (uni_method in cfg$dynEGA_variant_uni_methods) {
          if (identical(algorithm, "louvain") &&
              identical(uni_method, "expand")) {
            next
          }

          variant_id <- paste(
            algorithm,
            uni_method,
            "tefi",
            sep = "_"
          )

          add_spec(
            family = "dynEGA",
            label = paste0(
              "DynEGA[", algorithm, "|", uni_method, "]"
            ),
            algorithm = algorithm,
            uni_method = uni_method,
            n_embed = NA_integer_,
            n_embed_candidates = cfg$dynEGA_n_embed_candidates,
            variant = variant_id
          )
        }
      }
    }
  }

  if ("graphicalVAR" %in% cfg$methods) {
    valid_algorithms <- c("louvain", "walktrap", "leiden")
    graphical_algorithms <- unique(cfg$graphical_var_community_algorithms)
    invalid_algorithms <- setdiff(graphical_algorithms, valid_algorithms)
    if (length(invalid_algorithms) > 0L) {
      stop(
        "Unknown GraphicalVAR community algorithms: ",
        paste(invalid_algorithms, collapse = ", "),
        call. = FALSE
      )
    }

    for (algorithm in graphical_algorithms) {
      add_spec(
        family = "graphicalVAR",
        label = if (identical(algorithm, "louvain")) {
          method_labels[["graphicalVAR"]]
        } else {
          paste0("GraphicalVAR[", algorithm, "]")
        },
        algorithm = algorithm,
        variant = if (identical(algorithm, "louvain")) {
          "default"
        } else {
          algorithm
        }
      )
    }
  }

  if ("gimme" %in% cfg$methods) {
    valid_algorithms <- c("louvain", "walktrap", "leiden")
    gimme_algorithms <- unique(cfg$gimme_community_algorithms)
    invalid_algorithms <- setdiff(gimme_algorithms, valid_algorithms)
    if (length(invalid_algorithms) > 0L) {
      stop(
        "Unknown GIMME community algorithms: ",
        paste(invalid_algorithms, collapse = ", "),
        call. = FALSE
      )
    }

    for (algorithm in gimme_algorithms) {
      add_spec(
        family = "gimme",
        label = paste0("GIMME[", algorithm, "]"),
        algorithm = algorithm,
        variant = if (identical(algorithm, "louvain")) {
          "default"
        } else {
          algorithm
        }
      )
    }
  }

  if ("pca" %in% cfg$methods) {
    add_spec(
      family = "pca",
      label = method_labels[["pca"]]
    )
  }

  specs
}

pad_square_matrix <- function(mat, pad_value = 0) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  size <- max(nr, nc)

  out <- matrix(pad_value, nrow = size, ncol = size)
  out[seq_len(nr), seq_len(nc)] <- mat
  out
}

safe_scale_vector <- function(x) {
  x <- as.numeric(x)
  keep <- is.finite(x)
  out <- rep(NA_real_, length(x))
  if (sum(keep) < 2L) {
    return(out)
  }

  center <- mean(x[keep])
  spread <- stats::sd(x[keep])
  if (!is.finite(spread) || spread <= 0) {
    return(out)
  }

  out[keep] <- (x[keep] - center) / spread
  out
}

safe_scale_matrix <- function(mat) {
  mat <- as.matrix(mat)
  out <- matrix(NA_real_, nrow = nrow(mat), ncol = ncol(mat))
  colnames(out) <- colnames(mat)

  for (col_idx in seq_len(ncol(mat))) {
    out[, col_idx] <- safe_scale_vector(mat[, col_idx])
  }

  out
}

build_activation_schedule <- function(num_steps, NFAC, scen_row, video_idx) {
  active_gain <- as.numeric(scen_row$active_gain[[1]])
  background_gain <- as.numeric(scen_row$background_gain[[1]])
  switching <- isTRUE(scen_row$switching[[1]])
  active_dim <- integer(num_steps)

  if (!switching) {
    active_dim[] <- ((video_idx - 1L) %% NFAC) + 1L
  } else {
    min_len <- max(1L, floor(num_steps * as.numeric(scen_row$dwell_min_frac[[1]])))
    max_len <- max(min_len, ceiling(num_steps * as.numeric(scen_row$dwell_max_frac[[1]])))
    pos <- 1L
    current_dim <- ((video_idx - 1L) %% NFAC) + 1L

    while (pos <= num_steps) {
      dwell <- sample(seq.int(min_len, max_len), size = 1L)
      end_pos <- min(num_steps, pos + dwell - 1L)
      active_dim[pos:end_pos] <- current_dim
      pos <- end_pos + 1L

      if (NFAC > 1L) {
        candidates <- setdiff(seq_len(NFAC), current_dim)
        current_dim <- sample(candidates, size = 1L)
      }
    }
  }

  envelope <- matrix(background_gain, nrow = num_steps, ncol = NFAC)
  envelope[cbind(seq_len(num_steps), active_dim)] <- active_gain

  list(
    envelope = envelope,
    active_dim = active_dim,
    switch_count = sum(active_dim[-1L] != active_dim[-num_steps]),
    mean_dwell = num_steps / length(rle(active_dim)$lengths)
  )
}

simulate_benchmark_panel <- function(scen_row, cfg, seed) {
  set.seed(seed)

  NFAC <- as.integer(scen_row$scenario_nfac[[1]])
  NVAR <- as.integer(scen_row$NVAR[[1]])
  N_videos <- as.integer(scen_row$N[[1]])
  TimeP <- as.integer(scen_row$TimeP[[1]])
  dt <- as.numeric(scen_row$dt[[1]])
  item_names <- paste0("Y", seq_len(NFAC * NVAR))
  true_membership <- rep(seq_len(NFAC), each = NVAR)
  names(true_membership) <- item_names

  panel_rows <- vector("list", N_videos)
  subject_list <- vector("list", N_videos)
  latent_list <- vector("list", N_videos)
  schedule_rows <- vector("list", N_videos)

  for (id_idx in seq_len(N_videos)) {
    raw_latent <- simulate_latent_multi(
      dt = dt,
      num_steps = TimeP,
      NFAC = NFAC,
      eta = scen_row$eta[[1]],
      zeta = scen_row$zeta[[1]],
      sigma_q = scen_row$sigma_q[[1]],
      integrator = cfg$integrator
    )

    schedule <- build_activation_schedule(
      num_steps = TimeP,
      NFAC = NFAC,
      scen_row = scen_row,
      video_idx = id_idx
    )
    latent <- raw_latent * schedule$envelope

    obs_df <- generate_observables_multi(
      latent = latent,
      NFAC = NFAC,
      NVAR = NVAR,
      error = scen_row$Error,
      loadings = scen_row$Loadings
    )
    colnames(obs_df) <- item_names
    obs_df$time <- seq_len(nrow(obs_df))
    obs_df$id <- id_idx

    panel_rows[[id_idx]] <- obs_df[, c(item_names, "time", "id"), drop = FALSE]
    subject_list[[id_idx]] <- as.matrix(obs_df[, item_names, drop = FALSE])
    colnames(subject_list[[id_idx]]) <- item_names
    latent_list[[id_idx]] <- latent
    schedule_rows[[id_idx]] <- data.frame(
      id = id_idx,
      active_dim_start = schedule$active_dim[[1]],
      active_dim_unique = paste(sort(unique(schedule$active_dim)), collapse = ";"),
      switch_count = schedule$switch_count,
      mean_dwell = schedule$mean_dwell,
      stringsAsFactors = FALSE
    )
  }

  names(subject_list) <- paste0("subj", seq_len(N_videos))
  schedule_df <- dplyr::bind_rows(schedule_rows)

  list(
    panel = dplyr::bind_rows(panel_rows),
    subject_list = subject_list,
    latents = latent_list,
    item_names = item_names,
    true_membership = normalize_community_map(true_membership),
    NFAC_true = NFAC,
    NVAR = NVAR,
    N_videos = N_videos,
    dt = dt,
    schedule = schedule_df,
    switch_count_mean = mean(schedule_df$switch_count),
    mean_dwell = mean(schedule_df$mean_dwell)
  )
}

extract_dynega_membership <- function(dynega_obj, item_names) {
  wc <- dynega_obj$dynEGA$population$wc
  if (is.matrix(wc)) {
    wc <- wc[1, ]
  }
  names(wc) <- gsub("\\.Ord[01]$", "", names(wc))
  wc <- wc[item_names]
  if (length(wc) == 0L || all(is.na(wc))) {
    wc <- setNames(rep(1L, length(item_names)), item_names)
  }
  normalize_community_map(wc)
}

community_from_weight_matrix <- function(weight_mat, algorithm = "louvain") {
  algorithm <- algorithm %||% "louvain"
  if (length(algorithm) != 1L || is.na(algorithm) ||
      !(algorithm %in% c("louvain", "walktrap", "leiden"))) {
    stop("Unsupported community algorithm: ", algorithm, call. = FALSE)
  }

  weight_mat <- as.matrix(weight_mat)
  if (nrow(weight_mat) != ncol(weight_mat)) {
    stop("Weight matrix must be square.", call. = FALSE)
  }

  diag(weight_mat) <- 0
  weight_mat[!is.finite(weight_mat)] <- 0
  rownames(weight_mat) <- rownames(weight_mat) %||% colnames(weight_mat)
  colnames(weight_mat) <- colnames(weight_mat) %||% rownames(weight_mat)

  positive_mat <- weight_mat
  positive_mat[positive_mat < 0] <- 0

  if (sum(abs(positive_mat)) <= 0) {
    positive_mat <- abs(weight_mat)
  }

  if (sum(abs(positive_mat)) <= 0) {
    membership <- seq_len(nrow(positive_mat))
    names(membership) <- rownames(positive_mat)
    return(normalize_community_map(membership))
  }

  graph_obj <- igraph::graph_from_adjacency_matrix(
    positive_mat,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )

  edge_weights <- igraph::E(graph_obj)$weight
  membership <- switch(
    algorithm,
    louvain = igraph::cluster_louvain(
      graph_obj,
      weights = edge_weights
    )$membership,
    walktrap = igraph::cluster_walktrap(
      graph_obj,
      weights = edge_weights
    )$membership,
    leiden = igraph::cluster_leiden(
      graph_obj,
      objective_function = "modularity",
      weights = edge_weights
    )$membership
  )
  names(membership) <- rownames(positive_mat)
  normalize_community_map(membership)
}

truncate_text <- function(x, max_chars = 240L) {
  x <- paste(as.character(x), collapse = " ")
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(x)

  max_chars <- as.integer(max_chars)
  if (!is.finite(max_chars) || is.na(max_chars) || max_chars < 1L) {
    return(x)
  }

  if (nchar(x, type = "chars") <= max_chars) {
    return(x)
  }

  paste0(substr(x, 1L, max_chars - 3L), "...")
}

format_diag_number <- function(x, digits = 4L) {
  if (!length(x) || !is.finite(x) || is.na(x)) {
    return("NA")
  }

  formatC(x, digits = digits, format = "g")
}

summarize_gimme_output_files <- function(gimme_out_dir, max_files = 12L) {
  files <- list.files(gimme_out_dir, recursive = TRUE)
  if (length(files) == 0L) {
    return("none")
  }

  max_files <- max(1L, as.integer(max_files))
  shown <- utils::head(files, max_files)
  summary <- paste(shown, collapse = ", ")
  if (length(files) > max_files) {
    summary <- paste0(summary, sprintf(", ... (+%d more)", length(files) - max_files))
  }
  summary
}

summarize_gimme_summary_fit <- function(gimme_out_dir, max_fields = 6L) {
  summary_path <- file.path(gimme_out_dir, "summaryFit.csv")
  if (!file.exists(summary_path)) {
    return("missing")
  }

  summary_fit <- tryCatch(
    utils::read.csv(summary_path, stringsAsFactors = FALSE),
    error = function(e) NULL
  )

  if (!is.data.frame(summary_fit) || nrow(summary_fit) == 0L || ncol(summary_fit) == 0L) {
    return("present but unreadable")
  }

  max_fields <- max(1L, as.integer(max_fields))
  fields <- names(summary_fit)[seq_len(min(ncol(summary_fit), max_fields))]
  values <- vapply(
    fields,
    function(field) truncate_text(summary_fit[[field]][[1]], max_chars = 48L),
    character(1)
  )
  paste(paste(fields, values, sep = "="), collapse = "; ")
}

summarize_gimme_psi_payload <- function(gimme_fit) {
  psi_list <- gimme_fit$psi
  if (is.null(psi_list)) {
    return("fit$psi=NULL")
  }

  if (!is.list(psi_list)) {
    return(paste0("fit$psi class=", paste(class(psi_list), collapse = "/")))
  }

  if (length(psi_list) == 0L) {
    return("fit$psi=list(0)")
  }

  class_labels <- vapply(
    psi_list,
    function(x) paste(class(x), collapse = "/"),
    character(1)
  )
  class_tab <- sort(table(class_labels), decreasing = TRUE)
  class_summary <- paste(names(class_tab), as.integer(class_tab), sep = ":", collapse = ", ")

  matrix_idx <- vapply(psi_list, is.matrix, logical(1))
  dims_summary <- if (any(matrix_idx)) {
    dims <- unique(vapply(
      psi_list[matrix_idx],
      function(x) paste(dim(x), collapse = "x"),
      character(1)
    ))
    paste(dims, collapse = ", ")
  } else {
    "none"
  }

  logical_na_count <- sum(vapply(
    psi_list,
    function(x) is.logical(x) && length(x) == 1L && isTRUE(is.na(x)),
    logical(1)
  ))

  paste0(
    "entries=", length(psi_list),
    "; classes=", class_summary,
    "; matrix_dims=", dims_summary,
    "; logical_na_entries=", logical_na_count
  )
}

summarize_gimme_sim_data <- function(sim_data) {
  panel_stats <- lapply(sim_data$subject_list, function(x) {
    cor_mat <- suppressWarnings(stats::cor(x))
    cor_vals <- abs(cor_mat[upper.tri(cor_mat)])
    cor_vals <- cor_vals[is.finite(cor_vals)]

    cov_eigs <- tryCatch(
      eigen(stats::cov(x), symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) rep(NA_real_, ncol(x))
    )
    cov_eigs <- cov_eigs[is.finite(cov_eigs)]

    c(
      max_abs = max(abs(x), na.rm = TRUE),
      min_abs_cor = if (length(cor_vals) > 0L) min(cor_vals) else NA_real_,
      max_abs_cor = if (length(cor_vals) > 0L) max(cor_vals) else NA_real_,
      min_cov_eig = if (length(cov_eigs) > 0L) min(cov_eigs) else NA_real_,
      max_cov_eig = if (length(cov_eigs) > 0L) max(cov_eigs) else NA_real_
    )
  })

  stats_mat <- do.call(rbind, panel_stats)
  paste(
    paste0(
      "max_abs=", format_diag_number(min(stats_mat[, "max_abs"])),
      "..", format_diag_number(max(stats_mat[, "max_abs"]))
    ),
    paste0(
      "abs_cor=", format_diag_number(min(stats_mat[, "min_abs_cor"])),
      "..", format_diag_number(max(stats_mat[, "max_abs_cor"]))
    ),
    paste0(
      "cov_eig=", format_diag_number(min(stats_mat[, "min_cov_eig"])),
      "..", format_diag_number(max(stats_mat[, "max_cov_eig"]))
    ),
    sep = "; "
  )
}

persist_gimme_failure_bundle <- function(gimme_out_dir, cfg, scenario_label, report_lines) {
  debug_root <- file.path(cfg$out_dir, "gimme_failure_debug")
  dir.create(debug_root, recursive = TRUE, showWarnings = FALSE)

  safe_label <- gsub("[^A-Za-z0-9_.-]+", "_", scenario_label %||% "unknown_scenario")
  target_dir <- file.path(
    debug_root,
    sprintf("%s__pid%s", safe_label, Sys.getpid())
  )
  unlink(target_dir, recursive = TRUE, force = TRUE)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  source_files <- list.files(
    gimme_out_dir,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  if (length(source_files) > 0L) {
    file.copy(source_files, target_dir, recursive = TRUE)
  }

  writeLines(report_lines, con = file.path(target_dir, "diagnostics.txt"))
  target_dir
}

build_gimme_failure_message <- function(stage, failure, sim_data, fit,
                                        gimme_out_dir, gimme_warnings,
                                        cfg, scenario_label) {
  warning_samples <- unique(gimme_warnings)
  report_lines <- c(
    paste0("scenario: ", scenario_label %||% "unknown"),
    paste0("stage: ", stage),
    paste0("failure: ", truncate_text(conditionMessage(failure), max_chars = 400L)),
    paste0("warnings_n: ", length(warning_samples)),
    if (length(warning_samples) > 0L) {
      paste0("warning_1: ", truncate_text(warning_samples[[1]], max_chars = 400L))
    },
    paste0(
      "fit_names: ",
      if (is.null(fit)) "fit unavailable" else paste(names(fit), collapse = ", ")
    ),
    paste0(
      "psi_summary: ",
      if (is.null(fit)) "fit unavailable" else summarize_gimme_psi_payload(fit)
    ),
    paste0("output_files: ", summarize_gimme_output_files(gimme_out_dir)),
    paste0("summary_fit: ", summarize_gimme_summary_fit(gimme_out_dir)),
    paste0("observed_panel: ", summarize_gimme_sim_data(sim_data))
  )
  report_lines <- report_lines[nzchar(report_lines)]

  debug_dir <- persist_gimme_failure_bundle(
    gimme_out_dir = gimme_out_dir,
    cfg = cfg,
    scenario_label = scenario_label,
    report_lines = report_lines
  )

  paste(
    paste0("GIMME ", stage, " failure: ", truncate_text(conditionMessage(failure), max_chars = 240L)),
    paste0("warnings_n=", length(warning_samples)),
    if (length(warning_samples) > 0L) {
      paste0("warning_1=", truncate_text(warning_samples[[1]], max_chars = 180L))
    },
    paste0(
      "psi=",
      if (is.null(fit)) "fit unavailable" else summarize_gimme_psi_payload(fit)
    ),
    paste0("summaryFit=", summarize_gimme_summary_fit(gimme_out_dir)),
    paste0("output_files=", summarize_gimme_output_files(gimme_out_dir)),
    paste0("observed_panel=", summarize_gimme_sim_data(sim_data)),
    paste0("debug_dir=", debug_dir),
    sep = " | "
  )
}

extract_average_gimme_psi <- function(gimme_fit) {
  strip_lag_suffix <- function(name_vec) {
    name_vec <- as.character(name_vec)
    name_vec <- gsub("([._-](?:lag|L)?\\d+)$", "", name_vec, ignore.case = TRUE, perl = TRUE)
    name_vec <- gsub("\\.\\d+$", "", name_vec)
    name_vec
  }

  select_contemporaneous_block <- function(mat) {
    if (!is.matrix(mat)) {
      return(NULL)
    }

    n_row <- nrow(mat)
    n_col <- ncol(mat)
    row_ids <- rownames(mat)
    col_ids <- colnames(mat)

    if (n_row == 0L || n_col == 0L) {
      return(NULL)
    }

    if (n_row == n_col && !is.null(row_ids) && !is.null(col_ids)) {
      return(mat)
    }

    if (!is.null(row_ids) && !is.null(col_ids)) {
      row_ids_chr <- as.character(row_ids)
      col_ids_chr <- as.character(col_ids)

      if (all(row_ids_chr %in% col_ids_chr)) {
        col_match <- match(row_ids_chr, col_ids_chr)
        if (!anyNA(col_match)) {
          maybe_square <- mat[row_ids_chr, col_match, drop = FALSE]
          if (nrow(maybe_square) == ncol(maybe_square)) {
            return(maybe_square)
          }
        }
      }

      row_ids_stripped <- strip_lag_suffix(row_ids_chr)
      col_ids_stripped <- strip_lag_suffix(col_ids_chr)

      if (all(row_ids_stripped %in% col_ids_stripped)) {
        col_match <- vapply(
          row_ids_stripped,
          function(x) {
            found <- match(x, col_ids_stripped)
            if (is.na(found)) 0L else as.integer(found)
          },
          integer(1L)
        )
        if (!any(col_match == 0L)) {
          maybe_square <- mat[row_ids_chr, col_match, drop = FALSE]
          if (nrow(maybe_square) == ncol(maybe_square)) {
            return(maybe_square)
          }
        }
      }

      contemp_col_candidates <- which(col_ids_stripped %in% row_ids_stripped)
      if (length(contemp_col_candidates) >= n_row) {
        maybe_square <- mat[row_ids_chr, contemp_col_candidates[seq_len(n_row)], drop = FALSE]
        if (nrow(maybe_square) == ncol(maybe_square)) {
          return(maybe_square)
        }
      }
    }

    if (n_col > n_row) {
      maybe_square <- mat[, seq_len(n_row), drop = FALSE]
      if (nrow(maybe_square) == ncol(maybe_square)) {
        return(maybe_square)
      }
    }

    NULL
  }

  psi_list <- gimme_fit$psi
  psi_list <- psi_list[vapply(psi_list, is.matrix, logical(1))]
  if (length(psi_list) == 0L) {
    stop(
      paste(
        "GIMME fit did not return any matrices in fit$psi.",
        "This usually means estimation produced non-matrix placeholders",
        "(for example logical NA entries) instead of subject psi matrices."
      ),
      call. = FALSE
    )
  }

  psi_list <- lapply(psi_list, select_contemporaneous_block)
  psi_list <- psi_list[vapply(psi_list, Negate(is.null), logical(1))]

  if (length(psi_list) == 0L) {
    stop(
      paste(
        "GIMME fit returned psi matrices, but none could be reduced to a square",
        "contemporaneous block."
      ),
      call. = FALSE
    )
  }

  acc <- Reduce(`+`, psi_list)
  acc / length(psi_list)
}

align_memberships <- function(true_membership, est_membership) {
  common_items <- intersect(names(true_membership), names(est_membership))
  list(
    true = normalize_community_map(true_membership[common_items]),
    est = normalize_community_map(est_membership[common_items]),
    items = common_items
  )
}

compute_assignment_accuracy <- function(true_membership, est_membership) {
  aligned <- align_memberships(true_membership, est_membership)
  if (length(aligned$items) == 0L) {
    return(NA_real_)
  }

  tab <- table(aligned$true, aligned$est)
  padded <- pad_square_matrix(unclass(tab), pad_value = 0)
  max_val <- if (length(padded) > 0L) max(padded) else 0
  assignment <- clue::solve_LSAP(max_val - padded)
  matched <- sum(padded[cbind(seq_len(nrow(padded)), assignment)])
  matched / length(aligned$items)
}

build_common_scores <- function(panel, item_names, membership) {
  membership <- membership[item_names]
  membership <- membership[!is.na(membership)]
  if (length(membership) == 0L) {
    membership <- setNames(rep(1L, length(item_names)), item_names)
  }
  blocks <- sort(unique(membership))
  observed <- panel[, names(membership), drop = FALSE]
  standardized <- safe_scale_matrix(observed)

  score_list <- lapply(blocks, function(block_id) {
    vars <- names(membership)[membership == block_id]
    block_mat <- standardized[, vars, drop = FALSE]

    if (ncol(block_mat) == 1L) {
      score <- block_mat[, 1]
    } else {
      block_ok <- block_mat
      block_ok[!is.finite(block_ok)] <- 0
      score <- tryCatch(
        {
          pc <- stats::prcomp(block_ok, center = FALSE, scale. = FALSE)
          pc$x[, 1]
        },
        error = function(e) rowMeans(block_ok, na.rm = TRUE)
      )
    }

    safe_scale_vector(score)
  })

  score_mat <- do.call(cbind, score_list)
  score_mat <- as.matrix(score_mat)
  colnames(score_mat) <- paste0("dim_", seq_len(ncol(score_mat)))
  score_mat
}

extract_pca_membership <- function(loadings_mat) {
  membership <- max.col(abs(loadings_mat), ties.method = "first")
  names(membership) <- rownames(loadings_mat)
  normalize_community_map(membership)
}

compute_score_nrmse_alignment <- function(scores, latents, id_vector, penalty = 2) {
  if (is.null(scores) || is.null(latents) || length(latents) == 0L) {
    return(NA_real_)
  }

  score_mat <- as.matrix(scores)
  id_vector <- as.integer(id_vector)
  values <- c()

  for (id_idx in seq_along(latents)) {
    rows <- which(id_vector == id_idx)
    if (length(rows) == 0L) {
      next
    }

    latent_i <- latents[[id_idx]]
    score_i <- score_mat[rows, , drop = FALSE]
    len <- min(nrow(latent_i), nrow(score_i))
    if (len < 3L) {
      next
    }

    latent_i <- safe_scale_matrix(latent_i[seq_len(len), , drop = FALSE])
    score_i <- safe_scale_matrix(score_i[seq_len(len), , drop = FALSE])

    n_true <- ncol(latent_i)
    n_est <- ncol(score_i)

    if (n_true == 0L) {
      next
    }
    if (n_est == 0L) {
      values <- c(values, rep(penalty, n_true))
      next
    }

    cost_mat <- matrix(penalty, nrow = n_true, ncol = n_est)

    for (true_idx in seq_len(n_true)) {
      x <- latent_i[, true_idx]
      for (est_idx in seq_len(n_est)) {
        y <- score_i[, est_idx]
        keep <- is.finite(x) & is.finite(y)
        if (sum(keep) < 3L) {
          next
        }

        rmse_pos <- sqrt(mean((x[keep] - y[keep])^2))
        rmse_neg <- sqrt(mean((x[keep] + y[keep])^2))
        cost_mat[true_idx, est_idx] <- min(rmse_pos, rmse_neg)
      }
    }

    padded <- pad_square_matrix(cost_mat, pad_value = penalty)
    assignment <- clue::solve_LSAP(padded)
    values <- c(values, padded[cbind(seq_len(n_true), assignment[seq_len(n_true)])])
  }

  if (length(values) == 0L) {
    return(NA_real_)
  }
  mean(values, na.rm = TRUE)
}

run_dynega_method <- function(sim_data, cfg, method_spec = NULL, scen_row = NULL) {
  algorithm <- method_spec$community_algorithm %||% "louvain"
  uni_method <- method_spec$uni_method %||% "expand"
  n_embed_candidates <- method_spec$n_embed_candidates %||% cfg$dynEGA_n_embed_candidates
  if (isTRUE(cfg$dynEGA_n_embed_time_scaled) && !is.null(scen_row)) {
    TimeP <- as.integer(scen_row$TimeP[[1]])
    n_embed_candidates <- c(5L, 10L, 15L, floor(TimeP / 4), floor(TimeP / 2))
  }
  n_embed_candidates <- sort(unique(as.integer(n_embed_candidates)))
  n_embed_candidates <- n_embed_candidates[is.finite(n_embed_candidates) & n_embed_candidates >= 3L]
  n_embed_candidates <- n_embed_candidates[n_embed_candidates < nrow(sim_data$subject_list[[1]])]
  if (length(n_embed_candidates) == 0L) {
    stop("No valid DynEGA n.embed candidates for this TimeP.", call. = FALSE)
  }

  extract_tefi <- function(dy_fit) {
    tefi_val <- dy_fit$dynEGA$population$TEFI %||% NA_real_
    tefi_val <- suppressWarnings(as.numeric(tefi_val)[[1]])
    if (!is.finite(tefi_val) || is.na(tefi_val)) {
      return(NA_real_)
    }
    tefi_val
  }

  best_fit <- NULL
  fallback_fit <- NULL
  best_error <- NULL

  for (n_embed in n_embed_candidates) {
    fit_attempt <- tryCatch(
      quiet_eval(
        EGAnet::dynEGA(
          data = sim_data$panel[, c(sim_data$item_names, "id"), drop = FALSE],
          n.embed = as.integer(n_embed),
          tau = 1,
          delta = 1,
          id = length(sim_data$item_names) + 1L,
          algorithm = algorithm,
          uni.method = uni_method,
          use.derivatives = 1,
          ncores = cfg$dynEGA_ncores
        )
      ),
      error = function(e) e
    )

    if (inherits(fit_attempt, "error")) {
      best_error <- conditionMessage(fit_attempt)
      next
    }

    if (is.null(fallback_fit)) {
      fallback_fit <- list(
        dy = fit_attempt,
        n_embed_best = as.integer(n_embed),
        tefi_best = NA_real_
      )
    }

    tefi_val <- tryCatch(
      extract_tefi(fit_attempt),
      error = function(e) e
    )

    if (inherits(tefi_val, "error")) {
      best_error <- conditionMessage(tefi_val)
      next
    }

    if (!is.finite(tefi_val) || is.na(tefi_val)) {
      best_error <- "DynEGA fit did not return a finite TEFI value."
      next
    }

    if (is.null(best_fit) ||
        tefi_val < best_fit$tefi_best - 1e-8 ||
        (abs(tefi_val - best_fit$tefi_best) <= 1e-8 &&
         as.integer(n_embed) < best_fit$n_embed_best)) {
      best_fit <- list(
        dy = fit_attempt,
        n_embed_best = as.integer(n_embed),
        tefi_best = tefi_val
      )
    }
  }

  if (is.null(best_fit) && !is.null(fallback_fit)) {
    best_fit <- fallback_fit
  }

  if (is.null(best_fit)) {
    stop(
      "DynEGA n.embed search failed for algorithm=",
      algorithm,
      ", uni.method=",
      uni_method,
      ". Last error: ",
      best_error %||% "unknown error",
      call. = FALSE
    )
  }

  dy <- best_fit$dy

  native_scores <- compute_net_scores_safe(
    dy,
    sim_data$panel[, sim_data$item_names, drop = FALSE]
  )

  membership <- extract_dynega_membership(dy, sim_data$item_names)
  common_scores <- build_common_scores(sim_data$panel, sim_data$item_names, membership)

  list(
    membership = membership,
    n_dim_hat = length(unique(membership)),
    common_scores = common_scores,
    native_scores = native_scores,
    structure_matrix = NULL,
    n_embed_best = best_fit$n_embed_best,
    tefi_best = best_fit$tefi_best
  )
}

fit_graphical_var_structure <- function(sim_data) {
  fit <- quiet_eval(
    graphicalVAR::mlGraphicalVAR(
      data = sim_data$panel,
      vars = sim_data$item_names,
      idvar = "id",
      scale = TRUE,
      centerWithin = TRUE,
      gamma = 0.5,
      verbose = FALSE,
      subjectNetworks = FALSE
    )
  )

  fit$fixedPCC
}

run_graphical_var_method <- function(sim_data, method_spec = NULL, structure_mat = NULL) {
  algorithm <- method_spec$community_algorithm %||% "louvain"
  if (length(algorithm) != 1L || is.na(algorithm)) {
    algorithm <- "louvain"
  }

  if (is.null(structure_mat)) {
    structure_mat <- fit_graphical_var_structure(sim_data)
  }

  membership <- community_from_weight_matrix(structure_mat, algorithm = algorithm)
  common_scores <- build_common_scores(sim_data$panel, sim_data$item_names, membership)

  list(
    membership = membership,
    n_dim_hat = length(unique(membership)),
    common_scores = common_scores,
    native_scores = NULL,
    structure_matrix = structure_mat
  )
}

build_gimme_sem <- function() {
  src <- getFromNamespace("gimmeSEM", "gimme")
  gimme_fn <- as.function(
    c(formals(src), body(src)),
    envir = environment(src)
  )
  body_exprs <- as.list(body(gimme_fn))
  if (length(body_exprs) >= 2L) {
    body_exprs[[2L]] <- quote(arguments <- as.list(match.call())[-1L])
  }
  body(gimme_fn) <- as.call(body_exprs)
  gimme_fn
}

run_gimme_sem_safe <- local({
  cached <- NULL

  function(...) {
    if (is.null(cached)) {
      cached <<- build_gimme_sem()
    }
    do.call(cached, list(...))
  }
})

fit_gimme_structure <- function(sim_data, cfg, scenario_label = NULL) {
  gimme_out_dir <- file.path(
    tempdir(),
    sprintf("gimme-%s-%s", Sys.getpid(), as.integer(stats::runif(1, 1, 1e9)))
  )
  dir.create(gimme_out_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(gimme_out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  gimme_warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      quiet_eval(
        run_gimme_sem_safe(
          data = sim_data$subject_list,
          out = gimme_out_dir,
          subgroup = FALSE,
          plot = FALSE,
          ar = cfg$gimme_ar,
          standardize = cfg$gimme_standardize,
          VAR = cfg$gimme_var,
          hybrid = cfg$gimme_hybrid,
          stop_crit = cfg$gimme_stop_crit,
          group_correct = cfg$gimme_group_correct,
          indiv_correct = cfg$gimme_indiv_correct,
          alpha = cfg$gimme_alpha,
          groupcutoff = cfg$gimme_groupcutoff,
          subcutoff = cfg$gimme_subcutoff,
          rmsea_cutoff = cfg$gimme_rmsea_cutoff,
          srmr_cutoff = cfg$gimme_srmr_cutoff,
          nnfi_cutoff = cfg$gimme_nnfi_cutoff,
          cfi_cutoff = cfg$gimme_cfi_cutoff,
          n_excellent = cfg$gimme_n_excellent,
          dir_prop_cutoff = cfg$gimme_dir_prop_cutoff
        )
      ),
      warning = function(w) {
        gimme_warnings <<- c(gimme_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    stop(
      build_gimme_failure_message(
        stage = "estimation",
        failure = fit,
        sim_data = sim_data,
        fit = NULL,
        gimme_out_dir = gimme_out_dir,
        gimme_warnings = gimme_warnings,
        cfg = cfg,
        scenario_label = scenario_label
      ),
      call. = FALSE
    )
  }

  structure_mat <- tryCatch(
    extract_average_gimme_psi(fit),
    error = function(e) e
  )
  if (inherits(structure_mat, "error")) {
    stop(
      build_gimme_failure_message(
        stage = "post-estimation psi extraction",
        failure = structure_mat,
        sim_data = sim_data,
        fit = fit,
        gimme_out_dir = gimme_out_dir,
        gimme_warnings = gimme_warnings,
        cfg = cfg,
        scenario_label = scenario_label
      ),
      call. = FALSE
    )
  }

  rownames(structure_mat) <- rownames(structure_mat) %||% sim_data$item_names
  colnames(structure_mat) <- colnames(structure_mat) %||% sim_data$item_names

  structure_mat
}

run_gimme_method <- function(sim_data, cfg, method_spec = NULL,
                             scenario_label = NULL, structure_mat = NULL) {
  algorithm <- method_spec$community_algorithm %||% "louvain"
  if (length(algorithm) != 1L || is.na(algorithm)) {
    algorithm <- "louvain"
  }

  if (is.null(structure_mat)) {
    structure_mat <- fit_gimme_structure(
      sim_data = sim_data,
      cfg = cfg,
      scenario_label = scenario_label
    )
  }

  membership <- community_from_weight_matrix(structure_mat, algorithm = algorithm)
  common_scores <- build_common_scores(sim_data$panel, sim_data$item_names, membership)

  list(
    membership = membership,
    n_dim_hat = length(unique(membership)),
    common_scores = common_scores,
    native_scores = NULL,
    structure_matrix = structure_mat
  )
}

run_pca_method <- function(sim_data, cfg) {
  observed <- sim_data$panel[, sim_data$item_names, drop = FALSE]

  old_cores <- getOption("mc.cores")
  on.exit(options(mc.cores = old_cores), add = TRUE)
  options(mc.cores = 1)

  pa <- quiet_eval(
    psych::fa.parallel(
      observed,
      fa = "pc",
      plot = FALSE,
      sim = FALSE,
      n.iter = cfg$pca_parallel_iter
    )
  )

  n_comp <- as.integer(pa$ncomp)
  if (!is.finite(n_comp) || n_comp < 1L) {
    n_comp <- 1L
  }

  pca_fit <- psych::principal(
    observed,
    nfactors = n_comp,
    rotate = if (n_comp > 1L) "varimax" else "none",
    scores = TRUE
  )

  loadings_mat <- as.matrix(unclass(pca_fit$loadings))
  membership <- extract_pca_membership(loadings_mat)
  common_scores <- build_common_scores(sim_data$panel, sim_data$item_names, membership)
  native_scores <- as.matrix(pca_fit$scores)
  native_scores <- as.matrix(native_scores)
  colnames(native_scores) <- paste0("pc_", seq_len(ncol(native_scores)))

  list(
    membership = membership,
    n_dim_hat = length(unique(membership)),
    common_scores = common_scores,
    native_scores = native_scores,
    structure_matrix = loadings_mat
  )
}

run_method_fit <- function(method_spec, sim_data, cfg, scen_row = NULL) {
  start_time <- proc.time()[["elapsed"]]
  method_family <- method_spec$method_family
  scenario_label <- if (is.null(scen_row)) {
    NULL
  } else {
    paste0(
      scen_row$scenario[[1]],
      "__rep",
      as.integer(scen_row$rep[[1]])
    )
  }

  fit <- tryCatch(
    {
      time_limit <- if (identical(method_family, "graphicalVAR")) {
        cfg$graphical_var_timeout_sec
      } else if (identical(method_family, "gimme")) {
        cfg$gimme_timeout_sec
      } else {
        Inf
      }

      runner <- with_time_limit

      runner(time_limit, {
        if (identical(method_family, "dynEGA")) {
          run_dynega_method(sim_data, cfg, method_spec = method_spec, scen_row = scen_row)
        } else if (identical(method_family, "graphicalVAR")) {
          run_graphical_var_method(sim_data, method_spec = method_spec)
        } else if (identical(method_family, "gimme")) {
          run_gimme_method(
            sim_data,
            cfg = cfg,
            method_spec = method_spec,
            scenario_label = scenario_label
          )
        } else if (identical(method_family, "pca")) {
          run_pca_method(sim_data, cfg)
        } else {
          stop("Unsupported method: ", method_family, call. = FALSE)
        }
      })
    },
    error = function(e) e
  )

  elapsed <- proc.time()[["elapsed"]] - start_time

  if (inherits(fit, "error")) {
    return(list(
      status = "error",
      error_message = conditionMessage(fit),
      runtime_sec = elapsed
    ))
  }

  list(
    status = "ok",
    error_message = NA_character_,
    runtime_sec = elapsed,
    fit = fit
  )
}

method_seed_for_spec <- function(method_spec, method_idx, sim_seed) {
  family_seed <- match(method_spec$method_family, available_methods) * 1000L
  variant_seed <- method_idx * 10000L
  sim_seed + family_seed + variant_seed
}

run_graphical_var_specs_for_scenario <- function(method_specs, method_indices,
                                                sim_data, cfg, scen_row) {
  start_time <- proc.time()[["elapsed"]]
  structure_fit <- tryCatch(
    run_with_hard_timeout(
      cfg$graphical_var_timeout_sec,
      fit_graphical_var_structure(sim_data)
    ),
    error = function(e) e
  )
  fit_elapsed <- proc.time()[["elapsed"]] - start_time

  if (inherits(structure_fit, "error")) {
    fit_status <- if (grepl("^Timed out after", conditionMessage(structure_fit))) {
      "timeout"
    } else {
      "error"
    }
    return(lapply(method_specs, function(method_spec) {
      build_result_row(
        method_spec,
        list(
          status = fit_status,
          error_message = conditionMessage(structure_fit),
          runtime_sec = fit_elapsed
        ),
        sim_data,
        scen_row
      )
    }))
  }

  Map(
    function(method_spec, method_idx) {
      cluster_start <- proc.time()[["elapsed"]]
      fit <- tryCatch(
        run_graphical_var_method(
          sim_data,
          method_spec = method_spec,
          structure_mat = structure_fit
        ),
        error = function(e) e
      )
      cluster_elapsed <- proc.time()[["elapsed"]] - cluster_start

      if (inherits(fit, "error")) {
        return(build_result_row(
          method_spec,
          list(
            status = "error",
            error_message = conditionMessage(fit),
            runtime_sec = fit_elapsed + cluster_elapsed
          ),
          sim_data,
          scen_row
        ))
      }

      build_result_row(
        method_spec,
        list(
          status = "ok",
          error_message = NA_character_,
          runtime_sec = fit_elapsed + cluster_elapsed,
          fit = fit
        ),
        sim_data,
        scen_row
      )
    },
    method_specs,
    method_indices
  )
}

run_gimme_specs_for_scenario <- function(method_specs, method_indices,
                                         sim_data, cfg, scen_row) {
  scenario_label <- paste0(
    scen_row$scenario[[1]],
    "__rep",
    as.integer(scen_row$rep[[1]])
  )
  start_time <- proc.time()[["elapsed"]]
  structure_fit <- tryCatch(
    run_with_hard_timeout(
      cfg$gimme_timeout_sec,
      fit_gimme_structure(
        sim_data = sim_data,
        cfg = cfg,
        scenario_label = scenario_label
      )
    ),
    error = function(e) e
  )
  fit_elapsed <- proc.time()[["elapsed"]] - start_time

  if (inherits(structure_fit, "error")) {
    fit_status <- if (grepl("^Timed out after", conditionMessage(structure_fit))) {
      "timeout"
    } else {
      "error"
    }
    return(lapply(method_specs, function(method_spec) {
      build_result_row(
        method_spec,
        list(
          status = fit_status,
          error_message = conditionMessage(structure_fit),
          runtime_sec = fit_elapsed
        ),
        sim_data,
        scen_row
      )
    }))
  }

  Map(
    function(method_spec, method_idx) {
      cluster_start <- proc.time()[["elapsed"]]
      set.seed(method_seed_for_spec(method_spec, method_idx, scen_row$seed_base[[1]]))
      fit <- tryCatch(
        run_gimme_method(
          sim_data,
          cfg = cfg,
          method_spec = method_spec,
          scenario_label = scenario_label,
          structure_mat = structure_fit
        ),
        error = function(e) e
      )
      cluster_elapsed <- proc.time()[["elapsed"]] - cluster_start

      if (inherits(fit, "error")) {
        return(build_result_row(
          method_spec,
          list(
            status = "error",
            error_message = conditionMessage(fit),
            runtime_sec = fit_elapsed + cluster_elapsed
          ),
          sim_data,
          scen_row
        ))
      }

      build_result_row(
        method_spec,
        list(
          status = "ok",
          error_message = NA_character_,
          runtime_sec = fit_elapsed + cluster_elapsed,
          fit = fit
        ),
        sim_data,
        scen_row
      )
    },
    method_specs,
    method_indices
  )
}

run_method_specs_for_scenario <- function(method_specs, sim_data, cfg, scen_row,
                                          sim_seed) {
  rows <- vector("list", length(method_specs))
  graphical_idx <- which(vapply(
    method_specs,
    function(spec) identical(spec$method_family, "graphicalVAR"),
    logical(1)
  ))
  gimme_idx <- which(vapply(
    method_specs,
    function(spec) identical(spec$method_family, "gimme"),
    logical(1)
  ))

  for (method_idx in seq_along(method_specs)) {
    if (method_idx %in% c(graphical_idx, gimme_idx)) {
      next
    }

    method_spec <- method_specs[[method_idx]]
    set.seed(method_seed_for_spec(method_spec, method_idx, sim_seed))
    fit <- run_method_fit(method_spec, sim_data, cfg, scen_row = scen_row)
    rows[[method_idx]] <- build_result_row(method_spec, fit, sim_data, scen_row)
  }

  if (length(graphical_idx) > 0L) {
    for (i in seq_along(graphical_idx)) {
      method_idx <- graphical_idx[[i]]
      set.seed(method_seed_for_spec(method_specs[[method_idx]], method_idx, sim_seed))
    }

    graphical_rows <- run_graphical_var_specs_for_scenario(
      method_specs = method_specs[graphical_idx],
      method_indices = graphical_idx,
      sim_data = sim_data,
      cfg = cfg,
      scen_row = scen_row
    )
    rows[graphical_idx] <- graphical_rows
  }

  if (length(gimme_idx) > 0L) {
    method_idx <- gimme_idx[[1]]
    set.seed(method_seed_for_spec(method_specs[[method_idx]], method_idx, sim_seed))

    gimme_rows <- run_gimme_specs_for_scenario(
      method_specs = method_specs[gimme_idx],
      method_indices = gimme_idx,
      sim_data = sim_data,
      cfg = cfg,
      scen_row = scen_row
    )
    rows[gimme_idx] <- gimme_rows
  }

  rows
}

run_scenario_benchmark <- function(scen_idx, scenario_grid, cfg) {
  limit_blas_threads()
  method_specs <- build_method_specs(cfg)

  scen_row <- scenario_grid[scen_idx, , drop = FALSE]
  sim_seed <- scen_row$seed_base[[1]]

  sim_data <- simulate_benchmark_panel(
    scen_row = scen_row,
    cfg = cfg,
    seed = sim_seed
  )

  rows <- run_method_specs_for_scenario(method_specs, sim_data, cfg, scen_row, sim_seed)

  dplyr::bind_rows(rows)
}

run_benchmark_pass <- function(cfg, method_specs, workers) {
  if (length(method_specs) == 0L) {
    return(NULL)
  }

  scenario_grid <- build_scenario_grid(cfg)
  method_families <- unique(vapply(method_specs, `[[`, character(1), "method_family"))
  if (length(method_families) == 1L && identical(method_families[[1]], "gimme")) {
    scenario_grid <- build_gimme_bounded_grid(cfg)
  }
  n_jobs <- nrow(scenario_grid)
  if (n_jobs == 0L) {
    return(NULL)
  }
  method_names <- vapply(method_specs, `[[`, character(1), "method")
  checkpoint_file <- file.path(cfg$out_dir, "checkpoint_results.csv")
  existing_rows <- NULL

  if (isTRUE(cfg$resume) && file.exists(checkpoint_file)) {
    existing_rows <- utils::read.csv(checkpoint_file, stringsAsFactors = FALSE)
    existing_rows <- existing_rows[existing_rows$method %in% method_names, , drop = FALSE]
  }

  runner <- function(scen_idx, scenario_grid, cfg, method_specs) {
    limit_blas_threads()

    scen_row <- scenario_grid[scen_idx, , drop = FALSE]
    sim_seed <- scen_row$seed_base[[1]]

    if (isTRUE(cfg$progress)) {
      message(
        sprintf(
          "[%s] start scenario=%s methods=%s",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          scen_row$scenario[[1]],
          paste(vapply(method_specs, `[[`, character(1), "method"), collapse = ",")
        )
      )
    }

    sim_data <- simulate_benchmark_panel(
      scen_row = scen_row,
      cfg = cfg,
      seed = sim_seed
    )

    rows <- run_method_specs_for_scenario(method_specs, sim_data, cfg, scen_row, sim_seed)

    if (isTRUE(cfg$progress)) {
      status_counts <- table(vapply(rows, function(x) x$status[[1]], character(1)))
      ok_count <- if ("ok" %in% names(status_counts)) {
        as.integer(status_counts[["ok"]])
      } else {
        0L
      }
      error_count <- if (length(status_counts) == 0L) {
        0L
      } else {
        as.integer(sum(status_counts[names(status_counts) != "ok"]))
      }
      message(
        sprintf(
          "[%s] done scenario=%s ok=%d error=%d",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          scen_row$scenario[[1]],
          ok_count,
          error_count
        )
      )
    }

    dplyr::bind_rows(rows)
  }

  make_key <- function(df) {
    paste(
      df$method,
      df$scenario,
      df$N,
      df$TimeP,
      df$NVAR,
      df$Error,
      df$Q,
      df$Loadings,
      df$rep,
      sep = "\r"
    )
  }

  scenario_keys <- function(scen_row) {
    paste(
      method_names,
      scen_row$scenario[[1]],
      scen_row$N[[1]],
      scen_row$TimeP[[1]],
      scen_row$NVAR[[1]],
      scen_row$Error[[1]],
      scen_row$Q[[1]],
      scen_row$Loadings[[1]],
      scen_row$rep[[1]],
      sep = "\r"
    )
  }

  pending_idx <- seq_len(n_jobs)
  if (!is.null(existing_rows) && nrow(existing_rows) > 0L) {
    completed_keys <- unique(make_key(existing_rows))
    pending_idx <- pending_idx[vapply(
      pending_idx,
      function(i) !all(scenario_keys(scenario_grid[i, , drop = FALSE]) %in% completed_keys),
      logical(1)
    )]
  }

  append_checkpoint_rows <- function(df) {
    if (!isTRUE(cfg$checkpoint_each) || is.null(df) || nrow(df) == 0L) {
      return(invisible(NULL))
    }
    dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)
    utils::write.table(
      df,
      file = checkpoint_file,
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(checkpoint_file),
      append = file.exists(checkpoint_file),
      quote = TRUE
    )
    invisible(NULL)
  }

  if (length(pending_idx) == 0L) {
    rows <- list()
  } else if (workers > 1L && length(pending_idx) > 1L) {
    chunks <- split(pending_idx, ceiling(seq_along(pending_idx) / workers))
    rows <- vector("list", length(chunks))
    for (chunk_idx in seq_along(chunks)) {
      chunk <- chunks[[chunk_idx]]
      chunk_rows <- parallel::mclapply(
        chunk,
        runner,
        scenario_grid = scenario_grid,
        cfg = cfg,
        method_specs = method_specs,
        mc.cores = min(workers, length(chunk)),
        mc.preschedule = FALSE,
        mc.set.seed = FALSE
      )
      rows[[chunk_idx]] <- dplyr::bind_rows(chunk_rows)
      append_checkpoint_rows(rows[[chunk_idx]])
    }
  } else {
    rows <- vector("list", length(pending_idx))
    for (job_idx in seq_along(pending_idx)) {
      rows[[job_idx]] <- runner(
        pending_idx[[job_idx]],
        scenario_grid = scenario_grid,
        cfg = cfg,
        method_specs = method_specs
      )

      append_checkpoint_rows(rows[[job_idx]])
    }
  }

  dplyr::bind_rows(c(list(existing_rows), rows))
}

build_result_row <- function(method_spec, method_fit, sim_data, scen_row) {
  base_row <- data.frame(
    method = method_spec$method,
    method_family = method_spec$method_family,
    method_label = method_spec$method_label,
    scenario = scen_row$scenario,
    scenario_family = scen_row$scenario_family,
    scenario_mode = scen_row$scenario_mode,
    scenario_nfac = scen_row$scenario_nfac,
    Q2 = scen_row$Q2,
    N = scen_row$N,
    TimeP = scen_row$TimeP,
    NVAR = scen_row$NVAR,
    Error = scen_row$Error,
    Q = scen_row$Q,
    Loadings = scen_row$Loadings,
    base_err = scen_row$base_err,
    sigma_q = paste(round(scen_row$sigma_q[[1]], 4), collapse = ";"),
    rep = scen_row$rep,
    dt = scen_row$dt,
    eta = paste(round(scen_row$eta[[1]], 4), collapse = ";"),
    zeta = paste(round(scen_row$zeta[[1]], 4), collapse = ";"),
    factor_archetypes = paste(scen_row$factor_archetypes[[1]], collapse = ";"),
    active_gain = scen_row$active_gain,
    background_gain = scen_row$background_gain,
    dwell_min_frac = scen_row$dwell_min_frac,
    dwell_max_frac = scen_row$dwell_max_frac,
    switching = scen_row$switching,
    switch_count_mean = sim_data$switch_count_mean,
    mean_dwell = sim_data$mean_dwell,
    comparison_scope = if (identical(method_spec$method_family, "gimme")) {
      "gimme_bounded"
    } else {
      "main_full_grid"
    },
    community_algorithm = method_spec$community_algorithm,
    uni_method = method_spec$uni_method,
    n_embed = method_spec$n_embed,
    n_embed_best = NA_integer_,
    tefi_best = NA_real_,
    n_dim_true = sim_data$NFAC_true,
    runtime_sec = method_fit$runtime_sec,
    status = method_fit$status,
    error_message = method_fit$error_message,
    n_dim_hat = NA_integer_,
    n_dim_error = NA_real_,
    n_dim_exact = NA_real_,
    assignment_accuracy = NA_real_,
    score_cor_common = NA_real_,
    score_nrmse_common = NA_real_,
    score_cor_native = NA_real_,
    stringsAsFactors = FALSE
  )

  if (!identical(method_fit$status, "ok")) {
    return(base_row)
  }

  fit <- method_fit$fit
  common_cor <- compute_latent_score_alignment(
    scores = fit$common_scores,
    latents = sim_data$latents,
    id_vector = sim_data$panel$id
  )
  common_nrmse <- compute_score_nrmse_alignment(
    scores = fit$common_scores,
    latents = sim_data$latents,
    id_vector = sim_data$panel$id
  )
  native_cor <- compute_latent_score_alignment(
    scores = fit$native_scores,
    latents = sim_data$latents,
    id_vector = sim_data$panel$id
  )

  base_row$n_dim_hat <- fit$n_dim_hat
  base_row$n_dim_error <- fit$n_dim_hat - sim_data$NFAC_true
  base_row$n_dim_exact <- as.numeric(fit$n_dim_hat == sim_data$NFAC_true)
  base_row$n_embed_best <- fit$n_embed_best %||% NA_integer_
  base_row$tefi_best <- fit$tefi_best %||% NA_real_
  base_row$assignment_accuracy <- compute_assignment_accuracy(
    sim_data$true_membership,
    fit$membership
  )
  base_row$score_cor_common <- common_cor
  base_row$score_nrmse_common <- common_nrmse
  base_row$score_cor_native <- native_cor
  base_row
}

run_benchmark_suite <- function(cfg) {
  method_specs <- build_method_specs(cfg)

  fast_specs <- Filter(function(spec) {
    !spec$method_family %in% c("graphicalVAR", "gimme")
  }, method_specs)
  graphical_specs <- Filter(function(spec) {
    identical(spec$method_family, "graphicalVAR")
  }, method_specs)
  gimme_specs <- Filter(function(spec) {
    identical(spec$method_family, "gimme")
  }, method_specs)

  rows <- list(
    run_benchmark_pass(cfg, fast_specs, cfg$parallel_workers),
    run_benchmark_pass(cfg, graphical_specs, cfg$graphical_var_workers),
    run_benchmark_pass(cfg, gimme_specs, cfg$gimme_workers)
  )

  rows <- rows[!vapply(rows, is.null, logical(1))]
  dplyr::bind_rows(rows)
}

summarize_benchmark_results <- function(results_df) {
  metric_cols <- c(
    "n_dim_exact",
    "n_embed_best",
    "tefi_best",
    "assignment_accuracy",
    "score_cor_common",
    "score_nrmse_common",
    "runtime_sec"
  )

  results_df %>%
    dplyr::mutate(success = status == "ok") %>%
    dplyr::group_by(
      method,
      method_family,
      method_label,
      community_algorithm,
      uni_method,
      n_embed,
      scenario_family,
      scenario_mode,
      scenario_nfac,
      scenario,
      Q2,
      N,
      TimeP,
      NVAR,
      Error,
      Q,
      Loadings,
      base_err,
      sigma_q,
      n_dim_true
    ) %>%
    dplyr::summarize(
      reps = dplyr::n(),
      success_rate = mean(success, na.rm = TRUE),
      failure_rate = mean(!success, na.rm = TRUE),
      dplyr::across(
        dplyr::all_of(metric_cols),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ stats::sd(.x, na.rm = TRUE),
          ci_lower = ~ {
            n_ok <- sum(is.finite(.x))
            mean(.x, na.rm = TRUE) - 1.96 * stats::sd(.x, na.rm = TRUE) / sqrt(pmax(n_ok, 1))
          },
          ci_upper = ~ {
            n_ok <- sum(is.finite(.x))
            mean(.x, na.rm = TRUE) + 1.96 * stats::sd(.x, na.rm = TRUE) / sqrt(pmax(n_ok, 1))
          }
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

make_overview_plot <- function(summary_df, out_dir) {
  if (nrow(summary_df) == 0L) {
    return(invisible(NULL))
  }

  if (length(unique(summary_df$method_label)) > 20L) {
    return(invisible(NULL))
  }

  plot_df <- dplyr::bind_rows(
    summary_df %>%
      dplyr::transmute(
        method_label,
        metric = "Dimension Exactness",
        estimate = n_dim_exact_mean,
        ci_lower = n_dim_exact_ci_lower,
        ci_upper = n_dim_exact_ci_upper
      ),
    summary_df %>%
      dplyr::transmute(
        method_label,
        metric = "Assignment Accuracy",
        estimate = assignment_accuracy_mean,
        ci_lower = assignment_accuracy_ci_lower,
        ci_upper = assignment_accuracy_ci_upper
      ),
    summary_df %>%
      dplyr::transmute(
        method_label,
        metric = "Score Correlation",
        estimate = score_cor_common_mean,
        ci_lower = score_cor_common_ci_lower,
        ci_upper = score_cor_common_ci_upper
      ),
    summary_df %>%
      dplyr::transmute(
        method_label,
        metric = "Runtime (sec)",
        estimate = runtime_sec_mean,
        ci_lower = runtime_sec_ci_lower,
        ci_upper = runtime_sec_ci_upper
      )
  ) %>%
    dplyr::filter(is.finite(estimate))

  plot_df$method_label <- factor(plot_df$method_label, levels = unique(plot_df$method_label))

  gg <- ggplot2::ggplot(plot_df, ggplot2::aes(x = method_label, y = estimate, color = method_label)) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = 0.15) +
    ggplot2::facet_wrap(~ metric, scales = "free_y", ncol = 2) +
    ggplot2::labs(
      title = "Method Benchmark Overview",
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )

  ggplot2::ggsave(
    filename = file.path(out_dir, "benchmark_overview.png"),
    plot = gg,
    width = 11,
    height = 8,
    dpi = 180
  )
  invisible(gg)
}

write_metadata <- function(cfg, out_dir) {
  conn <- file(file.path(out_dir, "benchmark_metadata.txt"), open = "wt")
  on.exit(close(conn), add = TRUE)

  cat("run_date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "", file = conn)
  cat("methods: ", paste(cfg$methods, collapse = ","), "\n", sep = "", file = conn)
  cat("integrator: ", cfg$integrator, "\n", sep = "", file = conn)
  cat("dt_rule: TimeP=100 -> 0.4; TimeP=300/600 -> 0.25\n", file = conn)
  cat("TimeP_vals: ", paste(cfg$TimeP_vals, collapse = ","), "\n", sep = "", file = conn)
  cat("N_vals: ", paste(cfg$N_vals, collapse = ","), "\n", sep = "", file = conn)
  cat("NVAR_vals: ", paste(cfg$NVAR_vals, collapse = ","), "\n", sep = "", file = conn)
  cat("Error_vals: ", paste(cfg$Error_vals, collapse = ","), "\n", sep = "", file = conn)
  cat("Q_vals: ", paste(cfg$Q_vals, collapse = ","), "\n", sep = "", file = conn)
  cat("Loadings_vals: ", paste(cfg$Loadings_vals, collapse = ","), "\n", sep = "", file = conn)
  cat("sigma_q_rule: scenario_base_sigma_q * (Q / 0.75)\n", file = conn)
  cat("reps: ", cfg$reps, "\n", sep = "", file = conn)
  cat("dynEGA_n_embed_strategy: manual_tefi_search\n", file = conn)
  cat(
    "dynEGA_n_embed_candidates: ",
    if (isTRUE(cfg$dynEGA_n_embed_time_scaled)) {
      "5,10,15,floor(TimeP/4),floor(TimeP/2)"
    } else {
      paste(cfg$dynEGA_n_embed_candidates, collapse = ",")
    },
    "\n",
    sep = "",
    file = conn
  )
  cat("available_cores: ", cfg$available_cores, "\n", sep = "", file = conn)
  cat("parallel_workers: ", cfg$parallel_workers, "\n", sep = "", file = conn)
  cat("graphical_var_workers: ", cfg$graphical_var_workers, "\n", sep = "", file = conn)
  cat("gimme_workers: ", cfg$gimme_workers, "\n", sep = "", file = conn)
  cat("smoke: ", cfg$smoke, "\n", sep = "", file = conn)
  cat("smoke_profile: ", cfg$smoke_profile, "\n", sep = "", file = conn)
  cat("dry_run: ", cfg$dry_run, "\n", sep = "", file = conn)
  cat("pca_parallel_iter: ", cfg$pca_parallel_iter, "\n\n", sep = "", file = conn)
  cat("graphical_var_timeout_sec: ", cfg$graphical_var_timeout_sec, "\n", sep = "", file = conn)
  cat("gimme_timeout_sec: ", cfg$gimme_timeout_sec, "\n\n", sep = "", file = conn)
  cat("variant_smoke: ", cfg$variant_smoke, "\n", sep = "", file = conn)
  cat(
    "scenario_filter: ",
    if (length(cfg$scenario_filter) > 0L) {
      paste(cfg$scenario_filter, collapse = ",")
    } else {
      "<all>"
    },
    "\n",
    sep = "",
    file = conn
  )
  cat("progress: ", cfg$progress, "\n", sep = "", file = conn)
  cat("dynEGA_variant_algorithms: ", paste(cfg$dynEGA_variant_algorithms, collapse = ","), "\n", sep = "", file = conn)
  cat("dynEGA_variant_uni_methods: ", paste(cfg$dynEGA_variant_uni_methods, collapse = ","), "\n", sep = "", file = conn)
  cat("dynEGA_variant_mode: search_best_tefi\n\n", file = conn)
  cat("graphical_var_community_algorithms: ", paste(cfg$graphical_var_community_algorithms, collapse = ","), "\n", sep = "", file = conn)
  cat("graphical_var_variant_mode: one_mlGraphicalVAR_fit_then_posthoc_community_detection\n\n", file = conn)
  cat("gimme_community_algorithms: ", paste(cfg$gimme_community_algorithms, collapse = ","), "\n", sep = "", file = conn)
  cat("gimme_ar: ", cfg$gimme_ar, "\n", sep = "", file = conn)
  cat("gimme_standardize: ", cfg$gimme_standardize, "\n", sep = "", file = conn)
  cat("gimme_var: ", cfg$gimme_var, "\n", sep = "", file = conn)
  cat("gimme_hybrid: ", cfg$gimme_hybrid, "\n", sep = "", file = conn)
  cat("gimme_stop_crit: ", cfg$gimme_stop_crit, "\n", sep = "", file = conn)
  cat("gimme_group_correct: ", cfg$gimme_group_correct, "\n", sep = "", file = conn)
  cat("gimme_indiv_correct: ", cfg$gimme_indiv_correct, "\n", sep = "", file = conn)
  cat("gimme_alpha: ", cfg$gimme_alpha, "\n", sep = "", file = conn)
  cat("gimme_groupcutoff: ", cfg$gimme_groupcutoff, "\n", sep = "", file = conn)
  cat("gimme_subcutoff: ", cfg$gimme_subcutoff, "\n", sep = "", file = conn)
  cat("gimme_rmsea_cutoff: ", cfg$gimme_rmsea_cutoff, "\n", sep = "", file = conn)
  cat("gimme_srmr_cutoff: ", cfg$gimme_srmr_cutoff, "\n", sep = "", file = conn)
  cat("gimme_nnfi_cutoff: ", cfg$gimme_nnfi_cutoff, "\n", sep = "", file = conn)
  cat("gimme_cfi_cutoff: ", cfg$gimme_cfi_cutoff, "\n", sep = "", file = conn)
  cat("gimme_n_excellent: ", cfg$gimme_n_excellent, "\n", sep = "", file = conn)
  cat("gimme_dir_prop_cutoff: ", cfg$gimme_dir_prop_cutoff, "\n", sep = "", file = conn)
  cat("gimme_variant_mode: one_gimmeSEM_fit_then_posthoc_community_detection\n\n", file = conn)
  cat("gimme_scope: bounded subset N=50, TimeP=100, NVAR=3, Error=0.025, Q=0.75, Loadings=0.6\n\n", file = conn)
  cat("scenarios:\n", file = conn)
  for (scenario in cfg$scenarios) {
    cat(
      "  - ",
      scenario$scenario,
      ": family=",
      scenario$scenario_family,
      ", mode=",
      scenario$scenario_mode,
      ", nfac=",
      scenario$scenario_nfac,
      ", Q2=",
      scenario$Q2,
      ", sigma_q=",
      paste(round(scenario$sigma_q, 4), collapse = ";"),
      ", factor_archetypes=",
      paste(scenario$factor_archetypes, collapse = ";"),
      ", active_gain=",
      scenario$active_gain,
      ", background_gain=",
      scenario$background_gain,
      ", dwell_frac=",
      paste(round(c(scenario$dwell_min_frac, scenario$dwell_max_frac), 4), collapse = ";"),
      ", eta=",
      paste(round(scenario$eta, 4), collapse = ";"),
      ", zeta=",
      paste(round(scenario$zeta, 4), collapse = ";"),
      "\n",
      sep = "",
      file = conn
    )
  }
  cat("\n", file = conn)

  for (pkg in required_packages_for_methods(cfg$methods)) {
    cat(pkg, ": ", as.character(utils::packageVersion(pkg)), "\n", sep = "", file = conn)
  }

  cat("\n", file = conn)
  utils::capture.output(sessionInfo(), file = conn)
}

run_driver <- function(smoke = FALSE) {
  limit_blas_threads()
  cfg <- build_benchmark_config(smoke = smoke)

  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(cfg$dry_run)) {
    scenario_grid <- build_scenario_grid(cfg)
    main_grid <- scenario_grid
    gimme_grid <- build_gimme_bounded_grid(cfg)
    utils::write.csv(flatten_scenario_grid_for_csv(main_grid), file.path(cfg$out_dir, "benchmark_grid_main_full.csv"), row.names = FALSE)
    utils::write.csv(flatten_scenario_grid_for_csv(gimme_grid), file.path(cfg$out_dir, "benchmark_grid_gimme_bounded.csv"), row.names = FALSE)
    grid_counts <- data.frame(
      scope = c("main_full_grid_per_method", "gimme_bounded_per_method"),
      rows = c(nrow(main_grid), nrow(gimme_grid)),
      stringsAsFactors = FALSE
    )
    utils::write.csv(grid_counts, file.path(cfg$out_dir, "benchmark_grid_counts.csv"), row.names = FALSE)
    write_metadata(cfg, cfg$out_dir)
    cat("Saved dry-run benchmark grids to ", cfg$out_dir, "\n", sep = "")
    return(invisible(NULL))
  }

  assert_required_packages(cfg$methods)

  results_df <- run_benchmark_suite(cfg)
  summary_df <- summarize_benchmark_results(results_df)

  utils::write.csv(results_df, file.path(cfg$out_dir, "benchmark_results.csv"), row.names = FALSE)
  utils::write.csv(results_df, file.path(cfg$out_dir, "checkpoint_results.csv"), row.names = FALSE)
  saveRDS(results_df, file.path(cfg$out_dir, "benchmark_results.rds"))
  utils::write.csv(summary_df, file.path(cfg$out_dir, "benchmark_summary.csv"), row.names = FALSE)
  if (cfg$smoke) {
    overview_path <- file.path(cfg$out_dir, "benchmark_overview.png")
    if (file.exists(overview_path)) {
      unlink(overview_path)
    }
  } else {
    make_overview_plot(summary_df, cfg$out_dir)
  }
  write_metadata(cfg, cfg$out_dir)

  cat("Saved benchmark outputs to ", cfg$out_dir, "\n", sep = "")
}

if (parse_bool_env("RUN_METHOD_BENCHMARK_SMOKE")) {
  run_driver(smoke = TRUE)
} else if (parse_bool_env("RUN_METHOD_BENCHMARK")) {
  run_driver(smoke = FALSE)
} else {
  cat(
    "Set RUN_METHOD_BENCHMARK_SMOKE=1 or RUN_METHOD_BENCHMARK=1 to run this script.\n",
    sep = ""
  )
}
