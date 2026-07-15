REVISION_JOB_IDS <- c(
  "empirical_cv",
  "t100_reuse_audit",
  "t100_dynega",
  "t100_static_oracle",
  "t100_graphicalvar_fallback",
  "t100_gimme_fallback",
  "t25_dynega",
  "t25_graphicalvar",
  "t25_gimme",
  "t25_static_oracle",
  "challenge_dynega",
  "challenge_graphicalvar",
  "challenge_static_oracle",
  "aggregate_results",
  "rebuild_figures_tables",
  "integrate_manuscript"
)

REVISION_COMMUNITY_ALGORITHMS <- c("louvain", "leiden", "walktrap")
REVISION_T100_N_EMBED <- c(5L, 10L, 15L, 25L, 50L)
REVISION_T25_N_EMBED <- c(5L, 6L, 10L, 12L)
REVISION_SEED_ORIGIN <- 20260715L
REVISION_CONFIG_VERSION <- "revision_v2"

revision_scalar_or <- function(value, fallback) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1]])) {
    return(fallback)
  }
  value
}

revision_collapse <- function(values) {
  paste(values, collapse = ";")
}

revision_parse_numeric_vector <- function(value) {
  as.numeric(strsplit(as.character(value), ";", fixed = TRUE)[[1]])
}

revision_parse_character_vector <- function(value) {
  strsplit(as.character(value), ";", fixed = TRUE)[[1]]
}

revision_factor_archetypes <- function() {
  data.frame(
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
}

revision_scenario_families <- function() {
  data.frame(
    scenario_family = c(
      "old_low_single_stable",
      "old_high_single_stable",
      "old_high_switch_stable",
      "old_high_switch_volatile",
      "oai_ranked_persistence",
      "oai_broad_geometry"
    ),
    sigma_rule = c("scaled", "scaled", "scaled", "scaled", "fixed", "fixed"),
    sigma_value = c(0.50, 1.00, 1.00, 1.25, 0.25, 0.35),
    active_gain = c(0.35, 1.00, 1.00, 1.25, 1.00, 1.10),
    background_gain = c(0.05, 0.08, 0.08, 0.08, 0.08, 0.10),
    dwell_min_frac = c(1.00, 1.00, 0.20, 0.03, 1.00, 0.12),
    dwell_max_frac = c(1.00, 1.00, 0.45, 0.10, 1.00, 0.30),
    switching = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
}

revision_factor_sigma <- function(family_row, archetypes, n_factors) {
  if (identical(family_row$sigma_rule[[1]], "fixed")) {
    return(rep(family_row$sigma_value[[1]], n_factors))
  }
  archetypes$sigma_q[seq_len(n_factors)] * family_row$sigma_value[[1]]
}

revision_stability <- function(eta, zeta, dt) {
  max_real <- max(vapply(
    seq_along(eta),
    function(index) {
      transition <- matrix(
        c(0, 1, eta[[index]], zeta[[index]]),
        nrow = 2L,
        byrow = TRUE
      )
      max(Re(eigen(transition, only.values = TRUE)$values))
    },
    numeric(1)
  ))
  spectral_radius <- exp(dt * max_real)
  data.frame(
    continuous_max_real_eigenvalue = max_real,
    discrete_spectral_radius = spectral_radius,
    continuous_stable = max_real < 0,
    discrete_stable = spectral_radius < 1,
    stability_class = if (max_real < 0 && spectral_radius < 1) {
      "asymptotically_stable"
    } else {
      "unstable"
    },
    stringsAsFactors = FALSE
  )
}

revision_scenario_row <- function(family_row, n_factors, dt) {
  archetypes <- revision_factor_archetypes()
  factor_rows <- archetypes[seq_len(n_factors), , drop = FALSE]
  eta <- if (grepl("^oai_", family_row$scenario_family[[1]])) {
    -0.05 * seq_len(n_factors)
  } else {
    factor_rows$eta
  }
  zeta <- if (grepl("^oai_", family_row$scenario_family[[1]])) {
    0.06 + 0.02 * seq_len(n_factors)
  } else {
    factor_rows$zeta
  }
  sigma_q <- revision_factor_sigma(family_row, archetypes, n_factors)
  stability <- revision_stability(eta, zeta, dt)

  cbind(
    data.frame(
      scenario = paste0(family_row$scenario_family[[1]], "_", n_factors, "d"),
      scenario_family = family_row$scenario_family[[1]],
      scenario_mode = family_row$scenario_family[[1]],
      scenario_nfac = as.integer(n_factors),
      Q2 = c(`2` = 1.5, `3` = 2.5)[[as.character(n_factors)]],
      eta = revision_collapse(eta),
      zeta = revision_collapse(zeta),
      sigma_q = revision_collapse(sigma_q),
      factor_archetypes = revision_collapse(factor_rows$factor_archetype),
      active_gain = family_row$active_gain[[1]],
      background_gain = family_row$background_gain[[1]],
      dwell_min_frac = family_row$dwell_min_frac[[1]],
      dwell_max_frac = family_row$dwell_max_frac[[1]],
      switching = family_row$switching[[1]],
      stringsAsFactors = FALSE
    ),
    stability
  )
}

revision_scenarios <- function(dt = 0.4) {
  families <- revision_scenario_families()
  rows <- vector("list", nrow(families) * 2L)
  row_index <- 1L
  for (family_index in seq_len(nrow(families))) {
    for (n_factors in c(2L, 3L)) {
      rows[[row_index]] <- revision_scenario_row(
        families[family_index, , drop = FALSE],
        n_factors,
        dt
      )
      row_index <- row_index + 1L
    }
  }
  do.call(rbind, rows)
}

revision_challenge_variants <- function() {
  data.frame(
    challenge_id = c(
      "ideal_baseline",
      "independent_initialization",
      "smooth_activity_gate",
      "cross_loadings",
      "residual_equicorrelation",
      "latent_shock_equicorrelation",
      "softmax_bounded"
    ),
    initialization = c(
      "legacy_fixed",
      "stationary_if_stable_else_uniform",
      rep("legacy_fixed", 5L)
    ),
    stable_burn_in = c(0L, 25L, rep(0L, 5L)),
    antidamped_x0 = c(NA, "U[-0.25,0.25]", rep(NA, 5L)),
    antidamped_v0 = c(NA, "U[-2,2]", rep(NA, 5L)),
    activity_gate = c("step", "step", "half_cosine", rep("step", 4L)),
    gate_transition_observations = c(0L, 0L, 5L, rep(0L, 4L)),
    secondary_loading_distribution = c(rep(NA, 3L), "N(0,0.05^2)", rep(NA, 3L)),
    secondary_loading_limit = c(rep(NA, 3L), "[-0.10,0.10]", rep(NA, 3L)),
    primary_loading_constraint = c(rep(NA, 3L), "primary_largest", rep(NA, 3L)),
    residual_rho = c(rep(0, 4L), 0.10, 0, 0),
    latent_shock_rho = c(rep(0, 5L), 0.15, 0),
    observation_link = c(rep("linear", 6L), "softmax"),
    softmax_alpha = c(rep(NA_real_, 6L), 0),
    softmax_tau = c(rep(NA_real_, 6L), 1),
    logit_reference = c(rep(NA, 6L), "ideal_z_panel"),
    representations = c(rep("linear_z", 6L), "softmax_raw;softmax_clr"),
    stringsAsFactors = FALSE
  )
}

revision_candidate_fallback <- function() {
  links <- c("identity", "logistic", "logistic_renorm", "softmax")
  temperatures <- c(0.75, 1.00, 1.50, 2.00)
  intercepts <- c("none", "empirical_log_mean")
  rows <- vector("list", length(links) * length(temperatures) * length(intercepts))
  row_index <- 1L
  for (link in links) {
    for (temperature in temperatures) {
      for (intercept in intercepts) {
        rows[[row_index]] <- data.frame(
          candidate_id = row_index,
          stage = "L_link",
          candidate_label = sprintf(
            "link_%s_temp_%.2f_intercept_%s",
            link,
            temperature,
            intercept
          ),
          link = link,
          temperature = temperature,
          intercept_mode = intercept,
          candidate_rank = NA_integer_,
          candidate_source = "fixed_fallback",
          stringsAsFactors = FALSE
        )
        row_index <- row_index + 1L
      }
    }
  }
  do.call(rbind, rows)
}

revision_candidate_columns <- function() {
  c(
    "candidate_id",
    "stage",
    "candidate_label",
    "link",
    "temperature",
    "intercept_mode",
    "candidate_rank"
  )
}

revision_empirical_candidates <- function(repo_root = ".") {
  source_path <- file.path(
    repo_root,
    "simulation_outputs",
    "empirical_link_options",
    "oai-large-100",
    "metric_comparison_long.csv"
  )
  if (!file.exists(source_path)) {
    return(revision_candidate_fallback())
  }

  source_rows <- utils::read.csv(source_path, check.names = FALSE)
  required <- revision_candidate_columns()
  if (!all(required %in% names(source_rows))) {
    return(revision_candidate_fallback())
  }
  candidates <- unique(source_rows[, required, drop = FALSE])
  candidates <- candidates[order(candidates$candidate_id), , drop = FALSE]
  if (nrow(candidates) != 32L) {
    return(revision_candidate_fallback())
  }
  candidates$candidate_source <- "discovered_metric_comparison_long"
  rownames(candidates) <- NULL
  candidates
}

revision_hash_int <- function(value, modulus = 2147483629) {
  codepoints <- utf8ToInt(enc2utf8(as.character(value)))
  hash <- 0
  for (codepoint in codepoints) {
    hash <- (hash * 131 + codepoint) %% modulus
  }
  as.integer(hash)
}

revision_seed_from_key <- function(panel_key) {
  modulus <- 2147483646
  as.integer(
    (as.double(REVISION_SEED_ORIGIN) + as.double(revision_hash_int(panel_key))) %%
      modulus + 1
  )
}

revision_default_config_hash <- function(job_id) {
  revision_assert_job_id(job_id)
  material <- paste(
    REVISION_CONFIG_VERSION,
    job_id,
    revision_collapse(REVISION_COMMUNITY_ALGORITHMS),
    revision_collapse(REVISION_T100_N_EMBED),
    revision_collapse(REVISION_T25_N_EMBED),
    REVISION_SEED_ORIGIN,
    sep = "|"
  )
  sprintf("%08x", revision_hash_int(material))
}

revision_assert_job_id <- function(job_id) {
  if (length(job_id) != 1L || is.na(job_id) || !(job_id %in% REVISION_JOB_IDS)) {
    stop("Unknown revision job: ", paste(job_id, collapse = ", "), call. = FALSE)
  }
  invisible(job_id)
}

revision_horizon_metadata <- function(time_points, dt) {
  data.frame(
    n_timepoints = as.integer(time_points),
    n_transitions = as.integer(time_points - 1L),
    dt = as.numeric(dt),
    nominal_horizon = as.numeric(time_points * dt),
    observed_horizon = as.numeric((time_points - 1L) * dt),
    stringsAsFactors = FALSE
  )
}

revision_panel_keys <- function(grid, scope) {
  paste(
    REVISION_CONFIG_VERSION,
    scope,
    grid$scenario,
    paste0("N", grid$N),
    paste0("T", grid$TimeP),
    paste0("K", grid$NVAR),
    sprintf("E%.3f", grid$Error_sd),
    sprintf("L%.3f", grid$Loadings),
    sprintf("Q%.2f", grid$Q),
    paste0("R", sprintf("%03d", grid$rep)),
    sep = "|"
  )
}

revision_add_panel_identity <- function(grid, scope) {
  grid$panel_key <- revision_panel_keys(grid, scope)
  grid$representation_key <- paste(grid$panel_key, grid$representation, sep = "|")
  grid$seed_base <- vapply(grid$panel_key, revision_seed_from_key, integer(1))
  grid
}

revision_expand_scenarios <- function(design, scenarios) {
  scenario_index <- match(design$scenario, scenarios$scenario)
  scenario_columns <- setdiff(names(scenarios), "scenario")
  for (column in scenario_columns) {
    design[[column]] <- scenarios[[column]][scenario_index]
  }
  design
}

revision_add_common_panel_columns <- function(grid, time_points, grid_scope) {
  horizon <- revision_horizon_metadata(time_points, 0.4)
  grid$TimeP <- as.integer(time_points)
  grid$NVAR <- 4L
  grid$Error <- as.numeric(grid$Error_sd)
  grid$Error_variance <- as.numeric(grid$Error_sd)^2
  grid$Q <- 0.75
  grid$base_err <- as.numeric(grid$Error_sd)
  grid$benchmark_grid <- grid_scope
  grid$grid_scope <- grid_scope
  grid$n_timepoints <- horizon$n_timepoints[[1]]
  grid$n_transitions <- horizon$n_transitions[[1]]
  grid$dt <- horizon$dt[[1]]
  grid$nominal_horizon <- horizon$nominal_horizon[[1]]
  grid$observed_horizon <- horizon$observed_horizon[[1]]
  grid$n_embed_candidates <- revision_collapse(
    if (time_points == 25L) REVISION_T25_N_EMBED else REVISION_T100_N_EMBED
  )
  grid
}

revision_matched_panel_grid <- function(time_points, reps = 100L) {
  if (!(time_points %in% c(25L, 100L))) {
    stop("Matched revision grid supports TimeP 25 or 100.", call. = FALSE)
  }
  scenarios <- revision_scenarios(dt = 0.4)
  design <- expand.grid(
    scenario = scenarios$scenario,
    N = c(50L, 100L),
    Error_sd = c(0.125, 0.25, 0.50),
    Loadings = c(0.4, 0.6, 0.8),
    rep = seq_len(as.integer(reps)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  design <- revision_expand_scenarios(design, scenarios)
  design <- revision_add_common_panel_columns(
    design,
    time_points,
    paste0("revision_matched_t", time_points)
  )
  design$representation <- "linear_z"
  design <- revision_add_panel_identity(design, "matched")
  design[order(design$scenario, design$N, design$Error_sd, design$Loadings, design$rep), ]
}

revision_challenge_base_grid <- function(reps = 50L) {
  scenarios <- revision_scenarios(dt = 0.4)
  variants <- revision_challenge_variants()
  design <- expand.grid(
    scenario = scenarios$scenario,
    challenge_id = variants$challenge_id,
    rep = seq_len(as.integer(reps)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  design$N <- 100L
  design$Error_sd <- 0.25
  design$Loadings <- 0.6
  design <- revision_expand_scenarios(design, scenarios)
  variant_index <- match(design$challenge_id, variants$challenge_id)
  for (column in setdiff(names(variants), "challenge_id")) {
    design[[column]] <- variants[[column]][variant_index]
  }
  design <- revision_add_common_panel_columns(design, 100L, "revision_challenge")
  design$representation <- "linear_z"
  design$panel_key <- paste(
    revision_panel_keys(design, "challenge"),
    design$challenge_id,
    sep = "|"
  )
  design$seed_base <- vapply(design$panel_key, revision_seed_from_key, integer(1))
  design
}

revision_challenge_panel_grid <- function(reps = 50L) {
  base <- revision_challenge_base_grid(reps)
  ordinary <- base[base$challenge_id != "softmax_bounded", , drop = FALSE]
  softmax <- base[base$challenge_id == "softmax_bounded", , drop = FALSE]
  softmax_raw <- softmax
  softmax_raw$representation <- "softmax_raw"
  softmax_clr <- softmax
  softmax_clr$representation <- "softmax_clr"
  combined <- rbind(ordinary, softmax_raw, softmax_clr)
  combined$representation_key <- paste(
    combined$panel_key,
    combined$representation,
    sep = "|"
  )
  combined[order(
    combined$scenario,
    combined$challenge_id,
    combined$rep,
    combined$representation
  ), ]
}

revision_empirical_cv_plan <- function(repo_root = ".") {
  candidates <- revision_empirical_candidates(repo_root)
  training <- merge(
    expand.grid(
      fold = seq_len(5L),
      cv_tau = c(0.75, 1.00, 1.25),
      candidate_id = candidates$candidate_id,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ),
    candidates,
    by = "candidate_id",
    sort = FALSE
  )
  training$training_clip_count <- 80L
  training$heldout_clip_count <- 20L
  training$training_panel_replicates <- 1L
  training$selection_rule <- "minimum_training_only_continuous_loss"
  training$panel_key <- sprintf(
    "%s|empirical_cv|train|F%d|C%02d|TAU%.2f",
    REVISION_CONFIG_VERSION,
    training$fold,
    training$candidate_id,
    training$cv_tau
  )
  training$seed_base <- vapply(training$panel_key, revision_seed_from_key, integer(1))

  heldout <- expand.grid(
    fold = seq_len(5L),
    heldout_rep = seq_len(100L),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  heldout$training_clip_count <- 80L
  heldout$heldout_clip_count <- 20L
  heldout$selected_candidate_id <- NA_integer_
  heldout$selected_cv_tau <- NA_real_
  heldout$panel_key <- sprintf(
    "%s|empirical_cv|heldout|F%d|R%03d",
    REVISION_CONFIG_VERSION,
    heldout$fold,
    heldout$heldout_rep
  )
  heldout$seed_base <- vapply(heldout$panel_key, revision_seed_from_key, integer(1))

  list(candidates = candidates, training = training, heldout = heldout)
}

revision_method_ids <- function(job_id) {
  if (grepl("dynega$", job_id)) {
    return("dynEGA")
  }
  if (grepl("graphicalvar", job_id)) {
    return("graphicalVAR")
  }
  if (grepl("gimme", job_id)) {
    return("gimme")
  }
  if (grepl("static_oracle$", job_id)) {
    return(c("static_correlation", "oracle_blocks"))
  }
  character()
}

revision_readout_specs <- function(method_ids) {
  if (length(method_ids) == 0L) {
    return(data.frame())
  }
  specs <- expand.grid(
    method_id = method_ids,
    community_algorithm = REVISION_COMMUNITY_ALGORITHMS,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  specs$primary_readout <- specs$community_algorithm == "louvain"
  specs$resolution <- ifelse(specs$primary_readout, 1.0, NA_real_)
  specs$dynEGA_uni_method <- ifelse(specs$method_id == "dynEGA", "expand", NA)
  specs$correlation_louvain_sensitivity_resolution <- ifelse(
    specs$method_id == "static_correlation",
    0.95,
    NA_real_
  )
  specs
}

revision_job_catalog <- function() {
  data.frame(
    job_id = REVISION_JOB_IDS,
    stage = c(
      "empirical",
      rep("t100", 5L),
      rep("t25", 4L),
      rep("challenge", 3L),
      "aggregate",
      "publish",
      "manuscript"
    ),
    dependencies = c(
      "",
      "",
      "t100_reuse_audit",
      "t100_reuse_audit",
      "t100_reuse_audit",
      "t100_reuse_audit",
      rep("", 7L),
      paste(REVISION_JOB_IDS[seq_len(13L)], collapse = ";"),
      "aggregate_results",
      "rebuild_figures_tables"
    ),
    runner_kind = c(
      "planned_empirical_cv",
      "reuse_audit",
      "existing_core",
      "planned_static_oracle",
      "existing_core",
      "existing_core",
      "existing_core",
      "existing_core",
      "existing_core",
      "planned_static_oracle",
      "planned_challenge",
      "planned_challenge",
      "planned_challenge",
      "result_builder",
      "planned_rebuild",
      "planned_manuscript_integration"
    ),
    base_panels = c(
      980L,
      21600L,
      rep(21600L, 8L),
      rep(4200L, 3L),
      0L,
      0L,
      0L
    ),
    representation_panels = c(
      980L,
      21600L,
      rep(21600L, 8L),
      rep(4800L, 3L),
      0L,
      0L,
      0L
    ),
    structure_fits = c(
      0L,
      0L,
      21600L,
      43200L,
      21600L,
      21600L,
      21600L,
      21600L,
      21600L,
      43200L,
      4800L,
      4800L,
      9600L,
      0L,
      0L,
      0L
    ),
    community_readouts = c(
      0L,
      0L,
      64800L,
      129600L,
      64800L,
      64800L,
      64800L,
      64800L,
      64800L,
      129600L,
      14400L,
      14400L,
      28800L,
      0L,
      0L,
      0L
    ),
    stringsAsFactors = FALSE
  )
}

revision_job_row <- function(job_id) {
  revision_assert_job_id(job_id)
  catalog <- revision_job_catalog()
  catalog[catalog$job_id == job_id, , drop = FALSE]
}

revision_job_panels <- function(job_id, repo_root = ".") {
  revision_assert_job_id(job_id)
  if (identical(job_id, "empirical_cv")) {
    return(revision_empirical_cv_plan(repo_root))
  }
  if (grepl("^t100_", job_id)) {
    return(revision_matched_panel_grid(100L))
  }
  if (grepl("^t25_", job_id)) {
    return(revision_matched_panel_grid(25L))
  }
  if (grepl("^challenge_", job_id)) {
    return(revision_challenge_panel_grid())
  }
  NULL
}

revision_job_plan <- function(job_id, repo_root = ".") {
  job <- revision_job_row(job_id)
  methods <- revision_method_ids(job_id)
  list(
    job = job,
    methods = methods,
    readouts = revision_readout_specs(methods),
    panels = revision_job_panels(job_id, repo_root),
    config_hash = revision_default_config_hash(job_id),
    seed_origin = REVISION_SEED_ORIGIN
  )
}

revision_validate_empirical_plan <- function(plan) {
  if (nrow(plan$candidates) != 32L) {
    stop("Empirical CV must preserve 32 calibration candidates.", call. = FALSE)
  }
  if (nrow(plan$training) != 480L) {
    stop("Empirical CV must contain 480 training combinations.", call. = FALSE)
  }
  if (nrow(plan$heldout) != 500L) {
    stop("Empirical CV must contain 500 held-out evaluations.", call. = FALSE)
  }
  invisible(TRUE)
}

revision_validate_panel_plan <- function(job_id, plan) {
  panel_grid <- plan$panels
  expected <- plan$job$representation_panels[[1]]
  if (nrow(panel_grid) != expected) {
    stop(
      sprintf("%s has %d representation panels; expected %d.", job_id, nrow(panel_grid), expected),
      call. = FALSE
    )
  }
  base_count <- length(unique(panel_grid$panel_key))
  if (base_count != plan$job$base_panels[[1]]) {
    stop(
      sprintf("%s has %d base panels; expected %d.", job_id, base_count, plan$job$base_panels[[1]]),
      call. = FALSE
    )
  }
  expected_readouts <- nrow(panel_grid) * nrow(plan$readouts)
  if (expected_readouts != plan$job$community_readouts[[1]]) {
    stop(
      sprintf(
        "%s has %d planned readouts; expected %d.",
        job_id,
        expected_readouts,
        plan$job$community_readouts[[1]]
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

revision_validate_job_plan <- function(plan) {
  job_id <- plan$job$job_id[[1]]
  if (identical(job_id, "empirical_cv")) {
    return(revision_validate_empirical_plan(plan$panels))
  }
  if (!is.null(plan$panels) && !identical(job_id, "t100_reuse_audit")) {
    return(revision_validate_panel_plan(job_id, plan))
  }
  if (identical(job_id, "t100_reuse_audit") && nrow(plan$panels) != 21600L) {
    stop("T100 reuse audit must address 21,600 expected panels.", call. = FALSE)
  }
  invisible(TRUE)
}

revision_core_scenarios <- function() {
  scenarios <- revision_scenarios(dt = 0.4)
  lapply(seq_len(nrow(scenarios)), function(index) {
    row <- scenarios[index, , drop = FALSE]
    list(
      scenario = row$scenario[[1]],
      scenario_family = row$scenario_family[[1]],
      scenario_mode = row$scenario_mode[[1]],
      scenario_nfac = as.integer(row$scenario_nfac[[1]]),
      Q2 = as.numeric(row$Q2[[1]]),
      eta = revision_parse_numeric_vector(row$eta[[1]]),
      zeta = revision_parse_numeric_vector(row$zeta[[1]]),
      sigma_q = revision_parse_numeric_vector(row$sigma_q[[1]]),
      factor_archetypes = revision_parse_character_vector(row$factor_archetypes[[1]]),
      active_gain = as.numeric(row$active_gain[[1]]),
      background_gain = as.numeric(row$background_gain[[1]]),
      dwell_min_frac = as.numeric(row$dwell_min_frac[[1]]),
      dwell_max_frac = as.numeric(row$dwell_max_frac[[1]]),
      switching = isTRUE(row$switching[[1]])
    )
  })
}

revision_core_panel_grid <- function(panel_grid) {
  core <- panel_grid
  core$eta <- lapply(core$eta, revision_parse_numeric_vector)
  core$zeta <- lapply(core$zeta, revision_parse_numeric_vector)
  core$sigma_q <- lapply(core$sigma_q, revision_parse_numeric_vector)
  core$factor_archetypes <- lapply(
    core$factor_archetypes,
    revision_parse_character_vector
  )
  core
}

revision_required_upstream_jobs <- function() {
  REVISION_JOB_IDS[seq_len(13L)]
}
