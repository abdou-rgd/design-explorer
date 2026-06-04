# tests/testthat/test-sse_mrgsolve_exposure_preflight.R

library(testthat)

if (!exists("PROJECT_ROOT", inherits = TRUE)) {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  PROJECT_ROOT <- if (file.exists(file.path(cwd, "DESCRIPTION"))) {
    cwd
  } else {
    normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
  }
  Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
}

source(file.path(PROJECT_ROOT, "R", "sse_mrgsolve_exposure.R"))
source(file.path(PROJECT_ROOT, "R", "mrgsolve_bridge.R"))

make_preflight_patab <- function() {
  tibble::tibble(
    table = "pk_individuals.tab",
    sample = c(1L, 1L),
    kind = c("simulation", "estimation"),
    source_file = c("pk_individuals.tab-sim-1", "pk_individuals.tab-1"),
    ID = c(101, 101),
    ARM = c(0, 0),
    CL = c(1.0, 1.1),
    VC = c(20, 19),
    ETA1 = c(0.1, 0.2),
    NOTE = c("sim", "est")
  )
}

make_preflight_ready <- function() {
  build_sse_mrgsolve_exposure_preflight(
    individual_pk_data = make_preflight_patab(),
    model_param_names = c("CL", "V"),
    capture_names = "CP",
    concentration_output = "CP",
    selected_mapping = c(CL = "CL", V = "VC"),
    required_params = c("CL", "V")
  )
}

make_preflight_doses <- function() {
  data.frame(
    ID = 0L,
    time = 0,
    amt = 100,
    cmt = 1L,
    evid = 1L,
    rate = 0,
    stringsAsFactors = FALSE
  )
}

test_that("sse_mrgsolve_candidate_columns keeps numeric patab inputs only", {
  cols <- sse_mrgsolve_candidate_columns(make_preflight_patab())

  expect_setequal(cols, c("CL", "VC", "ETA1"))
})

test_that("build_sse_mrgsolve_parameter_mapping proposes exact matches", {
  mapping <- build_sse_mrgsolve_parameter_mapping(
    make_preflight_patab(),
    model_param_names = c("CL", "VC", "KA")
  )

  expect_equal(mapping$model_param, c("CL", "VC", "KA"))
  expect_equal(mapping$patab_column, c("CL", "VC", NA_character_))
  expect_equal(mapping$status, c("mapped", "mapped", "unmapped"))
  expect_equal(mapping$source, c("auto", "auto", "none"))

  empty <- build_sse_mrgsolve_parameter_mapping(
    make_preflight_patab(),
    model_param_names = character()
  )
  expect_equal(nrow(empty), 0L)
  expect_equal(
    names(empty),
    c("model_param", "patab_column", "status", "source")
  )
})

test_that("build_sse_mrgsolve_parameter_mapping accepts manual overrides", {
  mapping <- build_sse_mrgsolve_parameter_mapping(
    make_preflight_patab(),
    model_param_names = c("CL", "KA"),
    selected_mapping = c(CL = "VC", KA = "ETA1")
  )

  expect_equal(mapping$patab_column, c("VC", "ETA1"))
  expect_equal(mapping$source, c("manual", "manual"))
})

test_that("build_sse_mrgsolve_exposure_preflight reports ready and blocked states", {
  ready <- build_sse_mrgsolve_exposure_preflight(
    individual_pk_data = make_preflight_patab(),
    model_param_names = c("CL", "VC", "KA"),
    capture_names = c("CP", "RESP"),
    concentration_output = "CP",
    selected_mapping = c(CL = "CL", VC = "VC"),
    required_params = c("CL", "VC")
  )

  expect_equal(ready$status, "ready")
  expect_equal(ready$concentration_output, "CP")
  expect_equal(ready$n_mapped_required, 2L)
  expect_equal(ready$n_samples, 1L)
  expect_equal(ready$n_ids, 1L)

  needs_mapping <- build_sse_mrgsolve_exposure_preflight(
    individual_pk_data = make_preflight_patab(),
    model_param_names = c("CL", "VC", "KA"),
    capture_names = c("CP"),
    concentration_output = "CP",
    required_params = c("CL", "KA")
  )
  expect_equal(needs_mapping$status, "needs_mapping")
  expect_equal(needs_mapping$missing_required_params, "KA")

  needs_output <- build_sse_mrgsolve_exposure_preflight(
    individual_pk_data = make_preflight_patab(),
    model_param_names = c("CL"),
    capture_names = c("CP"),
    concentration_output = "BAD",
    required_params = "CL"
  )
  expect_equal(needs_output$status, "needs_output")

  needs_pk <- build_sse_mrgsolve_exposure_preflight(
    individual_pk_data = NULL,
    model_param_names = c("CL"),
    capture_names = c("CP"),
    concentration_output = "CP",
    required_params = "CL"
  )
  expect_equal(needs_pk$status, "needs_individual_pk")
})

test_that("build_sse_mrgsolve_sanity_inputs prepares idata and subject events", {
  inputs <- build_sse_mrgsolve_sanity_inputs(
    individual_pk_data = make_preflight_patab(),
    preflight = make_preflight_ready(),
    dosing_events = make_preflight_doses(),
    kind = "estimation",
    max_ids = 1L
  )

  expect_equal(inputs$status, "ready")
  expect_equal(names(inputs$idata), c("ID", "CL", "V"))
  expect_equal(inputs$idata$ID, 101)
  expect_equal(inputs$idata$CL, 1.1)
  expect_equal(inputs$idata$V, 19)
  expect_equal(inputs$events$ID, 101L)
  expect_equal(inputs$events$amt, 100)
})

test_that("run_sse_mrgsolve_sanity_check runs a small model and validates output", {
  skip_if_not_installed("mrgsolve")

  code <- "
$PARAM CL = 1, V = 10
$CMT CENT
$ODE
dxdt_CENT = -(CL / V) * CENT;
$TABLE
double CP = CENT / V;
$CAPTURE CP
"
  mod <- compile_mrgsolve_model(code, model_name = "sse_sanity_test")
  skip_if(is.character(mod), mod)

  result <- run_sse_mrgsolve_sanity_check(
    mod = mod,
    individual_pk_data = make_preflight_patab(),
    preflight = make_preflight_ready(),
    dosing_events = make_preflight_doses(),
    kind = "estimation",
    max_ids = 1L,
    end_time = 24,
    delta = 1
  )

  expect_equal(result$status, "ok")
  expect_equal(result$output, "CP")
  expect_equal(result$n_ids, 1L)
  expect_gt(result$n_rows, 1L)
  expect_true(result$value_range[2] > result$value_range[1])
  expect_true(all(c("ID", "time", "CP") %in% names(result$preview)))
})

test_that("compute_sse_mrgsolve_exposure_metrics summarizes one profile", {
  profile <- data.frame(
    sample = 1L,
    kind = "estimation",
    ID = 101L,
    time = c(0, 1, 2, 3),
    CP = c(0, 4, 2, 1)
  )

  metrics <- compute_sse_mrgsolve_exposure_metrics(
    profile,
    concentration_output = "CP",
    trough_times = 3
  )

  expect_setequal(metrics$metric, c("AUC", "Cmax", "Tmax", "Ctrough"))
  expect_equal(metrics$value[metrics$metric == "AUC"], 6.5)
  expect_equal(metrics$value[metrics$metric == "Cmax"], 4)
  expect_equal(metrics$value[metrics$metric == "Tmax"], 1)
  expect_equal(metrics$value[metrics$metric == "Ctrough"], 1)
})

test_that("compute_sse_mrgsolve_exposure_recovery compares sim and est exposures", {
  skip_if_not_installed("mrgsolve")

  code <- "
$PARAM CL = 1, V = 10
$CMT CENT
$ODE
dxdt_CENT = -(CL / V) * CENT;
$TABLE
double CP = CENT / V;
$CAPTURE CP
"
  mod <- compile_mrgsolve_model(code, model_name = "sse_recovery_test")
  skip_if(is.character(mod), mod)

  exposure <- compute_sse_mrgsolve_exposure_recovery(
    mod = mod,
    individual_pk_data = make_preflight_patab(),
    preflight = make_preflight_ready(),
    dosing_events = make_preflight_doses(),
    max_samples = 1L,
    max_ids = 1L,
    end_time = 24,
    delta = 1,
    trough_times = 24
  )

  expect_s3_class(exposure, "tbl_df")
  expect_setequal(exposure$metric, c("AUC", "Cmax", "Tmax", "Ctrough"))
  expect_true(all(c(
    "sample", "ID", "metric", "sim_value", "est_value",
    "absolute_error", "relative_error"
  ) %in% names(exposure)))
  expect_true(all(is.finite(exposure$sim_value)))
  expect_true(all(is.finite(exposure$est_value)))
  expect_true(any(abs(exposure$relative_error) > 0))
})
