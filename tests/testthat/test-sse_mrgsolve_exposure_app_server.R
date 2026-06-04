# tests/testthat/test-sse_mrgsolve_exposure_app_server.R

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

source(file.path(PROJECT_ROOT, "R", "source_core.R"))
source_core(PROJECT_ROOT)
source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"))
source(file.path(PROJECT_ROOT, "app", "R", "mod_sse_analysis.R"))

make_app_exposure_patab <- function() {
  tibble::tibble(
    table = "pk_individuals.tab",
    sample = c(1L, 1L),
    kind = c("simulation", "estimation"),
    source_file = c("pk_individuals.tab-sim-1", "pk_individuals.tab-1"),
    ID = c(101L, 101L),
    CL = c(1.0, 1.2),
    VC = c(10, 11)
  )
}

make_app_exposure_sse <- function() {
  tibble::tibble(
    sample = 1L,
    minimization_successful = 1L,
    converged = TRUE,
    covariance_step_successful = 1L,
    estimate_near_boundary = 0L,
    rounding_errors = 0L,
    THETA1 = 1
  )
}

make_app_exposure_doses <- function() {
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

test_that("SSE diagnostics PK exposure server path runs sanity and caches results", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("mrgsolve")

  suppressWarnings(library(shiny))
  suppressWarnings(library(DT))

  code <- "
$PARAM CL = 1, V = 10
$CMT CENT
$ODE
dxdt_CENT = -(CL / V) * CENT;
$TABLE
double CP = CENT / V;
$CAPTURE CP
"
  mod <- compile_mrgsolve_model(
    code,
    model_name = paste0("sse_app_exposure_", as.integer(Sys.time()))
  )
  skip_if(is.character(mod), mod)

  mrgsolve_state <- list(
    model = shiny::reactiveVal(mod),
    compile_error = shiny::reactiveVal(NULL),
    model_code = shiny::reactiveVal(code),
    model_hash = shiny::reactiveVal(mrgsolve_code_hash(code)),
    param_names = shiny::reactiveVal(c("CL", "V")),
    capture_names = shiny::reactiveVal("CP"),
    cmt_names = shiny::reactiveVal("CENT"),
    dose_events = shiny::reactiveVal(make_app_exposure_doses()),
    is_compiled = shiny::reactiveVal(TRUE)
  )

  shiny::testServer(
    mod_sse_analysis_server,
    args = list(
      sse_a_shared = shiny::reactive(make_app_exposure_sse()),
      sse_b_shared = shiny::reactive(NULL),
      name_a = shiny::reactive("Original"),
      name_b = shiny::reactive("Optimized"),
      true_vals = shiny::reactive(stats::setNames(numeric(), character())),
      param_labels = shiny::reactive(NULL),
      individual_pk_shared = shiny::reactive(make_app_exposure_patab()),
      mrgsolve_state = mrgsolve_state
    ),
    {
      session$setInputs(
        active_view = "pk_exposure",
        pk_exposure_output = "CP",
        pk_exposure_params = c("CL", "V"),
        pk_map_1 = "CL",
        pk_map_2 = "VC"
      )
      session$flushReact()

      preflight <- pk_exposure_preflight()
      expect_equal(preflight$status, "ready")
      expect_equal(preflight$concentration_output, "CP")
      expect_equal(
        preflight$mapping$patab_column[
          match(c("CL", "V"), preflight$mapping$model_param)
        ],
        c("CL", "VC")
      )

      session$setInputs(pk_exposure_run_sanity = 1L)
      session$flushReact()

      sanity <- pk_exposure_sanity()
      expect_equal(sanity$status, "ok")
      expect_equal(sanity$output, "CP")

      session$setInputs(
        pk_exposure_max_samples = 1L,
        pk_exposure_max_ids = 1L,
        pk_exposure_delta = 1,
        pk_exposure_trough_time = 24,
        pk_exposure_compute = 1L
      )
      session$flushReact()

      cache <- pk_exposure_cache()
      expect_type(cache$key, "character")
      expect_s3_class(cache$result, "tbl_df")
      expect_setequal(cache$result$metric, c("AUC", "Cmax", "Tmax", "Ctrough"))
      expect_true(all(c(
        "sample", "ID", "metric", "sim_value", "est_value",
        "absolute_error", "relative_error"
      ) %in% names(cache$result)))
      expect_true(any(abs(cache$result$relative_error) > 0))
    }
  )
})
