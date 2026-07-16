# tests/testthat/test-mrgsolve_shared_contract.R

library(testthat)

if (
  !exists("PROJECT_ROOT", inherits = TRUE) ||
    !file.exists(file.path(PROJECT_ROOT, "DESCRIPTION"))
) {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  PROJECT_ROOT <- if (file.exists(file.path(cwd, "DESCRIPTION"))) {
    cwd
  } else {
    normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
  }
  Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
}

read_app_file <- function(path) {
  paste(readLines(file.path(PROJECT_ROOT, path), warn = FALSE), collapse = "\n")
}

test_that("mod_mrgsolve_server exposes a shared compiled-model contract", {
  src <- read_app_file(file.path("app", "R", "mod_mrgsolve.R"))

  expected_fields <- c(
    "model",
    "compile_error",
    "model_code",
    "model_hash",
    "param_names",
    "capture_names",
    "cmt_names",
    "dose_events",
    "is_compiled",
    "status",
    "sim_data",
    "is_available",
    "dose_times",
    "tier",
    "warning_reason"
  )

  for (field in expected_fields) {
    expect_match(
      src,
      paste0("\\b", field, "\\s*="),
      perl = TRUE,
      info = paste("Missing mrgsolve contract field:", field)
    )
  }
})

test_that("mrgsolve output names follow the outvars list contract", {
  source(
    file.path(PROJECT_ROOT, "app", "R", "mod_mrgsolve.R"),
    local = TRUE
  )

  fake_model <- structure(list(), class = "fake_mrgsolve_model")
  fake_outvars <- function(mod) {
    expect_s3_class(mod, "fake_mrgsolve_model")
    list(cmt = c("DEPOT", "CENT"), capture = c("CP", "AUC"))
  }

  expect_identical(
    .mrgsolve_outvar_names(fake_model, "capture", fake_outvars),
    c("CP", "AUC")
  )
  expect_identical(
    .mrgsolve_outvar_names(fake_model, "cmt", fake_outvars),
    c("DEPOT", "CENT")
  )
  expect_identical(
    .mrgsolve_outvar_names(
      fake_model,
      "capture",
      function(mod) list(cmt = "CENT")
    ),
    character()
  )

  src <- read_app_file(file.path("app", "R", "mod_mrgsolve.R"))
  expect_false(grepl("mrgsolve::cmt", src, fixed = TRUE))
})

test_that("shared model identity stays bound to the compiled code", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("mrgsolve")

  suppressWarnings(suppressPackageStartupMessages(library(shiny)))
  source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
  source(file.path(PROJECT_ROOT, "R", "design_metrics.R"))
  source(file.path(PROJECT_ROOT, "R", "mrgsolve_bridge.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "mod_mrgsolve.R"))
  mrg_status <- has_mrgsolve()
  skip_if_not(mrg_status$available, mrg_status$reason)

  code_a <- paste(
    "$PARAM CL = 1, V = 10",
    "$CMT CENT",
    "$ODE",
    "dxdt_CENT = -(CL / V) * CENT;",
    "$TABLE",
    "double CP = CENT / V;",
    "$CAPTURE CP",
    sep = "\n"
  )
  code_b <- sub("CL = 1", "CL = 2", code_a, fixed = TRUE)

  shiny::testServer(
    mod_mrgsolve_server,
    args = list(
      ext_data = shiny::reactive(NULL),
      tab_data = shiny::reactive(NULL)
    ),
    {
      session$setInputs(mrg_code = code_a, compile = 1L)
      session$flushReact()

      hash_a <- compiled_model_hash()
      expect_false(is.null(compiled_model()))
      expect_identical(hash_a, mrgsolve_code_hash(code_a, n_chars = 32L))
      expect_identical(compiled_model_code(), code_a)

      session$setInputs(mrg_code = code_b)
      session$flushReact()

      expect_identical(compiled_model_hash(), hash_a)
      expect_identical(compiled_model_code(), code_a)
      expect_false(identical(
        compiled_model_hash(),
        mrgsolve_code_hash(code_b, n_chars = 32L)
      ))
    }
  )
})

test_that("shared simulation state cannot expose changed primary inputs", {
  skip_if_not_installed("shiny")

  suppressWarnings(suppressPackageStartupMessages(library(shiny)))
  source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
  source(file.path(PROJECT_ROOT, "R", "design_metrics.R"))
  source(file.path(PROJECT_ROOT, "R", "mrgsolve_bridge.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "mod_mrgsolve.R"))

  ext_state <- shiny::reactiveVal(data.frame(CL = 1, V = 10))
  tab_state <- shiny::reactiveVal(data.frame(
    ID = 1L,
    TIME = 0,
    EVID = 1L,
    AMT = 100
  ))
  available_status <- function() {
    list(available = TRUE, reason = NULL)
  }

  shiny::testServer(
    mod_mrgsolve_server,
    args = list(
      ext_data = ext_state,
      tab_data = tab_state,
      mrg_status_provider = available_status
    ),
    {
      session$flushReact()
      simulated <- data.frame(time = 0, IPRED = 1)

      sim_context(simulation_context())
      sim_result(simulated)
      session$flushReact()

      expect_identical(shared_state_contract$sim_data(), simulated)
      expect_true(shared_state_contract$is_available())

      model_before <- compiled_model()
      ext_state(data.frame(CL = 2, V = 10))

      # Direct context validation prevents a stale read before observers flush.
      expect_null(shared_state_contract$sim_data())
      expect_false(shared_state_contract$is_available())

      session$flushReact()
      expect_null(sim_result())
      expect_null(sim_context())
      expect_identical(compiled_model(), model_before)

      sim_context(simulation_context())
      sim_result(simulated)
      session$flushReact()
      expect_identical(shared_state_contract$sim_data(), simulated)

      changed_tab <- tab_state()
      changed_tab$AMT <- 200
      tab_state(changed_tab)

      expect_null(shared_state_contract$sim_data())
      expect_false(shared_state_contract$is_available())

      session$flushReact()
      expect_null(sim_result())
      expect_null(sim_context())
      expect_identical(compiled_model(), model_before)
    }
  )
})

test_that("shared simulation state tracks manual dose configuration", {
  skip_if_not_installed("shiny")

  suppressWarnings(suppressPackageStartupMessages(library(shiny)))
  source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
  source(file.path(PROJECT_ROOT, "R", "design_metrics.R"))
  source(file.path(PROJECT_ROOT, "R", "mrgsolve_bridge.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "mod_mrgsolve.R"))

  ext_state <- shiny::reactiveVal(data.frame(THETA1 = 1))
  tab_state <- shiny::reactiveVal(data.frame(
    ID = 1L,
    TIME = 0,
    EVID = 1L
  ))
  labels_state <- shiny::reactiveVal(c(THETA1 = "CL"))
  available_status <- function() {
    list(available = TRUE, reason = NULL)
  }

  shiny::testServer(
    mod_mrgsolve_server,
    args = list(
      ext_data = ext_state,
      tab_data = tab_state,
      theta_labels = labels_state,
      mrg_status_provider = available_status
    ),
    {
      session$setInputs(amt_1 = 100, rate_1 = 0)
      session$flushReact()
      compiled_model_hash("model-a")
      session$flushReact()

      simulated <- data.frame(time = 0, IPRED = 1)
      sim_context(simulation_context())
      sim_result(simulated)
      session$flushReact()

      expect_identical(shared_state_contract$sim_data(), simulated)
      expect_equal(dose_events()$amt, 100)

      session$setInputs(amt_1 = 200)

      expect_equal(dose_events()$amt, 200)
      expect_null(shared_state_contract$sim_data())
      expect_false(shared_state_contract$is_available())

      session$flushReact()
      expect_null(sim_result())
      expect_null(sim_context())

      sim_context(simulation_context())
      sim_result(simulated)
      session$flushReact()
      session$setInputs(rate_1 = 25)

      expect_equal(dose_events()$rate, 25)
      expect_null(shared_state_contract$sim_data())
      expect_false(shared_state_contract$is_available())

      session$flushReact()
      expect_null(sim_result())
      expect_null(sim_context())

      sim_context(simulation_context())
      sim_result(simulated)
      session$flushReact()
      labels_state(c(THETA1 = "VC"))

      expect_null(shared_state_contract$sim_data())
      session$flushReact()
      expect_null(sim_result())

      sim_context(simulation_context())
      sim_result(simulated)
      session$flushReact()
      compiled_model_hash("model-b")

      expect_null(shared_state_contract$sim_data())
      session$flushReact()
      expect_null(sim_result())
    }
  )
})

test_that("manual RATE remains configurable when AMT comes from the table", {
  skip_if_not_installed("shiny")

  suppressWarnings(suppressPackageStartupMessages(library(shiny)))
  source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
  source(file.path(PROJECT_ROOT, "R", "design_metrics.R"))
  source(file.path(PROJECT_ROOT, "R", "mrgsolve_bridge.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "mod_mrgsolve.R"))

  available_status <- function() {
    list(available = TRUE, reason = NULL)
  }

  shiny::testServer(
    mod_mrgsolve_server,
    args = list(
      ext_data = shiny::reactive(NULL),
      tab_data = shiny::reactive(data.frame(
        ID = 1L,
        TIME = 0,
        EVID = 1L,
        AMT = 100
      )),
      mrg_status_provider = available_status
    ),
    {
      session$setInputs(rate_1 = 25)
      session$flushReact()

      expect_true(needs_dose_input())
      expect_equal(dose_events()$amt, 100)
      expect_equal(dose_events()$rate, 25)
    }
  )
})

test_that("Optimal Times consumes shared mrgsolve state instead of owning upload", {
  src <- read_app_file(file.path("app", "R", "mod_times.R"))

  expect_match(src, "mrgsolve_state\\s*=\\s*NULL", perl = TRUE)
  expect_false(
    grepl("mod_mrgsolve_server\\s*\\(", src, perl = TRUE),
    info = "mod_times_server should not instantiate the mrgsolve upload module"
  )
})

test_that("SSE diagnostics receives shared mrgsolve state and gates PK exposure", {
  src <- read_app_file(file.path("app", "R", "mod_sse_analysis.R"))

  expect_match(src, "mrgsolve_state\\s*=\\s*NULL", perl = TRUE)
  expect_match(src, "\"PK Exposure\"\\s*=\\s*\"pk_exposure\"", perl = TRUE)
  expect_match(src, "output\\$pk_exposure_status", fixed = FALSE)
})

test_that("SSE PK exposure preflight exposes output and mapping controls", {
  analysis_src <- read_app_file(file.path("app", "R", "mod_sse_analysis.R"))
  source_core_src <- read_app_file(file.path("R", "source_core.R"))

  expect_match(source_core_src, "sse_mrgsolve_exposure\\.R")
  expect_match(analysis_src, "pk_exposure_output")
  expect_match(analysis_src, "pk_exposure_params")
  expect_match(analysis_src, "pk_exposure_mapping_table")
  expect_match(analysis_src, "pk_exposure_sanity_check")
  expect_match(analysis_src, "pk_exposure_sanity_table")
  expect_match(analysis_src, "pk_exposure_compute_controls")
  expect_match(analysis_src, "pk_exposure_results_table")
  expect_match(analysis_src, "export_pk_exposure_csv")
  expect_match(analysis_src, "pk_exposure_cache")
  expect_match(analysis_src, "compute_sse_mrgsolve_exposure_recovery\\(")
  expect_match(analysis_src, "run_sse_mrgsolve_sanity_check\\(")
  expect_match(analysis_src, "build_sse_mrgsolve_exposure_preflight\\(")
  expect_match(analysis_src, "sse_mrgsolve_candidate_columns\\(")
})

test_that("SSE documentation describes the generic mrgsolve exposure workflow", {
  doc_src <- read_app_file(file.path("app", "R", "mod_documentation.R"))

  expect_match(doc_src, "PK Exposure")
  expect_match(doc_src, "\\.cpp")
  expect_match(doc_src, "m1\\.zip")
  expect_match(doc_src, "mrgsolve-compatible")
  expect_match(doc_src, "sanity check")
  expect_match(doc_src, "cached")
})
