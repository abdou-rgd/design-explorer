# tests/testthat/test-mrgsolve_shared_contract.R

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
