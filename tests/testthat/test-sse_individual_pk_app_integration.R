# =============================================================================
# test-sse_individual_pk_app_integration.R -- app wiring for PK diagnostics
# =============================================================================

library(testthat)

project_root <- Sys.getenv("DESIGN_EXPLORER_ROOT", unset = NA_character_)
if (is.na(project_root) || !nzchar(project_root)) {
  project_root <- normalizePath(".")
}

read_project_file <- function(...) {
  paste(readLines(file.path(project_root, ...), warn = FALSE), collapse = "\n")
}

test_that("source_core loads individual PK diagnostic helpers", {
  source_core_txt <- read_project_file("R", "source_core.R")

  expect_match(source_core_txt, "sse_individual_pk\\.R")
  expect_match(source_core_txt, "sse_individual_pk_diagnostics\\.R")
})

test_that("SSE analysis exposes individual PK diagnostic views", {
  analysis_txt <- read_project_file("app", "R", "mod_sse_analysis.R")

  expect_match(analysis_txt, "hypothesis_filter")
  expect_match(analysis_txt, "Run composition")
  expect_match(analysis_txt, "PsN summary stats")
  expect_match(analysis_txt, "dOFV diagnostics")
  expect_match(analysis_txt, "compute_sse_run_composition\\(")
  expect_match(analysis_txt, "compute_sse_psn_parameter_summary\\(")
  expect_match(analysis_txt, "compute_sse_dofv_diagnostics\\(")
  expect_match(analysis_txt, "Individual PK intervals")
  expect_match(analysis_txt, "Individual PK outliers")
  expect_match(analysis_txt, "Individual PK heatmap")
  expect_match(analysis_txt, "pk_run_filter")
  expect_match(analysis_txt, "build_individual_pk_diagnostics\\(")
  expect_match(analysis_txt, "plot_individual_pk_error_forest\\(")
  expect_match(analysis_txt, "plot_individual_pk_outliers\\(")
  expect_match(analysis_txt, "plot_individual_pk_error_heatmap\\(")
})

test_that("Documentation explains PsN-inspired SSE diagnostics", {
  doc_txt <- read_project_file("app", "R", "mod_documentation.R")

  expect_match(doc_txt, "hypothesis")
  expect_match(doc_txt, "Run composition")
  expect_match(doc_txt, "condition_number")
  expect_match(doc_txt, "dOFV")
  expect_match(doc_txt, "seCL")
  expect_match(doc_txt, "pk_individuals\\.tab-1-1")
})
