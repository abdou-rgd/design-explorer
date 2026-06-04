# =============================================================================
# run_tests.R — Standalone test runner for design-explorer
#
# Usage (from project root):
#   "/c/Program Files/R/R-4.5.2/bin/Rscript" tests/run_tests.R
# =============================================================================

library(testthat)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Project root = cwd (always run from project root)
PROJECT_ROOT <- normalizePath(".")
Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)

cat("Project root:", PROJECT_ROOT, "\n")
cat("Running tests...\n\n")

source(file.path(PROJECT_ROOT, "R", "source_core.R"))
source_core(PROJECT_ROOT)

has_test_failures <- FALSE

run_testthat_file <- function(rel_path) {
  res <- test_file(file.path(PROJECT_ROOT, rel_path), reporter = "progress")
  failed <- sum(res$failed, na.rm = TRUE)
  errored <- sum(res$error, na.rm = TRUE)
  if (failed > 0L || errored > 0L) {
    has_test_failures <<- TRUE
  }
  invisible(res)
}

run_testthat_file(file.path("tests", "testthat", "test-parse_design_outputs.R"))
run_testthat_file(file.path("tests", "testthat", "test-fim_metrics.R"))
run_testthat_file(file.path("tests", "testthat", "test-sse_metrics.R"))
run_testthat_file(file.path("tests", "testthat", "test-tab_dispatch.R"))
run_testthat_file(file.path("tests", "testthat", "test-parser_hardening.R"))
run_testthat_file(file.path("tests", "testthat", "test-pk_templates.R"))
run_testthat_file(file.path("tests", "testthat", "test-mrgsolve_bridge.R"))
run_testthat_file(file.path("tests", "testthat", "test-mrgsolve_shared_contract.R"))
run_testthat_file(file.path("tests", "testthat", "test-sse_mrgsolve_exposure_preflight.R"))
run_testthat_file(file.path("tests", "testthat", "test-sse_mrgsolve_exposure_app_server.R"))
run_testthat_file(file.path("tests", "testthat", "test-project_contracts.R"))
run_testthat_file(file.path("tests", "testthat", "test-times_integration.R"))
run_testthat_file(file.path("tests", "testthat", "test-post_processing_roadmap.R"))

run_local_smoke <- function(script, required_paths) {
  missing <- required_paths[!file.exists(file.path(PROJECT_ROOT, required_paths))]
  if (length(missing) > 0L) {
    cat("\n\n--- Skipping ", basename(script), " (missing local fixture: ",
        missing[1], ") ---\n", sep = "")
    return(invisible(FALSE))
  }
  cat("\n\n--- ", basename(script), " ---\n", sep = "")
  source(file.path(PROJECT_ROOT, script))
  invisible(TRUE)
}

# Local SSE smoke tests depend on docs/results, which is intentionally ignored.
# Run them when fixtures are present; skip explicitly in clean public clones.
run_local_smoke("tests/test_sse_pipeline.R", c(
  "docs/results/SSE/raw_results_psm_eval_sparseSSE_TABLE.csv",
  "docs/results/SSE/raw_results_run_psm_opti.csv",
  "docs/results/psm_eval/psm_eval.ctl"
))

run_local_smoke("tests/test_sse_comparison.R", c(
  "docs/results/SSE/raw_results_psm_eval_sparseSSE_TABLE.csv",
  "docs/results/SSE/raw_results_run_psm_opti.csv",
  "docs/results/psm_eval/psm_eval.ctl"
))

if (has_test_failures) {
  stop("One or more testthat files failed", call. = FALSE)
}
