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

test_file(file.path(PROJECT_ROOT, "tests", "testthat",
                    "test-parse_design_outputs.R"),
          reporter = "progress")

test_file(file.path(PROJECT_ROOT, "tests", "testthat",
                    "test-fim_metrics.R"),
          reporter = "progress")

test_file(file.path(PROJECT_ROOT, "tests", "testthat",
                    "test-sse_metrics.R"),
          reporter = "progress")

test_file(file.path(PROJECT_ROOT, "tests", "testthat",
                    "test-tab_dispatch.R"),
          reporter = "progress")

test_file(file.path(PROJECT_ROOT, "tests", "testthat",
                    "test-parser_hardening.R"),
          reporter = "progress")

# SSE pipeline smoke test — standalone stopifnot() style, not testthat.
# Catches silent breakage if PsN column naming or the normalizer drifts.
cat("\n\n--- SSE pipeline smoke test ---\n")
source(file.path(PROJECT_ROOT, "tests", "test_sse_pipeline.R"))
