# =============================================================================
# run_tests.R — Standalone test runner for ClaudeProjets
#
# Usage (from project root):
#   "/c/Program Files/R/R-4.5.2/bin/Rscript" tests/run_tests.R
# =============================================================================

library(testthat)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Project root = cwd (always run from project root)
PROJECT_ROOT <- normalizePath(".")
Sys.setenv(CLAUDEPROJETS_ROOT = PROJECT_ROOT)

cat("Project root:", PROJECT_ROOT, "\n")
cat("Running tests...\n\n")

test_file(file.path(PROJECT_ROOT, "tests", "testthat",
                    "test-parse_design_outputs.R"),
          reporter = "progress")
