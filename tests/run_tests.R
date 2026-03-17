# =============================================================================
# run_tests.R — Standalone test runner for ClaudeProjets
#
# Usage (from project root):
#   "/c/Program Files/R/R-4.5.2/bin/Rscript" tests/run_tests.R
#   # or from an R session:
#   source("tests/run_tests.R")
# =============================================================================

library(testthat)

# Set project root so tests can find example files regardless of cwd
PROJECT_ROOT <- normalizePath(dirname(sys.frame(0)$ofile %||%
                                        tryCatch(this.path::this.path(),
                                                 error = function(e) getwd())),
                               mustWork = FALSE)
# Walk up from tests/ to project root
`%||%` <- function(x, y) if (is.null(x)) y else x
if (!file.exists(file.path(PROJECT_ROOT, "CLAUDE.md"))) {
  PROJECT_ROOT <- dirname(PROJECT_ROOT)
}
Sys.setenv(CLAUDEPROJETS_ROOT = PROJECT_ROOT)

cat("Project root:", PROJECT_ROOT, "\n")
cat("Running tests...\n\n")

test_file(file.path(PROJECT_ROOT, "tests", "testthat",
                    "test-parse_design_outputs.R"),
          reporter = "progress")
