# =============================================================================
# test-sse_metrics.R — Unit tests for R/sse_metrics.R
#
# Run via: source("tests/run_tests.R") from the project root
# =============================================================================

library(testthat)
library(dplyr)
library(ggplot2)

`%||%` <- function(x, y) if (is.null(x)) y else x

project_root <- Sys.getenv("DESIGN_EXPLORER_ROOT", unset = NA_character_)
if (is.na(project_root) || !nzchar(project_root)) {
  this_file <- tryCatch(normalizePath(sys.frame(0)$ofile),
                        error = function(e) NULL)
  if (!is.null(this_file)) {
    d <- dirname(this_file)
    for (i in seq_len(6)) {
      if (file.exists(file.path(d, "CLAUDE.md"))) { project_root <- d; break }
      d <- dirname(d)
    }
  }
  if (is.na(project_root) || !nzchar(project_root)) project_root <- getwd()
}
proj <- function(...) file.path(project_root, ...)

source(proj("R", "design_utils.R"))
source(proj("R", "sse_metrics.R"))


# =============================================================================
# A. .normalize_psn_cols()
# =============================================================================

test_that(".normalize_psn_cols() handles standard PsN headers", {
  out <- .normalize_psn_cols(c("--th1- CL", "--th2- V", "--eps1- Prop",
                               "ETA(1) CL", "OMEGA(2,1)"))
  expect_equal(out, c("THETA1", "THETA2", "SIGMA(1,1)",
                      "OMEGA(1,1)", "OMEGA(2,1)"))
})

test_that(".normalize_psn_cols() accepts --th10 without trailing dash", {
  out <- .normalize_psn_cols(c("--th10", "--th10 KA", "--th1- CL"))
  expect_equal(out, c("THETA10", "THETA10", "THETA1"))
})

test_that(".normalize_psn_cols() preserves SE prefix", {
  out <- .normalize_psn_cols(c("se--th1- CL", "seOMEGA(1,1)", "seETA(2)"))
  expect_equal(out, c("se_THETA1", "se_OMEGA(1,1)", "se_OMEGA(2,2)"))
})


# =============================================================================
# B. compare_fim_sse()
# =============================================================================

test_that("compare_fim_sse() flags FIM-only and SSE-only params via status", {
  sse_metrics <- tibble::tibble(
    param         = c("THETA1", "THETA2", "OMEGA(1,1)"),
    param_type    = c("THETA", "THETA", "OMEGA (diag.)"),
    param_label   = c("CL", "V", "OMEGA(1,1)"),
    rse_empirical = c(5, 10, 25),
    rmse_relative = c(6, 11, 27),
    relative_bias = c(0.5, -1, 2),
    rb_ci_lower   = c(-1, -3, -1),
    rb_ci_upper   = c(2, 1, 5)
  )
  fim_rse <- tibble::tibble(
    param   = c("THETA1", "THETA2", "THETA10"),
    rse_pct = c(4, 9, 50)
  )

  comp <- compare_fim_sse(sse_metrics, fim_rse)

  expect_setequal(comp$param,
                  c("THETA1", "THETA2", "OMEGA(1,1)", "THETA10"))
  expect_equal(comp$status[comp$param == "THETA1"], "matched")
  expect_equal(comp$status[comp$param == "THETA10"], "FIM only")
  expect_equal(comp$status[comp$param == "OMEGA(1,1)"], "SSE only")
})


# =============================================================================
# C. plot_rse_bar()
# =============================================================================

test_that("plot_rse_bar() includes FIM-only and SSE-only params as single bars", {
  comp <- tibble::tibble(
    param         = c("THETA1", "THETA10", "OMEGA(1,1)"),
    param_type    = c("THETA", "THETA", "OMEGA (diag.)"),
    param_label   = c("CL", "THETA10", "OMEGA(1,1)"),
    rse_fim       = c(5, 50, NA_real_),
    rse_sse       = c(6, NA_real_, 25),
    status        = c("matched", "FIM only", "SSE only")
  )

  p <- plot_rse_bar(comp)
  expect_s3_class(p, "ggplot")

  rendered <- p$data
  expect_setequal(as.character(unique(rendered$label)),
                  c("CL", "THETA10", "OMEGA(1,1)"))

  # FIM-only param has rse_fim non-NA but rse_sse NA after pivot_longer
  theta10 <- rendered[rendered$label == "THETA10", ]
  expect_true(any(theta10$source == "FIM predicted" & !is.na(theta10$rse)))
  expect_true(any(theta10$source == "SSE empirical" & is.na(theta10$rse)))
})

test_that("plot_rse_bar() returns empty-state ggplot when no RSE data", {
  comp <- tibble::tibble(
    param = character(), param_label = character(), param_type = character(),
    rse_fim = numeric(), rse_sse = numeric(), status = character()
  )
  p <- plot_rse_bar(comp)
  expect_s3_class(p, "ggplot")
})
