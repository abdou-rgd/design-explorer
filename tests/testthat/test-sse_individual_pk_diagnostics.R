# =============================================================================
# test-sse_individual_pk_diagnostics.R -- exploratory individual PK diagnostics
# =============================================================================

library(testthat)
library(dplyr)
library(ggplot2)

project_root <- Sys.getenv("DESIGN_EXPLORER_ROOT", unset = NA_character_)
if (is.na(project_root) || !nzchar(project_root)) {
  project_root <- normalizePath(".")
}
proj <- function(...) file.path(project_root, ...)

source(proj("R", "sse_individual_pk.R"))
source(proj("R", "sse_individual_pk_diagnostics.R"))

write_diag_patab_fixture <- function(root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "TABLE NO.  1",
      " ID ARM CL VC Q VP KA F1",
      " 1 0 1.10 10.0 0.50 20.0 0.10 0.70",
      " 1 0 1.10 10.0 0.50 20.0 0.10 0.70",
      " 2 1 1.20 11.0 0.55 21.0 0.11 0.71"
    ),
    file.path(root, "patab1.tab-1")
  )
  writeLines(
    c(
      "TABLE NO.  1",
      " ID ARM CL VC Q VP KA F1",
      " 1 0 1.00 10.0 0.40 20.0 0.10 0.70",
      " 1 0 1.00 10.0 0.40 20.0 0.10 0.70",
      " 2 1 1.00 10.0 0.50 20.0 0.10 0.70"
    ),
    file.path(root, "patab1.tab-sim-1")
  )
  writeLines(
    c(
      "TABLE NO.  1",
      " ID ARM CL VC Q VP KA F1",
      " 1 0 2.20 12.0 0.70 22.0 0.12 0.72",
      " 2 1 2.40 13.0 0.75 23.0 0.13 0.73"
    ),
    file.path(root, "patab1.tab-2")
  )
  writeLines(
    c(
      "TABLE NO.  1",
      " ID ARM CL VC Q VP KA F1",
      " 1 0 2.00 12.0 0.60 22.0 0.12 0.72",
      " 2 1 2.00 12.0 0.65 22.0 0.12 0.72"
    ),
    file.path(root, "patab1.tab-sim-2")
  )
}

make_diag_raw_status <- function() {
  tibble::tibble(
    sample = c(1L, 2L),
    minimization_successful = c(0L, 1L),
    covariance_step_successful = c(0L, 1L),
    estimate_near_boundary = c(1L, 0L),
    rounding_errors = c(1L, 0L),
    condition_number = c(NA_real_, 12.5)
  )
}

test_that("individual PK diagnostics flag very high condition numbers", {
  root <- tempfile("individual-pk-diag-")
  write_diag_patab_fixture(root)
  patab <- read_sse_patab_outputs(root)
  raw <- make_diag_raw_status()
  raw$condition_number[raw$sample == 2L] <- 764120

  diag <- build_individual_pk_diagnostics(raw, patab)

  expect_equal(
    unique(diag$individual_pk_recovery_long$run_qc_status[
      diag$individual_pk_recovery_long$sample == 2L
    ]),
    "high_condition_number"
  )
  expect_equal(
    diag$run_pk_error_burden$run_qc_status[
      diag$run_pk_error_burden$sample == 2L
    ],
    "high_condition_number"
  )
})

test_that("individual PK diagnostics annotate run status and summaries", {
  root <- tempfile("individual-pk-diag-")
  write_diag_patab_fixture(root)
  patab <- read_sse_patab_outputs(root)

  diag <- build_individual_pk_diagnostics(make_diag_raw_status(), patab)

  expect_named(
    diag,
    c(
      "individual_pk_recovery_long",
      "individual_pk_summary_by_param",
      "individual_pk_summary_by_id_param",
      "run_pk_error_burden",
      "prediction_columns"
    )
  )
  expect_equal(nrow(diag$individual_pk_recovery_long), 24L)
  expect_setequal(
    names(diag$individual_pk_recovery_long),
    c(
      "sample",
      "ID",
      "ARM",
      "param",
      "sim",
      "est",
      "relative_error",
      "abs_relative_error",
      "minimization_successful",
      "covariance_step_successful",
      "estimate_near_boundary",
      "rounding_errors",
      "condition_number",
      "run_qc_status"
    )
  )
  expect_equal(
    unique(diag$individual_pk_recovery_long$run_qc_status[
      diag$individual_pk_recovery_long$sample == 1L
    ]),
    "minimization_failed"
  )
  expect_equal(
    unique(diag$individual_pk_recovery_long$run_qc_status[
      diag$individual_pk_recovery_long$sample == 2L
    ]),
    "strict_qc_ok"
  )

  expect_setequal(
    diag$individual_pk_summary_by_param$status_filter,
    c("all_runs", "minimization_successful", "strict_qc_ok")
  )
  q_all <- diag$individual_pk_summary_by_param |>
    filter(param == "Q", status_filter == "all_runs")
  expect_equal(q_all$n, 4L)
  expect_equal(q_all$pct_abs_error_over_20, 25)

  id_param <- diag$individual_pk_summary_by_id_param
  expect_true(all(
    c(
      "median_relative_error",
      "p5_relative_error",
      "p95_relative_error",
      "outlier_rank"
    ) %in%
      names(id_param)
  ))
  expect_equal(min(id_param$outlier_rank), 1L)

  run_burden <- diag$run_pk_error_burden
  expect_equal(
    run_burden$run_qc_status[run_burden$sample == 1L],
    "minimization_failed"
  )
  expect_equal(
    run_burden$run_qc_status[run_burden$sample == 2L],
    "strict_qc_ok"
  )
})

test_that("individual PK diagnostic plots are ggplot objects", {
  root <- tempfile("individual-pk-diag-")
  write_diag_patab_fixture(root)
  patab <- read_sse_patab_outputs(root)
  diag <- build_individual_pk_diagnostics(make_diag_raw_status(), patab)

  expect_s3_class(
    plot_individual_pk_error_forest(diag$individual_pk_summary_by_param),
    "ggplot"
  )
  expect_s3_class(
    plot_individual_pk_error_heatmap(diag$individual_pk_summary_by_id_param),
    "ggplot"
  )
  expect_s3_class(
    plot_individual_pk_outliers(diag$individual_pk_summary_by_id_param),
    "ggplot"
  )
  expect_s3_class(
    plot_run_pk_error_burden(diag$run_pk_error_burden),
    "ggplot"
  )
  expect_s3_class(
    plot_individual_pk_recovery_by_status(diag$individual_pk_recovery_long),
    "ggplot"
  )
})

test_that("individual PK diagnostics skip prediction outputs when columns absent", {
  root <- tempfile("individual-pk-diag-")
  write_diag_patab_fixture(root)
  patab <- read_sse_patab_outputs(root)
  diag <- build_individual_pk_diagnostics(make_diag_raw_status(), patab)

  expect_equal(diag$prediction_columns, character())
  expect_null(plot_individual_pk_prediction_diagnostics(patab))
})

test_that("local PsN fixture keeps expected individual PK dimensions", {
  raw_path <- proj(
    "docs",
    "results",
    "sse_further_explore",
    "raw_results_psm_eval_sparseSSE_TABLE.csv"
  )
  zip_path <- proj("docs", "results", "sse_further_explore", "m1.zip")
  skip_if_not(
    file.exists(raw_path) && file.exists(zip_path),
    "local SSE fixture not available"
  )

  raw <- read.csv(raw_path, check.names = FALSE)
  patab <- read_sse_patab_outputs(zip_path)
  diag <- build_individual_pk_diagnostics(raw, patab)

  expect_equal(length(unique(diag$individual_pk_recovery_long$sample)), 20L)
  expect_equal(length(unique(diag$individual_pk_recovery_long$ID)), 80L)
  expect_equal(length(unique(diag$individual_pk_recovery_long$param)), 6L)
  expect_equal(nrow(diag$individual_pk_recovery_long), 9600L)
  expect_setequal(
    unique(diag$individual_pk_recovery_long$sample[
      diag$individual_pk_recovery_long$run_qc_status == "minimization_failed"
    ]),
    c(1L, 15L)
  )
})
