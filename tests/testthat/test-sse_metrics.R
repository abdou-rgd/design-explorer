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
source(proj("R", "design_metrics.R"))
source(proj("R", "report_design.R"))
source(proj("R", "sse_metrics.R"))
source(proj("R", "sse_diagnostics.R"))
source(proj("R", "sse_individual_pk.R"))

make_sse_20_sample_fixture <- function() {
  vals <- function(center, spread = 0.1) {
    round(center * (1 + seq(-spread, spread, length.out = 20)), 8)
  }
  data.frame(
    hypothesis = "simulation",
    sample = seq_len(20),
    minimization_successful = c(rep(1, 18), 0, 0),
    covariance_step_successful = c(rep(1, 11), rep(0, 9)),
    estimate_near_boundary = c(rep(0, 11), rep(1, 9)),
    rounding_errors = c(rep(0, 18), 1, 1),
    ofv = seq(5900, 6100, length.out = 20),
    THETA1 = vals(0.005933),
    THETA2 = vals(3.035, 0.05),
    THETA3 = vals(0.01755, 0.2),
    THETA4 = vals(2.97, 0.08),
    THETA5 = vals(0.007549, 0.15),
    THETA6 = vals(0.6703, 0.07),
    `OMEGA(1,1)` = vals(0.06501, 0.2),
    `OMEGA(2,1)` = vals(0.02584, 0.25),
    `OMEGA(2,2)` = vals(0.04412, 0.2),
    `OMEGA(3,3)` = vals(0.3331, 0.5),
    `OMEGA(4,4)` = vals(0.0618, 0.45),
    `OMEGA(5,5)` = vals(0.2419, 0.3),
    `OMEGA(6,6)` = vals(0.4362, 0.25),
    `SIGMA(1,1)` = vals(0.01897, 0.08),
    `SIGMA(2,2)` = c(vals(0.9463, 0.1)[1:18], 40, 60),
    se_THETA1 = c(rep(0.0001, 11), rep(NA_real_, 9)),
    se_THETA2 = c(rep(0.1, 11), rep(NA_real_, 9)),
    `se_OMEGA(3,3)` = c(rep(0.8, 11), rep(NA_real_, 9)),
    `se_SIGMA(2,2)` = c(rep(4, 11), rep(NA_real_, 9)),
    `shrinkage_eta1(%)` = seq(7, 13, length.out = 20),
    `shrinkage_eta2(%)` = seq(24, 36, length.out = 20),
    `shrinkage_eta3(%)` = seq(45, 95, length.out = 20),
    `shrinkage_eta4(%)` = seq(44, 96, length.out = 20),
    `shrinkage_eta5(%)` = seq(26, 80, length.out = 20),
    `shrinkage_eta6(%)` = seq(18, 55, length.out = 20),
    check.names = FALSE
  ) |>
    tibble::as_tibble()
}

true_values_20_sample <- c(
  THETA1 = 0.005933, THETA2 = 3.035, THETA3 = 0.01755,
  THETA4 = 2.970, THETA5 = 0.007549, THETA6 = 0.6703,
  `OMEGA(1,1)` = 0.06501, `OMEGA(2,1)` = 0.02584,
  `OMEGA(2,2)` = 0.04412, `OMEGA(3,3)` = 0.3331,
  `OMEGA(4,4)` = 0.0618, `OMEGA(5,5)` = 0.2419,
  `OMEGA(6,6)` = 0.4362, `SIGMA(1,1)` = 0.01897,
  `SIGMA(2,2)` = 0.9463
)


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

test_that(".normalize_psn_cols() maps unnumbered --th- positionally", {
  # PsN writes --th<N>- when the .ctl ;label has a digit; otherwise --th-.
  # With a FIXED THETA7, --th8- anchors the counter to 8 so the next
  # unnumbered --th- becomes THETA10 (not THETA9).
  hdr <- c(
    "--th1- CL", "--th2- V", "--th6- F1",
    "--th8- Allo_CL", "--th9- Allo_V",
    "--th- COV1", "--th- COV2", "--th- COV3",
    "OMEGA(2,1)",
    "--th-", "--th-_", "--th-__",
    "--eps1- Prop", "--eps2- Add",
    "se--th1- CL", "se--th8- Allo_CL",
    "se--th- COV1", "se--th-",
    "se--eps1- Prop"
  )
  out <- .normalize_psn_cols(hdr)
  expect_equal(out, c(
    "THETA1", "THETA2", "THETA6",
    "THETA8", "THETA9",
    "THETA10", "THETA11", "THETA12",
    "OMEGA(2,1)",
    "THETA13", "THETA14", "THETA15",
    "SIGMA(1,1)", "SIGMA(2,2)",
    "se_THETA1", "se_THETA8",
    "se_THETA9", "se_THETA10",
    "se_SIGMA(1,1)"
  ))
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

test_that("compare_fim_sse() preserves NA when capping missing values", {
  sse_metrics <- tibble::tibble(
    param = "THETA1", param_type = "THETA", param_label = "CL",
    rse_empirical = NA_real_, rmse_relative = NA_real_,
    relative_bias = 0, rb_ci_lower = -1, rb_ci_upper = 1
  )
  fim_rse <- tibble::tibble(param = "THETA1", rse_pct = 250)

  comp <- compare_fim_sse(sse_metrics, fim_rse, max_rse = 200)

  expect_equal(comp$rse_fim_capped, 200)
  expect_true(is.na(comp$rse_sse_capped))
  expect_true(is.na(comp$rmse_sse_capped))
})

test_that("read_true_values() parses inline OMEGA and SIGMA BLOCK values", {
  vals <- read_true_values(c(
    "$THETA (0, 1, 10)",
    "$OMEGA BLOCK(2) 0.1 0.01 0.2",
    "$SIGMA BLOCK(2) 1D-2 0.003 0.04"
  ))

  expect_equal(unname(vals["THETA1"]), 1)
  expect_equal(unname(vals["OMEGA(1,1)"]), 0.1)
  expect_equal(unname(vals["OMEGA(2,1)"]), 0.01)
  expect_equal(unname(vals["OMEGA(2,2)"]), 0.2)
  expect_equal(unname(vals["SIGMA(1,1)"]), 0.01)
  expect_equal(unname(vals["SIGMA(2,1)"]), 0.003)
  expect_equal(unname(vals["SIGMA(2,2)"]), 0.04)
})

test_that("compute_empirical_correlations() returns the SSE correlation matrix", {
  sse_raw <- tibble::tibble(
    THETA1 = c(1.0, 1.1, 0.9, 1.2),
    THETA2 = c(2.0, 2.2, 1.8, 2.1)
  )
  true_values <- c(THETA1 = 1, THETA2 = 2)

  corr <- compute_empirical_correlations(sse_raw, true_values)

  expect_true(is.matrix(corr))
  expect_equal(dim(corr), c(2L, 2L))
  expect_equal(unname(diag(corr)), c(1, 1), tolerance = 1e-12)
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


# =============================================================================
# D. SSE reliability map and 20-sample smoke shape
# =============================================================================

test_that("20-sample raw_results shape is run-level and has no individual PK columns", {
  sse <- make_sse_20_sample_fixture()
  sse$converged <- as.numeric(sse$minimization_successful) == 1

  expect_equal(nrow(sse), 20L)
  expect_equal(sum(sse$converged), 18L)
  expect_equal(length(intersect(names(true_values_20_sample), names(sse))), 15L)

  pk_cols <- detect_individual_pk_columns(sse)
  expect_equal(pk_cols, character())

  shrink_cols <- grep("^shrinkage_eta\\d+\\(%\\)$", names(sse), value = TRUE)
  expect_length(shrink_cols, 6L)
  expect_true(any(!is.na(unlist(sse[shrink_cols]))))
})

test_that("compute_sse_reliability_map joins precision, diagnostics, and shrinkage", {
  sse <- make_sse_20_sample_fixture()
  sse$converged <- as.numeric(sse$minimization_successful) == 1

  rel <- compute_sse_reliability_map(sse, true_values_20_sample)

  expect_equal(nrow(rel), 15L)
  expect_named(rel, c(
    "param", "param_label", "param_type", "rse_empirical", "relative_bias",
    "pct_se_na", "pct_rse_over_100", "mean_shrinkage", "risk_score"
  ))
  expect_true(all(c("THETA1", "OMEGA(3,3)", "SIGMA(2,2)") %in% rel$param))
  expect_true(rel$pct_se_na[rel$param == "THETA1"] > 0)
  expect_true(rel$pct_rse_over_100[rel$param == "OMEGA(3,3)"] > 0)
  expect_true(rel$mean_shrinkage[rel$param == "OMEGA(3,3)"] > 50)
  expect_true(is.na(rel$mean_shrinkage[rel$param == "THETA1"]))
})

test_that("plot_sse_reliability_map returns an empty state or a faceted ggplot", {
  empty_plot <- plot_sse_reliability_map(tibble::tibble())
  expect_s3_class(empty_plot, "ggplot")

  sse <- make_sse_20_sample_fixture()
  sse$converged <- as.numeric(sse$minimization_successful) == 1
  rel <- compute_sse_reliability_map(sse, true_values_20_sample)
  p <- plot_sse_reliability_map(rel)

  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})


# =============================================================================
# E. PsN patab individual PK outputs
# =============================================================================

write_patab_fixture <- function(root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "TABLE NO.  1",
    " ID ARM CL VC Q VP KA F1 ETA1 ETA2",
    " 1 0 1.10 10.0 0.50 20.0 0.10 0.70 0.01 0.02",
    " 1 0 1.10 10.0 0.50 20.0 0.10 0.70 0.01 0.02",
    " ID ARM CL VC Q VP KA F1 ETA1 ETA2",
    " 2 1 1.20 11.0 0.55 21.0 0.11 0.71 0.03 0.04",
    " 2 1 1.20 11.0 0.55 21.0 0.11 0.71 0.03 0.04"
  ), file.path(root, "patab1.tab-1"))
  writeLines(c(
    "TABLE NO.  1",
    " ID ARM CL VC Q VP KA F1 ETA1 ETA2",
    " 1 0 1.00 10.0 0.40 20.0 0.10 0.70 0.00 0.00",
    " 1 0 1.00 10.0 0.40 20.0 0.10 0.70 0.00 0.00",
    " 2 1 1.00 10.0 0.50 20.0 0.10 0.70 0.00 0.00",
    " 2 1 1.00 10.0 0.50 20.0 0.10 0.70 0.00 0.00"
  ), file.path(root, "patab1.tab-sim-1"))
  writeLines(c(
    "TABLE NO.  1",
    " ID ARM CL VC Q VP KA F1 ETA1 ETA2",
    " 1 0 2.20 12.0 0.70 22.0 0.12 0.72 0.05 0.06",
    " 2 1 2.40 13.0 0.75 23.0 0.13 0.73 0.07 0.08"
  ), file.path(root, "patab1.tab-2"))
  writeLines(c(
    "TABLE NO.  1",
    " ID ARM CL VC Q VP KA F1 ETA1 ETA2",
    " 1 0 2.00 12.0 0.60 22.0 0.12 0.72 0.00 0.00",
    " 2 1 2.00 12.0 0.65 22.0 0.12 0.72 0.00 0.00"
  ), file.path(root, "patab1.tab-sim-2"))
}

test_that("read_sse_patab_outputs() parses and deduplicates PsN patab files", {
  root <- tempfile("patab-fixture-")
  write_patab_fixture(root)

  patab <- read_sse_patab_outputs(root)

  expect_equal(nrow(patab), 8L)
  expect_setequal(unique(patab$kind), c("estimation", "simulation"))
  expect_equal(length(unique(patab$sample)), 2L)
  expect_equal(length(unique(patab$ID)), 2L)
  expect_true(all(c("CL", "VC", "Q", "VP", "KA", "F1", "ETA1", "ETA2") %in%
                    names(patab)))
})

test_that("read_sse_patab_outputs() accepts explicit pk_individuals table names", {
  root <- tempfile("pk-individuals-fixture-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "TABLE NO.  1",
    " ID ARM CL VC Q VP KA F1",
    " 1 0 1.10 10.0 0.50 20.0 0.10 0.70"
  ), file.path(root, "pk_individuals.tab-1"))
  writeLines(c(
    "TABLE NO.  1",
    " ID ARM CL VC Q VP KA F1",
    " 1 0 1.00 10.0 0.40 20.0 0.10 0.70"
  ), file.path(root, "pk_individuals.tab-sim-1"))

  patab <- read_sse_patab_outputs(root, table_pattern = "pk_individuals")

  expect_equal(unique(patab$table), "pk_individuals.tab")
  expect_equal(nrow(patab), 2L)
  expect_setequal(unique(patab$kind), c("estimation", "simulation"))
})

test_that("read_sse_patab_outputs() reads uploaded zip paths without extensions", {
  root <- tempfile("pk-individuals-zip-fixture-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "TABLE NO.  1",
    " ID ARM CL VC Q VP KA F1",
    " 1 0 1.10 10.0 0.50 20.0 0.10 0.70"
  ), file.path(root, "pk_individuals.tab-1"))
  writeLines(c(
    "TABLE NO.  1",
    " ID ARM CL VC Q VP KA F1",
    " 1 0 1.00 10.0 0.40 20.0 0.10 0.70"
  ), file.path(root, "pk_individuals.tab-sim-1"))

  zip_path <- tempfile(fileext = ".zip")
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(zipfile = zip_path, files = list.files(root))
  uploaded_path <- tempfile("shiny-upload-")
  file.copy(zip_path, uploaded_path, overwrite = TRUE)

  patab <- read_sse_patab_outputs(uploaded_path)

  expect_equal(nrow(patab), 2L)
  expect_setequal(unique(patab$kind), c("estimation", "simulation"))
})

test_that("compute_individual_pk_recovery() compares estimation against simulation", {
  root <- tempfile("patab-fixture-")
  write_patab_fixture(root)
  patab <- read_sse_patab_outputs(root)

  rec <- compute_individual_pk_recovery(patab)

  expect_true(all(c("table", "sample", "ID", "param", "sim", "est", "relative_error") %in%
                    names(rec)))
  expect_true(all(c("CL", "VC", "Q", "VP", "KA", "F1") %in% rec$param))
  cl_row <- rec[rec$sample == 1 & rec$ID == 1 & rec$param == "CL", ]
  expect_equal(cl_row$sim, 1)
  expect_equal(cl_row$est, 1.1)
  expect_equal(cl_row$relative_error, 10)
})

test_that("individual PK plots return ggplot objects", {
  root <- tempfile("patab-fixture-")
  write_patab_fixture(root)
  patab <- read_sse_patab_outputs(root)
  rec <- compute_individual_pk_recovery(patab)

  expect_s3_class(plot_individual_pk_recovery(rec), "ggplot")
  expect_s3_class(
    ggplot2::ggplot_build(plot_individual_pk_recovery(rec, log_axes = TRUE)),
    "ggplot_built"
  )
  expect_s3_class(plot_individual_pk_error_distribution(rec), "ggplot")
})
