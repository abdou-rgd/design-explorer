# =============================================================================
# test-tab_dispatch.R
# Unit tests for R/tab_dispatch.R — detect_tab_pattern()
#
# Run via: "/c/Program Files/R/R-4.5.2/bin/Rscript" tests/run_tests.R
# =============================================================================

library(testthat)
library(dplyr)

source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
source(file.path(PROJECT_ROOT, "R", "design_io.R"))
source(file.path(PROJECT_ROOT, "R", "tab_dispatch.R"))

test_that("elementary_fo: single ID, TSTRAT present, 1 block", {
  tab <- load_tab("app/examples/example2/warfarin2.tab")
  expect_equal(detect_tab_pattern(tab), "elementary_fo")
})

test_that("focei_repl: many IDs, TSTRAT, single block", {
  tab <- load_tab("docs/results/opti_fenetre_large_FOCEI/psm_opti/psm_opti.tab")
  expect_equal(detect_tab_pattern(tab), "focei_repl")
})

test_that("robust_subprob: multiple table_no blocks", {
  tab <- load_tab("app/examples/example3/priortrue.tab")
  expect_equal(detect_tab_pattern(tab), "robust_subprob")
})

test_that("pkpd_multi: CMT column with >1 values", {
  tab <- load_tab("app/examples/example4/warfarin_pkpd_eval.tab")
  expect_equal(detect_tab_pattern(tab), "pkpd_multi")
})

test_that("dose_time_opt: AMT varies across rows", {
  tab <- load_tab("app/examples/example6/tmdd2.tab")
  expect_equal(detect_tab_pattern(tab), "dose_time_opt")
})

test_that("classical: many IDs, no TSTRAT", {
  tab <- make_classical_tab(n_ids = 10)
  expect_equal(detect_tab_pattern(tab), "classical")
})

test_that("stratified: STRAT columns are detected before generic classical", {
  tab <- make_classical_tab(n_ids = 10)
  tab$STRAT <- rep(c(1L, 2L), length.out = nrow(tab))
  expect_equal(detect_tab_pattern(tab), "stratified")
})

test_that("discrete: NMIN/NMAX or DISCRETE control stream flags are detected", {
  tab <- make_classical_tab(n_ids = 10)
  tab$NMIN <- 1L
  expect_equal(detect_tab_pattern(tab), "discrete")

  tab$NMIN <- NULL
  expect_equal(detect_tab_pattern(tab, "$DESIGN DISCRETE NMIN=1 NMAX=3"),
               "discrete")
})

test_that("unknown: empty tab", {
  expect_equal(detect_tab_pattern(tibble::tibble()), "unknown")
  expect_equal(detect_tab_pattern(NULL), "unknown")
})

source(file.path(PROJECT_ROOT, "R", "pk_templates.R"))
source(file.path(PROJECT_ROOT, "R", "ctl_parsers.R"))

test_that("pick_smooth_curve_engine returns tier=template for ADVAN2 TRANS2 ctl", {
  tab <- load_tab("app/examples/example2/warfarin2.tab")
  ctl <- load_ctl_lines("app/examples/example2/warfarin2.ctl")
  res <- pick_smooth_curve_engine(
    tab = tab, ctl_lines = ctl,
    theta_values = c(CL = 0.15, V = 8.0, KA = 1.0),
    mrgsolve_available = FALSE
  )
  expect_equal(res$tier, "template")
  expect_true(nrow(res$sim_data) > 0L)
  expect_true(all(is.finite(res$sim_data$IPRED)))
})

test_that("pick_smooth_curve_engine downgrades to dots when ADVAN unknown", {
  tab <- tibble::tibble(ID = 1, TIME = 0:10, IPRED = runif(11))
  res <- pick_smooth_curve_engine(
    tab = tab, ctl_lines = c("$PROBLEM X"),
    theta_values = numeric(),
    mrgsolve_available = FALSE
  )
  expect_equal(res$tier, "dots")
})

test_that("pick_smooth_curve_engine maps common theta label aliases", {
  tab <- load_tab("app/examples/example2/warfarin2.tab")
  ctl <- load_ctl_lines("app/examples/example2/warfarin2.ctl")
  res <- pick_smooth_curve_engine(
    tab = tab, ctl_lines = ctl,
    theta_values = c(Clearance = 0.15, Vc = 8.0, Kabs = 1.0),
    mrgsolve_available = FALSE
  )
  expect_equal(res$tier, "template")
})

test_that("extract_tab_dose_times falls back to .tab dose rows without mrgsolve", {
  tab <- tibble::tibble(
    ID = c(1, 1, 1, 1),
    TIME = c(0, 1, 12, 24),
    EVID = c(1, 0, 0, 1),
    AMT = c(100, 0, 0, 100),
    IPRED = c(0, 1, 2, 0)
  )
  expect_equal(extract_tab_dose_times(tab), c(0, 24))
})
