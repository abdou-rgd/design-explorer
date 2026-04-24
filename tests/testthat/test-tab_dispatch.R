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

test_that("unknown: empty tab", {
  expect_equal(detect_tab_pattern(tibble::tibble()), "unknown")
  expect_equal(detect_tab_pattern(NULL), "unknown")
})
