# tests/testthat/test-parser_hardening.R
# Parser hardening tests — prepare_tab_obs per-ID TIME=0 fix

source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
source(file.path(PROJECT_ROOT, "R", "design_io.R"))
source(file.path(PROJECT_ROOT, "R", "design_metrics.R"))
source(file.path(PROJECT_ROOT, "R", "report_design.R"))

test_that("prepare_tab_obs drops TIME=0 per ID, not globally", {
  # 3 IDs, each with a dose at TIME=0 + 2 observations
  tab <- tibble::tibble(
    table_no = 1L,
    ID    = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    TIME  = c(0, 1.5, 3.7, 0, 1.5, 3.7, 0, 1.5, 3.7),
    EVID  = c(1, 0, 0, 1, 0, 0, 1, 0, 0),
    IPRED = c(0, 6.8, 8.1, 0, 6.9, 8.0, 0, 6.7, 8.2)
  )
  obs <- prepare_tab_obs(tab)
  expect_equal(nrow(obs), 6L)
  expect_false(any(obs$EVID == 1))
  expect_true(all(obs$TIME > 0))
})

test_that("prepare_tab_obs drops TIME=0 baseline per ID even when EVID missing", {
  # No EVID column — 3 IDs, each with TIME=0 baseline + 2 obs
  tab <- tibble::tibble(
    table_no = 1L,
    ID    = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    TIME  = c(0, 1.5, 3.7, 0, 1.5, 3.7, 0, 1.5, 3.7),
    IPRED = c(0, 6.8, 8.1, 0, 6.9, 8.0, 0, 6.7, 8.2)
  )
  obs <- prepare_tab_obs(tab)
  expect_equal(nrow(obs), 6L)
  expect_true(all(obs$TIME > 0))
})
