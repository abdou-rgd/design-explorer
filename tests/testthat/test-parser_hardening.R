# tests/testthat/test-parser_hardening.R
# Parser hardening tests — prepare_tab_obs per-ID TIME=0 fix

source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
source(file.path(PROJECT_ROOT, "R", "design_io.R"))
source(file.path(PROJECT_ROOT, "R", "design_metrics.R"))
source(file.path(PROJECT_ROOT, "R", "report_design.R"))
source(file.path(PROJECT_ROOT, "R", "ctl_parsers.R"))

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

test_that("select_representative_ids returns one ID per arm when TSTRAT signatures collide", {
  # Two arms (ID 1-3 vs 4-6) share the same TSTRAT values but are different arms.
  obs <- tibble::tibble(
    ID     = rep(1:6, each = 3),
    TSTRAT = rep(c(1, 2, 3), times = 6),
    TIME   = rep(c(1, 5, 24), times = 6)
  )
  rep_ids <- .select_representative_ids(obs, "TSTRAT", max_ids = 4L)
  # Collision signature must not drop the second arm — expect at least 2 IDs
  expect_gte(length(rep_ids), 2L)
})

test_that("select_representative_ids falls back to ARM column when TSTRAT missing", {
  obs <- tibble::tibble(
    ID   = rep(1:10, each = 3),
    ARM  = rep(c("IV", "SC"), each = 15),
    TIME = rep(c(1, 5, 24), times = 10)
  )
  rep_ids <- .select_representative_ids(obs, "TSTRAT", max_ids = 4L)
  # With TSTRAT absent, fallback should find both ARMs
  expect_gte(length(rep_ids), 2L)
})

test_that("select_representative_ids collapses REPL-expanded FOCEI tab signatures", {
  tab <- load_tab("docs/results/opti_fenetre_large_FOCEI/psm_opti/psm_opti.tab")
  obs <- prepare_tab_obs(tab)
  rep_ids <- .select_representative_ids(obs, "TSTRAT", max_ids = 4L)

  expect_equal(rep_ids, c(1, 2))
})

test_that("parse_advan_trans extracts ADVAN2 TRANS2 from example2.ctl", {
  lines <- load_ctl_lines("app/examples/example2/warfarin2.ctl")
  res <- parse_advan_trans(lines)
  expect_equal(res$advan, "ADVAN2")
  expect_equal(res$trans, "TRANS2")
})

test_that("parse_advan_trans returns NULL when $SUBROUTINES absent", {
  expect_null(parse_advan_trans(c("$PROBLEM X", "$THETA (0, 1)")))
})

test_that("parse_advan_trans handles multi-whitespace and lowercase", {
  lines <- c("$subroutines advan4  TRANS4 ", "$THETA (0, 1)")
  res <- parse_advan_trans(lines)
  expect_equal(res$advan, "ADVAN4")
  expect_equal(res$trans, "TRANS4")
})
