# =============================================================================
# test-parse_design_outputs.R
# Unit tests for scripts/parse_design_outputs.R
#
# Run via: source("tests/run_tests.R") from the project root
# =============================================================================

library(testthat)
library(dplyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Determine root: prefer env var set by run_tests.R, else climb file tree
project_root <- Sys.getenv("CLAUDEPROJETS_ROOT", unset = NA_character_)
if (is.na(project_root) || !nzchar(project_root)) {
  this_file <- tryCatch(
    normalizePath(sys.frame(0)$ofile),
    error = function(e) NULL
  )
  if (!is.null(this_file)) {
    d <- dirname(this_file)
    for (i in seq_len(6)) {
      if (file.exists(file.path(d, "CLAUDE.md"))) {
        project_root <- d
        break
      }
      d <- dirname(d)
    }
  }
  if (is.na(project_root) || !nzchar(project_root)) {
    project_root <- getwd()
  }
}

proj <- function(...) file.path(project_root, ...)

# Source the script under test
source(proj("R", "parse_design_outputs.R"))

# ── Paths to example files ────────────────────────────────────────────────────
ex1_ext <- proj("app/examples/example1/warfarin.ext")
ex1_shk <- proj("app/examples/example1/warfarin.shk")
ex1_coi <- proj("app/examples/example1/warfarin.coi")
ex2_tab <- proj("app/examples/example2/warfarin2b.tab")
ex3_ext <- proj("app/examples/example3/priortrue.ext")
ex3_shk <- proj("app/examples/example3/priortrue.shk")
ex6_coi <- proj("app/examples/example6/tmdd2.coi")
ex6_clt <- proj("app/examples/example6/tmdd2.clt")


# =============================================================================
# A. read_ext()
# =============================================================================

test_that("read_ext() returns a tibble with required columns", {
  ext <- read_ext(ex1_ext)
  expect_s3_class(ext, "tbl_df")
  expect_true("table_no"  %in% names(ext))
  expect_true("type"      %in% names(ext))
  expect_true("ITERATION" %in% names(ext))
  expect_true("OBJ"       %in% names(ext))
  expect_true(any(startsWith(names(ext), "THETA")))
  expect_true(any(startsWith(names(ext), "OMEGA")))
  expect_true(any(startsWith(names(ext), "SIGMA")))
})

test_that("read_ext() sentinel values (1e10) are replaced by NA", {
  ext <- read_ext(ex1_ext)
  param_cols <- setdiff(
    names(ext),
    c("table_no", "type", "ITERATION", "OBJ")
  )
  raw_max <- max(
    unlist(lapply(ext[param_cols], function(x) max(abs(x), na.rm = TRUE))),
    na.rm = TRUE
  )
  # No value should remain at sentinel magnitude
  expect_lt(raw_max, 1e9)
  # Fixed / off-diagonal params were 1e10, so NAs must be present
  has_na <- any(sapply(ext[param_cols], function(x) any(is.na(x))))
  expect_true(has_na)
})

test_that("read_ext() has exactly one 'final' row in example1 (1 table)", {
  ext <- read_ext(ex1_ext)
  final_rows <- filter(ext, type == "final")
  expect_equal(nrow(final_rows), 1L)
  expect_equal(final_rows$table_no, 1L)
})

test_that("read_ext() has 'se' row present in example1", {
  ext <- read_ext(ex1_ext)
  expect_gte(nrow(filter(ext, type == "se")), 1L)
})

test_that("read_ext() type column uses only expected labels", {
  ext <- read_ext(ex1_ext)
  valid_types <- c(
    "iteration", "final", "se", "eigenvalues", "condition",
    "sd_corr", "se_sd_corr", "fixed_flags", "termination", "gradient"
  )
  expect_true(all(ext$type %in% valid_types))
})

test_that("read_ext() handles example3 with 1000 TABLE NO.s", {
  ext <- read_ext(ex3_ext)
  tables_present <- sort(unique(ext$table_no))
  expect_gte(length(tables_present), 2L)
  expect_equal(tables_present[1], 1L)
  expect_equal(tables_present[2], 2L)
  # Consecutive numbering: max == count
  expect_equal(max(tables_present), length(tables_present))
})

test_that("read_ext() errors with 'introuvable' on missing file", {
  expect_error(
    read_ext(proj("app/examples/does_not_exist.ext")),
    "introuvable"
  )
})


# =============================================================================
# B. get_ofv()
# =============================================================================

test_that("get_ofv() returns numeric(1) for a valid run", {
  ext <- read_ext(ex1_ext)
  ofv <- get_ofv(ext)
  expect_type(ofv, "double")
  expect_length(ofv, 1L)
  expect_false(is.na(ofv))
})

test_that("get_ofv() returns correct OFV value for example1", {
  ext <- read_ext(ex1_ext)
  # From warfarin.ext ITER -1e9: OBJ = -39.518205662077754
  expect_equal(get_ofv(ext), -39.518205662077754, tolerance = 1e-6)
})

test_that("get_ofv() returns length-0 or NA for a missing table_no", {
  ext <- read_ext(ex1_ext)
  result <- get_ofv(ext, table_no = 9999L)
  # get_final_params returns 0-row tibble → $OBJ is numeric(0).
  # Callers must guard with length(); document actual behaviour here.
  expect_true(
    length(result) == 0L ||
      (length(result) == 1L && is.na(result))
  )
})

test_that("get_ofv() on 0-row ext does not throw an error", {
  ext <- read_ext(ex1_ext)
  empty_ext <- filter(ext, FALSE)
  # max() on empty vector emits a warning; we suppress it deliberately.
  result <- suppressWarnings(
    tryCatch(get_ofv(empty_ext), error = function(e) e)
  )
  expect_false(inherits(result, "error"))
})


# =============================================================================
# C. get_rse()
# =============================================================================

test_that("get_rse() returns tibble with required columns", {
  ext <- read_ext(ex1_ext)
  rse <- get_rse(ext)
  expect_s3_class(rse, "tbl_df")
  expect_true(all(c("param", "estimate", "se", "rse_pct") %in% names(rse)))
})

test_that("get_rse() rse_pct values are all non-negative", {
  ext <- read_ext(ex1_ext)
  rse <- get_rse(ext)
  expect_gt(nrow(rse), 0L)
  expect_true(all(rse$rse_pct >= 0))
})

test_that("get_rse() excludes fixed parameters (those with NA SE)", {
  ext <- read_ext(ex1_ext)
  rse <- get_rse(ext)
  expect_false(any(is.na(rse$se)))
  expect_false(any(is.na(rse$estimate)))
})

test_that("get_rse() returns 0-row tibble gracefully when SE row absent", {
  ext    <- read_ext(ex1_ext)
  no_se  <- filter(ext, type != "se")
  result <- get_rse(no_se)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("get_rse() param column contains THETA and SIGMA names", {
  ext    <- read_ext(ex1_ext)
  params <- get_rse(ext)$param
  expect_true(any(startsWith(params, "THETA")))
  expect_true(any(startsWith(params, "SIGMA")))
})


# =============================================================================
# D. read_shk()
# =============================================================================

test_that("read_shk() returns tibble with required columns", {
  shk <- read_shk(ex1_shk)
  expect_s3_class(shk, "tbl_df")
  expect_true("table_no" %in% names(shk))
  expect_true("type_id"  %in% names(shk))
  expect_true("subpop"   %in% names(shk))
})

test_that("read_shk() ETA columns are named ETAn (no parentheses)", {
  shk      <- read_shk(ex1_shk)
  eta_cols <- grep("^ETA", names(shk), value = TRUE)
  expect_gte(length(eta_cols), 1L)
  expect_false(any(grepl("\\(", eta_cols)))
  expect_true(all(grepl("^ETA\\d+$", eta_cols)))
})

test_that("read_shk() TYPE 11 (RELATIVEINF%) row is present in example1", {
  shk <- read_shk(ex1_shk)
  expect_gte(nrow(filter(shk, type_id == 11L)), 1L)
})

test_that("read_shk() type_id column is integer", {
  shk <- read_shk(ex1_shk)
  expect_type(shk$type_id, "integer")
})

test_that("read_shk() handles example3 with 1000 TABLE NO.s", {
  shk <- read_shk(ex3_shk)
  expect_gte(max(shk$table_no), 2L)
  expect_true(any(shk$type_id == 11L))
})

test_that("read_shk() errors with 'introuvable' on missing file", {
  expect_error(
    read_shk(proj("app/examples/does_not_exist.shk")),
    "introuvable"
  )
})


# =============================================================================
# E. get_condition_number()
# =============================================================================

test_that("get_condition_number() returns list with correct element names", {
  ext    <- read_ext(ex1_ext)
  result <- get_condition_number(ext)
  expect_type(result, "list")
  expect_true(all(
    c("condition_number", "min_eigenvalue", "max_eigenvalue") %in%
      names(result)
  ))
})

test_that("get_condition_number() returns NAs when condition row absent", {
  # example1 .ext has no condition sentinel row (-1e9-3)
  ext       <- read_ext(ex1_ext)
  cond_rows <- filter(ext, type == "condition")
  if (nrow(cond_rows) == 0L) {
    result <- get_condition_number(ext)
    expect_true(is.na(result$condition_number))
    expect_true(is.na(result$min_eigenvalue))
    expect_true(is.na(result$max_eigenvalue))
  } else {
    result <- get_condition_number(ext)
    expect_type(result$condition_number, "double")
  }
})

test_that("get_condition_number() returns NAs gracefully on 0-row ext", {
  ext       <- read_ext(ex1_ext)
  empty_ext <- filter(ext, FALSE)
  result    <- suppressWarnings(get_condition_number(empty_ext))
  expect_true(is.na(result$condition_number))
  expect_true(is.na(result$min_eigenvalue))
  expect_true(is.na(result$max_eigenvalue))
})


# =============================================================================
# F. prepare_tab_obs()
# =============================================================================

test_that("prepare_tab_obs() removes dose row (row 1) when EVID absent", {
  tab         <- select(read_tab(ex2_tab), -any_of("EVID"))
  n_before    <- nrow(tab)
  result      <- prepare_tab_obs(tab)
  expect_equal(nrow(result), n_before - 1L)
})

test_that("prepare_tab_obs() filters to EVID==0 when EVID present", {
  tab <- read_tab(ex2_tab)
  if ("EVID" %in% names(tab)) {
    result <- prepare_tab_obs(tab)
    expect_true(all(result$EVID == 0))
  } else {
    skip("EVID column not present in example2 tab")
  }
})

test_that("prepare_tab_obs() adds TSTRAT=1 when column absent", {
  tab    <- data.frame(TIME = c(0, 1, 4, 8), IPRED = c(0, 1.2, 2.3, 1.1))
  result <- prepare_tab_obs(tab)
  expect_true("TSTRAT" %in% names(result))
  expect_true(all(result$TSTRAT == 1L))
})

test_that("prepare_tab_obs() preserves existing TSTRAT without overwriting", {
  tab <- data.frame(
    TIME   = c(0, 1, 4, 8),
    TSTRAT = c(0L, 1L, 2L, 3L),
    IPRED  = c(0, 1.2, 2.3, 1.1)
  )
  result <- prepare_tab_obs(tab)
  expect_true("TSTRAT" %in% names(result))
  # Row 1 removed (no EVID) → remaining TSTRAT values are 1, 2, 3
  expect_false(all(result$TSTRAT == 1L))
})

test_that("prepare_tab_obs() preserves all original columns", {
  tab    <- data.frame(TIME = c(0, 1, 4), IPRED = c(0, 1.0, 2.0))
  result <- prepare_tab_obs(tab)
  expect_true(all(c("TIME", "IPRED") %in% names(result)))
})


# =============================================================================
# G. read_coi() — Fisher Information Matrix
# =============================================================================

test_that("read_coi() returns a named numeric matrix", {
  fim <- read_coi(ex1_coi)
  expect_true(is.matrix(fim))
  expect_type(fim, "double")
  expect_false(is.null(rownames(fim)))
  expect_false(is.null(colnames(fim)))
})

test_that("read_coi() matrix is square and symmetric", {
  fim <- read_coi(ex1_coi)
  expect_equal(nrow(fim), ncol(fim))
  expect_equal(fim, t(fim))
})

test_that("read_coi() column names include NONMEM THETA names", {
  fim <- read_coi(ex1_coi)
  expect_true(any(startsWith(colnames(fim), "THETA")))
})

test_that("read_coi() errors with 'introuvable' on missing file", {
  expect_error(
    read_coi(proj("app/examples/does_not_exist.coi")),
    "introuvable"
  )
})

test_that("read_coi() falls back to last table with warning when table_no=1 absent", {
  # ex6 .coi has TABLE NO. 4 only (4 chained $DESIGN blocks)
  expect_warning(
    fim <- read_coi(ex6_coi, table_no = 1L),
    "repli sur la derniere table"
  )
  expect_true(is.matrix(fim))
  expect_equal(nrow(fim), ncol(fim))
  expect_true(any(startsWith(colnames(fim), "THETA")))
})


# =============================================================================
# G2. read_clt() — FIM triangulaire inférieure
# =============================================================================

test_that("read_clt() returns a named numeric square matrix", {
  fim <- read_clt(proj("app/examples/example1/warfarin.clt"))
  expect_true(is.matrix(fim))
  expect_type(fim, "double")
  expect_equal(nrow(fim), ncol(fim))
  expect_false(is.null(rownames(fim)))
  expect_false(is.null(colnames(fim)))
})

test_that("read_clt() matrix is symmetric", {
  fim <- read_clt(proj("app/examples/example1/warfarin.clt"))
  expect_equal(fim, t(fim))
})

test_that("read_clt() falls back to last table with warning when table_no=1 absent", {
  # ex6 .clt has TABLE NO. 4 only
  expect_warning(
    fim <- read_clt(ex6_clt, table_no = 1L),
    "repli sur la derniere table"
  )
  expect_true(is.matrix(fim))
  expect_equal(nrow(fim), ncol(fim))
})

test_that("read_clt() errors with 'introuvable' on missing file", {
  expect_error(
    read_clt(proj("app/examples/does_not_exist.clt")),
    "introuvable"
  )
})


# =============================================================================
# H. get_relativeinf()
# =============================================================================

test_that("get_relativeinf() returns tibble with eta and relativeinf_pct", {
  shk <- read_shk(ex1_shk)
  ri  <- get_relativeinf(shk)
  expect_s3_class(ri, "tbl_df")
  expect_true("eta"            %in% names(ri))
  expect_true("relativeinf_pct" %in% names(ri))
  expect_gt(nrow(ri), 0L)
})

test_that("get_relativeinf() values sum to ~100 for example1", {
  shk   <- read_shk(ex1_shk)
  ri    <- get_relativeinf(shk)
  total <- sum(ri$relativeinf_pct, na.rm = TRUE)
  # RELATIVEINF(%) partitions total FIM information across ETAs
  expect_equal(total, 100, tolerance = 1)
})


# =============================================================================
# I. get_d_criterion()
# =============================================================================

test_that("get_d_criterion() computes exp(-ofv / n_params)", {
  ofv <- -39.518205662077754
  n   <- 6L
  dc  <- get_d_criterion(ofv, n)
  expect_type(dc, "double")
  expect_gt(dc, 0)
  expect_equal(dc, exp(-ofv / n), tolerance = 1e-8)
})

test_that("get_d_criterion() returns NA for NA ofv", {
  expect_true(is.na(get_d_criterion(NA_real_, 6L)))
})

test_that("get_d_criterion() returns NA for n_params <= 0", {
  expect_true(is.na(get_d_criterion(-10, 0L)))
})


# =============================================================================
# J. Integration: summary_design() runs without error on example1
# =============================================================================

test_that("summary_design() runs without error on example1", {
  ext    <- read_ext(ex1_ext)
  shk    <- read_shk(ex1_shk)
  result <- tryCatch(
    capture.output(summary_design(ext, shk, run_name = "test")),
    error = function(e) e
  )
  expect_false(inherits(result, "error"))
})
