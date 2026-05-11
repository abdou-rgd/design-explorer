# =============================================================================
# test-fim_metrics.R
# Unit tests for R/fim_metrics.R (power, NSN, power curve)
#
# Run via: source("tests/run_tests.R") from the project root
# =============================================================================

library(testthat)
library(dplyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Determine root
project_root <- Sys.getenv("DESIGN_EXPLORER_ROOT", unset = NA_character_)
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

# Source dependencies
source(proj("R", "design_utils.R"))
source(proj("R", "design_io.R"))
source(proj("R", "design_metrics.R"))
source(proj("R", "design_summary.R"))
source(proj("R", "ctl_parsers.R"))
source(proj("R", "report_design.R"))
source(proj("R", "fim_metrics.R"))


# =============================================================================
# A. compute_power_wald()
# =============================================================================

test_that("power_wald: large effect gives power ~1", {
  # theta=2, RSE=20% => SE=0.4, W=-5, power ≈ 1
  p <- compute_power_wald(2, 20, h0 = 0, alpha = 0.05)
  expect_gt(p, 0.99)
})

test_that("power_wald: H0 = theta gives power = alpha", {
  # When H0 = true value, power = type I error rate
  p <- compute_power_wald(2, 20, h0 = 2, alpha = 0.05)
  expect_equal(p, 0.05, tolerance = 0.01)
})

test_that("power_wald: matches PopED Wald equation for two-sided test", {
  theta <- 1.5
  rse <- 25
  h0 <- 0.3
  alpha <- 0.05

  se <- abs(theta) * rse / 100
  wald_stat <- (h0 - theta) / se
  norm_val <- abs(qnorm(alpha / 2))
  expected <- 1 - pnorm(wald_stat + norm_val) + pnorm(wald_stat - norm_val)

  expect_equal(
    compute_power_wald(theta, rse, h0 = h0, alpha = alpha, two_sided = TRUE),
    expected,
    tolerance = 1e-12
  )
})

test_that("power_wald: one-sided H1 theta > h0 respects direction", {
  p_above <- compute_power_wald(2, 20, h0 = 0, alpha = 0.05,
                                two_sided = FALSE)
  p_below <- compute_power_wald(-2, 20, h0 = 0, alpha = 0.05,
                                two_sided = FALSE)
  p_null <- compute_power_wald(2, 20, h0 = 2, alpha = 0.05,
                               two_sided = FALSE)

  expect_gt(p_above, 0.99)
  expect_lt(p_below, 1e-6)
  expect_equal(p_null, 0.05, tolerance = 0.01)
})

test_that("power_wald: theta=0 returns NA", {
  expect_true(is.na(compute_power_wald(0, 20)))
})

test_that("power_wald: rse=NA returns NA", {
  expect_true(is.na(compute_power_wald(2, NA_real_)))
})

test_that("power_wald: one-sided gives higher power than two-sided", {
  p1 <- compute_power_wald(2, 30, h0 = 0, alpha = 0.05, two_sided = TRUE)
  p2 <- compute_power_wald(2, 30, h0 = 0, alpha = 0.05, two_sided = FALSE)
  expect_gt(p2, p1)
})

test_that("power_wald: negative theta works", {
  # Negative parameters are common (e.g., log-transformed)
  p <- compute_power_wald(-2, 20, h0 = 0, alpha = 0.05)
  expect_gt(p, 0.99)
})

test_that("power_wald: high RSE gives low power", {
  p <- compute_power_wald(0.5, 200, h0 = 0, alpha = 0.05)
  expect_lt(p, 0.20)
})

test_that("power_wald: rse=0 returns NA (fixed param)", {
  expect_true(is.na(compute_power_wald(2, 0)))
})

test_that("n_needed: rse=0 returns NA", {
  res <- compute_n_needed(2, 0, 50)
  expect_true(is.na(res$n_needed))
})


# =============================================================================
# B. compute_n_needed()
# =============================================================================

test_that("n_needed: already powerful gives n <= current", {
  # RSE=5% at N=50 => very precise, n_needed should be small
  res <- compute_n_needed(2, 5, 50, power_target = 0.80)
  expect_lte(res$n_needed, 50L)
  expect_type(res$n_needed, "integer")
})

test_that("n_needed: high RSE needs more subjects", {
  res <- compute_n_needed(2, 40, 50, h0 = 0, power_target = 0.80)
  expect_gt(res$n_needed, 50L)
})

test_that("n_needed: theta=0 returns NA", {
  res <- compute_n_needed(0, 20, 50)
  expect_true(is.na(res$n_needed))
  expect_true(is.na(res$rse_needed))
})

test_that("n_needed: returns at least 1", {
  res <- compute_n_needed(10, 1, 100, power_target = 0.80)
  expect_gte(res$n_needed, 1L)
})

test_that("n_needed: scaling is consistent with power", {
  # At n_needed, power should be >= target
  res <- compute_n_needed(2, 30, 50, h0 = 0, power_target = 0.80)
  rse_at_n_needed <- 30 * sqrt(50 / res$n_needed)
  p <- compute_power_wald(2, rse_at_n_needed, h0 = 0, alpha = 0.05)
  expect_gte(p, 0.80 - 0.01)  # tolerance for ceiling
})

test_that("n_needed: matches PopED needed SE plus FIM scaling", {
  theta <- 1.5
  rse <- 35
  n0 <- 40L
  h0 <- 0.3
  alpha <- 0.05
  target <- 0.80

  norm_val <- abs(qnorm(alpha / 2))
  need_se <- abs(h0 - theta) / (norm_val - qnorm(1 - target))
  need_rse <- need_se / abs(theta) * 100
  expected_n <- ceiling((rse / need_rse)^2 * n0)

  res <- compute_n_needed(theta, rse, n0, h0 = h0, alpha = alpha,
                          power_target = target, two_sided = TRUE)

  expect_equal(res$rse_needed, need_rse, tolerance = 1e-12)
  expect_equal(res$n_needed, as.integer(expected_n))
})

test_that("n_needed: one-sided H1 theta > h0 is impossible when theta <= h0", {
  res <- compute_n_needed(-2, 20, 50, h0 = 0, two_sided = FALSE)
  expect_true(is.na(res$n_needed))
  expect_true(is.na(res$rse_needed))
})

test_that("n_needed: higher power target requires more subjects", {
  res80 <- compute_n_needed(2, 30, 50, power_target = 0.80)
  res90 <- compute_n_needed(2, 30, 50, power_target = 0.90)
  expect_gt(res90$n_needed, res80$n_needed)
})


# =============================================================================
# C. compute_power_table()
# =============================================================================

test_that("compute_power_table: works with example1 .ext", {
  ext_path <- proj("app/examples/example1/warfarin.ext")
  if (!file.exists(ext_path)) skip("Example 1 .ext not found")
  ext <- read_ext(ext_path)
  tbl <- compute_power_table(ext, groupsize = 32L)
  expect_s3_class(tbl, "data.frame")
  expect_true(nrow(tbl) > 0)
  expect_true(all(c("param", "label", "estimate", "rse_pct", "power", "n_needed") %in% names(tbl)))
  # All powers should be numeric (may contain NA for theta~0)
  expect_type(tbl$power, "double")
})


# =============================================================================
# D. plot_power_curve()
# =============================================================================

test_that("plot_power_curve: returns ggplot", {
  p <- plot_power_curve(2, 20, 50, param_name = "CL")
  expect_s3_class(p, "ggplot")
})

test_that("plot_power_curve: handles theta=0 gracefully", {
  p <- plot_power_curve(0, 20, 50)
  expect_s3_class(p, "ggplot")
})


# =============================================================================
# E. parse_groupsize()
# =============================================================================

test_that("parse_groupsize: extracts GROUPSIZE from $DESIGN block", {
  lines <- c(
    "$PROB TEST",
    "$DESIGN GROUPSIZE=80 FIMTYPE=1 APPROX=FOCEI MAXEVAL=0"
  )
  expect_equal(parse_groupsize(lines), 80L)
})

test_that("parse_groupsize: returns NA when no $DESIGN", {
  lines <- c("$PROB TEST", "$EST METHOD=1")
  expect_true(is.na(parse_groupsize(lines)))
})

test_that("parse_groupsize: returns NA when GROUPSIZE absent", {
  lines <- c("$PROB TEST", "$DESIGN FIMTYPE=1 MAXEVAL=0")
  expect_true(is.na(parse_groupsize(lines)))
})

test_that("parse_groupsize: handles multiline $DESIGN", {
  lines <- c(
    "$PROB TEST",
    "$DESIGN FIMTYPE=1",
    "  GROUPSIZE=120",
    "  MAXEVAL=9999",
    "$TABLE TIME IPRED"
  )
  expect_equal(parse_groupsize(lines), 120L)
})

test_that("parse_groupsize: handles NULL input", {
  expect_true(is.na(parse_groupsize(NULL)))
})

test_that("robust D-criterion summarizes all TABLE NO. blocks", {
  ext <- tibble::tibble(
    table_no = c(1L, 2L, 3L),
    type = rep("final", 3),
    ITERATION = NA_real_,
    OBJ = c(-10, -12, -14)
  )

  rdc <- get_robust_d_criterion(ext, n_params = 2L)

  expect_equal(rdc$n_subprob, 3L)
  expect_equal(rdc$d_robust, exp(-mean(ext$OBJ) / 2), tolerance = 1e-12)
  expect_equal(rdc$d_p10, exp(-stats::quantile(ext$OBJ, 0.90)[[1]] / 2),
               tolerance = 1e-12)
  expect_equal(rdc$d_p90, exp(-stats::quantile(ext$OBJ, 0.10)[[1]] / 2),
               tolerance = 1e-12)
})

test_that("robust D-criterion refuses single-table or parameterless inputs", {
  ext <- tibble::tibble(
    table_no = 1L,
    type = "final",
    ITERATION = NA_real_,
    OBJ = -10
  )

  expect_null(get_robust_d_criterion(ext, n_params = 2L))
  expect_null(get_robust_d_criterion(ext, n_params = 0L))
})


# =============================================================================
# F. compute_power_tost()
# =============================================================================

test_that("tost: centered (beta1=0) is most powerful position", {
  # beta1=0 (symmetric) should give higher power than off-center
  p_center <- compute_power_tost(2, 20, delta_L = 3, h0 = 2)  # beta1=0
  p_off    <- compute_power_tost(2, 20, delta_L = 3, h0 = 0)  # beta1=2
  expect_gt(p_center, p_off)
})

test_that("tost: small deviation + tight SE gives high power", {
  # theta=2, h0=0, delta_L=3 => beta1=2, inside [-3,3], SE=2*10/100=0.2
  p <- compute_power_tost(2, 10, delta_L = 3, h0 = 0)
  expect_gt(p, 0.90)
})

test_that("tost: matches PFIM equation 4 for negative branch", {
  theta <- 0.8
  h0 <- 1
  rse <- 20
  delta_L <- 0.6
  alpha <- 0.05

  beta1 <- theta - h0
  se <- abs(theta) * rse / 100
  z_alpha <- qnorm(1 - alpha)
  expected <- 1 - pnorm(z_alpha - (beta1 + delta_L) / se)

  expect_equal(
    compute_power_tost(theta, rse, delta_L = delta_L, h0 = h0, alpha = alpha),
    expected,
    tolerance = 1e-12
  )
})

test_that("tost: matches PFIM equation 5 for positive branch", {
  theta <- 1.2
  h0 <- 1
  rse <- 20
  delta_L <- 0.6
  alpha <- 0.05

  beta1 <- theta - h0
  se <- abs(theta) * rse / 100
  z_alpha <- qnorm(1 - alpha)
  expected <- pnorm(-z_alpha - (beta1 - delta_L) / se)

  expect_equal(
    compute_power_tost(theta, rse, delta_L = delta_L, h0 = h0, alpha = alpha),
    expected,
    tolerance = 1e-12
  )
})

test_that("tost: outside margin returns 0", {
  # theta=0.5, delta_L=0.2 => beta1=0.5 > 0.2
  p <- compute_power_tost(0.5, 10, delta_L = 0.2, h0 = 0)
  expect_equal(p, 0)
})

test_that("tost: negative branch works", {
  # theta=-1, h0=0, delta_L=2 => beta1=-1, in [-2, 0]
  p <- compute_power_tost(-1, 15, delta_L = 2, h0 = 0)
  expect_gt(p, 0)
  expect_lte(p, 1)
})

test_that("tost: theta=0 returns NA", {
  expect_true(is.na(compute_power_tost(0, 20, delta_L = 0.5)))
})

test_that("tost: rse=NA returns NA", {
  expect_true(is.na(compute_power_tost(1, NA_real_, delta_L = 0.5)))
})

test_that("tost: delta_L<=0 returns NA", {
  expect_true(is.na(compute_power_tost(1, 20, delta_L = 0)))
  expect_true(is.na(compute_power_tost(1, 20, delta_L = -1)))
})

test_that("tost: higher rse gives lower power (monotonicity)", {
  p1 <- compute_power_tost(1, 10, delta_L = 2, h0 = 0)
  p2 <- compute_power_tost(1, 30, delta_L = 2, h0 = 0)
  expect_gt(p1, p2)
})

test_that("tost: higher alpha gives higher power", {
  p1 <- compute_power_tost(1, 20, delta_L = 2, h0 = 0, alpha = 0.05)
  p2 <- compute_power_tost(1, 20, delta_L = 2, h0 = 0, alpha = 0.10)
  expect_gt(p2, p1)
})

test_that("tost: boundary (beta1 = delta_L exactly) returns 0", {
  p <- compute_power_tost(0.2, 10, delta_L = 0.2, h0 = 0)
  expect_equal(p, 0)
})


# =============================================================================
# G. compute_nsn_tost()
# =============================================================================

test_that("nsn_tost: already powerful gives n <= current", {
  # theta=1, RSE=5%, delta_L=2 => very precise, inside margin
  res <- compute_nsn_tost(1, 5, 50, delta_L = 2, power_target = 0.80)
  expect_lte(res$n_needed, 50L)
  expect_type(res$n_needed, "integer")
})

test_that("nsn_tost: high RSE needs more subjects", {
  # RSE=80% at N=50, delta_L=2 => needs many more subjects
  res <- compute_nsn_tost(1, 80, 50, delta_L = 2, power_target = 0.80)
  expect_gt(res$n_needed, 50L)
})

test_that("nsn_tost: outside margin returns NA", {
  res <- compute_nsn_tost(5, 20, 50, delta_L = 0.2)
  expect_true(is.na(res$n_needed))
  expect_true(is.na(res$rse_needed))
})

test_that("nsn_tost: consistency — power at n_needed >= target", {
  res <- compute_nsn_tost(1, 30, 50, delta_L = 2, h0 = 0,
                          alpha = 0.05, power_target = 0.80)
  rse_at_n <- 30 * sqrt(50 / res$n_needed)
  p <- compute_power_tost(1, rse_at_n, delta_L = 2, h0 = 0, alpha = 0.05)
  expect_gte(p, 0.80 - 0.01)  # tolerance for ceiling
})

test_that("nsn_tost: matches PFIM equations 6 and 7 plus FIM scaling", {
  n0 <- 50L
  rse <- 30
  delta_L <- 0.6
  alpha <- 0.05
  target <- 0.80
  z_alpha <- qnorm(1 - alpha)

  theta_neg <- 0.8
  beta_neg <- theta_neg - 1
  nse_neg <- (-beta_neg - delta_L) / (-z_alpha + qnorm(1 - target))
  rse_needed_neg <- nse_neg / abs(theta_neg) * 100
  n_needed_neg <- ceiling((rse / rse_needed_neg)^2 * n0)

  res_neg <- compute_nsn_tost(theta_neg, rse, n0, delta_L = delta_L,
                              h0 = 1, alpha = alpha, power_target = target)
  expect_equal(res_neg$rse_needed, rse_needed_neg, tolerance = 1e-12)
  expect_equal(res_neg$n_needed, as.integer(n_needed_neg))

  theta_pos <- 1.2
  beta_pos <- theta_pos - 1
  nse_pos <- (-beta_pos + delta_L) / (z_alpha + qnorm(target))
  rse_needed_pos <- nse_pos / abs(theta_pos) * 100
  n_needed_pos <- ceiling((rse / rse_needed_pos)^2 * n0)

  res_pos <- compute_nsn_tost(theta_pos, rse, n0, delta_L = delta_L,
                              h0 = 1, alpha = alpha, power_target = target)
  expect_equal(res_pos$rse_needed, rse_needed_pos, tolerance = 1e-12)
  expect_equal(res_pos$n_needed, as.integer(n_needed_pos))
})

test_that("nsn_tost: higher power_target needs more subjects", {
  res80 <- compute_nsn_tost(1, 30, 50, delta_L = 2, power_target = 0.80)
  res90 <- compute_nsn_tost(1, 30, 50, delta_L = 2, power_target = 0.90)
  expect_gt(res90$n_needed, res80$n_needed)
})


# =============================================================================
# H. compute_equiv_table()
# =============================================================================

test_that("compute_equiv_table: works with example1 .ext", {
  ext_path <- proj("app/examples/example1/warfarin.ext")
  if (!file.exists(ext_path)) skip("Example 1 .ext not found")
  ext <- read_ext(ext_path)
  tbl <- compute_equiv_table(ext, groupsize = 32L, delta_L = 0.5)
  expect_s3_class(tbl, "data.frame")
  expect_true(nrow(tbl) > 0)
  expect_true(all(c("param", "label", "estimate", "rse_pct", "power",
                     "n_needed", "outside_margin") %in% names(tbl)))
  expect_type(tbl$power, "double")
  expect_type(tbl$outside_margin, "logical")
})
