# =============================================================================
# test-times_integration.R
# Integration walkthrough tests for Bauer examples 2-7
# Tests the full pipeline: dispatcher → renderer → plot
#
# Run via: "/c/Program Files/R/R-4.5.2/bin/Rscript" tests/run_tests.R
# =============================================================================

library(testthat)
library(dplyr)
library(ggplot2)

source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
source(file.path(PROJECT_ROOT, "R", "design_io.R"))
source(file.path(PROJECT_ROOT, "R", "design_metrics.R"))
source(file.path(PROJECT_ROOT, "R", "ctl_parsers.R"))
source(file.path(PROJECT_ROOT, "R", "pk_templates.R"))
source(file.path(PROJECT_ROOT, "R", "report_design.R"))
source(file.path(PROJECT_ROOT, "R", "tab_dispatch.R"))

#' Walkthrough a single example: detect pattern, pick engine, render plot.
#' Verifies that the pipeline doesn't crash and produces either a ggplot or
#' an empty-state fallback (also ggplot) without errors.
#'
#' @param tab_rel relative path to .tab file
#' @param ctl_rel relative path to .ctl file
#' @param expected_pattern expected result of detect_tab_pattern()
walk_example <- function(tab_rel, ctl_rel, expected_pattern) {
  tab <- load_tab(tab_rel)
  ctl <- load_ctl_lines(ctl_rel)

  # Step 1: Detect pattern
  pat <- detect_tab_pattern(tab, ctl)
  expect_equal(pat, expected_pattern,
               info = paste("Pattern mismatch for", basename(tab_rel)))

  # Step 2: Pick smooth-curve engine (Tier 1/2/3)
  thetas <- c(CL = 0.15, V = 8.0, KA = 1.0, V2 = 8.0, Q = 0.5, V3 = 20.0)
  s <- pick_smooth_curve_engine(tab, ctl, theta_values = thetas,
                                mrgsolve_available = FALSE)
  expect_true(s$tier %in% c("template", "dots", "mrgsolve"),
              info = paste("Unexpected tier:", s$tier))

  # Step 3: Render plot based on pattern
  # - For patterns that render timelines: pass to render_pk_timeline
  # - For deferred patterns: pass to render_empty_state
  p <- NULL

  if (pat %in% c("elementary_fo", "focei_repl", "pkpd_multi", "dose_time_opt")) {
    # These patterns have an implementation path (either timeline or fallback dots)
    obs <- prepare_tab_obs(tab)
    if (s$tier == "dots") {
      p <- render_fallback_dot_plot(tab)
    } else {
      p <- render_pk_timeline(s, obs_points = obs)
    }
  } else if (pat %in% c("robust_subprob", "classical", "stratified", "discrete")) {
    # These patterns use empty-state placeholders
    p <- render_empty_state(pat)
  } else {
    # Unknown or undefined pattern
    p <- render_empty_state(pat)
  }

  # Step 4: Verify output is a ggplot (or inherits from gg)
  expect_true(inherits(p, "ggplot") || inherits(p, "gg"),
              info = paste("Expected ggplot, got", class(p)))
}

# =============================================================================
# Example 2: Warfarin, elementary design, FO evaluation
# =============================================================================
test_that("example2 — elementary_fo, FO evaluation", {
  walk_example("app/examples/example2/warfarin2.tab",
               "app/examples/example2/warfarin2.ctl",
               "elementary_fo")
})

# =============================================================================
# Example 3: Warfarin with prior, robust design (multiple subproblems)
# =============================================================================
test_that("example3 — robust_subprob, true prior sampling", {
  walk_example("app/examples/example3/priortrue.tab",
               "app/examples/example3/priortrue.ctl",
               "robust_subprob")
})

# =============================================================================
# Example 4: PK-PD two-compartment, multi-response
# =============================================================================
test_that("example4 — pkpd_multi, PK-PD evaluation", {
  walk_example("app/examples/example4/warfarin_pkpd_eval.tab",
               "app/examples/example4/warfarin_pkpd_eval.ctl",
               "pkpd_multi")
})

# =============================================================================
# Example 5: Design space optimization, elementary design, FO
# =============================================================================
test_that("example5 — elementary_fo, design optimization", {
  walk_example("app/examples/example5/optdesign2.tab",
               "app/examples/example5/optdesign2.ctl",
               "elementary_fo")
})

# =============================================================================
# Example 6: TMDD target-mediated drug disposition, dose & time optimization
# =============================================================================
test_that("example6 — dose_time_opt, TMDD evaluation", {
  walk_example("app/examples/example6/tmdd2.tab",
               "app/examples/example6/tmdd2.ctl",
               "dose_time_opt")
})

# =============================================================================
# Example 7: TMDD with Bayesian FIM, dose & time optimization
# =============================================================================
test_that("example7 — dose_time_opt, TMDD Bayes optimization", {
  walk_example("app/examples/example7/tmdd2b.tab",
               "app/examples/example7/tmdd2b.ctl",
               "dose_time_opt")
})
