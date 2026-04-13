# Quick verification of sse_comparison.R pure functions
# Run from design-explorer/ root:
#   "/c/Program Files/R/R-4.5.2/bin/Rscript" tests/test_sse_comparison.R

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

source("R/design_utils.R")
source("R/design_metrics.R")
source("R/design_io.R")
source("R/sse_metrics.R")
source("R/sse_diagnostics.R")
source("R/report_design.R")
source("R/sse_comparison.R")

cat("=== Loading SSE data ===\n")
orig <- read_sse_raw_all("docs/results/SSE/raw_results_psm_eval_sparseSSE_TABLE.csv")
opti <- read_sse_raw_all("docs/results/SSE/raw_results_run_psm_opti.csv")
cat(sprintf("Original: %d rows, %d converged\n", nrow(orig), sum(orig$converged)))
cat(sprintf("Optimized: %d rows, %d converged\n", nrow(opti), sum(opti$converged)))

cat("\n=== Loading true values ===\n")
ctl_lines <- readLines("docs/results/psm_eval/psm_eval.ctl", warn = FALSE)
tv <- read_true_values(ctl_lines)
cat(sprintf("%d parameters: %s\n", length(tv), paste(names(tv), collapse = ", ")))

cat("\n=== compare_run_health ===\n")
h1 <- compute_run_health(orig)
h2 <- compute_run_health(opti)
hc <- compare_run_health(h1, h2)
print(hc)
stopifnot(nrow(hc) == 5)
cat("OK: 5-row tibble with delta_pct\n")

cat("\n=== compare_rse ===\n")
m1 <- compute_sse_metrics(orig[orig$converged, ], tv)
m2 <- compute_sse_metrics(opti[opti$converged, ], tv)
rc <- compare_rse(m1, m2)
print(rc[, c("param_label", "rse_orig", "rse_opti", "delta_rse", "pct_change")])
stopifnot(nrow(rc) > 0)
cat(sprintf("OK: %d params compared\n", nrow(rc)))

n_improved <- sum(rc$delta_rse < 0, na.rm = TRUE)
n_worsened <- sum(rc$delta_rse > 0, na.rm = TRUE)
cat(sprintf("Improved: %d, Worsened: %d\n", n_improved, n_worsened))

cat("\n=== plot_rse_comparison ===\n")
p1 <- plot_rse_comparison(rc)
stopifnot(inherits(p1, "ggplot"))
cat("OK: ggplot object created\n")

cat("\n=== plot_distribution_overlay ===\n")
d1 <- compute_param_distributions(orig, tv)
d2 <- compute_param_distributions(opti, tv)
p2 <- plot_distribution_overlay(d1, d2)
stopifnot(inherits(p2, "ggplot"))
cat("OK: ggplot object created\n")

cat("\n=== Correlation delta ===\n")
c1 <- compute_empirical_correlations(orig, tv)
c2 <- compute_empirical_correlations(opti, tv)
if (!is.null(c1) && !is.null(c2)) {
  common <- intersect(colnames(c1), colnames(c2))
  delta <- c2[common, common] - c1[common, common]
  cat(sprintf("Correlation delta matrix: %dx%d\n", nrow(delta), ncol(delta)))
  cat("Max |delta|:", round(max(abs(delta[upper.tri(delta)])), 3), "\n")
} else {
  cat("SKIP: not enough parameters for correlation\n")
}

cat("\n=== ALL TESTS PASSED ===\n")
