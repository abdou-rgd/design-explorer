# Smoke test: SSE pipeline round-trip on the two real PsN raw_results files.
# Goal: catch silent breakage if PsN column naming or the normalizer drifts.
# Run from design-explorer/ root:
#   "/c/Program Files/R/R-4.5.2/bin/Rscript" tests/test_sse_pipeline.R

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

source("R/design_utils.R")
source("R/design_metrics.R")
source("R/design_io.R")
source("R/ctl_parsers.R")
source("R/sse_metrics.R")
source("R/sse_diagnostics.R")
source("R/report_design.R")

SSE_FILES <- list(
  eval = "docs/results/SSE/raw_results_psm_eval_sparseSSE_TABLE.csv",
  opti = "docs/results/SSE/raw_results_run_psm_opti.csv"
)
CTL_FILE <- "docs/results/psm_eval/psm_eval.ctl"

ctl_lines <- readLines(CTL_FILE, warn = FALSE)
tv <- read_true_values(ctl_lines)
stopifnot(length(tv) > 0L)
cat(sprintf("true_values: %d params (%s)\n",
            length(tv), paste(head(names(tv), 5), collapse = ", ")))

for (label in names(SSE_FILES)) {
  file <- SSE_FILES[[label]]
  cat(sprintf("\n=== %s :: %s ===\n", label, basename(file)))

  sse <- read_sse_raw_all(file)
  stopifnot(inherits(sse, "tbl_df"))
  stopifnot("converged" %in% names(sse))
  n_total <- attr(sse, "n_total")
  n_success <- attr(sse, "n_success")
  stopifnot(n_total > 0L)
  cat(sprintf("  rows=%d (converged=%d), cols=%d\n",
              n_total, n_success, ncol(sse)))

  # column-normalization should produce canonical NONMEM-style names
  stopifnot(any(grepl("^THETA\\d+$",            names(sse))))
  stopifnot(any(grepl("^OMEGA\\(\\d+,\\d+\\)$", names(sse))))
  stopifnot(any(grepl("^SIGMA\\(\\d+,\\d+\\)$", names(sse))))

  # true_values / SSE column intersection must be non-empty
  common <- intersect(names(tv), names(sse))
  stopifnot(length(common) > 0L)
  cat(sprintf("  true_values intersect SSE: %d params\n", length(common)))

  # compute_param_distributions: one block per shared param
  dd <- compute_param_distributions(sse, tv)
  stopifnot(nrow(dd) > 0L)
  stopifnot(length(unique(dd$param)) == length(common))

  # compute_param_diagnostics: one row per shared param
  pd <- compute_param_diagnostics(sse, tv)
  stopifnot(nrow(pd) == length(common))

  # Shrinkage columns should exist (schema check), but may be entirely NA
  # on eval/opti runs where PsN didn't compute post-hoc shrinkage.
  shrink_cols <- grep("^shrinkage_eta\\d+\\(%\\)$", names(sse), value = TRUE)
  stopifnot(length(shrink_cols) > 0L)

  sl <- compute_shrinkage_long(sse, only_converged = TRUE)
  status <- attr(sl, "status")
  cat(sprintf("  shrinkage_long status=%s (rows=%d)\n", status, nrow(sl)))
  stopifnot(status %in% c("ok", "all_na", "no_rows"))

  ss <- compute_shrinkage_summary(sse, only_converged = TRUE)
  # summary either has one row per ETA column (ok path) or 0 rows (all-NA)
  stopifnot(nrow(ss) %in% c(0L, length(shrink_cols)))

  if (status == "ok") {
    # Live shrinkage data — exercise the live path
    stopifnot(nrow(sl) > 0L)
    stopifnot(length(unique(sl$eta)) <= length(shrink_cols))
    # H2 check — run_id should match PsN sample column when available
    if ("sample" %in% names(sse)) {
      stopifnot(all(sl$run_id %in% sse$sample[sse$converged]))
      cat("  run_id uses PsN sample column (H2 OK)\n")
    }
  } else {
    cat("  (shrinkage all-NA: live path skipped, empty-state path covered)\n")
  }

  # Scatter plot must build whether shrinkage is present or not.
  # H1 path — pre-computed shrink_sum passed by caller.
  p <- plot_shrinkage_rse_scatter(sse, tv, param_labels = NULL, shrink_sum = ss)
  stopifnot(inherits(p, "ggplot"))
  cat("  plot_shrinkage_rse_scatter w/ pre-computed shrink_sum OK (H1)\n")

  # Auto-compute fallback path.
  p2 <- plot_shrinkage_rse_scatter(sse, tv, param_labels = NULL)
  stopifnot(inherits(p2, "ggplot"))

  cat(sprintf("  %s: PASS\n", label))
}

cat("\nAll SSE pipeline smoke tests passed.\n")
