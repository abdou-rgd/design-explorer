# =============================================================================
# sse_comparison.R — Two-SSE comparison (original vs optimized design)
#
# Pure computation + plot functions for comparing two SSE results side by side.
# Reuses single-SSE functions from sse_diagnostics.R and sse_metrics.R.
#
# Prereqs: ggplot2, dplyr, tidyr (already loaded by app.R)
# =============================================================================


# =============================================================================
# compare_run_health() — Side-by-side run health stages
# =============================================================================

#' Compare run health between two SSE results.
#'
#' @param health_orig List from compute_run_health() for original design
#' @param health_opti List from compute_run_health() for optimized design
#' @return Tibble: stage, n_orig, denom_orig, pct_orig,
#'                 n_opti, denom_opti, pct_opti, delta_pct
#' @export
compare_run_health <- function(health_orig, health_opti) {
  s_orig <- health_orig$stages
  s_opti <- health_opti$stages

  merged <- dplyr::inner_join(
    s_orig, s_opti,
    by = "stage", suffix = c("_orig", "_opti")
  )

  merged$delta_pct <- merged$pct_opti - merged$pct_orig

  # Preserve stage order from original
  merged$stage <- factor(merged$stage, levels = s_orig$stage)
  merged
}


# =============================================================================
# compare_rse() — RSE comparison between two SSE results
# =============================================================================

#' Compare empirical RSE from two SSE runs.
#'
#' @param metrics_orig Tibble from compute_sse_metrics() for original design
#' @param metrics_opti Tibble from compute_sse_metrics() for optimized design
#' @return Tibble: param, param_label, param_type, true_value,
#'                 rse_orig, rse_opti, delta_rse, pct_change,
#'                 rrmse_orig, rrmse_opti, bias_orig, bias_opti,
#'                 n_orig, n_opti
#' @export
compare_rse <- function(metrics_orig, metrics_opti) {
  merged <- dplyr::inner_join(
    metrics_orig, metrics_opti,
    by = "param", suffix = c("_orig", "_opti")
  )

  data.frame(
    param       = merged$param,
    param_label = merged$param_label_orig,
    param_type  = merged$param_type_orig,
    true_value  = merged$true_value_orig,
    rse_orig    = merged$rse_empirical_orig,
    rse_opti    = merged$rse_empirical_opti,
    delta_rse   = merged$rse_empirical_opti - merged$rse_empirical_orig,
    pct_change  = ifelse(
      abs(merged$rse_empirical_orig) > 1e-10,
      round(100 * (merged$rse_empirical_opti - merged$rse_empirical_orig) /
              merged$rse_empirical_orig, 1),
      NA_real_
    ),
    rrmse_orig  = merged$rmse_relative_orig,
    rrmse_opti  = merged$rmse_relative_opti,
    bias_orig   = merged$relative_bias_orig,
    bias_opti   = merged$relative_bias_opti,
    n_orig      = merged$n_orig,
    n_opti      = merged$n_opti,
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# plot_rse_comparison() — Grouped bar chart of RSE: original vs optimized
# =============================================================================

#' Plot RSE comparison between two SSE designs.
#'
#' Grouped bar chart: one group per parameter, two bars (original, optimized).
#' Horizontal reference line at 30% RSE (pharmacometrics convention).
#'
#' @param rse_comp  Tibble from compare_rse()
#' @param name_orig Display name for original design (default "Original")
#' @param name_opti Display name for optimized design (default "Optimized")
#' @return ggplot object
#' @export
plot_rse_comparison <- function(rse_comp, name_orig = "Original",
                                name_opti = "Optimized") {
  if (is.null(rse_comp) || nrow(rse_comp) == 0L) {
    return(ggplot() + labs(title = "No data for RSE comparison") +
           .theme_design())
  }

  # Pivot to long format for grouped bars
  long <- data.frame(
    param_label = rep(rse_comp$param_label, 2),
    param_type  = rep(rse_comp$param_type, 2),
    design      = c(rep(name_orig, nrow(rse_comp)),
                     rep(name_opti, nrow(rse_comp))),
    rse         = c(rse_comp$rse_orig, rse_comp$rse_opti),
    stringsAsFactors = FALSE
  )

  # Order parameters: THETA first, then OMEGA, then SIGMA
  type_order <- c("THETA", "OMEGA (diag.)", "OMEGA (off-diag.)",
                   "SIGMA (diag.)", "SIGMA (off-diag.)", "Autre")
  long$param_label <- factor(
    long$param_label,
    levels = rse_comp$param_label[order(match(rse_comp$param_type, type_order))]
  )
  long$design <- factor(long$design, levels = c(name_orig, name_opti))

  p <- ggplot(long, aes(x = param_label, y = rse, fill = design)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6,
             color = "white") +
    geom_hline(yintercept = 30, linetype = "dashed", color = "#94a3b8",
               size = 0.5) +
    annotate("text", x = 0.5, y = 31, label = "30% threshold",
             hjust = 0, vjust = 0, size = 3, color = "#94a3b8",
             fontface = "italic") +
    scale_fill_manual(
      values = setNames(c("#3b82f6", "#16a34a"), c(name_orig, name_opti)),
      name = NULL
    ) +
    labs(title = "Empirical RSE Comparison (SSE)",
         subtitle = paste(name_orig, "vs", name_opti),
         x = NULL, y = "Empirical RSE (%)") +
    .theme_design() +
    theme(
      axis.text.x = element_text(angle = 40, hjust = 1, size = 9),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey50"),
      legend.position = "top"
    )

  p
}


# =============================================================================
# plot_distribution_overlay() — Overlaid density plots for two SSE designs
# =============================================================================

#' Plot overlaid parameter estimate distributions from two SSE results.
#'
#' Faceted density plots with two fills (one per design). Red dashed line = true
#' value, vertical lines for each median.
#'
#' @param dist_orig  Tibble from compute_param_distributions() for original
#' @param dist_opti  Tibble from compute_param_distributions() for optimized
#' @param name_orig  Display name for original design (default "Original")
#' @param name_opti  Display name for optimized design (default "Optimized")
#' @param show_failed Include failed runs? (default FALSE)
#' @return ggplot object
#' @export
plot_distribution_overlay <- function(dist_orig, dist_opti,
                                      name_orig = "Original",
                                      name_opti = "Optimized",
                                      show_failed = FALSE) {
  if (is.null(dist_orig) || is.null(dist_opti) ||
      nrow(dist_orig) == 0L || nrow(dist_opti) == 0L) {
    return(ggplot() + labs(title = "No data for distribution overlay") +
           .theme_design())
  }

  # Tag each dataset
  d1 <- dist_orig
  d1$design <- name_orig
  d2 <- dist_opti
  d2$design <- name_opti

  combined <- rbind(d1, d2)

  if (!show_failed) {
    combined <- combined[combined$converged, ]
  }
  combined <- combined[!is.na(combined$estimate), ]

  if (nrow(combined) == 0L) {
    return(ggplot() + labs(title = "No valid estimates") + .theme_design())
  }

  combined$design <- factor(combined$design, levels = c(name_orig, name_opti))

  # True values for reference lines
  tv <- unique(combined[, c("param_label", "true_value")])

  p <- ggplot(combined, aes(x = estimate, fill = design)) +
    geom_density(alpha = 0.4, color = NA) +
    geom_vline(data = tv, aes(xintercept = true_value),
               color = "#dc2626", linetype = "dashed", size = 0.7) +
    facet_wrap(~ param_label, scales = "free", ncol = 3) +
    scale_fill_manual(
      values = setNames(c("#3b82f6", "#16a34a"), c(name_orig, name_opti)),
      name = NULL
    ) +
    labs(title = "Parameter Estimate Distributions",
         subtitle = paste(name_orig, "(blue) vs", name_opti,
                          "(green) | Red dashed = true value"),
         x = "Estimate", y = "Density") +
    .theme_design() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey50"),
      legend.position = "top"
    )

  p
}
