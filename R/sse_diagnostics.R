# =============================================================================
# sse_diagnostics.R — SSE intrinsic analysis (run health, distributions, etc.)
#
# Pure computation functions for analyzing SSE results themselves (not FIM vs SSE
# comparison — that lives in sse_metrics.R).
#
# Prereqs: ggplot2, dplyr, tidyr, stringr (already loaded by app.R)
# =============================================================================


# =============================================================================
# read_sse_raw_all() — Read PsN raw_results CSV WITHOUT filtering
# =============================================================================

#' Read a PsN SSE raw results CSV, normalize columns, keep all rows.
#'
#' Unlike read_sse_raw() which filters on minimization_successful == 1,
#' this returns every row with a `converged` column (TRUE/FALSE) so the
#' caller can decide how to partition the data.
#'
#' @param file Path to the raw results CSV
#' @return Tibble with normalized columns + `converged` logical column.
#'         Attributes: n_total, n_success, pre_filtered
#' @export
read_sse_raw_all <- function(file) {
  if (!file.exists(file)) stop("SSE file not found: ", file)

  raw <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  n_total <- nrow(raw)
  names(raw) <- .normalize_psn_cols(names(raw))

  pre_filtered <- FALSE
  if ("minimization_successful" %in% names(raw)) {
    raw$minimization_successful <- as.numeric(raw$minimization_successful)
    raw$converged <- !is.na(raw$minimization_successful) &
                     raw$minimization_successful == 1
  } else {
    pre_filtered <- TRUE
    raw$converged <- TRUE
  }

  n_success <- sum(raw$converged)
  result <- tibble::as_tibble(raw)
  attr(result, "n_total") <- n_total
  attr(result, "n_success") <- n_success
  attr(result, "pre_filtered") <- pre_filtered
  result
}


# =============================================================================
# compute_run_health() — Run attrition funnel
# =============================================================================

#' Compute run health summary from unfiltered SSE data.
#'
#' Returns counts and percentages at each QC stage:
#' total -> minimization OK -> no boundary -> covariance OK -> no rounding
#'
#' @param sse_all Tibble from read_sse_raw_all() (all rows, `converged` column)
#' @return List with $stages (tibble: stage, n, pct) and $total
#' @export
compute_run_health <- function(sse_all) {
  n_total <- nrow(sse_all)

  has_min <- "minimization_successful" %in% names(sse_all)
  has_bnd <- "estimate_near_boundary" %in% names(sse_all)
  has_cov <- "covariance_step_successful" %in% names(sse_all)
  has_rnd <- "rounding_errors" %in% names(sse_all)

  n_min_ok <- if (has_min) {
    sum(as.numeric(sse_all$minimization_successful) == 1, na.rm = TRUE)
  } else n_total

  # Among minimization OK: no boundary
  min_ok_rows <- if (has_min) {
    sse_all[!is.na(sse_all$minimization_successful) &
            as.numeric(sse_all$minimization_successful) == 1, , drop = FALSE]
  } else sse_all

  n_no_boundary <- if (has_bnd) {
    sum(as.numeric(min_ok_rows$estimate_near_boundary) == 0, na.rm = TRUE)
  } else n_min_ok

  # Among minimization OK: covariance step OK
  n_cov_ok <- if (has_cov) {
    sum(as.numeric(min_ok_rows$covariance_step_successful) == 1, na.rm = TRUE)
  } else n_min_ok

  # Among minimization OK: no rounding errors
  n_no_rounding <- if (has_rnd) {
    sum(as.numeric(min_ok_rows$rounding_errors) == 0, na.rm = TRUE)
  } else n_min_ok

  safe_pct <- function(n, total) {
    if (total == 0L) 0 else round(100 * n / total, 1)
  }

  stages <- data.frame(
    stage = c("Total runs", "Minimization OK", "No boundary estimates",
              "Covariance OK", "No rounding errors"),
    n     = c(n_total, n_min_ok, n_no_boundary, n_cov_ok, n_no_rounding),
    denom = c(n_total, n_total, n_min_ok, n_min_ok, n_min_ok),
    pct   = c(100, safe_pct(n_min_ok, n_total),
              safe_pct(n_no_boundary, n_min_ok),
              safe_pct(n_cov_ok, n_min_ok),
              safe_pct(n_no_rounding, n_min_ok)),
    stringsAsFactors = FALSE
  )

  list(
    stages = tibble::as_tibble(stages),
    total  = n_total,
    n_success = n_min_ok
  )
}


# =============================================================================
# compute_param_distributions() — Long-format estimates for distribution plots
# =============================================================================

#' Build long-format tibble of parameter estimates across SSE runs.
#'
#' @param sse_all    Tibble from read_sse_raw_all()
#' @param true_values Named numeric vector from read_true_values()
#' @param param_labels Named character vector (optional)
#' @return Tibble: param, param_label, param_type, run_id, estimate,
#'         true_value, converged
#' @export
compute_param_distributions <- function(sse_all, true_values,
                                        param_labels = NULL) {
  available <- intersect(names(true_values), names(sse_all))
  if (length(available) == 0L) return(tibble::tibble())

  rows <- lapply(available, function(pname) {
    estimates <- as.numeric(sse_all[[pname]])
    label <- if (!is.null(param_labels) && pname %in% names(param_labels)) {
      param_labels[[pname]]
    } else {
      pname
    }
    data.frame(
      param       = pname,
      param_label = label,
      param_type  = .param_type(pname),
      run_id      = seq_along(estimates),
      estimate    = estimates,
      true_value  = true_values[[pname]],
      converged   = sse_all$converged,
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(dplyr::bind_rows(rows))
}


# =============================================================================
# compute_empirical_correlations() — Spearman correlation of estimates
# =============================================================================

#' Compute Spearman correlation matrix of parameter estimates across runs.
#'
#' @param sse_all    Tibble from read_sse_raw_all()
#' @param true_values Named numeric vector (used for column selection)
#' @param only_converged Logical, filter to converged runs (default TRUE)
#' @param param_labels Named character vector for display names (optional)
#' @return Named correlation matrix
#' @export
compute_empirical_correlations <- function(sse_all, true_values,
                                           only_converged = TRUE,
                                           param_labels = NULL) {
  available <- intersect(names(true_values), names(sse_all))
  if (length(available) < 2L) return(NULL)

  dat <- if (only_converged) {
    sse_all[sse_all$converged, , drop = FALSE]
  } else {
    sse_all
  }

  mat <- as.matrix(dat[, available, drop = FALSE])
  mat <- apply(mat, 2, as.numeric)

  # Remove rows with any NA
  complete <- complete.cases(mat)
  if (sum(complete) < 3L) return(NULL)
  mat <- mat[complete, , drop = FALSE]

  cor_mat <- cor(mat, method = "spearman")

  # Apply display labels
  if (!is.null(param_labels)) {
    disp <- vapply(available, function(p) {
      if (p %in% names(param_labels)) param_labels[[p]] else p
    }, character(1))
    rownames(cor_mat) <- disp
    colnames(cor_mat) <- disp
  }

  cor_mat
}


# =============================================================================
# compute_param_diagnostics() — Per-parameter boundary/failure counts
# =============================================================================

#' Compute per-parameter diagnostic counts across SSE runs.
#'
#' For each parameter: count of runs with SE = NA, RSE > 100%, and
#' estimate at zero (possible boundary hit).
#'
#' @param sse_all    Tibble from read_sse_raw_all()
#' @param true_values Named numeric vector
#' @param param_labels Named character vector (optional)
#' @return Tibble: param, param_label, param_type, n_se_na, pct_se_na,
#'         n_rse_over_100, pct_rse_over_100, n_zero_estimate, pct_zero_estimate
#' @export
compute_param_diagnostics <- function(sse_all, true_values,
                                      param_labels = NULL) {
  available <- intersect(names(true_values), names(sse_all))
  if (length(available) == 0L) return(tibble::tibble())

  # Work only on converged runs for SE/RSE diagnostics

  dat <- sse_all[sse_all$converged, , drop = FALSE]
  n_runs <- nrow(dat)
  if (n_runs == 0L) return(tibble::tibble())

  safe_pct <- function(n) round(100 * n / n_runs, 1)

  rows <- lapply(available, function(pname) {
    estimates <- as.numeric(dat[[pname]])
    true_val  <- true_values[[pname]]

    # SE column
    se_col <- paste0("se_", pname)
    se_vals <- if (se_col %in% names(dat)) as.numeric(dat[[se_col]]) else rep(NA, n_runs)

    n_se_na <- sum(is.na(se_vals))

    # RSE > 100% (among runs with valid SE)
    rse_vals <- ifelse(!is.na(se_vals) & abs(estimates) > 1e-15,
                       100 * se_vals / abs(estimates), NA_real_)
    n_rse_over_100 <- sum(!is.na(rse_vals) & rse_vals > 100)

    # Estimate at zero (potential boundary for variances)
    n_zero <- sum(!is.na(estimates) & abs(estimates) < 1e-15)

    label <- if (!is.null(param_labels) && pname %in% names(param_labels)) {
      param_labels[[pname]]
    } else {
      pname
    }

    data.frame(
      param           = pname,
      param_label     = label,
      param_type      = .param_type(pname),
      n_runs          = n_runs,
      n_se_na         = n_se_na,
      pct_se_na       = safe_pct(n_se_na),
      n_rse_over_100  = n_rse_over_100,
      pct_rse_over_100 = safe_pct(n_rse_over_100),
      n_zero_estimate = n_zero,
      pct_zero_estimate = safe_pct(n_zero),
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(dplyr::bind_rows(rows))
}


# =============================================================================
# plot_param_distributions() — Density/violin per parameter
# =============================================================================

#' Plot parameter estimate distributions across SSE runs.
#'
#' Faceted density plots with vertical lines for true value (red dashed)
#' and median estimate (blue solid). Optionally shows failed runs in grey.
#'
#' @param dist_data Tibble from compute_param_distributions()
#' @param show_failed Logical — include failed runs? (default FALSE)
#' @return ggplot object
#' @export
plot_param_distributions <- function(dist_data, show_failed = FALSE) {
  if (is.null(dist_data) || nrow(dist_data) == 0L) {
    return(ggplot() + labs(title = "No data") + .theme_design())
  }

  df <- if (show_failed) dist_data else dist_data[dist_data$converged, ]
  df <- df[!is.na(df$estimate), ]
  if (nrow(df) == 0L) {
    return(ggplot() + labs(title = "No valid estimates") + .theme_design())
  }

  # Compute medians and true values per parameter
  summaries <- df |>
    dplyr::group_by(param_label, true_value) |>
    dplyr::summarise(median_est = median(estimate, na.rm = TRUE),
                     .groups = "drop")

  p <- ggplot(df, aes(x = estimate))

  if (show_failed && any(!df$converged)) {
    p <- p +
      geom_density(data = df[!df$converged, ],
                   fill = "grey80", alpha = 0.4, color = "grey60") +
      geom_density(data = df[df$converged, ],
                   fill = "#3b82f6", alpha = 0.5, color = "#1e40af")
  } else {
    p <- p +
      geom_density(fill = "#3b82f6", alpha = 0.5, color = "#1e40af")
  }

  p <- p +
    geom_vline(data = summaries,
               aes(xintercept = true_value),
               color = "#dc2626", linetype = "dashed", size = 0.7) +
    geom_vline(data = summaries,
               aes(xintercept = median_est),
               color = "#1e40af", linetype = "solid", size = 0.7) +
    facet_wrap(~ param_label, scales = "free", ncol = 3) +
    labs(title = "Parameter Estimate Distributions (SSE)",
         subtitle = "Red dashed = true value | Blue solid = median estimate",
         x = "Estimate", y = "Density") +
    .theme_design() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey50"))

  p
}


# =============================================================================
# plot_ofv_distribution() — OFV histogram across runs
# =============================================================================

#' Plot OFV distribution across SSE runs.
#'
#' @param sse_all Tibble from read_sse_raw_all()
#' @param color_by_status Logical — color bars by convergence status?
#' @return ggplot object
#' @export
plot_ofv_distribution <- function(sse_all, color_by_status = FALSE) {
  if (!"ofv" %in% names(sse_all)) {
    return(ggplot() + labs(title = "No OFV column found") + .theme_design())
  }

  df <- sse_all[!is.na(as.numeric(sse_all$ofv)), ]
  df$ofv <- as.numeric(df$ofv)
  if (nrow(df) == 0L) {
    return(ggplot() + labs(title = "No valid OFV values") + .theme_design())
  }

  median_ofv <- median(df$ofv[df$converged], na.rm = TRUE)

  if (color_by_status) {
    df$status <- ifelse(df$converged, "Converged", "Failed")
    p <- ggplot(df, aes(x = ofv, fill = status)) +
      geom_histogram(bins = 30, alpha = 0.7, position = "identity",
                     color = "white", size = 0.2) +
      scale_fill_manual(values = c("Converged" = "#3b82f6",
                                   "Failed" = "#ef4444"),
                        name = NULL)
  } else {
    p <- ggplot(df[df$converged, ], aes(x = ofv)) +
      geom_histogram(bins = 30, fill = "#3b82f6", alpha = 0.7,
                     color = "white", size = 0.2)
  }

  p <- p +
    geom_vline(xintercept = median_ofv, color = "#1e40af",
               linetype = "dashed", size = 0.8) +
    annotate("text", x = median_ofv, y = Inf, vjust = 2, hjust = -0.1,
             label = sprintf("Median: %.1f", median_ofv),
             color = "#1e40af", size = 3.5, fontface = "bold") +
    labs(title = "OFV Distribution Across SSE Runs",
         x = "Objective Function Value", y = "Count") +
    .theme_design() +
    theme(plot.title = element_text(hjust = 0.5))

  p
}


# =============================================================================
# plot_empirical_cor_heatmap() — Correlation heatmap of estimates
# =============================================================================

#' Plot Spearman correlation heatmap of parameter estimates across SSE runs.
#'
#' @param cor_matrix Named correlation matrix from compute_empirical_correlations()
#' @return ggplot object
#' @export
plot_empirical_cor_heatmap <- function(cor_matrix) {
  if (is.null(cor_matrix) || ncol(cor_matrix) < 2L) {
    return(ggplot() + labs(title = "Not enough parameters for correlation") +
           .theme_design())
  }

  # Long format for ggplot
  params <- colnames(cor_matrix)
  df <- expand.grid(Var1 = params, Var2 = params, stringsAsFactors = FALSE)
  df$value <- as.vector(cor_matrix)

  # Preserve parameter order
  df$Var1 <- factor(df$Var1, levels = params)
  df$Var2 <- factor(df$Var2, levels = rev(params))

  p <- ggplot(df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = ifelse(abs(value) > 0.3,
                                 sprintf("%.2f", value), "")),
              size = 2.8, color = "black") +
    scale_fill_gradient2(low = "#2563eb", mid = "white", high = "#dc2626",
                         midpoint = 0, limits = c(-1, 1),
                         name = "Spearman r") +
    labs(title = "Empirical Correlation of Parameter Estimates (SSE)",
         subtitle = "Across successful runs — values shown for |r| > 0.3",
         x = NULL, y = NULL) +
    .theme_design() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey50"),
          legend.position = "right")

  p
}
