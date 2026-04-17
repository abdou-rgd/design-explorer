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

  # Use readr::read_csv — quote-aware (handles OMEGA(1,1) commas) and robust
  # type inference that doesn't mistype all-leading-NA columns as logical.
  # Fall back to read.csv if readr is unavailable.
  if (requireNamespace("readr", quietly = TRUE)) {
    raw <- suppressWarnings(suppressMessages(
      readr::read_csv(file, show_col_types = FALSE, progress = FALSE,
                      guess_max = 10000)
    ))
    raw <- as.data.frame(raw, check.names = FALSE)
  } else {
    raw <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  }
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
# compute_shrinkage_summary() — Per-ETA shrinkage statistics from SSE
# =============================================================================

#' Compute per-ETA shrinkage statistics across SSE replicates.
#'
#' Reads the `shrinkage_eta*(%)` columns from PsN raw_results and summarises
#' the distribution across the K converged replicates. Each ETA(N) is mapped
#' to its corresponding OMEGA(N,N) diagonal parameter.
#'
#' Shrinkage interpretation (Savic & Karlsson 2009):
#'   < 30 %  : posterior dominated by data (good)
#'   30-50 % : acceptable
#'   > 50 %  : posterior dominated by prior (design weakly informative)
#'
#' @param sse_all      Tibble from read_sse_raw_all()
#' @param param_labels Named character vector mapping OMEGA(N,N) -> display name
#' @param only_converged Logical, filter to converged runs (default TRUE)
#' @return Tibble: eta, omega, param_label, n, mean_shrink, median_shrink,
#'         sd_shrink, q25, q75, p5, p95
#' @export
compute_shrinkage_summary <- function(sse_all,
                                      param_labels = NULL,
                                      only_converged = TRUE) {
  shrink_cols <- grep("^shrinkage_eta\\d+\\(%\\)$", names(sse_all), value = TRUE)
  if (length(shrink_cols) == 0L) return(tibble::tibble())

  dat <- if (only_converged) sse_all[sse_all$converged, , drop = FALSE] else sse_all
  if (nrow(dat) == 0L) return(tibble::tibble())

  rows <- lapply(shrink_cols, function(col) {
    idx <- as.integer(sub("shrinkage_eta(\\d+)\\(%\\)", "\\1", col))
    vals <- as.numeric(dat[[col]])
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0L) return(NULL)

    omega <- sprintf("OMEGA(%d,%d)", idx, idx)
    label <- if (!is.null(param_labels) && omega %in% names(param_labels)) {
      param_labels[[omega]]
    } else {
      paste0("ETA(", idx, ")")
    }

    qs <- quantile(vals, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), names = FALSE)

    data.frame(
      eta = paste0("ETA(", idx, ")"),
      omega = omega,
      param_label = label,
      n = length(vals),
      mean_shrink   = round(mean(vals), 2),
      median_shrink = round(qs[3], 2),
      sd_shrink     = round(sd(vals), 2),
      p5  = round(qs[1], 2),
      q25 = round(qs[2], 2),
      q75 = round(qs[4], 2),
      p95 = round(qs[5], 2),
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(dplyr::bind_rows(rows))
}


# =============================================================================
# compute_shrinkage_long() — Long-format shrinkage per replicate
# =============================================================================

#' Build long-format tibble of per-replicate shrinkage for boxplot.
#'
#' @param sse_all      Tibble from read_sse_raw_all()
#' @param param_labels Named character vector mapping OMEGA(N,N) -> label
#' @param only_converged Logical (default TRUE)
#' @return Tibble: eta, omega, param_label, run_id, shrinkage
#' @export
compute_shrinkage_long <- function(sse_all,
                                   param_labels = NULL,
                                   only_converged = TRUE) {
  shrink_cols <- grep("^shrinkage_eta\\d+\\(%\\)$", names(sse_all), value = TRUE)
  empty <- tibble::tibble()
  if (length(shrink_cols) == 0L) {
    attr(empty, "status") <- "no_columns"
    return(empty)
  }

  dat <- if (only_converged) sse_all[sse_all$converged, , drop = FALSE] else sse_all
  if (nrow(dat) == 0L) {
    attr(empty, "status") <- "no_rows"
    return(empty)
  }

  rows <- lapply(shrink_cols, function(col) {
    idx <- as.integer(sub("shrinkage_eta(\\d+)\\(%\\)", "\\1", col))
    omega <- sprintf("OMEGA(%d,%d)", idx, idx)
    label <- if (!is.null(param_labels) && omega %in% names(param_labels)) {
      param_labels[[omega]]
    } else {
      paste0("ETA(", idx, ")")
    }
    data.frame(
      eta = paste0("ETA(", idx, ")"),
      omega = omega,
      param_label = label,
      run_id = seq_len(nrow(dat)),
      shrinkage = as.numeric(dat[[col]]),
      stringsAsFactors = FALSE
    )
  })

  out <- tibble::as_tibble(dplyr::bind_rows(rows))
  result <- out[!is.na(out$shrinkage), ]
  if (nrow(result) == 0L) {
    attr(result, "status") <- "all_na"
  } else {
    attr(result, "status") <- "ok"
  }
  result
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
# plot_shrinkage_boxplot() — Distribution of shrinkage per ETA
# =============================================================================

#' Boxplot of per-replicate shrinkage for each ETA.
#'
#' Visualises how stable the shrinkage is across SSE replicates. High median
#' with narrow IQR = structurally weak identifiability; wide IQR = shrinkage
#' varies sample to sample.
#'
#' @param shrink_long Tibble from compute_shrinkage_long()
#' @param title       Plot title (NULL = auto)
#' @return ggplot object
#' @export
plot_shrinkage_boxplot <- function(shrink_long, title = NULL) {
  if (is.null(shrink_long) || nrow(shrink_long) == 0L) {
    status <- attr(shrink_long, "status") %||% "no_columns"
    msg <- switch(status,
      no_columns = "No shrinkage_eta*(%) columns in raw_results",
      all_na     = "Shrinkage columns present but empty (PsN -no_shrinkage?)",
      no_rows    = "No converged replicates available",
      "No shrinkage data to display")
    return(ggplot() + labs(title = msg) + .theme_design())
  }

  df <- shrink_long
  df$param_label <- factor(df$param_label, levels = unique(df$param_label))

  ttl <- title %||% "Shrinkage Distribution per ETA"
  n_rep <- length(unique(df$run_id))

  p <- ggplot(df, aes(x = param_label, y = shrinkage)) +
    # Reference zones
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 30,
             fill = "#16a34a", alpha = 0.06) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 30, ymax = 50,
             fill = "#d97706", alpha = 0.06) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = Inf,
             fill = "#dc2626", alpha = 0.06) +
    geom_hline(yintercept = c(30, 50), linetype = "dashed",
               color = "grey50", size = 0.3) +
    geom_boxplot(fill = "#2B6991", alpha = 0.7, color = "grey30",
                 width = 0.6, outlier.size = 0.8) +
    labs(
      title = ttl,
      subtitle = sprintf(
        "N=%d replicates | Green <30%% | Amber 30-50%% | Red >50%% (Savic & Karlsson 2009)",
        n_rep
      ),
      x = NULL, y = "Shrinkage (%)"
    ) +
    .theme_design() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 8)
    )

  p
}


# =============================================================================
# plot_shrinkage_rse_scatter() — RSE vs shrinkage scatter, identifiability tiers
# =============================================================================

#' Scatter plot of empirical RSE vs mean shrinkage for each OMEGA parameter.
#'
#' Classifies each variance parameter into an identifiability tier:
#'   Green  : RSE < 30 % AND shrinkage < 50 %  (estimable, informative)
#'   Amber  : one side marginal
#'   Red    : shrinkage > 80 %  (prior dominates, design not informative)
#'
#' The shrinkage axis uses Savic & Karlsson (2009) thresholds; the RSE axis
#' uses the pharmacometrics convention (< 30 % = good precision).
#'
#' @param sse_all      Tibble from read_sse_raw_all()
#' @param true_values  Named numeric vector from read_true_values()
#' @param param_labels Named character vector (optional)
#' @return ggplot object
#' @export
plot_shrinkage_rse_scatter <- function(sse_all, true_values,
                                       param_labels = NULL) {
  if (is.null(sse_all) || nrow(sse_all) == 0L || length(true_values) == 0L) {
    return(ggplot() +
      labs(title = "Load SSE data and .ctl to see identifiability scatter") +
      .theme_design())
  }

  shrink_sum <- compute_shrinkage_summary(sse_all, param_labels)
  if (nrow(shrink_sum) == 0L) {
    shrink_cols <- grep("^shrinkage_eta\\d+\\(%\\)$", names(sse_all), value = TRUE)
    msg <- if (length(shrink_cols) == 0L)
      "No shrinkage_eta*(%) columns in raw_results"
    else
      "Shrinkage columns present but empty (PsN -no_shrinkage?)"
    return(ggplot() + labs(title = msg) + .theme_design())
  }

  converged <- sse_all[sse_all$converged, , drop = FALSE]
  metrics <- compute_sse_metrics(converged, true_values, param_labels)
  omega_metrics <- metrics[grepl("^OMEGA\\(", metrics$param), , drop = FALSE]

  df <- dplyr::inner_join(
    shrink_sum |>
      dplyr::select(param = omega, param_label, shrinkage = mean_shrink),
    omega_metrics |>
      dplyr::select(param, rse = rse_empirical),
    by = "param"
  )

  if (nrow(df) == 0L) {
    return(ggplot() +
      labs(title = "No OMEGA parameters shared between shrinkage and SSE metrics") +
      .theme_design())
  }

  df$tier <- dplyr::case_when(
    df$shrinkage > 80 ~ "Red (shrink >80%)",
    df$rse < 30 & df$shrinkage < 50 ~ "Green (RSE<30% & shrink<50%)",
    TRUE ~ "Amber (marginal)"
  )
  df$tier <- factor(df$tier, levels = c(
    "Green (RSE<30% & shrink<50%)",
    "Amber (marginal)",
    "Red (shrink >80%)"
  ))

  x_max <- max(100, max(df$shrinkage, na.rm = TRUE) * 1.1)
  y_max <- max(60, max(df$rse, na.rm = TRUE) * 1.1)

  p <- ggplot(df, aes(x = shrinkage, y = rse)) +
    # Tier rectangles (background)
    annotate("rect", xmin = -Inf, xmax = 50, ymin = -Inf, ymax = 30,
             fill = "#16a34a", alpha = 0.08) +
    annotate("rect", xmin = 80, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = "#dc2626", alpha = 0.08) +
    # Threshold lines
    geom_hline(yintercept = 30, linetype = "dashed",
               color = "#16a34a", size = 0.4) +
    geom_vline(xintercept = 50, linetype = "dashed",
               color = "#d97706", size = 0.4) +
    geom_vline(xintercept = 80, linetype = "dashed",
               color = "#dc2626", size = 0.4) +
    # Points
    geom_point(aes(color = tier), size = 4, alpha = 0.85) +
    geom_text(aes(label = param_label), nudge_y = y_max * 0.025,
              size = 3, check_overlap = TRUE) +
    scale_color_manual(
      values = c(
        "Green (RSE<30% & shrink<50%)" = "#16a34a",
        "Amber (marginal)"              = "#d97706",
        "Red (shrink >80%)"             = "#dc2626"
      ),
      drop = FALSE, name = NULL
    ) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max)) +
    labs(
      title = "Identifiability: Empirical RSE vs Mean Shrinkage",
      subtitle = paste0(
        "One point per OMEGA | RSE threshold 30% | Shrinkage thresholds 50/80% ",
        "(Savic & Karlsson 2009)"
      ),
      x = "Mean shrinkage (%) across SSE replicates",
      y = "Empirical RSE (%)"
    ) +
    .theme_design() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 8)
    )

  p
}
