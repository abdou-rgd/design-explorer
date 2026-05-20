# =============================================================================
# sse_individual_pk_diagnostics.R -- exploratory individual PK SSE diagnostics
# =============================================================================

# =============================================================================
# Run status helpers
# =============================================================================

.pk_diag_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

.pk_diag_pct <- function(x) {
  if (length(x) == 0L) {
    return(NA_real_)
  }
  round(100 * mean(x, na.rm = TRUE), 1)
}

.pk_diag_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = p, names = FALSE, na.rm = TRUE))
}

.pk_diag_run_status <- function(
  min_ok,
  cov_ok,
  near_boundary,
  rounding,
  condition_number,
  condition_number_threshold = 1000
) {
  min_ok <- .pk_diag_num(min_ok)
  cov_ok <- .pk_diag_num(cov_ok)
  near_boundary <- .pk_diag_num(near_boundary)
  rounding <- .pk_diag_num(rounding)
  condition_number <- .pk_diag_num(condition_number)

  dplyr::case_when(
    !is.na(min_ok) & min_ok != 1 ~ "minimization_failed",
    !is.na(cov_ok) & cov_ok != 1 ~ "covariance_or_boundary_issue",
    !is.na(near_boundary) & near_boundary != 0 ~
      "covariance_or_boundary_issue",
    !is.na(condition_number) &
      condition_number > condition_number_threshold ~ "high_condition_number",
    !is.na(rounding) & rounding != 0 ~ "rounding_issue",
    TRUE ~ "strict_qc_ok"
  )
}

.pk_diag_status_lookup <- function(
  sse_all,
  samples,
  condition_number_threshold = 1000
) {
  samples <- sort(unique(as.integer(samples)))
  empty <- tibble::tibble(sample = samples)
  if (is.null(sse_all) || nrow(sse_all) == 0L) {
    empty$minimization_successful <- NA_real_
    empty$covariance_step_successful <- NA_real_
    empty$estimate_near_boundary <- NA_real_
    empty$rounding_errors <- NA_real_
    empty$condition_number <- NA_real_
    empty$run_qc_status <- "strict_qc_ok"
    return(empty)
  }

  dat <- tibble::as_tibble(sse_all)
  if (!"sample" %in% names(dat)) {
    dat$sample <- seq_len(nrow(dat))
  }

  status_cols <- c(
    "minimization_successful",
    "covariance_step_successful",
    "estimate_near_boundary",
    "rounding_errors",
    "condition_number"
  )
  for (col in status_cols) {
    if (!col %in% names(dat)) dat[[col]] <- NA_real_
  }

  out <- dat |>
    dplyr::transmute(
      sample = as.integer(.data$sample),
      minimization_successful = .pk_diag_num(.data$minimization_successful),
      covariance_step_successful = .pk_diag_num(
        .data$covariance_step_successful
      ),
      estimate_near_boundary = .pk_diag_num(.data$estimate_near_boundary),
      rounding_errors = .pk_diag_num(.data$rounding_errors),
      condition_number = .pk_diag_num(.data$condition_number)
    ) |>
    dplyr::distinct(.data$sample, .keep_all = TRUE)

  out$run_qc_status <- .pk_diag_run_status(
    out$minimization_successful,
    out$covariance_step_successful,
    out$estimate_near_boundary,
    out$rounding_errors,
    out$condition_number,
    condition_number_threshold = condition_number_threshold
  )

  dplyr::left_join(empty, out, by = "sample") |>
    dplyr::mutate(
      run_qc_status = dplyr::if_else(
        is.na(.data$run_qc_status),
        "status_unknown",
        .data$run_qc_status
      )
    )
}


# =============================================================================
# Summary builders
# =============================================================================

.pk_diag_summarise_recovery <- function(df, status_filter) {
  df |>
    dplyr::group_by(.data$param) |>
    dplyr::summarise(
      status_filter = status_filter,
      n = dplyr::n(),
      n_samples = dplyr::n_distinct(.data$sample),
      n_ids = dplyr::n_distinct(.data$ID),
      median_relative_error = stats::median(
        .data$relative_error,
        na.rm = TRUE
      ),
      p5_relative_error = .pk_diag_quantile(.data$relative_error, 0.05),
      p95_relative_error = .pk_diag_quantile(.data$relative_error, 0.95),
      iqr_relative_error = stats::IQR(.data$relative_error, na.rm = TRUE),
      median_abs_relative_error = stats::median(
        .data$abs_relative_error,
        na.rm = TRUE
      ),
      p95_abs_relative_error = .pk_diag_quantile(
        .data$abs_relative_error,
        0.95
      ),
      pct_abs_error_over_20 = .pk_diag_pct(.data$abs_relative_error > 20),
      pct_abs_error_over_50 = .pk_diag_pct(.data$abs_relative_error > 50),
      .groups = "drop"
    ) |>
    dplyr::select("status_filter", "param", dplyr::everything())
}

.pk_diag_by_param <- function(recovery_long) {
  groups <- list(
    all_runs = recovery_long,
    minimization_successful = recovery_long |>
      dplyr::filter(.data$minimization_successful == 1),
    strict_qc_ok = recovery_long |>
      dplyr::filter(.data$run_qc_status == "strict_qc_ok")
  )
  rows <- lapply(names(groups), function(nm) {
    dat <- groups[[nm]]
    if (nrow(dat) == 0L) {
      return(NULL)
    }
    .pk_diag_summarise_recovery(dat, nm)
  })
  tibble::as_tibble(dplyr::bind_rows(rows)) |>
    dplyr::select(
      "status_filter",
      "param",
      dplyr::everything()
    ) |>
    dplyr::arrange(.data$status_filter, .data$param)
}

.pk_diag_by_id_param <- function(recovery_long) {
  out <- recovery_long |>
    dplyr::group_by(.data$ID, .data$ARM, .data$param) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_samples = dplyr::n_distinct(.data$sample),
      median_relative_error = stats::median(
        .data$relative_error,
        na.rm = TRUE
      ),
      p5_relative_error = .pk_diag_quantile(.data$relative_error, 0.05),
      p95_relative_error = .pk_diag_quantile(.data$relative_error, 0.95),
      median_abs_relative_error = stats::median(
        .data$abs_relative_error,
        na.rm = TRUE
      ),
      p95_abs_relative_error = .pk_diag_quantile(
        .data$abs_relative_error,
        0.95
      ),
      pct_abs_error_over_20 = .pk_diag_pct(.data$abs_relative_error > 20),
      pct_abs_error_over_50 = .pk_diag_pct(.data$abs_relative_error > 50),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$median_abs_relative_error),
      dplyr::desc(.data$p95_abs_relative_error)
    )

  out$outlier_rank <- seq_len(nrow(out))
  out
}

.pk_diag_run_burden <- function(recovery_long) {
  recovery_long |>
    dplyr::group_by(
      .data$sample,
      .data$minimization_successful,
      .data$covariance_step_successful,
      .data$estimate_near_boundary,
      .data$rounding_errors,
      .data$condition_number,
      .data$run_qc_status
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_ids = dplyr::n_distinct(.data$ID),
      n_params = dplyr::n_distinct(.data$param),
      median_abs_relative_error = stats::median(
        .data$abs_relative_error,
        na.rm = TRUE
      ),
      p95_abs_relative_error = .pk_diag_quantile(
        .data$abs_relative_error,
        0.95
      ),
      pct_abs_error_over_20 = .pk_diag_pct(.data$abs_relative_error > 20),
      pct_abs_error_over_50 = .pk_diag_pct(.data$abs_relative_error > 50),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$median_abs_relative_error),
      dplyr::desc(.data$p95_abs_relative_error)
    )
}

#' Filter individual PK recovery rows by run QC subset.
#'
#' @param recovery_long Long table from `build_individual_pk_diagnostics()`.
#' @param status_filter One of `all_runs`, `minimization_successful`, or
#'   `strict_qc_ok`.
#' @return Filtered tibble.
#' @export
filter_individual_pk_recovery <- function(
  recovery_long,
  status_filter = "minimization_successful"
) {
  if (is.null(recovery_long) || nrow(recovery_long) == 0L) {
    return(tibble::tibble())
  }
  if (is.null(status_filter) || length(status_filter) == 0L) {
    status_filter <- "minimization_successful"
  }
  if (identical(status_filter, "all_runs")) {
    return(recovery_long)
  }
  if (identical(status_filter, "strict_qc_ok")) {
    return(
      recovery_long |>
        dplyr::filter(.data$run_qc_status == "strict_qc_ok")
    )
  }
  recovery_long |>
    dplyr::filter(.data$minimization_successful == 1)
}

#' Summarise individual PK recovery by ID and parameter.
#'
#' @param recovery_long Long table from `build_individual_pk_diagnostics()`.
#' @return Per-ID and per-parameter summary tibble.
#' @export
summarize_individual_pk_by_id_param <- function(recovery_long) {
  if (is.null(recovery_long) || nrow(recovery_long) == 0L) {
    return(tibble::tibble())
  }
  .pk_diag_by_id_param(recovery_long)
}

#' Build exploratory individual PK diagnostics from PsN SSE outputs.
#'
#' @param sse_all Raw PsN `raw_results` data, preferably unfiltered.
#' @param patab_data Tibble from `read_sse_patab_outputs()`.
#' @param params PK columns to compare between simulation and estimation tables.
#' @param condition_number_threshold Numeric threshold above which an otherwise
#'   successful run is flagged as high-condition-number.
#' @return Named list of reusable diagnostic tables.
#' @export
build_individual_pk_diagnostics <- function(
  sse_all,
  patab_data,
  params = c("CL", "VC", "Q", "VP", "KA", "F1"),
  condition_number_threshold = 1000
) {
  recovery <- compute_individual_pk_recovery(patab_data, params = params)
  if (nrow(recovery) == 0L) {
    empty <- tibble::tibble()
    return(list(
      individual_pk_recovery_long = empty,
      individual_pk_summary_by_param = empty,
      individual_pk_summary_by_id_param = empty,
      run_pk_error_burden = empty,
      prediction_columns = detect_individual_pk_prediction_columns(patab_data)
    ))
  }

  status <- .pk_diag_status_lookup(
    sse_all,
    recovery$sample,
    condition_number_threshold = condition_number_threshold
  )
  recovery_long <- recovery |>
    dplyr::left_join(status, by = "sample") |>
    dplyr::mutate(
      abs_relative_error = abs(.data$relative_error)
    ) |>
    dplyr::select(dplyr::all_of(c(
      "sample",
      "ID",
      "ARM",
      "param",
      "sim",
      "est",
      "relative_error",
      "abs_relative_error",
      "minimization_successful",
      "covariance_step_successful",
      "estimate_near_boundary",
      "rounding_errors",
      "condition_number",
      "run_qc_status"
    ))) |>
    dplyr::arrange(.data$sample, .data$ID, .data$param)

  list(
    individual_pk_recovery_long = recovery_long,
    individual_pk_summary_by_param = .pk_diag_by_param(recovery_long),
    individual_pk_summary_by_id_param = .pk_diag_by_id_param(recovery_long),
    run_pk_error_burden = .pk_diag_run_burden(recovery_long),
    prediction_columns = detect_individual_pk_prediction_columns(patab_data)
  )
}


# =============================================================================
# Prediction diagnostics placeholders
# =============================================================================

#' Detect whether kept tables contain prediction diagnostic columns.
#'
#' @param patab_data Tibble from `read_sse_patab_outputs()`.
#' @return Character vector of available prediction/time columns.
#' @export
detect_individual_pk_prediction_columns <- function(patab_data) {
  if (is.null(patab_data) || length(names(patab_data)) == 0L) {
    return(character())
  }
  intersect(c("TIME", "DV", "PRED", "IPRED"), names(patab_data))
}

#' Plot optional prediction diagnostics when prediction columns are present.
#'
#' @param patab_data Tibble from `read_sse_patab_outputs()`.
#' @return A ggplot object, or NULL when required columns are absent.
#' @export
plot_individual_pk_prediction_diagnostics <- function(patab_data) {
  cols <- detect_individual_pk_prediction_columns(patab_data)
  if (!all(c("TIME", "IPRED") %in% cols)) {
    return(NULL)
  }

  df <- patab_data |>
    dplyr::filter(.data$kind %in% c("simulation", "estimation")) |>
    dplyr::select(
      dplyr::any_of(c("sample", "kind", "ID", "TIME", "DV", "PRED", "IPRED"))
    )
  if (nrow(df) == 0L) {
    return(NULL)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data$TIME, y = .data$IPRED)) +
    ggplot2::geom_line(
      ggplot2::aes(
        group = interaction(.data$sample, .data$kind, .data$ID),
        color = .data$kind
      ),
      alpha = 0.18,
      linewidth = 0.25
    ) +
    ggplot2::scale_color_manual(
      values = c(simulation = "#2563eb", estimation = "#dc2626"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "Individual Prediction Profiles",
      subtitle = "Available only when kept tables contain TIME and IPRED",
      x = "Time",
      y = "IPRED"
    ) +
    .pk_diag_theme()
}


# =============================================================================
# Plot helpers
# =============================================================================

.pk_diag_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )
}

.pk_diag_status_palette <- function() {
  c(
    strict_qc_ok = "#2563eb",
    high_condition_number = "#7c3aed",
    covariance_or_boundary_issue = "#d97706",
    rounding_issue = "#9333ea",
    minimization_failed = "#dc2626",
    status_unknown = "#64748b"
  )
}

#' Plot median relative error and empirical 5/95% interval by PK parameter.
#'
#' @param summary_by_param Table from `build_individual_pk_diagnostics()`.
#' @param status_filter Which run subset to display.
#' @return ggplot object.
#' @export
plot_individual_pk_error_forest <- function(
  summary_by_param,
  status_filter = "minimization_successful"
) {
  if (is.null(summary_by_param) || nrow(summary_by_param) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "No individual PK summary available") +
        .pk_diag_theme()
    )
  }
  df <- summary_by_param |>
    dplyr::filter(.data$status_filter == .env$status_filter)
  if (nrow(df) == 0L) {
    df <- summary_by_param
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = stats::reorder(.data$param, .data$median_abs_relative_error),
      y = .data$median_relative_error
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -20,
      ymax = 20,
      fill = "#16a34a",
      alpha = 0.06
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
    ggplot2::geom_hline(
      yintercept = c(-20, 20),
      linetype = "dashed",
      color = "#d97706",
      linewidth = 0.35
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$p5_relative_error,
        ymax = .data$p95_relative_error
      ),
      width = 0.15,
      color = "#334155"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = .data$pct_abs_error_over_20,
        color = .data$median_abs_relative_error
      ),
      alpha = 0.9
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_gradient(
      low = "#2563eb",
      high = "#dc2626",
      name = "Median |error| (%)"
    ) +
    ggplot2::scale_size_continuous(range = c(2, 6), name = "|error| >20% (%)") +
    ggplot2::labs(
      title = "Individual PK Empirical Error Intervals",
      subtitle = paste0(
        "Median relative error with empirical 5/95% interval; subset: ",
        unique(df$status_filter)[1]
      ),
      x = NULL,
      y = "Relative error (%)"
    ) +
    .pk_diag_theme()
}

#' Plot a heatmap of individual median absolute relative error.
#'
#' @param summary_by_id_param Table from `build_individual_pk_diagnostics()`.
#' @return ggplot object.
#' @export
plot_individual_pk_error_heatmap <- function(summary_by_id_param) {
  if (is.null(summary_by_id_param) || nrow(summary_by_id_param) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "No individual PK ID summary available") +
        .pk_diag_theme()
    )
  }
  df <- summary_by_id_param
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$param,
      y = factor(.data$ID),
      fill = .data$median_abs_relative_error
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.12) +
    ggplot2::scale_fill_gradientn(
      colors = c("#e0f2fe", "#fef3c7", "#dc2626"),
      name = "Median |error| (%)"
    ) +
    ggplot2::labs(
      title = "Individual PK Error Heatmap",
      subtitle = "Each cell is the median absolute relative error by ID and parameter",
      x = NULL,
      y = "ID"
    ) +
    .pk_diag_theme() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 5))
}

#' Plot the largest individual-parameter PK outliers.
#'
#' @param summary_by_id_param Table from `build_individual_pk_diagnostics()`.
#' @param top_n Number of outlier rows to display.
#' @return ggplot object.
#' @export
plot_individual_pk_outliers <- function(summary_by_id_param, top_n = 20L) {
  if (is.null(summary_by_id_param) || nrow(summary_by_id_param) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "No individual PK outlier data available") +
        .pk_diag_theme()
    )
  }
  df <- summary_by_id_param |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(label = paste0("ID ", .data$ID, " - ", .data$param))
  df$label <- factor(df$label, levels = rev(df$label))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$median_abs_relative_error, y = .data$label)
  ) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data$p95_abs_relative_error),
      width = 0.68
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", .data$median_abs_relative_error)),
      hjust = -0.08,
      size = 3
    ) +
    ggplot2::scale_fill_gradient(
      low = "#fef3c7",
      high = "#dc2626",
      name = "P95 |error| (%)"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.16))
    ) +
    ggplot2::labs(
      title = "Top Individual PK Outliers",
      subtitle = "Ranked by median absolute relative error across SSE samples",
      x = "Median absolute relative error (%)",
      y = NULL
    ) +
    .pk_diag_theme()
}

#' Plot run-level individual PK error burden.
#'
#' @param run_burden Table from `build_individual_pk_diagnostics()`.
#' @return ggplot object.
#' @export
plot_run_pk_error_burden <- function(run_burden) {
  if (is.null(run_burden) || nrow(run_burden) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "No run-level PK burden available") +
        .pk_diag_theme()
    )
  }
  df <- run_burden |>
    dplyr::arrange(.data$sample)

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = factor(.data$sample),
      y = .data$median_abs_relative_error,
      fill = .data$run_qc_status
    )
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$p95_abs_relative_error),
      color = "#111827",
      size = 1.8
    ) +
    ggplot2::scale_fill_manual(
      values = .pk_diag_status_palette(),
      name = "Run status"
    ) +
    ggplot2::labs(
      title = "Run-Level Individual PK Error Burden",
      subtitle = "Bars: median |error|; dots: P95 |error|",
      x = "SSE sample",
      y = "Absolute relative error (%)"
    ) +
    .pk_diag_theme()
}

#' Plot simulated vs estimated individual PK values coloured by run status.
#'
#' @param recovery_long Long table from `build_individual_pk_diagnostics()`.
#' @param converged_only Keep minimization-successful runs only.
#' @param log_axes Use log10 axes when all displayed values are positive.
#' @return ggplot object.
#' @export
plot_individual_pk_recovery_by_status <- function(
  recovery_long,
  converged_only = FALSE,
  log_axes = FALSE
) {
  if (is.null(recovery_long) || nrow(recovery_long) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "No individual PK recovery data available") +
        .pk_diag_theme()
    )
  }
  df <- recovery_long |>
    dplyr::filter(is.finite(.data$sim), is.finite(.data$est))
  if (isTRUE(converged_only)) {
    df <- df |>
      dplyr::filter(.data$minimization_successful == 1)
  }
  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "No finite individual PK recovery values") +
        .pk_diag_theme()
    )
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$sim, y = .data$est)) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      color = "grey45",
      linewidth = 0.35
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$run_qc_status),
      alpha = 0.35,
      size = 1
    ) +
    ggplot2::facet_wrap(~param, scales = "free", ncol = 3) +
    ggplot2::scale_color_manual(
      values = .pk_diag_status_palette(),
      name = "Run status"
    ) +
    ggplot2::labs(
      title = "Individual PK Recovery by Run Status",
      subtitle = "Estimated vs simulated individual PK values",
      x = "Simulated individual value",
      y = "Estimated individual value"
    ) +
    .pk_diag_theme()

  if (
    isTRUE(log_axes) &&
      all(df$sim > 0, na.rm = TRUE) &&
      all(df$est > 0, na.rm = TRUE)
  ) {
    p <- p + ggplot2::scale_x_log10() + ggplot2::scale_y_log10()
  }
  p
}
