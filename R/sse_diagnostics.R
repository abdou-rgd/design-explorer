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
  raw <- .read_sse_raw_base(file)
  n_total <- nrow(raw)

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
# PsN hypothesis helpers and summary statistics
# =============================================================================

.sse_hypothesis_type <- function(hypothesis) {
  out <- rep("unknown", length(hypothesis))
  out[is.na(hypothesis) | !nzchar(hypothesis)] <- "unknown"
  out[grepl("^simulation$", hypothesis, ignore.case = TRUE)] <- "simulation"
  out[grepl("alternative", hypothesis, ignore.case = TRUE)] <- "alternative"
  out
}

.sse_numeric_col <- function(dat, col, default = NA_real_) {
  if (!col %in% names(dat)) {
    return(rep(default, nrow(dat)))
  }
  suppressWarnings(as.numeric(dat[[col]]))
}

.sse_safe_quantile <- function(x, prob) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, names = FALSE, na.rm = TRUE))
}

.sse_safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  mean(x)
}

.sse_safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  stats::sd(x)
}

.sse_safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  min(x)
}

.sse_safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  max(x)
}

.sse_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3L) {
    return(NA_real_)
  }
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) {
    return(NA_real_)
  }
  mean(((x - mean(x)) / s)^3)
}

.sse_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 4L) {
    return(NA_real_)
  }
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) {
    return(NA_real_)
  }
  mean(((x - mean(x)) / s)^4) - 3
}

#' List PsN hypotheses available in an SSE raw results table.
#'
#' PsN SSE raw results can contain several hypotheses such as `simulation` and
#' `mc-alternative_1`. The app exposes these as a filter to avoid silently
#' mixing simulated-reference rows with alternative estimation rows.
#'
#' @param sse_all Tibble from `read_sse_raw_all()`.
#' @return Tibble with `value` and `label` columns for Shiny controls.
#' @export
sse_hypothesis_choices <- function(sse_all) {
  if (
    is.null(sse_all) || nrow(sse_all) == 0L || !"hypothesis" %in% names(sse_all)
  ) {
    return(tibble::tibble(
      value = c("auto", "all"),
      label = c("Auto: alternatives if available", "All hypotheses")
    ))
  }

  hypotheses <- unique(as.character(sse_all$hypothesis))
  hypotheses <- hypotheses[!is.na(hypotheses) & nzchar(hypotheses)]
  labels <- paste0(hypotheses, " (", .sse_hypothesis_type(hypotheses), ")")

  tibble::tibble(
    value = c("auto", "all", hypotheses),
    label = c("Auto: alternatives if available", "All hypotheses", labels)
  )
}

#' Filter an SSE table to one PsN hypothesis.
#'
#' The `auto` setting keeps alternative-model rows when PsN produced them,
#' otherwise it keeps the full table. This mirrors the common SSE workflow where
#' `simulation` rows are reference rows and `mc-alternative_*` rows are the
#' estimation model of interest.
#'
#' @param sse_all Tibble from `read_sse_raw_all()`.
#' @param hypothesis One of `auto`, `all`, or a concrete PsN hypothesis value.
#' @return Filtered tibble.
#' @export
filter_sse_hypothesis <- function(sse_all, hypothesis = "auto") {
  if (
    is.null(sse_all) || nrow(sse_all) == 0L || !"hypothesis" %in% names(sse_all)
  ) {
    return(sse_all)
  }
  if (
    is.null(hypothesis) || length(hypothesis) == 0L || is.na(hypothesis[[1]])
  ) {
    hypothesis <- "auto"
  }
  if (identical(hypothesis, "all")) {
    return(sse_all)
  }

  hyp <- as.character(sse_all$hypothesis)
  if (identical(hypothesis, "auto")) {
    alt <- .sse_hypothesis_type(hyp) == "alternative"
    if (any(alt, na.rm = TRUE)) {
      return(sse_all[alt, , drop = FALSE])
    }
    return(sse_all)
  }

  sse_all[hyp == hypothesis, , drop = FALSE]
}

#' Summarise run composition by PsN hypothesis.
#'
#' @param sse_all Tibble from `read_sse_raw_all()`.
#' @param condition_number_threshold Condition number threshold used to flag
#'   numerically fragile covariance matrices. PsN's default is commonly 1000.
#' @return Tibble with run counts and QC counts per hypothesis.
#' @export
compute_sse_run_composition <- function(
  sse_all,
  condition_number_threshold = 1000
) {
  if (is.null(sse_all) || nrow(sse_all) == 0L) {
    return(tibble::tibble())
  }

  dat <- tibble::as_tibble(sse_all)
  if (!"hypothesis" %in% names(dat)) {
    dat$hypothesis <- "all"
  }
  dat$hypothesis <- as.character(dat$hypothesis)
  dat$hypothesis_type <- .sse_hypothesis_type(dat$hypothesis)

  dat$minimization_successful_num <- .sse_numeric_col(
    dat,
    "minimization_successful"
  )
  dat$covariance_step_successful_num <- .sse_numeric_col(
    dat,
    "covariance_step_successful"
  )
  dat$estimate_near_boundary_num <- .sse_numeric_col(
    dat,
    "estimate_near_boundary"
  )
  dat$rounding_errors_num <- .sse_numeric_col(dat, "rounding_errors")
  dat$condition_number_num <- .sse_numeric_col(dat, "condition_number")
  dat$ofv_num <- .sse_numeric_col(dat, "ofv")
  has_sample <- "sample" %in% names(dat)
  has_converged <- "converged" %in% names(dat)

  dat |>
    dplyr::group_by(hypothesis, hypothesis_type) |>
    dplyr::summarise(
      n_runs = dplyr::n(),
      n_samples = if (has_sample) {
        dplyr::n_distinct(.data$sample, na.rm = TRUE)
      } else {
        NA_integer_
      },
      sample_min = if (has_sample) {
        .sse_safe_min(suppressWarnings(as.numeric(.data$sample)))
      } else {
        NA_real_
      },
      sample_max = if (has_sample) {
        .sse_safe_max(suppressWarnings(as.numeric(.data$sample)))
      } else {
        NA_real_
      },
      n_minimization_ok = sum(minimization_successful_num == 1, na.rm = TRUE),
      n_covariance_ok = sum(covariance_step_successful_num == 1, na.rm = TRUE),
      n_boundary = sum(estimate_near_boundary_num == 1, na.rm = TRUE),
      n_rounding_errors = sum(rounding_errors_num == 1, na.rm = TRUE),
      n_high_condition_number = sum(
        condition_number_num > condition_number_threshold,
        na.rm = TRUE
      ),
      n_converged = if (has_converged) {
        sum(.data$converged, na.rm = TRUE)
      } else {
        sum(minimization_successful_num == 1, na.rm = TRUE)
      },
      ofv_median = .sse_safe_quantile(ofv_num, 0.50),
      ofv_p5 = .sse_safe_quantile(ofv_num, 0.05),
      ofv_p95 = .sse_safe_quantile(ofv_num, 0.95),
      .groups = "drop"
    ) |>
    dplyr::arrange(hypothesis_type, hypothesis)
}

#' Compute PsN-style parameter summary statistics for SSE estimates.
#'
#' @param sse_all Tibble from `read_sse_raw_all()`.
#' @param true_values Named numeric vector of true parameter values.
#' @param param_labels Optional named labels for parameters.
#' @param only_converged Logical; keep minimization-successful runs only.
#' @return Long tibble with one row per parameter.
#' @export
compute_sse_psn_parameter_summary <- function(
  sse_all,
  true_values,
  param_labels = NULL,
  only_converged = TRUE
) {
  if (
    is.null(sse_all) ||
      nrow(sse_all) == 0L ||
      is.null(true_values) ||
      length(true_values) == 0L
  ) {
    return(tibble::tibble())
  }

  dat <- tibble::as_tibble(sse_all)
  if (isTRUE(only_converged) && "converged" %in% names(dat)) {
    dat <- dat[dat$converged, , drop = FALSE]
  }
  available <- intersect(names(true_values), names(dat))
  if (length(available) == 0L || nrow(dat) == 0L) {
    return(tibble::tibble())
  }

  rows <- lapply(available, function(param) {
    estimates <- suppressWarnings(as.numeric(dat[[param]]))
    estimates <- estimates[is.finite(estimates)]
    true_value <- suppressWarnings(as.numeric(true_values[[param]]))
    if (!is.finite(true_value)) {
      true_value <- NA_real_
    }
    err <- estimates - true_value
    rel_err <- if (is.finite(true_value) && abs(true_value) > 1e-15) {
      100 * err / abs(true_value)
    } else {
      rep(NA_real_, length(estimates))
    }
    label <- if (!is.null(param_labels) && param %in% names(param_labels)) {
      param_labels[[param]]
    } else {
      param
    }

    data.frame(
      param = param,
      param_label = label,
      param_type = .param_type(param),
      true_value = true_value,
      n = length(estimates),
      mean_estimate = .sse_safe_mean(estimates),
      median_estimate = .sse_safe_quantile(estimates, 0.50),
      sd_estimate = .sse_safe_sd(estimates),
      min_estimate = if (length(estimates) == 0L) NA_real_ else min(estimates),
      max_estimate = if (length(estimates) == 0L) NA_real_ else max(estimates),
      p5_estimate = .sse_safe_quantile(estimates, 0.05),
      p95_estimate = .sse_safe_quantile(estimates, 0.95),
      skewness = .sse_skewness(estimates),
      kurtosis = .sse_kurtosis(estimates),
      rmse = sqrt(.sse_safe_mean(err^2)),
      relative_rmse = sqrt(.sse_safe_mean(rel_err^2)),
      bias = .sse_safe_mean(err),
      relative_bias = .sse_safe_mean(rel_err),
      relative_absolute_bias = abs(.sse_safe_mean(rel_err)),
      rse = if (is.finite(true_value) && abs(true_value) > 1e-15) {
        100 * .sse_safe_sd(estimates) / abs(true_value)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(dplyr::bind_rows(rows))
}

#' Compute per-sample dOFV diagnostics between PsN hypotheses.
#'
#' @param sse_all Tibble from `read_sse_raw_all()`.
#' @param reference Reference hypothesis; defaults to `simulation` when present.
#' @return List with `$long` per-sample dOFV rows and `$summary` by hypothesis.
#' @export
compute_sse_dofv_diagnostics <- function(sse_all, reference = NULL) {
  empty <- list(long = tibble::tibble(), summary = tibble::tibble())
  if (
    is.null(sse_all) ||
      nrow(sse_all) == 0L ||
      !all(c("hypothesis", "sample", "ofv") %in% names(sse_all))
  ) {
    return(empty)
  }

  dat <- tibble::as_tibble(sse_all)
  dat$hypothesis <- as.character(dat$hypothesis)
  dat$ofv <- suppressWarnings(as.numeric(dat$ofv))
  dat <- dat[is.finite(dat$ofv), , drop = FALSE]
  if (nrow(dat) == 0L) {
    return(empty)
  }

  hypotheses <- unique(dat$hypothesis)
  hypotheses <- hypotheses[!is.na(hypotheses) & nzchar(hypotheses)]
  if (is.null(reference) || !reference %in% hypotheses) {
    reference <- if ("simulation" %in% hypotheses) {
      "simulation"
    } else {
      hypotheses[[1]]
    }
  }

  ref <- dat |>
    dplyr::filter(hypothesis == reference) |>
    dplyr::select(
      sample,
      reference_hypothesis = hypothesis,
      reference_ofv = ofv,
      dplyr::any_of("converged")
    )
  if ("converged" %in% names(ref)) {
    names(ref)[names(ref) == "converged"] <- "reference_converged"
  } else {
    ref$reference_converged <- NA
  }

  comp <- dat |>
    dplyr::filter(hypothesis != reference) |>
    dplyr::select(sample, hypothesis, ofv, dplyr::any_of("converged"))
  if (!"converged" %in% names(comp)) {
    comp$converged <- NA
  }

  long <- dplyr::inner_join(comp, ref, by = "sample") |>
    dplyr::mutate(dofv = ofv - reference_ofv) |>
    dplyr::select(
      sample,
      reference_hypothesis,
      hypothesis,
      reference_ofv,
      ofv,
      dofv,
      converged,
      reference_converged
    ) |>
    dplyr::arrange(hypothesis, sample)

  if (nrow(long) == 0L) {
    return(empty)
  }

  summary <- long |>
    dplyr::group_by(hypothesis, reference_hypothesis) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_negative = sum(dofv < 0, na.rm = TRUE),
      pct_negative = round(100 * n_negative / n, 1),
      mean_dofv = .sse_safe_mean(dofv),
      median_dofv = .sse_safe_quantile(dofv, 0.50),
      p5_dofv = .sse_safe_quantile(dofv, 0.05),
      p95_dofv = .sse_safe_quantile(dofv, 0.95),
      pct_dofv_gt_3_84 = round(100 * sum(dofv > 3.84, na.rm = TRUE) / n, 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(hypothesis)

  list(long = long, summary = summary)
}

#' Plot dOFV distributions between PsN hypotheses.
#'
#' @param dofv_long `$long` from `compute_sse_dofv_diagnostics()`.
#' @return ggplot object.
#' @export
plot_sse_dofv_distribution <- function(dofv_long) {
  if (is.null(dofv_long) || nrow(dofv_long) == 0L) {
    return(
      ggplot() +
        labs(title = "No dOFV diagnostics available") +
        .theme_design()
    )
  }

  ggplot(dofv_long, aes(x = dofv, fill = hypothesis)) +
    geom_histogram(bins = 24, alpha = 0.75, color = "white") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#334155") +
    geom_vline(xintercept = 3.84, linetype = "dotted", color = "#b45309") +
    facet_wrap(~hypothesis, scales = "free_y") +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    labs(
      title = "dOFV Diagnostics by PsN Hypothesis",
      subtitle = "dOFV = OFV(hypothesis) - OFV(reference) matched by sample",
      x = "dOFV",
      y = "Number of SSE samples"
    ) +
    .theme_design()
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
  } else {
    n_total
  }

  # Among minimization OK: no boundary
  min_ok_rows <- if (has_min) {
    sse_all[
      !is.na(sse_all$minimization_successful) &
        as.numeric(sse_all$minimization_successful) == 1,
      ,
      drop = FALSE
    ]
  } else {
    sse_all
  }

  n_no_boundary <- if (has_bnd) {
    sum(as.numeric(min_ok_rows$estimate_near_boundary) == 0, na.rm = TRUE)
  } else {
    n_min_ok
  }

  # Among minimization OK: covariance step OK
  n_cov_ok <- if (has_cov) {
    sum(as.numeric(min_ok_rows$covariance_step_successful) == 1, na.rm = TRUE)
  } else {
    n_min_ok
  }

  # Among minimization OK: no rounding errors
  n_no_rounding <- if (has_rnd) {
    sum(as.numeric(min_ok_rows$rounding_errors) == 0, na.rm = TRUE)
  } else {
    n_min_ok
  }

  safe_pct <- function(n, total) {
    if (total == 0L) 0 else round(100 * n / total, 1)
  }

  stages <- data.frame(
    stage = c(
      "Total runs",
      "Minimization OK",
      "No boundary estimates",
      "Covariance OK",
      "No rounding errors"
    ),
    n = c(n_total, n_min_ok, n_no_boundary, n_cov_ok, n_no_rounding),
    denom = c(n_total, n_total, n_min_ok, n_min_ok, n_min_ok),
    pct = c(
      100,
      safe_pct(n_min_ok, n_total),
      safe_pct(n_no_boundary, n_min_ok),
      safe_pct(n_cov_ok, n_min_ok),
      safe_pct(n_no_rounding, n_min_ok)
    ),
    stringsAsFactors = FALSE
  )

  list(
    stages = tibble::as_tibble(stages),
    total = n_total,
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
compute_param_distributions <- function(
  sse_all,
  true_values,
  param_labels = NULL
) {
  available <- intersect(names(true_values), names(sse_all))
  if (length(available) == 0L) {
    return(tibble::tibble())
  }

  rows <- lapply(available, function(pname) {
    estimates <- as.numeric(sse_all[[pname]])
    label <- if (!is.null(param_labels) && pname %in% names(param_labels)) {
      param_labels[[pname]]
    } else {
      pname
    }
    data.frame(
      param = pname,
      param_label = label,
      param_type = .param_type(pname),
      run_id = seq_along(estimates),
      estimate = estimates,
      true_value = true_values[[pname]],
      converged = sse_all$converged,
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
#' Shrinkage interpretation:
#'   <= 20 % : low shrinkage
#'   20-30 % : caution range
#'   > 30 %  : EBE-based diagnostics may be unreliable
#'
#' Savic & Karlsson (2009) recommend caution when eta- or epsilon-shrinkage
#' is substantial, usually greater than about 20-30%.
#'
#' @param sse_all      Tibble from read_sse_raw_all()
#' @param param_labels Named character vector mapping OMEGA(N,N) -> display name
#' @param only_converged Logical, filter to converged runs (default TRUE)
#' @return Tibble: eta, omega, param_label, n, mean_shrink, median_shrink,
#'         sd_shrink, q25, q75, p5, p95
#' @export
compute_shrinkage_summary <- function(
  sse_all,
  param_labels = NULL,
  only_converged = TRUE
) {
  shrink_cols <- grep(
    "^shrinkage_eta\\d+\\(%\\)$",
    names(sse_all),
    value = TRUE
  )
  if (length(shrink_cols) == 0L) {
    return(tibble::tibble())
  }

  dat <- if (only_converged) {
    sse_all[sse_all$converged, , drop = FALSE]
  } else {
    sse_all
  }
  if (nrow(dat) == 0L) {
    return(tibble::tibble())
  }

  rows <- lapply(shrink_cols, function(col) {
    idx <- as.integer(sub("shrinkage_eta(\\d+)\\(%\\)", "\\1", col))
    vals <- as.numeric(dat[[col]])
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0L) {
      return(NULL)
    }

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
      mean_shrink = round(mean(vals), 2),
      median_shrink = round(qs[3], 2),
      sd_shrink = round(sd(vals), 2),
      p5 = round(qs[1], 2),
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
compute_shrinkage_long <- function(
  sse_all,
  param_labels = NULL,
  only_converged = TRUE
) {
  shrink_cols <- grep(
    "^shrinkage_eta\\d+\\(%\\)$",
    names(sse_all),
    value = TRUE
  )
  empty <- tibble::tibble()
  if (length(shrink_cols) == 0L) {
    attr(empty, "status") <- "no_columns"
    return(empty)
  }

  dat <- if (only_converged) {
    sse_all[sse_all$converged, , drop = FALSE]
  } else {
    sse_all
  }
  if (nrow(dat) == 0L) {
    attr(empty, "status") <- "no_rows"
    return(empty)
  }

  run_ids <- if ("sample" %in% names(dat)) {
    as.integer(dat$sample)
  } else {
    seq_len(nrow(dat))
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
      run_id = run_ids,
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
compute_param_diagnostics <- function(
  sse_all,
  true_values,
  param_labels = NULL
) {
  available <- intersect(names(true_values), names(sse_all))
  if (length(available) == 0L) {
    return(tibble::tibble())
  }

  # Work only on converged runs for SE/RSE diagnostics

  dat <- sse_all[sse_all$converged, , drop = FALSE]
  n_runs <- nrow(dat)
  if (n_runs == 0L) {
    return(tibble::tibble())
  }

  safe_pct <- function(n) round(100 * n / n_runs, 1)

  rows <- lapply(available, function(pname) {
    estimates <- as.numeric(dat[[pname]])
    true_val <- true_values[[pname]]

    # SE column
    se_col <- paste0("se_", pname)
    se_vals <- if (se_col %in% names(dat)) {
      as.numeric(dat[[se_col]])
    } else {
      rep(NA, n_runs)
    }

    n_se_na <- sum(is.na(se_vals))

    # RSE > 100% (among runs with valid SE)
    rse_vals <- ifelse(
      !is.na(se_vals) & abs(estimates) > 1e-15,
      100 * se_vals / abs(estimates),
      NA_real_
    )
    n_rse_over_100 <- sum(!is.na(rse_vals) & rse_vals > 100)

    # Estimate at zero (potential boundary for variances)
    n_zero <- sum(!is.na(estimates) & abs(estimates) < 1e-15)

    label <- if (!is.null(param_labels) && pname %in% names(param_labels)) {
      param_labels[[pname]]
    } else {
      pname
    }

    data.frame(
      param = pname,
      param_label = label,
      param_type = .param_type(pname),
      n_runs = n_runs,
      n_se_na = n_se_na,
      pct_se_na = safe_pct(n_se_na),
      n_rse_over_100 = n_rse_over_100,
      pct_rse_over_100 = safe_pct(n_rse_over_100),
      n_zero_estimate = n_zero,
      pct_zero_estimate = safe_pct(n_zero),
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(dplyr::bind_rows(rows))
}


# =============================================================================
# detect_individual_pk_columns() — Identify subject/record-level PK outputs
# =============================================================================

#' Detect individual PK output columns in an uploaded SSE table.
#'
#' PsN `raw_results_*.csv` stores one row per simulated estimation run and
#' contains run-level population estimates. Individual PK variables only appear
#' when the NONMEM model writes them in `$TABLE` files and PsN keeps those table
#' outputs.
#'
#' @param data Data frame or tibble with uploaded SSE columns.
#' @return Character vector of detected individual PK-style column names.
#' @export
detect_individual_pk_columns <- function(data) {
  if (is.null(data) || length(names(data)) == 0L) {
    return(character())
  }

  nms <- names(data)
  canonical <- c(
    "ID",
    "TIME",
    "TAD",
    "DV",
    "PRED",
    "IPRED",
    "IRES",
    "IWRES",
    "CL",
    "VC",
    "V",
    "V1",
    "Q",
    "VP",
    "V2",
    "KA",
    "F1",
    "ETACL",
    "ETAVC",
    "ETAQ",
    "ETAVP",
    "ETAKA",
    "ETAF1"
  )
  eta_like <- "^ETA\\d+$"

  nms[nms %in% canonical | grepl(eta_like, nms)]
}


# =============================================================================
# compute_sse_reliability_map() — Combined precision / diagnostics map
# =============================================================================

#' Combine SSE empirical precision, diagnostics, and shrinkage into one table.
#'
#' @param sse_all Tibble from `read_sse_raw_all()` with a `converged` column.
#' @param true_values Named numeric vector from `read_true_values()`.
#' @param param_labels Optional named character vector for display labels.
#' @return Tibble with parameter-level reliability diagnostics.
#' @export
compute_sse_reliability_map <- function(
  sse_all,
  true_values,
  param_labels = NULL
) {
  empty <- tibble::tibble(
    param = character(),
    param_label = character(),
    param_type = character(),
    rse_empirical = numeric(),
    relative_bias = numeric(),
    pct_se_na = numeric(),
    pct_rse_over_100 = numeric(),
    mean_shrinkage = numeric(),
    risk_score = numeric()
  )
  if (
    is.null(sse_all) ||
      nrow(sse_all) == 0L ||
      is.null(true_values) ||
      length(true_values) == 0L
  ) {
    return(empty)
  }

  if (!"converged" %in% names(sse_all)) {
    sse_all$converged <- TRUE
  }

  converged <- sse_all[sse_all$converged, , drop = FALSE]
  metrics <- compute_sse_metrics(converged, true_values, param_labels)
  if (nrow(metrics) == 0L) {
    return(empty)
  }

  diag <- compute_param_diagnostics(sse_all, true_values, param_labels)
  shrink <- compute_shrinkage_summary(
    sse_all,
    param_labels,
    only_converged = TRUE
  )
  shrink_lookup <- if (nrow(shrink) > 0L) {
    shrink |>
      dplyr::select(param = omega, mean_shrinkage = mean_shrink)
  } else {
    tibble::tibble(param = character(), mean_shrinkage = numeric())
  }

  out <- metrics |>
    dplyr::select(
      param,
      param_label,
      param_type,
      rse_empirical,
      relative_bias
    ) |>
    dplyr::left_join(
      diag |>
        dplyr::select(param, pct_se_na, pct_rse_over_100),
      by = "param"
    ) |>
    dplyr::left_join(shrink_lookup, by = "param")

  safe_abs <- function(x) ifelse(is.na(x), 0, abs(x))
  safe_val <- function(x) ifelse(is.na(x), 0, x)
  out$risk_score <- safe_abs(out$relative_bias) +
    safe_val(out$rse_empirical) +
    safe_val(out$pct_se_na) +
    safe_val(out$pct_rse_over_100) +
    safe_val(out$mean_shrinkage)

  out |>
    dplyr::arrange(dplyr::desc(risk_score))
}


# =============================================================================
# plot_sse_reliability_map() — Bias / RSE reliability plot
# =============================================================================

#' Plot SSE reliability across parameters.
#'
#' x = relative bias, y = empirical RSE. Point size reflects unstable SE/RSE
#' diagnostics, and facets separate THETA/OMEGA/SIGMA families.
#'
#' @param reliability_df Tibble from `compute_sse_reliability_map()`.
#' @return ggplot object.
#' @export
plot_sse_reliability_map <- function(reliability_df) {
  if (is.null(reliability_df) || nrow(reliability_df) == 0L) {
    return(
      ggplot() +
        labs(title = "No SSE reliability data available") +
        .theme_design()
    )
  }

  df <- reliability_df |>
    dplyr::filter(!is.na(rse_empirical), !is.na(relative_bias))
  if (nrow(df) == 0L) {
    return(
      ggplot() +
        labs(title = "No valid bias/RSE values for reliability map") +
        .theme_design()
    )
  }

  df$type_group <- dplyr::case_when(
    grepl("^THETA", df$param_type) ~ "Fixed effects",
    grepl("^OMEGA", df$param_type) ~ "IIV",
    grepl("^SIGMA", df$param_type) ~ "Residual",
    TRUE ~ df$param_type
  )
  df$family <- dplyr::case_when(
    grepl("^THETA", df$param_type) ~ "THETA",
    grepl("^OMEGA", df$param_type) ~ "OMEGA",
    grepl("^SIGMA", df$param_type) ~ "SIGMA",
    TRUE ~ "Other"
  )
  df$issue_burden <- pmax(
    ifelse(is.na(df$pct_se_na), 0, df$pct_se_na),
    ifelse(is.na(df$pct_rse_over_100), 0, df$pct_rse_over_100)
  )

  col_fixed <- "#6C2B91"
  col_iiv <- "#2B6991"
  col_resid <- "#E07B39"
  rse_thresholds <- data.frame(
    rse_empirical = c(30, 50, 100),
    threshold_color = c("#16a34a", "#d97706", "#dc2626"),
    stringsAsFactors = FALSE
  )

  ggplot(df, aes(x = relative_bias, y = rse_empirical)) +
    annotate(
      "rect",
      xmin = -20,
      xmax = 20,
      ymin = -Inf,
      ymax = 30,
      fill = "#16a34a",
      alpha = 0.06
    ) +
    geom_hline(
      data = rse_thresholds,
      aes(yintercept = rse_empirical, color = threshold_color),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.35
    ) +
    geom_vline(
      xintercept = c(-20, 20),
      linetype = "dotted",
      color = "#6b7280",
      linewidth = 0.35
    ) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.35) +
    geom_point(aes(color = type_group, size = issue_burden), alpha = 0.82) +
    geom_text(
      aes(label = param_label),
      nudge_y = 3,
      size = 3,
      check_overlap = TRUE
    ) +
    facet_wrap(~family, scales = "free_y") +
    scale_color_manual(
      values = c(
        "Fixed effects" = col_fixed,
        "IIV" = col_iiv,
        "Residual" = col_resid,
        "#16a34a" = "#16a34a",
        "#d97706" = "#d97706",
        "#dc2626" = "#dc2626"
      ),
      breaks = c("Fixed effects", "IIV", "Residual"),
      name = NULL
    ) +
    scale_size_continuous(
      range = c(2.5, 7),
      name = "SE/RSE issue burden (%)"
    ) +
    labs(
      title = "SSE Reliability Map",
      subtitle = "Green zone: |relative bias| <= 20% and empirical RSE < 30%",
      x = "Relative bias (%)",
      y = "Empirical RSE (%)"
    ) +
    .theme_design() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 8)
    )
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

  # Keep this summary in base R for compatibility with dplyr 1.0.x + vctrs 0.6+.
  summary_rows <- lapply(
    split(df, list(df$param_label, df$true_value), drop = TRUE),
    function(group) {
      data.frame(
        param_label = group$param_label[[1]],
        true_value = group$true_value[[1]],
        median_est = stats::median(group$estimate, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  )
  summaries <- tibble::as_tibble(dplyr::bind_rows(summary_rows))

  p <- ggplot(df, aes(x = estimate))

  if (show_failed && any(!df$converged)) {
    p <- p +
      geom_density(
        data = df[!df$converged, ],
        fill = "grey80",
        alpha = 0.4,
        color = "grey60"
      ) +
      geom_density(
        data = df[df$converged, ],
        fill = "#3b82f6",
        alpha = 0.5,
        color = "#1e40af"
      )
  } else {
    p <- p +
      geom_density(fill = "#3b82f6", alpha = 0.5, color = "#1e40af")
  }

  p <- p +
    geom_vline(
      data = summaries,
      aes(xintercept = true_value),
      color = "#dc2626",
      linetype = "dashed",
      size = 0.7
    ) +
    geom_vline(
      data = summaries,
      aes(xintercept = median_est),
      color = "#1e40af",
      linetype = "solid",
      size = 0.7
    ) +
    facet_wrap(~param_label, scales = "free", ncol = 3) +
    labs(
      title = "Parameter Estimate Distributions (SSE)",
      subtitle = "Red dashed = true value | Blue solid = median estimate",
      x = "Estimate",
      y = "Density"
    ) +
    .theme_design() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey50")
    )

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
      geom_histogram(
        bins = 30,
        alpha = 0.7,
        position = "identity",
        color = "white",
        size = 0.2
      ) +
      scale_fill_manual(
        values = c("Converged" = "#3b82f6", "Failed" = "#ef4444"),
        name = NULL
      )
  } else {
    p <- ggplot(df[df$converged, ], aes(x = ofv)) +
      geom_histogram(
        bins = 30,
        fill = "#3b82f6",
        alpha = 0.7,
        color = "white",
        size = 0.2
      )
  }

  p <- p +
    geom_vline(
      xintercept = median_ofv,
      color = "#1e40af",
      linetype = "dashed",
      size = 0.8
    ) +
    annotate(
      "text",
      x = median_ofv,
      y = Inf,
      vjust = 2,
      hjust = -0.1,
      label = sprintf("Median: %.1f", median_ofv),
      color = "#1e40af",
      size = 3.5,
      fontface = "bold"
    ) +
    labs(
      title = "OFV Distribution Across SSE Runs",
      x = "Objective Function Value",
      y = "Count"
    ) +
    .theme_design() +
    theme(plot.title = element_text(hjust = 0.5))

  p
}


# =============================================================================
# plot_shrinkage_boxplot() — Distribution of shrinkage per ETA
# =============================================================================

#' Boxplot of per-replicate shrinkage for each ETA.
#'
#' Visualises how stable the shrinkage is across SSE replicates. A high median
#' indicates that EBE-based diagnostics for this ETA need caution; a wide IQR
#' indicates that shrinkage varies sample to sample.
#'
#' @param shrink_long Tibble from compute_shrinkage_long()
#' @param title       Plot title (NULL = auto)
#' @return ggplot object
#' @export
plot_shrinkage_boxplot <- function(shrink_long, title = NULL) {
  if (is.null(shrink_long) || nrow(shrink_long) == 0L) {
    status <- attr(shrink_long, "status") %||% "no_columns"
    msg <- switch(
      status,
      no_columns = "No shrinkage_eta*(%) columns in raw_results",
      all_na = "Shrinkage columns present but empty (PsN -no_shrinkage?)",
      no_rows = "No converged replicates available",
      "No shrinkage data to display"
    )
    return(ggplot() + labs(title = msg) + .theme_design())
  }

  df <- shrink_long
  df$param_label <- factor(df$param_label, levels = unique(df$param_label))

  ttl <- title %||% "Shrinkage Distribution per ETA"
  n_rep <- length(unique(df$run_id))

  p <- ggplot(df, aes(x = param_label, y = shrinkage)) +
    # Reference zones
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = 20,
      fill = "#16a34a",
      alpha = 0.06
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 20,
      ymax = 30,
      fill = "#d97706",
      alpha = 0.06
    ) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 30,
      ymax = Inf,
      fill = "#dc2626",
      alpha = 0.06
    ) +
    geom_hline(
      yintercept = c(20, 30),
      linetype = "dashed",
      color = "grey50",
      size = 0.3
    ) +
    geom_boxplot(
      fill = "#2B6991",
      alpha = 0.7,
      color = "grey30",
      width = 0.6,
      outlier.size = 0.8
    ) +
    labs(
      title = ttl,
      subtitle = sprintf(
        "N=%d replicates | Green <=20%% | Amber 20-30%% | Red >30%%: caution for EBE diagnostics (Savic & Karlsson 2009)",
        n_rep
      ),
      x = NULL,
      y = "Shrinkage (%)"
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
# plot_shrinkage_rse_scatter() — Exploratory RSE vs shrinkage scatter
# =============================================================================

#' Exploratory scatter plot of empirical RSE vs mean shrinkage per OMEGA.
#'
#' This app-derived view combines two literature-supported diagnostics:
#' empirical SSE precision and ETA shrinkage. Savic & Karlsson (2009) motivate
#' interpreting EBE-based diagnostics in light of shrinkage; they do not propose
#' this exact RSE-vs-shrinkage plot.
#'
#' Classifies each variance parameter into a review tier:
#'   Green  : RSE < 30 % AND shrinkage <= 20 %
#'   Amber  : RSE >= 30 % OR shrinkage 20-30 %
#'   Red    : shrinkage > 30 %, where EBE-based diagnostics may be unreliable
#'
#' The shrinkage axis marks the Savic & Karlsson (2009) caution range
#' (>20-30%); the RSE axis uses the local precision target (<30%).
#'
#' @param sse_all      Tibble from read_sse_raw_all()
#' @param true_values  Named numeric vector from read_true_values()
#' @param param_labels Named character vector (optional)
#' @param shrink_sum   Optional pre-computed tibble from compute_shrinkage_summary().
#'                     If provided, must be built from the converged subset so it
#'                     aligns with the RSE metrics computed here. When NULL
#'                     (default), shrinkage is recomputed internally.
#' @return ggplot object
#' @export
plot_shrinkage_rse_scatter <- function(
  sse_all,
  true_values,
  param_labels = NULL,
  shrink_sum = NULL
) {
  if (is.null(sse_all) || nrow(sse_all) == 0L || length(true_values) == 0L) {
    return(
      ggplot() +
        labs(
          title = "Load SSE data and .ctl to see exploratory RSE-shrinkage map"
        ) +
        .theme_design()
    )
  }

  if (is.null(shrink_sum)) {
    shrink_sum <- compute_shrinkage_summary(sse_all, param_labels)
  }
  if (nrow(shrink_sum) == 0L) {
    shrink_cols <- grep(
      "^shrinkage_eta\\d+\\(%\\)$",
      names(sse_all),
      value = TRUE
    )
    msg <- if (length(shrink_cols) == 0L) {
      "No shrinkage_eta*(%) columns in raw_results"
    } else {
      "Shrinkage columns present but empty (PsN -no_shrinkage?)"
    }
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
    return(
      ggplot() +
        labs(
          title = "No OMEGA parameters shared between shrinkage and SSE metrics"
        ) +
        .theme_design()
    )
  }

  df$tier <- dplyr::case_when(
    df$shrinkage > 30 ~ "Red (shrink >30%)",
    df$rse < 30 & df$shrinkage <= 20 ~ "Green (RSE<30% & shrink<=20%)",
    TRUE ~ "Amber (marginal)"
  )
  df$tier <- factor(
    df$tier,
    levels = c(
      "Green (RSE<30% & shrink<=20%)",
      "Amber (marginal)",
      "Red (shrink >30%)"
    )
  )

  x_max <- max(100, max(df$shrinkage, na.rm = TRUE) * 1.1)
  y_max <- max(60, max(df$rse, na.rm = TRUE) * 1.1)

  p <- ggplot(df, aes(x = shrinkage, y = rse)) +
    # Tier rectangles (background)
    annotate(
      "rect",
      xmin = -Inf,
      xmax = 20,
      ymin = -Inf,
      ymax = 30,
      fill = "#16a34a",
      alpha = 0.08
    ) +
    annotate(
      "rect",
      xmin = 30,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "#dc2626",
      alpha = 0.08
    ) +
    # Threshold lines
    geom_hline(
      yintercept = 30,
      linetype = "dashed",
      color = "#16a34a",
      size = 0.4
    ) +
    geom_vline(
      xintercept = 20,
      linetype = "dashed",
      color = "#d97706",
      size = 0.4
    ) +
    geom_vline(
      xintercept = 30,
      linetype = "dashed",
      color = "#dc2626",
      size = 0.4
    ) +
    # Points
    geom_point(aes(color = tier), size = 4, alpha = 0.85) +
    geom_text(
      aes(label = param_label),
      nudge_y = y_max * 0.025,
      size = 3,
      check_overlap = TRUE
    ) +
    scale_color_manual(
      values = c(
        "Green (RSE<30% & shrink<=20%)" = "#16a34a",
        "Amber (marginal)" = "#d97706",
        "Red (shrink >30%)" = "#dc2626"
      ),
      drop = FALSE,
      name = NULL
    ) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(0, y_max)) +
    labs(
      title = "Exploratory Precision vs Mean ETA Shrinkage",
      subtitle = paste0(
        "App-derived map: empirical SSE RSE plus shrinkage caution range ",
        ">20-30% motivated by Savic & Karlsson 2009"
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
