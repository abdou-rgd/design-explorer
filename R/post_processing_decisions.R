# =============================================================================
# post_processing_decisions.R
# Decision-oriented post-processing helpers for NONMEM $DESIGN and SSE outputs.
# =============================================================================

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)


# =============================================================================
# Covariate effect decisions
# =============================================================================

#' Classify a covariate-effect confidence interval against clinical margins.
#'
#' @param lower Lower confidence bound on the ratio scale
#' @param upper Upper confidence bound on the ratio scale
#' @param margin Numeric length-2 relevance margin, default c(0.8, 1.25)
#' @return Character vector: relevant, non-relevant, inconclusive
#' @export
classify_covariate_effect <- function(lower, upper, margin = c(0.8, 1.25)) {
  if (length(margin) != 2L || any(is.na(margin)) || margin[1] >= margin[2]) {
    stop("margin must be a numeric length-2 vector with lower < upper")
  }
  dplyr::case_when(
    is.na(lower) | is.na(upper) ~ "inconclusive",
    upper < margin[1] | lower > margin[2] ~ "relevant",
    lower >= margin[1] & upper <= margin[2] ~ "non-relevant",
    TRUE ~ "inconclusive"
  )
}

#' Compute ratio-scale confidence interval and relevance decision.
#'
#' @param effect Estimate on the selected transform scale
#' @param se Standard error on the same scale as effect
#' @param transform log_ratio, ratio, or identity
#' @param margin Numeric length-2 relevance margin on ratio scale
#' @param ci Confidence level, default 0.90
#' @return Tibble with ratio, CI, and decision
#' @export
compute_covariate_effect <- function(effect, se,
                                     transform = "log_ratio",
                                     margin = c(0.8, 1.25),
                                     ci = 0.90) {
  transform <- match.arg(transform, c("log_ratio", "ratio", "identity"))
  if (!is.numeric(ci) || length(ci) != 1L || is.na(ci) || ci <= 0 || ci >= 1) {
    stop("ci must be a number between 0 and 1")
  }
  if (length(effect) != length(se)) {
    if (length(effect) == 1L) effect <- rep(effect, length(se))
    else if (length(se) == 1L) se <- rep(se, length(effect))
    else stop("effect and se must have compatible lengths")
  }

  z <- qnorm(1 - (1 - ci) / 2)
  lower_raw <- effect - z * se
  upper_raw <- effect + z * se

  if (transform == "log_ratio") {
    ratio <- exp(effect)
    lower <- exp(lower_raw)
    upper <- exp(upper_raw)
  } else {
    ratio <- effect
    lower <- lower_raw
    upper <- upper_raw
  }

  tibble::tibble(
    effect = effect,
    se = se,
    transform = transform,
    ratio = ratio,
    ci_lower = lower,
    ci_upper = upper,
    margin_lower = margin[1],
    margin_upper = margin[2],
    ci = ci,
    decision = classify_covariate_effect(lower, upper, margin)
  )
}

.infer_covariate_from_label <- function(label) {
  if (is.null(label)) return(NA_character_)
  label <- trimws(as.character(label))
  if (!nzchar(label) || is.na(label)) return(NA_character_)
  on_match <- regexec("(?i)^\\s*([^,=]+?)\\s+on\\s+.+$", label, perl = TRUE)
  on_hit <- regmatches(label, on_match)[[1]]
  if (length(on_hit) >= 2L) return(trimws(on_hit[2]))
  beta_match <- regexec("(?i)^BETA_([^_]+)_([^_]+)$", label, perl = TRUE)
  beta_hit <- regmatches(label, beta_match)[[1]]
  if (length(beta_hit) >= 3L) return(trimws(beta_hit[3]))
  NA_character_
}

.normalise_covariate_mapping <- function(mapping) {
  map <- tibble::as_tibble(mapping)
  if (!("param" %in% names(map))) stop("mapping must contain a 'param' column")
  if (!("effect_label" %in% names(map))) map$effect_label <- map$param
  if (!("transform" %in% names(map))) map$transform <- "log_ratio"
  if (!("relationship" %in% names(map))) map$relationship <- "Exp"
  if (!("covariate" %in% names(map))) {
    map$covariate <- vapply(map$effect_label, .infer_covariate_from_label, character(1))
  } else {
    missing_cov <- is.na(map$covariate) | !nzchar(trimws(map$covariate))
    map$covariate[missing_cov] <- vapply(map$effect_label[missing_cov],
                                         .infer_covariate_from_label,
                                         character(1))
  }
  map
}

.match_column <- function(column, available) {
  if (is.null(column) || length(column) == 0L) return(NULL)
  column <- trimws(as.character(column[1]))
  if (!nzchar(column)) return(NULL)
  exact <- available[available == column]
  if (length(exact) > 0L) return(exact[1])
  case_match <- available[tolower(available) == tolower(column)]
  if (length(case_match) > 0L) return(case_match[1])
  NULL
}

.match_columns <- function(columns, available) {
  if (is.null(columns) || length(columns) == 0L) return(character())
  matched <- vapply(columns, function(column) {
    hit <- .match_column(column, available)
    if (is.null(hit)) NA_character_ else hit
  }, character(1))
  unique(stats::na.omit(matched))
}

#' Read a covariate dataset CSV, including NONMEM CSVs with a preamble line.
#'
#' @param file CSV path
#' @return Tibble
#' @export
read_covariate_dataset_csv <- function(file) {
  if (is.null(file) || !file.exists(file)) return(tibble::tibble())
  lines <- readLines(file, n = 25L, warn = FALSE)
  header_idx <- which(grepl(",", lines, fixed = TRUE) | grepl("\t", lines, fixed = TRUE))
  skip <- if (length(header_idx) > 0L) header_idx[1] - 1L else 0L
  header <- if (length(header_idx) > 0L) lines[header_idx[1]] else ""
  delim <- if (grepl("\t", header, fixed = TRUE)) "\t" else ","
  readr::read_delim(
    file,
    delim = delim,
    skip = skip,
    na = c("", "NA", ".", "NaN"),
    show_col_types = FALSE,
    progress = FALSE
  )
}

#' Summarise covariate columns from a NONMEM-like dataset.
#'
#' @param data Data frame containing covariate columns
#' @param covariates Character vector of covariate columns to summarise
#' @param id_col Optional subject identifier. When present, one row per subject is used.
#' @param time_col Optional time column. If baseline_time is present, baseline rows are preferred.
#' @param baseline_time Optional baseline time, default 0
#' @return Tibble with reference and contrast values per covariate
#' @export
summarise_covariate_dataset <- function(data,
                                        covariates = NULL,
                                        id_col = NULL,
                                        time_col = NULL,
                                        baseline_time = 0) {
  if (is.null(data) || nrow(data) == 0L) return(tibble::tibble())
  df <- tibble::as_tibble(data)
  id_col <- .match_column(id_col, names(df))
  time_col <- .match_column(time_col, names(df))
  if (!is.null(time_col)) {
    baseline <- df[!is.na(df[[time_col]]) & df[[time_col]] == baseline_time, , drop = FALSE]
    if (nrow(baseline) > 0L) df <- baseline
  }
  if (!is.null(id_col)) {
    df <- df[!duplicated(df[[id_col]]), , drop = FALSE]
  }
  if (is.null(covariates)) {
    excluded <- c(id_col, time_col, "TIME", "ID", "DV", "MDV", "EVID", "AMT", "RATE")
    covariates <- setdiff(names(df), excluded)
    covariates <- covariates[vapply(df[covariates], is.numeric, logical(1))]
  }
  covariates <- .match_columns(covariates, names(df))
  if (length(covariates) == 0L) return(tibble::tibble())

  rows <- lapply(covariates, function(cov) {
    x <- suppressWarnings(as.numeric(df[[cov]]))
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(NULL)
    ux <- sort(unique(x))
    is_binary <- length(ux) == 2L && all(ux %in% c(0, 1))
    type <- if (is_binary) "binary" else "continuous"
    if (type == "continuous") {
      qs <- stats::quantile(x, probs = c(0.1, 0.5, 0.9), names = FALSE, na.rm = TRUE)
      tibble::tibble(
        covariate = cov,
        type = type,
        n = length(x),
        reference = qs[2],
        value_low = qs[1],
        value_high = qs[3],
        low_label = "P10",
        high_label = "P90"
      )
    } else {
      ref <- if (0 %in% ux) 0 else ux[1]
      high <- if (1 %in% ux) 1 else ux[length(ux)]
      tibble::tibble(
        covariate = cov,
        type = type,
        n = length(x),
        reference = ref,
        value_low = ref,
        value_high = high,
        low_label = as.character(ref),
        high_label = as.character(high)
      )
    }
  })
  dplyr::bind_rows(rows)
}

#' Build covariate contrasts from a mapping and dataset summary.
#'
#' @param mapping Data frame with param, effect_label, covariate, relationship
#' @param covariate_summary Output from summarise_covariate_dataset()
#' @return Tibble of contrast rows
#' @export
build_covariate_contrasts <- function(mapping, covariate_summary) {
  if (is.null(mapping) || nrow(mapping) == 0L ||
      is.null(covariate_summary) || nrow(covariate_summary) == 0L) {
    return(tibble::tibble())
  }
  map <- .normalise_covariate_mapping(mapping)
  summary <- tibble::as_tibble(covariate_summary)
  map <- map |>
    dplyr::filter(!is.na(.data$covariate), nzchar(.data$covariate)) |>
    dplyr::left_join(summary, by = "covariate") |>
    dplyr::filter(!is.na(.data$reference), !is.na(.data$value_high))
  if (nrow(map) == 0L) return(tibble::tibble())

  pieces <- lapply(seq_len(nrow(map)), function(i) {
    row <- map[i, ]
    high <- tibble::tibble(
      param = row$param,
      effect_label = row$effect_label,
      covariate = row$covariate,
      relationship = row$relationship,
      reference = row$reference,
      value = row$value_high,
      contrast_label = if (row$type == "continuous") "P90 vs median"
        else paste0(row$high_label, " vs ", row$low_label)
    )
    if (row$type == "continuous" && !is.na(row$value_low) && row$value_low != row$reference) {
      low <- high
      low$value <- row$value_low
      low$contrast_label <- "P10 vs median"
      dplyr::bind_rows(high, low)
    } else {
      high
    }
  })
  dplyr::bind_rows(pieces)
}

#' Compute Fayette-style covariate contrast ratios and confidence intervals.
#'
#' @param mapping Covariate effect mapping
#' @param fim_rse Tibble from get_rse()
#' @param covariate_data Optional raw covariate dataset
#' @param contrast_tbl Optional pre-built contrast table
#' @param margin Ratio-scale clinical margin
#' @param ci Confidence level
#' @return Tibble ready for plotting
#' @export
compute_covariate_contrast_effects <- function(mapping, fim_rse,
                                               covariate_data = NULL,
                                               contrast_tbl = NULL,
                                               id_col = NULL,
                                               time_col = NULL,
                                               baseline_time = 0,
                                               margin = c(0.8, 1.25),
                                               ci = 0.90) {
  if (is.null(mapping) || nrow(mapping) == 0L || is.null(fim_rse)) {
    return(tibble::tibble())
  }
  map <- .normalise_covariate_mapping(mapping)
  if (is.null(contrast_tbl)) {
    covs <- unique(stats::na.omit(map$covariate))
    cov_summary <- summarise_covariate_dataset(
      covariate_data,
      covariates = covs,
      id_col = id_col,
      time_col = time_col,
      baseline_time = baseline_time
    )
    contrast_tbl <- build_covariate_contrasts(map, cov_summary)
  }
  if (is.null(contrast_tbl) || nrow(contrast_tbl) == 0L) return(tibble::tibble())
  z <- stats::qnorm(1 - (1 - ci) / 2)
  joined <- tibble::as_tibble(contrast_tbl) |>
    dplyr::left_join(fim_rse, by = "param") |>
    dplyr::filter(!is.na(.data$estimate), !is.na(.data$se))
  if (nrow(joined) == 0L) return(tibble::tibble())

  joined |>
    dplyr::mutate(
      delta = .data$value - .data$reference,
      relationship = dplyr::coalesce(.data$relationship, "Exp"),
      ratio = dplyr::case_when(
        .data$relationship == "Exp" ~ exp(.data$estimate * .data$delta),
        TRUE ~ .data$estimate
      ),
      ci_1 = dplyr::case_when(
        .data$relationship == "Exp" ~ exp((.data$estimate - z * .data$se) * .data$delta),
        TRUE ~ .data$estimate - z * .data$se
      ),
      ci_2 = dplyr::case_when(
        .data$relationship == "Exp" ~ exp((.data$estimate + z * .data$se) * .data$delta),
        TRUE ~ .data$estimate + z * .data$se
      ),
      ci_lower = pmin(.data$ci_1, .data$ci_2),
      ci_upper = pmax(.data$ci_1, .data$ci_2),
      margin_lower = margin[1],
      margin_upper = margin[2],
      ci = ci,
      transform = "covariate_contrast",
      effect_label = paste0(.data$effect_label, " (", .data$contrast_label, ")"),
      decision = classify_covariate_effect(.data$ci_lower, .data$ci_upper, margin)
    ) |>
    dplyr::select(
      "param", "effect_label", "covariate", "contrast_label",
      "relationship", "reference", "value", "delta",
      "estimate", "se", "transform", "ratio", "ci_lower", "ci_upper",
      "margin_lower", "margin_upper", "ci", "decision",
      dplyr::everything(), -"ci_1", -"ci_2"
    )
}

#' Build a covariate-effect table from a user mapping and FIM RSE table.
#'
#' @param mapping Data frame with param and optional effect_label, transform
#' @param fim_rse Tibble from get_rse()
#' @param margin Ratio-scale relevance margin
#' @param ci Confidence level
#' @return Tibble ready for plotting
#' @export
compute_covariate_effects_table <- function(mapping, fim_rse,
                                            margin = c(0.8, 1.25),
                                            ci = 0.90) {
  if (is.null(mapping) || nrow(mapping) == 0L || is.null(fim_rse)) {
    return(tibble::tibble())
  }
  if (!("param" %in% names(mapping))) {
    stop("mapping must contain a 'param' column")
  }

  map <- tibble::as_tibble(mapping)
  if (!("effect_label" %in% names(map))) map$effect_label <- map$param
  if (!("transform" %in% names(map))) map$transform <- "log_ratio"

  joined <- map |>
    dplyr::left_join(fim_rse, by = "param") |>
    dplyr::filter(!is.na(estimate), !is.na(se))
  if (nrow(joined) == 0L) return(tibble::tibble())

  pieces <- lapply(seq_len(nrow(joined)), function(i) {
    ce <- compute_covariate_effect(
      joined$estimate[i],
      joined$se[i],
      transform = joined$transform[i],
      margin = margin,
      ci = ci
    )
    meta <- joined[i, setdiff(names(joined), c("estimate", "se", "transform"))]
    ce <- ce |>
      dplyr::mutate(estimate = .data$effect, .before = "se") |>
      dplyr::select(-"effect")
    dplyr::bind_cols(meta, ce)
  })

  dplyr::bind_rows(pieces) |>
    dplyr::mutate(
      effect_label = dplyr::if_else(
        is.na(.data$effect_label) | .data$effect_label == "",
        .data$param,
        .data$effect_label
      )
    )
}

#' Plot covariate effects as a forest plot.
#'
#' @param effects_tbl Output from compute_covariate_effects_table()
#' @return ggplot object
#' @export
plot_covariate_forest <- function(effects_tbl) {
  if (is.null(effects_tbl) || nrow(effects_tbl) == 0L) {
    return(ggplot() +
      labs(title = "No covariate effects available") +
      .theme_design())
  }

  df <- effects_tbl |>
    dplyr::mutate(
      effect_label = factor(.data$effect_label, levels = rev(.data$effect_label)),
      decision = factor(.data$decision,
                        levels = c("relevant", "inconclusive", "non-relevant"))
    )

  ggplot(df, aes(y = effect_label, x = ratio, color = decision)) +
    geom_rect(
      aes(xmin = margin_lower[1], xmax = margin_upper[1], ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = "#d1fae5", alpha = 0.35
    ) +
    geom_vline(xintercept = 1, color = "grey45", linetype = "solid", linewidth = 0.35) +
    geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), orientation = "y",
                  height = 0.16, linewidth = 0.7) +
    geom_point(size = 3) +
    scale_color_manual(
      values = c(
        "relevant" = "#b91c1c",
        "inconclusive" = "#b45309",
        "non-relevant" = "#047857"
      ),
      name = NULL
    ) +
    labs(
      title = "Covariate effect clinical relevance",
      subtitle = "Point = ratio estimate | Bar = confidence interval | Green band = non-relevance margin",
      x = "Ratio vs reference",
      y = NULL
    ) +
    .theme_design() +
    theme(legend.position = "bottom")
}


# =============================================================================
# Parameter families and precision summaries
# =============================================================================

.covariate_map_params <- function(covariate_map = NULL) {
  if (is.null(covariate_map) || !("param" %in% names(covariate_map))) character()
  else unique(as.character(covariate_map$param))
}

#' Classify a NONMEM parameter into a decision-facing family.
#'
#' @param param NONMEM parameter name
#' @param label Optional display label
#' @param covariate_map Optional mapping table with param column
#' @return Character vector: base, covariate, iiv, residual, other
#' @export
classify_parameter_family <- function(param, label = NULL, covariate_map = NULL) {
  cov_params <- .covariate_map_params(covariate_map)
  if (is.null(label)) label <- rep(NA_character_, length(param))
  if (length(label) == 1L && length(param) > 1L) label <- rep(label, length(param))

  lbl <- tolower(label %||% NA_character_)
  is_cov_label <- !is.na(lbl) & (
    grepl("\\bon\\b", lbl) |
      grepl("covariate|cov|weight|wt|age|sex|egfr|crcl|bmi|albumin|renal", lbl)
  )

  dplyr::case_when(
    param %in% cov_params ~ "covariate",
    grepl("^THETA", param) & is_cov_label ~ "covariate",
    grepl("^THETA", param) ~ "base",
    grepl("^OMEGA", param) ~ "iiv",
    grepl("^SIGMA", param) ~ "residual",
    TRUE ~ "other"
  )
}

#' Summarise FIM/SSE precision metrics by parameter family.
#'
#' @param fim_sse_tbl Tibble from compare_fim_sse()
#' @param covariate_map Optional mapping table with param column
#' @return Tibble with median RSE and band-pass summaries by family
#' @export
summarise_precision_by_family <- function(fim_sse_tbl, covariate_map = NULL) {
  if (is.null(fim_sse_tbl) || nrow(fim_sse_tbl) == 0L) {
    return(tibble::tibble())
  }
  df <- fim_sse_tbl |>
    dplyr::mutate(
      parameter_family = classify_parameter_family(
        .data$param,
        label = .data$param_label %||% NA_character_,
        covariate_map = covariate_map
      ),
      high_rse = pmax(.data$rse_fim, .data$rse_sse, na.rm = TRUE) > 100
    )

  df |>
    dplyr::group_by(.data$parameter_family) |>
    dplyr::summarise(
      n_params = dplyr::n(),
      n_matched = sum(.data$status == "matched", na.rm = TRUE),
      median_rse_fim = median(.data$rse_fim, na.rm = TRUE),
      median_rse_sse = median(.data$rse_sse, na.rm = TRUE),
      within_20_pct = mean(.data$pass_20pct, na.rm = TRUE),
      hidden_high_rse = sum(.data$high_rse, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        c("median_rse_fim", "median_rse_sse", "within_20_pct"),
        ~ ifelse(is.nan(.x), NA_real_, .x)
      )
    )
}


# =============================================================================
# FIM approximation provenance
# =============================================================================

.extract_design_value <- function(text, key) {
  pat <- paste0("(?i)\\b", key, "\\s*=\\s*([^\\s,;]+)")
  m <- regexpr(pat, text, perl = TRUE)
  if (m[1] < 0) return(NA_character_)
  val <- regmatches(text, m)
  sub(paste0("(?i).*", key, "\\s*=\\s*([^\\s,;]+).*"), "\\1", val, perl = TRUE)
}

.extract_design_int <- function(text, key) {
  val <- .extract_design_value(text, key)
  out <- suppressWarnings(as.integer(val))
  ifelse(is.na(out), NA_integer_, out)
}

#' Extract design-method provenance from control-stream text.
#'
#' @param ctl_lines Character vector from a .ctl/.mod/.con file
#' @param ext_lines Optional raw .ext lines, reserved for future provenance
#' @return List of method flags
#' @export
parse_design_provenance <- function(ctl_lines, ext_lines = NULL) {
  if (is.null(ctl_lines)) ctl_lines <- character()
  clean <- gsub(";.*$", "", ctl_lines)
  text <- paste(clean, collapse = " ")
  design_text <- paste(grep("(?i)\\$DESIGN|APPROX=|FIMDIAG=|FIMTYPE=|OFVTYPE=|VARCROSS=|MAXEVAL=",
                            clean, value = TRUE, perl = TRUE), collapse = " ")
  if (!nzchar(design_text)) design_text <- text

  approx <- toupper(.extract_design_value(design_text, "APPROX"))
  if (is.na(approx)) approx <- NA_character_
  maxeval <- .extract_design_int(design_text, "MAXEVAL")

  list(
    approx = approx,
    fimdiag = .extract_design_int(design_text, "FIMDIAG"),
    fimtype = .extract_design_int(design_text, "FIMTYPE"),
    ofvtype = .extract_design_int(design_text, "OFVTYPE"),
    varcross = .extract_design_int(design_text, "VARCROSS"),
    maxeval = maxeval,
    has_prior = any(grepl("(?i)^\\s*\\$PRIOR\\b", clean, perl = TRUE)),
    run_mode = if (!is.na(maxeval) && maxeval == 0L) "evaluation" else "optimization"
  )
}

#' Score whether FIM-derived conclusions need SSE/context validation.
#'
#' @param provenance List from parse_design_provenance()
#' @param has_sse Whether a matched SSE validation is available
#' @param fim_sse_agreement Optional proportion in +/-20 percent band
#' @return List with level, label, and message
#' @export
score_fim_approximation_risk <- function(provenance,
                                         has_sse = FALSE,
                                         fim_sse_agreement = NULL) {
  if (is.null(provenance)) provenance <- list()
  approx <- provenance$approx %||% NA_character_
  fimdiag <- provenance$fimdiag %||% NA_integer_
  ofvtype <- provenance$ofvtype %||% NA_integer_
  has_prior <- isTRUE(provenance$has_prior)

  reasons <- character()
  if (!is.na(approx) && approx == "FO") reasons <- c(reasons, "FO approximation")
  if (!is.na(fimdiag) && fimdiag == 1L) reasons <- c(reasons, "block-diagonal FIM")
  if (!is.na(ofvtype) && ofvtype == 8L) reasons <- c(reasons, "Bayesian FIM")
  if (has_prior) reasons <- c(reasons, "prior-informed design")
  if (!has_sse) reasons <- c(reasons, "no SSE validation loaded")

  if (has_sse && !is.null(fim_sse_agreement) &&
      !is.na(fim_sse_agreement) && fim_sse_agreement >= 0.8 &&
      length(setdiff(reasons, c("Bayesian FIM", "prior-informed design"))) == 0L) {
    return(list(
      level = "low_risk",
      label = "Low risk",
      message = "SSE is available and agrees with FIM precision for most matched parameters."
    ))
  }

  if (!is.na(ofvtype) && ofvtype == 8L || has_prior) {
    return(list(
      level = "needs_validation",
      label = "Bayesian/prior-informed",
      message = paste(
        "Bayesian or prior-informed FIM: compare OFV and D-criterion cautiously.",
        paste(reasons, collapse = "; ")
      )
    ))
  }

  if (length(reasons) > 0L) {
    return(list(
      level = "needs_validation",
      label = "Needs validation",
      message = paste("FIM conclusions should be validated with SSE:", paste(reasons, collapse = "; "))
    ))
  }

  list(
    level = "context_only",
    label = "Context available",
    message = "No high-risk FIM approximation flags detected from the loaded control stream."
  )
}


# =============================================================================
# Identifiability directions
# =============================================================================

#' Compute parameters that dominate the weakest matrix eigen-directions.
#'
#' @param matrix Numeric square matrix, usually FIM correlation or empirical corr
#' @param top_n Number of top contributors to return for the weakest direction
#' @return Tibble with eigen-direction contributors
#' @export
compute_identifiability_directions <- function(matrix, top_n = 3) {
  if (is.null(matrix) || nrow(matrix) < 2L || nrow(matrix) != ncol(matrix)) {
    return(tibble::tibble())
  }
  mat <- as.matrix(matrix)
  keep <- stats::complete.cases(mat) & stats::complete.cases(t(mat))
  mat <- mat[keep, keep, drop = FALSE]
  if (nrow(mat) < 2L) return(tibble::tibble())

  ev <- tryCatch(eigen(mat, symmetric = TRUE), error = function(e) NULL)
  if (is.null(ev)) return(tibble::tibble())

  idx <- which.min(ev$values)
  vec <- ev$vectors[, idx]
  params <- rownames(mat) %||% paste0("P", seq_len(nrow(mat)))
  ord <- order(abs(vec), decreasing = TRUE)
  ord <- ord[seq_len(min(top_n, length(ord)))]

  tibble::tibble(
    direction = "weakest",
    eigenvalue = ev$values[idx],
    param = params[ord],
    loading = vec[ord],
    contribution_pct = abs(vec[ord]) / sum(abs(vec)) * 100
  )
}

#' Plot weakest identifiability direction contributors.
#'
#' @param direction_tbl Output from compute_identifiability_directions()
#' @return ggplot object
#' @export
plot_identifiability_direction <- function(direction_tbl) {
  if (is.null(direction_tbl) || nrow(direction_tbl) == 0L) {
    return(ggplot() +
      labs(title = "Identifiability direction unavailable") +
      .theme_design())
  }

  df <- direction_tbl |>
    dplyr::mutate(param = factor(.data$param, levels = rev(.data$param)))

  ggplot(df, aes(x = contribution_pct, y = param)) +
    geom_col(fill = "#2563eb", alpha = 0.82) +
    labs(
      title = "Weakest identifiable direction",
      subtitle = sprintf("Smallest eigenvalue: %.3g", df$eigenvalue[1]),
      x = "Absolute loading contribution (%)",
      y = NULL
    ) +
    .theme_design()
}


# =============================================================================
# Pediatric scenario readout
# =============================================================================

#' Classify pediatric scenario readiness from post-processing metrics.
#'
#' @param metrics Named list or one-row data frame
#' @param thresholds Named list: precision, bias_ready, bias_caution, failed_ready, failed_caution
#' @return ready, caution, or weak
#' @export
classify_pediatric_readiness <- function(metrics,
                                         thresholds = list(
                                           precision = 30,
                                           bias_ready = 20,
                                           bias_caution = 30,
                                           failed_ready = 5,
                                           failed_caution = 15
                                         )) {
  getv <- function(name, default = NA_real_) {
    val <- metrics[[name]]
    if (is.null(val) || length(val) == 0L) default else as.numeric(val[[1]])
  }
  rse <- getv("rse_sse", getv("rse_empirical"))
  bias <- abs(getv("relative_bias"))
  failed <- getv("failed_pct", 0)

  if (!is.na(rse) && !is.na(bias) &&
      rse <= thresholds$precision &&
      bias <= thresholds$bias_ready &&
      failed <= thresholds$failed_ready) {
    return("ready")
  }
  if (!is.na(rse) && !is.na(bias) &&
      rse <= thresholds$precision &&
      bias <= thresholds$bias_caution &&
      failed <= thresholds$failed_caution) {
    return("caution")
  }
  "weak"
}

#' Summarise pediatric design scenarios from existing SSE/design metrics.
#'
#' @param sse_tbl Scenario-level metrics, or parameter metrics plus scenario col
#' @param scenario_metadata Optional scenario metadata keyed by scenario
#' @param thresholds Readiness thresholds
#' @return Scenario summary tibble
#' @export
summarise_pediatric_scenarios <- function(sse_tbl,
                                          scenario_metadata = NULL,
                                          thresholds = list(
                                            precision = 30,
                                            bias_ready = 20,
                                            bias_caution = 30,
                                            failed_ready = 5,
                                            failed_caution = 15
                                          )) {
  if (is.null(sse_tbl) || nrow(sse_tbl) == 0L) return(tibble::tibble())
  df <- tibble::as_tibble(sse_tbl)
  if (!("scenario" %in% names(df))) df$scenario <- "Scenario"

  metric_col <- function(candidates) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0L) NA_character_ else hit[1]
  }
  rse_col <- metric_col(c("rse_sse", "rse_empirical", "RSE SSE (%)"))
  bias_col <- metric_col(c("relative_bias", "Rel. Bias (%)"))
  rmse_col <- metric_col(c("rmse_sse", "rmse_relative", "RRMSE SSE (%)"))
  fail_col <- metric_col(c("failed_pct", "failure_pct"))

  out <- df |>
    dplyr::group_by(.data$scenario) |>
    dplyr::summarise(
      rse_sse = if (!is.na(rse_col)) median(.data[[rse_col]], na.rm = TRUE) else NA_real_,
      relative_bias = if (!is.na(bias_col)) median(.data[[bias_col]], na.rm = TRUE) else NA_real_,
      rmse_sse = if (!is.na(rmse_col)) median(.data[[rmse_col]], na.rm = TRUE) else NA_real_,
      failed_pct = if (!is.na(fail_col)) max(.data[[fail_col]], na.rm = TRUE) else 0,
      n_patients = if ("n_patients" %in% names(df)) max(.data$n_patients, na.rm = TRUE) else NA_real_,
      samples_per_patient = if ("samples_per_patient" %in% names(df)) max(.data$samples_per_patient, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(c("rse_sse", "relative_bias", "rmse_sse",
                      "failed_pct", "n_patients", "samples_per_patient"),
                    ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x))
    )

  if (!is.null(scenario_metadata) && nrow(scenario_metadata) > 0L &&
      "scenario" %in% names(scenario_metadata)) {
    out <- out |>
      dplyr::left_join(tibble::as_tibble(scenario_metadata), by = "scenario")
  }

  out$readiness <- vapply(seq_len(nrow(out)), function(i) {
    classify_pediatric_readiness(as.list(out[i, ]), thresholds)
  }, character(1))

  out
}
