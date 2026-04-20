# =============================================================================
# sse_metrics.R
# Parseurs et metriques pour la validation FIM vs SSE (Stochastic Simulation
# and Estimation). Lit les resultats bruts PsN, calcule les metriques
# empiriques, et compare avec les predictions FIM.
#
# Contenu :
#   read_sse_raw()         — Lire et filtrer un CSV PsN brut
#   .normalize_psn_cols()  — Normaliser les noms de colonnes PsN -> NONMEM
#   read_true_values()     — Extraire valeurs vraies depuis un .ctl
#   compute_sse_metrics()  — Calculer RSE/RMSE/biais empiriques
#   compare_fim_sse()      — Joindre metriques FIM et SSE
#   plot_fim_vs_sse()      — Scatter FIM RSE predite vs SSE RSE empirique
#
# Prerequis : ggplot2, dplyr, stringr
# =============================================================================

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)


# =============================================================================
# .normalize_psn_cols() — Normaliser noms colonnes PsN -> format NONMEM
# =============================================================================

#' Normalise les noms de colonnes PsN vers le format NONMEM standard.
#'
#' Mapping :
#'   "--th1- CL"        -> "THETA1"
#'   "--eps1- Prop"      -> "SIGMA(1,1)"
#'   "ETA(3) Q"          -> "OMEGA(3,3)"
#'   "OMEGA(1,1)"        -> "OMEGA(1,1)" (inchange)
#'   "se--th1- CL"       -> "se_THETA1"
#'   "seOMEGA(1,1)"      -> "se_OMEGA(1,1)"
#'   "seETA(3) Q"        -> "se_OMEGA(3,3)"
#'
#' @param nms Character vector de noms de colonnes
#' @return Character vector de noms normalises
.normalize_psn_cols <- function(nms) {
  nms <- trimws(nms)
  out <- nms

  for (i in seq_along(out)) {
    nm <- out[i]
    se_prefix <- ""

    # Detect and strip SE prefix
    if (grepl("^se", nm)) {
      # "se--th1-..." or "seOMEGA..." or "seETA..."
      if (grepl("^se--", nm)) {
        se_prefix <- "se_"
        nm <- sub("^se", "", nm)
      } else if (grepl("^seOMEGA", nm)) {
        se_prefix <- "se_"
        nm <- sub("^se", "", nm)
      } else if (grepl("^seETA", nm)) {
        se_prefix <- "se_"
        nm <- sub("^se", "", nm)
      } else if (grepl("^se--eps", nm)) {
        se_prefix <- "se_"
        nm <- sub("^se", "", nm)
      }
    }

    # THETA: "--th1- CL" -> "THETA1"
    if (grepl("^--th(\\d+)-", nm)) {
      idx <- sub("^--th(\\d+)-.*", "\\1", nm)
      out[i] <- paste0(se_prefix, "THETA", idx)
      next
    }

    # SIGMA: "--eps1- Proportional" -> "SIGMA(1,1)"
    if (grepl("^--eps(\\d+)-", nm)) {
      idx <- sub("^--eps(\\d+)-.*", "\\1", nm)
      out[i] <- paste0(se_prefix, "SIGMA(", idx, ",", idx, ")")
      next
    }

    # ETA: "ETA(3) Q" -> "OMEGA(3,3)"  (diagonal IIV)
    if (grepl("^ETA\\((\\d+)\\)", nm)) {
      idx <- sub("^ETA\\((\\d+)\\).*", "\\1", nm)
      out[i] <- paste0(se_prefix, "OMEGA(", idx, ",", idx, ")")
      next
    }

    # OMEGA(x,y) stays as-is, just add se_ prefix if needed
    if (grepl("^OMEGA\\(", nm)) {
      out[i] <- paste0(se_prefix, nm)
      next
    }

    # Other columns: add se_ prefix if detected, otherwise keep as-is
    if (nchar(se_prefix) > 0L) {
      out[i] <- paste0(se_prefix, nm)
    }
  }

  out
}


# =============================================================================
# .detect_sse_format() — Detect CSV format (raw_results vs sse_results summary)
# =============================================================================

.detect_sse_format <- function(file) {
  first_line <- readLines(file, n = 1L, warn = FALSE)
  if (grepl("^SSE run info", first_line, ignore.case = TRUE)) {
    return("summary")
  }
  "raw"
}


# =============================================================================
# read_sse_auto() — Auto-detect format and dispatch to appropriate reader
# =============================================================================

#' Read SSE results from either PsN raw_results CSV or sse_results summary.
#'
#' Detects the file format automatically:
#' - raw_results_*.csv: individual run estimates (one row per run)
#' - sse_results.csv: PsN summary file with pre-computed statistics
#'
#' For raw_results, returns the filtered tibble (same as read_sse_raw()).
#' For sse_results summary, returns a list with true_values and pre-computed
#' metrics directly usable by the app.
#'
#' @param file Path to the CSV file
#' @return List with format ("raw" or "summary") and data.
#'   For "raw": $data = tibble (same as read_sse_raw())
#'   For "summary": $true_values, $metrics (tibble), $n_samples, $sim_model
#' @export
read_sse_auto <- function(file) {
  fmt <- .detect_sse_format(file)
  if (fmt == "summary") {
    summary_data <- read_sse_summary(file)
    list(format = "summary", data = summary_data)
  } else {
    raw_data <- read_sse_raw(file)
    list(format = "raw", data = raw_data)
  }
}


# =============================================================================
# read_sse_summary() — Parse PsN sse_results.csv summary file
# =============================================================================

#' Parse PsN sse_results.csv summary file.
#'
#' Extracts true values, sample count, and pre-computed metrics (mean, sd,
#' rmse, relative_rmse, bias, relative_bias, rse) directly from the PsN
#' summary output.
#'
#' @param file Path to sse_results.csv
#' @return List: true_values (named numeric), metrics (tibble), n_samples,
#'         sim_model (character)
#' @export
read_sse_summary <- function(file) {
  lines <- readLines(file, warn = FALSE)

  # Helper: parse a CSV line respecting quotes (handles OMEGA(1,1) etc.)
  .parse_csv_line <- function(ln) {
    row <- read.csv(textConnection(ln), header = FALSE,
                    stringsAsFactors = FALSE, check.names = FALSE)
    trimws(as.character(row[1, ]))
  }

  # --- Extract run info (line 3) ---
  info_parts <- .parse_csv_line(lines[3])
  n_samples <- as.integer(info_parts[3])
  sim_model <- info_parts[4]

  # --- Extract true values (lines 5-6) ---
  # Line 5: header = "", "ofv", "THETA1", ..., "OMEGA(1,1)", ...
  true_header <- .parse_csv_line(lines[5])
  true_vals_line <- .parse_csv_line(lines[6])

  # Drop first two columns (empty + ofv/label)
  param_names <- true_header[-(1:2)]
  param_vals  <- true_vals_line[-(1:2)]
  valid <- suppressWarnings(!is.na(as.numeric(param_vals)))
  param_names <- param_names[valid]
  param_vals  <- as.numeric(param_vals[valid])
  true_values <- setNames(param_vals, param_names)

  # --- Extract statistics (lines 10+) ---
  # Line 10: abbreviated header ("", "ofv", "TH_1", "OM_1", ...)
  # Lines 11+: "mean", "sd", "rmse", "relative_bias", "rse", ...
  # Map abbreviated -> NONMEM names by position
  n_params <- length(param_names)

  # Parse stat rows (lines 11+), using simple split (no parens in TH_1/OM_1)
  stat_rows <- list()
  for (i in 11:length(lines)) {
    ln <- lines[i]
    if (grepl("^\\s*$", ln) || grepl("^[A-Z]", ln) ||
        grepl("standard error CI", ln, ignore.case = TRUE)) next
    parts <- .parse_csv_line(ln)
    row_label <- parts[1]
    if (nchar(row_label) == 0L) next
    if (grepl("^[0-9.]+%$", row_label)) next
    vals <- parts[-(1:2)]  # drop label + ofv
    stat_rows[[row_label]] <- suppressWarnings(
      as.numeric(vals[seq_len(n_params)])
    )
  }

  # --- Build metrics tibble ---
  get_stat <- function(name) {
    v <- stat_rows[[name]]
    if (is.null(v)) rep(NA_real_, n_params) else v
  }

  metrics <- data.frame(
    param = param_names,
    param_label = param_names,
    param_type = vapply(param_names, .param_type, character(1),
                        USE.NAMES = FALSE),
    true_value = param_vals,
    mean_estimate = round(get_stat("mean"), 6),
    rse_empirical = round(get_stat("rse"), 2),
    rmse_relative = round(get_stat("relative_rmse"), 2),
    relative_bias = round(get_stat("relative_bias"), 2),
    rb_ci_lower = NA_real_,  # Not available in summary format
    rb_ci_upper = NA_real_,
    n = rep(n_samples, n_params),
    stringsAsFactors = FALSE
  )

  # Compute CI from bias and rse if possible:
  # sd(REE) ~ rse (since rse = sd/true*100 ~ sd(REE) when bias is small)
  # CI = RB +/- 1.96 * sd(REE) / sqrt(K)
  sd_vals <- get_stat("sd")
  for (j in seq_len(n_params)) {
    if (!is.na(sd_vals[j]) && abs(param_vals[j]) > 1e-15 && !is.na(n_samples)) {
      sd_ree <- sd_vals[j] / abs(param_vals[j]) * 100
      se_rb <- sd_ree / sqrt(n_samples)
      metrics$rb_ci_lower[j] <- round(metrics$relative_bias[j] - 1.96 * se_rb, 2)
      metrics$rb_ci_upper[j] <- round(metrics$relative_bias[j] + 1.96 * se_rb, 2)
    }
  }

  list(
    true_values = true_values,
    metrics = tibble::as_tibble(metrics),
    n_samples = n_samples,
    sim_model = sim_model
  )
}


# =============================================================================
# read_sse_raw() — Read and filter PsN raw_results CSV
# =============================================================================

#' Read a PsN SSE raw results CSV file (one row per run).
#'
#' Filters runs with minimization_successful == 1 (if column exists).
#' Normalizes column names to standard NONMEM format.
#' If the minimization_successful column is absent, assumes the file was
#' pre-filtered (e.g. via PsN -out_filter option).
#'
#' @param file Path to the raw results CSV
#' @return Tibble with normalized columns.
#'         Attributes: n_total, n_success, pre_filtered
#' @export
read_sse_raw <- function(file) {
  if (!file.exists(file)) stop("Fichier SSE introuvable : ", file)

  # Prefer readr::read_csv — quote-aware (handles OMEGA(1,1) commas) and
  # robust type inference. Fall back to read.csv if readr unavailable.
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

  # Normalize column names
  names(raw) <- .normalize_psn_cols(names(raw))

  # Filter successful minimizations (if column exists)
  # If the column is absent, the user likely already filtered via PsN
  # -out_filter=minimization_successful.eq.1 (PsN SSE User Guide v5.7.0)
  pre_filtered <- FALSE
  if ("minimization_successful" %in% names(raw)) {
    raw$minimization_successful <- as.numeric(raw$minimization_successful)
    raw <- raw[!is.na(raw$minimization_successful) &
               raw$minimization_successful == 1, , drop = FALSE]
  } else {
    pre_filtered <- TRUE
  }

  n_success <- nrow(raw)
  result <- tibble::as_tibble(raw)
  attr(result, "n_total") <- n_total
  attr(result, "n_success") <- n_success
  attr(result, "pre_filtered") <- pre_filtered
  result
}


# =============================================================================
# read_true_values() — Extraire valeurs vraies depuis un .ctl
# =============================================================================

#' Extrait les valeurs vraies (INIT) des parametres depuis un control stream.
#'
#' Parse les blocs $THETA, $OMEGA et $SIGMA pour extraire les valeurs initiales.
#' Gere les formats (lower, init, upper) et $OMEGA BLOCK.
#'
#' @param ctl_lines Character vector des lignes du .ctl
#' @return Named numeric vector : c(THETA1=x, OMEGA(1,1)=y, SIGMA(1,1)=z, ...)
#' @export
read_true_values <- function(ctl_lines) {
  if (is.null(ctl_lines) || length(ctl_lines) == 0L) {
    warning("Pas de lignes .ctl fournies")
    return(numeric(0L))
  }

  lines_clean <- sub(";.*$", "", ctl_lines)  # strip comments
  dollar_lines <- which(grepl("^\\s*\\$", lines_clean))

  # --- Helper: find block range ---
  .block_range <- function(keyword) {
    starts <- which(grepl(paste0("^\\s*\\$", keyword, "\\b"), lines_clean))
    if (length(starts) == 0L) return(list())
    ranges <- list()
    for (s in starts) {
      later <- dollar_lines[dollar_lines > s]
      end_idx <- if (length(later) > 0L) later[1L] - 1L else length(lines_clean)
      ranges <- c(ranges, list(c(s, end_idx)))
    }
    ranges
  }

  # --- Helper: extract ALL THETA init values from a single line ---
  # Handles multi-value lines: (0,0.15,1) (0,8.0,50) (0,1.0,5)
  # Also handles: standalone 0.15, mixed (0,0.15,1) 8.0, and FIX/FIXED suffixes
  .extract_theta_inits <- function(stripped) {
    vals <- numeric(0L)
    # 1) Extract all parenthesized groups: (lower,init,upper) or (init)
    paren_locs <- gregexpr("\\([^)]+\\)", stripped)[[1]]
    if (paren_locs[1] > 0) {
      paren_strs <- regmatches(stripped, list(paren_locs))[[1]]
      for (ps in paren_strs) {
        inner <- sub("^\\((.*)\\)$", "\\1", ps)
        nums <- as.numeric(trimws(strsplit(inner, ",")[[1]]))
        nums <- nums[!is.na(nums)]
        if (length(nums) == 3L)      vals <- c(vals, nums[2])
        else if (length(nums) == 2L) vals <- c(vals, nums[2])
        else if (length(nums) == 1L) vals <- c(vals, nums[1])
      }
      remaining <- gsub("\\([^)]+\\)", "", stripped)
    } else {
      remaining <- stripped
    }
    # 2) Extract bare numbers from remaining text (e.g. "0.15 FIX 8.0")
    remaining <- gsub("(?i)\\b(FIX(ED)?|SAME|UNINT)\\b", "", remaining)
    bare_locs <- gregexpr("-?[0-9]+\\.?[0-9]*([eEdD][+-]?[0-9]+)?", remaining)[[1]]
    if (bare_locs[1] > 0) {
      bare_strs <- regmatches(remaining, list(bare_locs))[[1]]
      for (bs in bare_strs) {
        val <- as.numeric(sub("[dD]", "e", bs))
        if (!is.na(val)) vals <- c(vals, val)
      }
    }
    vals
  }

  # --- Parse $THETA ---
  theta_vals <- numeric(0L)
  theta_names <- character(0L)
  theta_idx <- 0L
  for (rng in .block_range("THETA")) {
    for (i in rng[1]:rng[2]) {
      ln <- lines_clean[i]
      stripped <- sub("^\\s*\\$THETA\\s*", "", ln)
      stripped <- trimws(stripped)
      if (nchar(stripped) == 0L) next
      inits <- .extract_theta_inits(stripped)
      for (val in inits) {
        theta_idx <- theta_idx + 1L
        theta_vals <- c(theta_vals, val)
        theta_names <- c(theta_names, paste0("THETA", theta_idx))
      }
    }
  }

  # --- Parse $OMEGA ---
  omega_vals <- numeric(0L)
  omega_names <- character(0L)
  omega_row <- 0L  # global row counter across blocks

  for (rng in .block_range("OMEGA")) {
    first_line <- lines_clean[rng[1]]
    is_block <- grepl("BLOCK\\s*\\(", first_line, ignore.case = TRUE)

    if (is_block) {
      # BLOCK(n): lower-triangular values
      n <- as.integer(sub(".*BLOCK\\s*\\(\\s*(\\d+)\\s*\\).*", "\\1", first_line))
      all_nums <- numeric(0L)
      for (i in (rng[1] + 1L):rng[2]) {
        ln <- lines_clean[i]
        if (grepl("^\\s*$", ln)) next
        nums_raw <- regmatches(ln, gregexpr("-?[0-9.]+(?:[eEdD][+-]?[0-9]+)?", ln))[[1]]
        nums_raw <- nums_raw[!toupper(nums_raw) %in% c("FIX", "FIXED")]
        all_nums <- c(all_nums, as.numeric(gsub("[dD]", "E", nums_raw)))
      }
      # Lower-triangular: row 1 has 1 element, row 2 has 2, etc.
      idx <- 1L
      for (row in seq_len(n)) {
        for (col in seq_len(row)) {
          if (idx > length(all_nums)) break
          val <- all_nums[idx]
          r_global <- omega_row + row
          c_global <- omega_row + col
          omega_vals <- c(omega_vals, val)
          omega_names <- c(omega_names, paste0("OMEGA(", r_global, ",", c_global, ")"))
          idx <- idx + 1L
        }
      }
      omega_row <- omega_row + n
    } else {
      # Diagonal values (one per line)
      for (i in rng[1]:rng[2]) {
        ln <- lines_clean[i]
        stripped <- sub("^\\s*\\$OMEGA\\s*", "", ln)
        stripped <- trimws(stripped)
        if (nchar(stripped) == 0L) next
        # Handle bounded (lower, init, upper) or standalone
        if (grepl("\\(", stripped)) {
          inner <- sub("^\\(([^)]+)\\).*", "\\1", stripped)
          nums <- as.numeric(trimws(strsplit(inner, ",")[[1]]))
          nums <- nums[!is.na(nums)]
          val <- if (length(nums) >= 2L) nums[2] else if (length(nums) == 1L) nums[1] else next
        } else {
          val <- as.numeric(sub("^(-?[0-9.eEdD]+).*", "\\1", stripped))
          if (is.na(val)) next
        }
        omega_row <- omega_row + 1L
        omega_vals <- c(omega_vals, val)
        omega_names <- c(omega_names, paste0("OMEGA(", omega_row, ",", omega_row, ")"))
      }
    }
  }

  # --- Parse $SIGMA ---
  sigma_vals <- numeric(0L)
  sigma_names <- character(0L)
  sigma_row <- 0L

  for (rng in .block_range("SIGMA")) {
    first_line <- lines_clean[rng[1]]
    is_block <- grepl("BLOCK\\s*\\(", first_line, ignore.case = TRUE)

    if (is_block) {
      n <- as.integer(sub(".*BLOCK\\s*\\(\\s*(\\d+)\\s*\\).*", "\\1", first_line))
      all_nums <- numeric(0L)
      for (i in (rng[1] + 1L):rng[2]) {
        ln <- lines_clean[i]
        if (grepl("^\\s*$", ln)) next
        nums_raw <- regmatches(ln, gregexpr("-?[0-9.]+(?:[eEdD][+-]?[0-9]+)?", ln))[[1]]
        all_nums <- c(all_nums, as.numeric(gsub("[dD]", "E", nums_raw)))
      }
      idx <- 1L
      for (row in seq_len(n)) {
        for (col in seq_len(row)) {
          if (idx > length(all_nums)) break
          val <- all_nums[idx]
          r_global <- sigma_row + row
          c_global <- sigma_row + col
          sigma_vals <- c(sigma_vals, val)
          sigma_names <- c(sigma_names, paste0("SIGMA(", r_global, ",", c_global, ")"))
          idx <- idx + 1L
        }
      }
      sigma_row <- sigma_row + n
    } else {
      for (i in rng[1]:rng[2]) {
        ln <- lines_clean[i]
        stripped <- sub("^\\s*\\$SIGMA\\s*", "", ln)
        stripped <- trimws(stripped)
        if (nchar(stripped) == 0L) next
        if (grepl("\\(", stripped)) {
          inner <- sub("^\\(([^)]+)\\).*", "\\1", stripped)
          nums <- as.numeric(trimws(strsplit(inner, ",")[[1]]))
          nums <- nums[!is.na(nums)]
          val <- if (length(nums) >= 2L) nums[2] else if (length(nums) == 1L) nums[1] else next
        } else {
          val <- as.numeric(sub("^(-?[0-9.eEdD]+).*", "\\1", stripped))
          if (is.na(val)) next
        }
        sigma_row <- sigma_row + 1L
        sigma_vals <- c(sigma_vals, val)
        sigma_names <- c(sigma_names, paste0("SIGMA(", sigma_row, ",", sigma_row, ")"))
      }
    }
  }

  setNames(c(theta_vals, omega_vals, sigma_vals),
           c(theta_names, omega_names, sigma_names))
}


# =============================================================================
# compute_sse_metrics() — Calculer RSE/RMSE/biais empiriques par parametre
# =============================================================================

#' Calcule les metriques empiriques a partir des estimations SSE.
#'
#' @param sse_raw    Tibble de read_sse_raw() (colonnes normalisees)
#' @param true_values Named numeric vector de read_true_values()
#' @param param_labels Named character vector (optionnel) pour renommer les params
#'
#' @return Tibble : param, param_type, true_value, mean_estimate,
#'         rse_empirical, rmse_relative, relative_bias, n
#' @export
compute_sse_metrics <- function(sse_raw, true_values,
                                param_labels = NULL) {
  # Match SSE columns to true values
  available <- intersect(names(true_values), names(sse_raw))
  if (length(available) == 0L) {
    warning("Aucun parametre commun entre SSE et valeurs vraies")
    return(tibble(param = character(), param_type = character(),
                  true_value = numeric(), rse_empirical = numeric()))
  }

  results <- lapply(available, function(pname) {
    estimates <- as.numeric(sse_raw[[pname]])
    estimates <- estimates[!is.na(estimates)]
    true_val <- true_values[[pname]]

    if (length(estimates) < 2L || abs(true_val) < 1e-15) return(NULL)

    mean_est <- mean(estimates)
    sd_est <- sd(estimates)
    bias <- mean_est - true_val
    rse_emp <- 100 * sd_est / abs(true_val)
    rmse <- sqrt(mean((estimates - true_val)^2))
    rmse_rel <- 100 * rmse / abs(true_val)
    rel_bias <- 100 * bias / abs(true_val)

    # Display label
    display <- if (!is.null(param_labels) && pname %in% names(param_labels)) {
      param_labels[[pname]]
    } else {
      pname
    }

    # 95% CI of relative bias: RB +/- 1.96 * sd(REE) / sqrt(K)
    # where REE_k = (estimate_k - true) / true * 100
    ree <- (estimates - true_val) / true_val * 100
    se_rb <- sd(ree) / sqrt(length(ree))
    ci_lower <- rel_bias - 1.96 * se_rb
    ci_upper <- rel_bias + 1.96 * se_rb

    data.frame(
      param = pname,
      param_label = display,
      param_type = .param_type(pname),
      true_value = true_val,
      mean_estimate = round(mean_est, 6),
      rse_empirical = round(rse_emp, 2),
      rmse_relative = round(rmse_rel, 2),
      relative_bias = round(rel_bias, 2),
      rb_ci_lower = round(ci_lower, 2),
      rb_ci_upper = round(ci_upper, 2),
      n = length(estimates),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(results)
}


# =============================================================================
# compute_empirical_d_criterion() — D-criterion from SSE variance-covariance
# =============================================================================

#' Compute the empirical D-criterion from SSE parameter estimates.
#'
#' The empirical D-criterion is defined as det(VarCov)^(1/p) where VarCov is
#' the full empirical variance-covariance matrix of the estimated parameters
#' across K successful SSE runs, and p is the number of parameters.
#' (Fayette et al. 2026, Pharm Res)
#'
#' When the empirical variance-covariance matrix is ill-conditioned
#' (rcond < threshold), the D-criterion cannot be reliably estimated.
#' This was observed by Fayette et al. 2026 in the crossover example
#' with NONMEM-SAEM and NONMEM-FOCE.
#'
#' @param sse_raw    Tibble from read_sse_raw() (normalized column names)
#' @param true_values Named numeric vector from read_true_values()
#' @param rcond_threshold Minimum reciprocal condition number (default 1e-15)
#' @return List with components:
#'   d_criterion (numeric or NA), p (integer), rcond (numeric),
#'   ill_conditioned (logical), vcov (matrix)
#' @export
compute_empirical_d_criterion <- function(sse_raw, true_values,
                                          rcond_threshold = 1e-15) {
  available <- intersect(names(true_values), names(sse_raw))
  p <- length(available)

  result <- list(
    d_criterion = NA_real_,
    p = p,
    rcond = NA_real_,
    ill_conditioned = FALSE,
    vcov = NULL
  )

  if (p < 2L) return(result)

  # Build matrix of estimates (K rows x p columns)
  est_matrix <- as.matrix(sse_raw[, available, drop = FALSE])
  est_matrix <- est_matrix[complete.cases(est_matrix), , drop = FALSE]

  if (nrow(est_matrix) < p + 1L) return(result)

  vcov <- cov(est_matrix)
  result$vcov <- vcov

  rc <- rcond(vcov)
  result$rcond <- rc

  if (is.na(rc) || rc < rcond_threshold) {
    result$ill_conditioned <- TRUE
    return(result)
  }

  det_val <- det(vcov)
  if (is.na(det_val) || det_val <= 0) {
    result$ill_conditioned <- TRUE
    return(result)
  }

  result$d_criterion <- det_val^(1 / p)
  result
}


# =============================================================================
# compare_fim_sse() — Joindre metriques FIM et SSE
# =============================================================================

#' Compare les RSE predites par FIM avec les RSE empiriques SSE.
#'
#' @param sse_metrics Tibble de compute_sse_metrics()
#' @param fim_rse     Tibble de get_rse() (colonnes: param, rse_pct)
#' @param max_rse     Cap RSE pour eviter les outliers (defaut 200%)
#'
#' @return Tibble avec colonnes:
#'   param, param_type, rse_fim, rse_sse, rmse_sse, ratio, pass_20pct, status
#' @export
compare_fim_sse <- function(sse_metrics, fim_rse, max_rse = 200) {
  # Prepare FIM side
  fim_df <- fim_rse |>
    dplyr::select(param, rse_fim = rse_pct)

  # Prepare SSE side
  sse_df <- sse_metrics |>
    dplyr::select(param, param_type, param_label,
                  rse_sse = rse_empirical, rmse_sse = rmse_relative,
                  relative_bias, rb_ci_lower, rb_ci_upper)

  # Full join to keep all params

  comp <- dplyr::full_join(sse_df, fim_df, by = "param")

  # Fill param_type for FIM-only params
  comp$param_type <- dplyr::if_else(
    is.na(comp$param_type),
    .param_type(comp$param),
    comp$param_type
  )

  # Status column
  comp$status <- dplyr::case_when(
    !is.na(comp$rse_fim) & !is.na(comp$rse_sse) ~ "matched",
    !is.na(comp$rse_fim) & is.na(comp$rse_sse)  ~ "FIM only",
    is.na(comp$rse_fim) & !is.na(comp$rse_sse)  ~ "SSE only",
    TRUE ~ "unknown"
  )

  # Ratio and pass flag (only for matched)
  comp$ratio <- dplyr::if_else(
    comp$status == "matched" & abs(comp$rse_sse) > 1e-10,
    comp$rse_fim / comp$rse_sse,
    NA_real_
  )
  comp$pass_20pct <- !is.na(comp$ratio) & abs(comp$ratio - 1) <= 0.20

  # Cap extreme RSE values for plotting
  comp$rse_fim_capped <- pmin(comp$rse_fim, max_rse, na.rm = TRUE)
  comp$rse_sse_capped <- pmin(comp$rse_sse, max_rse, na.rm = TRUE)
  comp$rmse_sse_capped <- pmin(comp$rmse_sse, max_rse, na.rm = TRUE)

  comp
}


# =============================================================================
# plot_fim_vs_sse() — Scatter FIM RSE predite vs SSE RSE empirique
# =============================================================================

#' Scatter plot de validation FIM vs SSE.
#'
#' Reproduit l'esthetique de docs/results/plots/scatter_fim_vs_sse.png :
#' bande +/-20%, points RSE (ronds) et RMSE (triangles fades),
#' segments pointilles RSE->RMSE.
#'
#' @param comparison_df Tibble de compare_fim_sse()
#' @param title         Titre (NULL = automatique)
#' @return Objet ggplot2
#' @export
plot_fim_vs_sse <- function(comparison_df, title = NULL) {
  # Filter to matched params only, remove NA
  df <- comparison_df |>
    dplyr::filter(status == "matched",
                  !is.na(rse_fim_capped), !is.na(rse_sse_capped))

  if (nrow(df) == 0L) {
    return(ggplot() +
      labs(title = "No common parameters between FIM and SSE") +
      .theme_design())
  }

  # Simplify param_type for color legend
  df$type_group <- dplyr::case_when(
    grepl("^THETA", df$param_type)  ~ "Fixed effects",
    grepl("^OMEGA", df$param_type)  ~ "IIV",
    grepl("^SIGMA", df$param_type)  ~ "Residual",
    TRUE                            ~ df$param_type
  )

  col_fixed <- "#6C2B91"
  col_iiv   <- "#2B6991"
  col_resid <- "#E07B39"

  lim_max <- max(c(df$rse_fim_capped, df$rse_sse_capped), na.rm = TRUE) * 1.1
  lim <- c(0, lim_max)

  # Display label
  df$label <- dplyr::if_else(
    is.na(df$param_label) | df$param_label == df$param,
    df$param,
    df$param_label
  )

  ttl <- title %||% "FIM vs SSE Validation"
  n_pass <- sum(df$pass_20pct, na.rm = TRUE)
  n_total <- nrow(df)
  sub_txt <- sprintf(
    "Circles = RSE vs RSE | Triangles = RRMSE (includes bias) | +/-20%% band | %d/%d within band",
    n_pass, n_total
  )

  p <- ggplot(df, aes(x = rse_sse_capped, y = rse_fim_capped)) +
    # +/-20% band
    geom_ribbon(
      data = data.frame(x = seq(0, lim_max, length.out = 200)),
      aes(x = x, ymin = x * 0.8, ymax = x * 1.2, y = NULL),
      fill = col_fixed, alpha = 0.1, inherit.aes = FALSE
    ) +
    # Identity line
    geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "grey50") +
    # RMSE triangles (faded)
    geom_point(aes(x = rmse_sse_capped, color = type_group),
               size = 2.5, alpha = 0.3, shape = 17) +
    # Segments RSE -> RMSE (horizontal, showing bias impact)
    geom_segment(aes(x = rse_sse_capped, xend = rmse_sse_capped,
                     y = rse_fim_capped, yend = rse_fim_capped,
                     color = type_group),
                 alpha = 0.3, size = 0.5, linetype = "dotted") +
    # RSE points (main)
    geom_point(aes(color = type_group), size = 3.5) +
    # Labels
    geom_text(aes(label = label), nudge_y = lim_max * 0.03,
              size = 3, check_overlap = TRUE) +
    scale_color_manual(
      values = c("Fixed effects" = col_fixed,
                 "IIV" = col_iiv,
                 "Residual" = col_resid),
      name = NULL
    ) +
    coord_equal(xlim = lim, ylim = lim) +
    labs(
      title = ttl,
      subtitle = sub_txt,
      x = "Empirical SSE RSE (%)",
      y = "FIM predicted RSE (%)"
    ) +
    .theme_design() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  p
}


# =============================================================================
# compute_ree_distribution() — REE distribution for boxplot
# =============================================================================

#' Compute per-run REE values and summary quantiles for each parameter.
#'
#' @param sse_raw      Tibble from read_sse_raw()
#' @param true_values  Named numeric vector from read_true_values()
#' @param param_labels Named character vector (optional)
#' @return List with $individual (long tibble: param, param_type, ree) and
#'         $summary (tibble: param, param_type, p5, q25, median, q75, p95,
#'         rb, ci_lower, ci_upper)
#' @export
compute_ree_distribution <- function(sse_raw, true_values,
                                     param_labels = NULL) {
  available <- intersect(names(true_values), names(sse_raw))
  if (length(available) == 0L) {
    return(list(
      individual = tibble::tibble(param = character(), param_type = character(),
                                  param_label = character(), ree = numeric()),
      summary = tibble::tibble(param = character(), param_type = character(),
                               param_label = character(),
                               p5 = numeric(), q25 = numeric(),
                               median = numeric(), q75 = numeric(),
                               p95 = numeric(), rb = numeric(),
                               ci_lower = numeric(), ci_upper = numeric())
    ))
  }

  indiv_list <- list()
  summ_list  <- list()

  for (pname in available) {
    estimates <- as.numeric(sse_raw[[pname]])
    estimates <- estimates[!is.na(estimates)]
    true_val  <- true_values[[pname]]

    if (length(estimates) < 2L || abs(true_val) < 1e-15) next

    ree <- (estimates - true_val) / true_val * 100

    label <- if (!is.null(param_labels) && pname %in% names(param_labels)) {
      param_labels[[pname]]
    } else {
      pname
    }

    indiv_list[[pname]] <- data.frame(
      param = pname,
      param_type = .param_type(pname),
      param_label = label,
      ree = ree,
      stringsAsFactors = FALSE
    )

    qs <- quantile(ree, probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
                   names = FALSE)
    rb <- mean(ree)
    se_rb <- sd(ree) / sqrt(length(ree))

    summ_list[[pname]] <- data.frame(
      param = pname,
      param_type = .param_type(pname),
      param_label = label,
      p5 = qs[1], q25 = qs[2], median = qs[3], q75 = qs[4], p95 = qs[5],
      rb = round(rb, 2),
      ci_lower = round(rb - 1.96 * se_rb, 2),
      ci_upper = round(rb + 1.96 * se_rb, 2),
      stringsAsFactors = FALSE
    )
  }

  list(
    individual = tibble::as_tibble(dplyr::bind_rows(indiv_list)),
    summary    = tibble::as_tibble(dplyr::bind_rows(summ_list))
  )
}


# =============================================================================
# plot_ree_boxplot() — REE distribution boxplot per parameter
# =============================================================================

#' Boxplot of Relative Estimation Error (REE) per parameter.
#'
#' Uses pre-computed quantiles (5th/95th as whiskers), shows Relative Bias
#' as a black diamond with 95% CI error bar.
#' Inspired by Fayette et al. 2026, Fig. 2.
#'
#' @param ree_dist List from compute_ree_distribution()
#' @param title    Plot title (NULL = auto)
#' @return ggplot2 object
#' @export
plot_ree_boxplot <- function(ree_dist, title = NULL) {
  summ <- ree_dist$summary
  if (is.null(summ) || nrow(summ) == 0L) {
    return(ggplot() +
      labs(title = "No REE data available") +
      .theme_design())
  }

  # Map type for colors
  summ$type_group <- dplyr::case_when(
    grepl("^THETA", summ$param_type) ~ "Fixed effects",
    grepl("^OMEGA", summ$param_type) ~ "IIV",
    grepl("^SIGMA", summ$param_type) ~ "Residual",
    TRUE ~ summ$param_type
  )

  col_fixed <- "#6C2B91"
  col_iiv   <- "#2B6991"
  col_resid <- "#E07B39"

  # Order params: THETA, OMEGA, SIGMA
  summ$param_label <- factor(summ$param_label, levels = summ$param_label)

  ttl <- title %||% "REE Distribution by Parameter"

  p <- ggplot(summ, aes(x = param_label)) +
    # Reference line at 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    # Boxplot with pre-computed quantiles
    geom_boxplot(
      aes(ymin = p5, lower = q25, middle = median,
          upper = q75, ymax = p95, fill = type_group),
      stat = "identity", width = 0.6, alpha = 0.7,
      color = "grey30", size = 0.4
    ) +
    # Relative Bias as black diamond
    geom_point(aes(y = rb), shape = 18, size = 3, color = "black") +
    # 95% CI error bar for bias
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2, size = 0.5, color = "black") +
    scale_fill_manual(
      values = c("Fixed effects" = col_fixed,
                 "IIV" = col_iiv,
                 "Residual" = col_resid),
      name = NULL
    ) +
    labs(
      title = ttl,
      subtitle = paste0(
        "Boxes = 25th-75th pct | Whiskers = 5th-95th pct | ",
        "Diamond = Relative Bias | Error bar = 95% CI of bias\n",
        "Pantaleo (2026) thresholds: |RBias| < 20% and NRMSE < 20%"
      ),
      x = NULL,
      y = "REE (%)"
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
# plot_rse_bar() — Grouped bar chart FIM RSE vs Empirical RSE
# =============================================================================

#' Grouped bar chart comparing FIM-predicted RSE with empirical SSE RSE.
#'
#' Inspired by Fayette et al. 2026, Fig. 3.
#'
#' @param comparison_df Tibble from compare_fim_sse()
#' @param title         Plot title (NULL = auto)
#' @return ggplot2 object
#' @export
plot_rse_bar <- function(comparison_df, title = NULL) {
  df <- comparison_df |>
    dplyr::filter(status == "matched",
                  !is.na(rse_fim), !is.na(rse_sse))

  if (nrow(df) == 0L) {
    return(ggplot() +
      labs(title = "No matched parameters for RSE comparison") +
      .theme_design())
  }

  # Prepare long format for grouped bars
  df$param_label <- factor(df$param_label, levels = df$param_label)

  df_long <- tidyr::pivot_longer(
    df,
    cols = c(rse_fim, rse_sse),
    names_to = "source",
    values_to = "rse"
  )
  df_long$source <- dplyr::if_else(
    df_long$source == "rse_fim",
    "FIM predicted",
    "SSE empirical"
  )

  ttl <- title %||% "FIM vs SSE: RSE Comparison"

  p <- ggplot(df_long, aes(x = param_label, y = rse, fill = source)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    # Reference lines
    geom_hline(yintercept = 20, linetype = "dashed", color = "#16a34a",
               size = 0.4, alpha = 0.7) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "#d97706",
               size = 0.4, alpha = 0.7) +
    scale_fill_manual(
      values = c("FIM predicted" = "#4682B4",
                 "SSE empirical" = "#CD5C5C"),
      name = NULL
    ) +
    labs(
      title = ttl,
      subtitle = "Dashed lines at 20% (good) and 50% (acceptable)",
      x = NULL,
      y = "RSE (%)"
    ) +
    .theme_design() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 8)
    )

  p
}
