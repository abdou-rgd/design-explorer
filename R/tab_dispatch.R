# =============================================================================
# tab_dispatch.R
# .tab pattern detection + smooth-curve engine selection + renderer dispatch
#
# Patterns: elementary_fo, focei_repl, robust_subprob, pkpd_multi,
#           dose_time_opt, classical, stratified, discrete, unknown
# =============================================================================

library(dplyr)


#' Detect the NONMEM $DESIGN .tab shape pattern.
#'
#' Heuristic — no .ctl required, but .ctl strengthens.
#'
#' @param tab tibble from read_tab()
#' @param ctl_lines optional character vector from read_lines(.ctl)
#' @return one of: "elementary_fo", "focei_repl", "robust_subprob",
#'   "pkpd_multi", "dose_time_opt", "classical", "stratified",
#'   "discrete", "unknown"
#' @export
detect_tab_pattern <- function(tab, ctl_lines = NULL) {
  if (is.null(tab) || nrow(tab) == 0L) return("unknown")

  cols <- names(tab)
  ctl_text <- if (is.null(ctl_lines)) "" else paste(ctl_lines, collapse = "\n")
  ctl_text <- toupper(gsub(";.*$", "", ctl_text))

  n_blocks <- if ("table_no" %in% cols) dplyr::n_distinct(tab$table_no) else 1L
  if (n_blocks > 1L) return("robust_subprob")

  has_discrete_ctl <- grepl("\\b(DISCRETE|NMIN|NMAX)\\b", ctl_text)
  has_discrete_cols <- any(c("NMIN", "NMAX") %in% toupper(cols))
  if (has_discrete_ctl || has_discrete_cols) return("discrete")

  has_cmt <- "CMT" %in% cols && dplyr::n_distinct(tab$CMT) > 1L

  has_amt_varying <- FALSE
  if ("AMTSTRAT" %in% cols) {
    has_amt_varying <- TRUE
  } else if ("AMT" %in% cols || "DOSE" %in% cols) {
    amt_col <- if ("AMT" %in% cols) tab$AMT else tab$DOSE
    has_amt_varying <- dplyr::n_distinct(amt_col[amt_col > 0]) > 1L
  }

  if (has_amt_varying) return("dose_time_opt")
  if (has_cmt) return("pkpd_multi")

  has_strat_ctl <- grepl("\\b(STRAT|STRATF)\\b", ctl_text)
  has_strat_cols <- any(c("STRAT", "STRATF") %in% toupper(cols))
  if (has_strat_ctl || has_strat_cols) return("stratified")

  n_ids  <- if ("ID" %in% cols) dplyr::n_distinct(tab$ID) else 1L
  has_ts <- "TSTRAT" %in% cols

  if (has_ts && n_ids >  4L) return("focei_repl")
  if (has_ts)               return("elementary_fo")
  if (n_ids >  4L)          return("classical")

  "unknown"
}


#' Select the smooth-curve engine tier for the given .tab + .ctl.
#'
#' Tier 1 (green):  mrgsolve — if mrgsolve_sim is non-null
#' Tier 2 (yellow): closed-form template — if ADVAN parse + THETAs map cleanly
#' Tier 3 (red):    dot-connect — graceful floor
#'
#' @param tab             tibble from read_tab
#' @param ctl_lines       character vector of .ctl lines (may be NULL)
#' @param theta_values    named numeric vector; names matching template params
#' @param mrgsolve_sim    optional tibble returned by mrgsolve simulation
#' @param mrgsolve_available logical; hint from mod_mrgsolve
#' @return list(tier, sim_data, warning)
#' @export
pick_smooth_curve_engine <- function(tab, ctl_lines = NULL,
                                     theta_values = numeric(),
                                     mrgsolve_sim = NULL,
                                     mrgsolve_available = FALSE) {

  if (!is.null(mrgsolve_sim) && nrow(mrgsolve_sim) > 0L) {
    return(list(tier = "mrgsolve", sim_data = mrgsolve_sim, warning = NULL))
  }

  advan_trans <- tryCatch(parse_advan_trans(ctl_lines), error = function(e) NULL)
  tmpl <- pk_template_for_advan(advan_trans)

  if (!is.null(tmpl)) {
    theta_values <- .match_template_theta_values(theta_values, tmpl$required)
    missing_params <- setdiff(tmpl$required, names(theta_values))
    if (length(missing_params) == 0L) {
      sim <- .simulate_template(tab, tmpl, theta_values)
      if (!is.null(sim) && nrow(sim) > 0L) {
        warn <- if (mrgsolve_available)
          "mrgsolve available but no sim result — using closed-form template"
        else NULL
        return(list(tier = "template", sim_data = sim, warning = warn))
      }
    }
  }

  list(tier = "dots", sim_data = NULL,
       warning = "Smooth curve unavailable — showing IPRED points only")
}

.match_template_theta_values <- function(theta_values, required) {
  if (length(theta_values) == 0L || is.null(names(theta_values))) {
    return(theta_values)
  }

  norm <- function(x) toupper(gsub("[^A-Za-z0-9]", "", x))
  aliases <- list(
    CL = c("CL", "CLEARANCE"),
    V  = c("V", "VC", "V1", "V2", "CENTRALVOLUME", "VOLUME"),
    V2 = c("V2", "VC", "V", "CENTRALVOLUME"),
    Q  = c("Q", "Q2", "INTERCOMPARTMENTALCLEARANCE"),
    V3 = c("V3", "VP", "PERIPHERALVOLUME"),
    KA = c("KA", "KABS", "ABSORPTION")
  )

  out <- stats::setNames(rep(NA_real_, length(required)), required)
  src_names <- names(theta_values)
  src_norm <- norm(src_names)

  for (req in required) {
    candidates <- norm(c(req, aliases[[req]] %||% character()))
    idx <- match(TRUE, src_norm %in% candidates)
    if (!is.na(idx)) out[req] <- unname(theta_values[idx])
  }

  out[is.finite(out)]
}

#' Extract dose times from a NONMEM .tab-like data frame.
#'
#' @param tab data frame containing TIME plus EVID/AMT/DOSE when available
#' @return numeric vector of unique dose times, or NULL when none are detected
#' @export
extract_tab_dose_times <- function(tab) {
  if (is.null(tab) || nrow(tab) == 0L || !"TIME" %in% names(tab)) return(NULL)

  dose_rows <- tab[FALSE, , drop = FALSE]
  if ("EVID" %in% names(tab)) {
    dose_rows <- tab[!is.na(tab$EVID) & tab$EVID == 1L, , drop = FALSE]
  }
  if (nrow(dose_rows) == 0L && "AMT" %in% names(tab)) {
    dose_rows <- tab[!is.na(tab$AMT) & tab$AMT > 0, , drop = FALSE]
  }
  if (nrow(dose_rows) == 0L && "DOSE" %in% names(tab)) {
    dose_rows <- tab[!is.na(tab$DOSE) & tab$DOSE > 0, , drop = FALSE]
  }
  if (nrow(dose_rows) == 0L) return(NULL)

  out <- sort(unique(dose_rows$TIME[is.finite(dose_rows$TIME)]))
  if (length(out) == 0L) NULL else out
}


# Internal: simulate the template at a dense grid spanning the tab's TIME range.
.simulate_template <- function(tab, tmpl, theta_values) {
  if (is.null(tab) || !"TIME" %in% names(tab)) return(NULL)
  t_max <- max(tab$TIME, na.rm = TRUE)
  if (!is.finite(t_max) || t_max <= 0) return(NULL)
  times <- seq(0, t_max, length.out = 300L)

  dose_val <- 100
  dose_t   <- extract_tab_dose_times(tab) %||% 0
  if (nrow(tab) > 0L) {
    if ("EVID" %in% names(tab)) {
      dose_rows <- tab[!is.na(tab$EVID) & tab$EVID == 1L, , drop = FALSE]
    } else {
      dose_rows <- tab[FALSE, , drop = FALSE]
    }
    if (nrow(dose_rows) == 0L && "AMT" %in% names(tab)) {
      dose_rows <- tab[!is.na(tab$AMT) & tab$AMT > 0, , drop = FALSE]
    }
    if (nrow(dose_rows) == 0L && "DOSE" %in% names(tab)) {
      dose_rows <- tab[!is.na(tab$DOSE) & tab$DOSE > 0, , drop = FALSE]
    }
    if (nrow(dose_rows) > 0L) {
      amt_col <- intersect(c("AMT", "DOSE"), names(dose_rows))[1L]
      if (!is.na(amt_col)) {
        amt_vals <- dose_rows[[amt_col]]
        amt_vals <- amt_vals[!is.na(amt_vals) & amt_vals > 0]
        if (length(amt_vals) > 0L) dose_val <- amt_vals[1L]
      }
    }
  }

  args <- list(times = times, dose = dose_val, dose_times = dose_t)
  for (p in tmpl$required) args[[p]] <- unname(theta_values[p])

  ipred <- tryCatch(do.call(tmpl$fn, args), error = function(e) NULL)
  if (is.null(ipred)) return(NULL)

  tibble::tibble(time = times, IPRED = ipred, cmt = 1L, arm = 1L)
}


# =============================================================================
# Renderers dispatched by mod_times
# =============================================================================

#' Render an explanatory empty-state plot for deferred patterns.
#'
#' @param pattern one of "classical","stratified","discrete","unknown"
#' @return ggplot
#' @export
render_empty_state <- function(pattern) {
  msg <- switch(pattern,
    classical  = paste("Pattern detected: classical population design",
                       "(one ID per subject, no TSTRAT).",
                       "Timeline rendering for this pattern is not implemented yet.",
                       "Showing raw optimal-times table below.", sep = "\n"),
    stratified = paste("Pattern detected: stratified design (STRAT/STRATF).",
                       "Timeline rendering not implemented — see raw table below.",
                       sep = "\n"),
    discrete   = paste("Pattern detected: DISCRETE design (NMIN/NMAX, MDV toggles).",
                       "Timeline rendering not implemented — see raw table below.",
                       sep = "\n"),
    paste("Unable to classify this .tab shape.",
          "Showing raw optimal-times table below.", sep = "\n")
  )
  .empty_plot(msg)
}

#' Fallback plot: connect IPRED points with dashed line (tier 3).
#' Thin wrapper around existing plot_model_prediction().
#' @export
render_fallback_dot_plot <- function(tab, time_unit = "hours",
                                     arm_labels = NULL, cmt_labels = NULL) {
  plot_model_prediction(
    tab_data    = tab,
    time_unit   = time_unit,
    arm_labels  = arm_labels,
    cmt_labels  = cmt_labels,
    show_doses  = TRUE
  )
}

#' Render the smooth PK timeline (tier 1 or tier 2) with overlays.
#'
#' @param smooth  list from pick_smooth_curve_engine() (must not be tier "dots")
#' @param obs_points tab (already filtered to obs) for Optimise markers
#' @param compare_points optional 2nd-run tab for CTP markers
#' @param dose_times numeric vector or data.frame with $TIME
#' @param time_unit "hours" | "days"
#' @param facet "route" | "cmt" | "id" | "auto" | "none"
#' @param arm_labels named character vector
#' @param cmt_labels named character vector
#' @param xlim optional c(xmin, xmax) for brush-zoom
#' @return ggplot
#' @export
render_pk_timeline <- function(smooth, obs_points, compare_points = NULL,
                               dose_times = NULL, time_unit = "hours",
                               facet = "auto", arm_labels = NULL,
                               cmt_labels = NULL, xlim = NULL) {

  # Delegate to existing plot_pk_profile for the heavy lifting — it already
  # does facet, rug, CTP overlay, sec.axis, dose markers.
  p <- plot_pk_profile(
    sim_data       = smooth$sim_data,
    obs_points     = obs_points,
    dose_times     = if (is.data.frame(dose_times)) dose_times$TIME else dose_times,
    time_unit      = time_unit,
    cmt_labels     = cmt_labels,
    arm_labels     = arm_labels,
    compare_points = compare_points
  )

  if (!is.null(xlim) && length(xlim) == 2L && all(is.finite(xlim))) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }

  p
}
