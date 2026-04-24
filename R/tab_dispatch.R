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

  n_blocks <- if ("table_no" %in% cols) dplyr::n_distinct(tab$table_no) else 1L
  if (n_blocks > 1L) return("robust_subprob")

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

  n_ids  <- if ("ID" %in% cols) dplyr::n_distinct(tab$ID) else 1L
  has_ts <- "TSTRAT" %in% cols

  if (has_ts && n_ids >  4L) return("focei_repl")
  if (has_ts)               return("elementary_fo")
  if (n_ids >  4L)          return("classical")

  "unknown"
}
