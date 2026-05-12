# =============================================================================
# source_core.R -- shared source loader for scripts, tests, and Shiny app
# =============================================================================

source_core <- function(root = ".", local = parent.frame()) {
  core_files <- c(
    "design_utils.R",
    "design_io.R",
    "design_metrics.R",
    "design_summary.R",
    "ctl_parsers.R",
    "report_design.R",
    "fim_metrics.R",
    "sse_metrics.R",
    "sse_diagnostics.R",
    "sse_individual_pk.R",
    "sse_comparison.R",
    "mrgsolve_bridge.R",
    "pk_templates.R",
    "tab_dispatch.R"
  )

  invisible(lapply(file.path(root, "R", core_files), source, local = local))
}
