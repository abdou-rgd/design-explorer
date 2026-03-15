# =============================================================================
# helpers_ui.R — Composants UI réutilisables
# =============================================================================

metric_card <- function(label, value, sub = NULL, color = "blue") {
  div(
    class = paste("metric-card", color),
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    if (!is.null(sub)) div(class = "metric-sub", sub)
  )
}

rse_badge <- function(x) {
  if (is.na(x)) return(span("\u2014", class = "ri-na"))
  cls <- if (x < 20) "rse-good" else if (x < 50) "rse-moderate" else "rse-poor"
  span(sprintf("%.2f%%", x), class = cls)
}

ri_badge <- function(x) {
  if (is.na(x)) return(span("\u2014", class = "ri-na"))
  cls <- if (x >= 50) "ri-good" else if (x >= 20) "ri-moderate" else "ri-poor"
  span(sprintf("%.2f%%", x), class = cls)
}

param_type_badge <- function(param) {
  if (str_starts(param, "THETA")) return(span("THETA", class = "badge-theta"))
  if (str_detect(param, "^OMEGA")) return(span("OMEGA", class = "badge-omega"))
  if (str_detect(param, "^SIGMA")) return(span("SIGMA", class = "badge-sigma"))
  span(param)
}

# Multi-run color palette (max 4 runs)
.RUN_COLORS <- c(
  "Run A" = "#2563eb",
  "Run B" = "#dc2626",
  "Run C" = "#16a34a",
  "Run D" = "#d97706"
)

run_color <- function(run_name) {
  .RUN_COLORS[run_name] %||% "#6b7280"
}

detect_criterion <- function(lines) {
  hdr <- lines[str_detect(lines, "TABLE NO\\..*OPTIMALITY|DESIGN")][1]
  if (is.na(hdr)) return("D-OPTIMALITY")
  if (str_detect(hdr, "A-OPT"))  return("A-OPTIMALITY")
  if (str_detect(hdr, "DS-OPT")) return("DS-OPTIMALITY")
  if (str_detect(hdr, "R-OPT"))  return("R-OPTIMALITY")
  "D-OPTIMALITY"
}
