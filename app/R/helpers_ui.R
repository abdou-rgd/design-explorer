# =============================================================================
# helpers_ui.R — Composants UI réutilisables V4
# =============================================================================

# -- Polices Google Fonts (injectées une fois dans app.R) -------------------
google_fonts_link <- function() {
  tags$link(
    rel  = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700;800&family=JetBrains+Mono:wght@400;600;700&display=swap"
  )
}

# -- Metric card -------------------------------------------------------------
metric_card <- function(label, value, sub = NULL, color = "blue") {
  div(
    class = paste("metric-card", color),
    div(class = "metric-label", label),
    div(class = "metric-value",  value),
    if (!is.null(sub)) div(class = "metric-sub", sub)
  )
}

# -- KPI bar (rendu dans app.R, pas dans les modules) -----------------------
kpi_bar_ui <- function(id) {
  div(id = "kpi-bar", uiOutput(id))
}

render_kpi_bar <- function(run_name, file_name, d_crit, rse_mean,
                            n_poor, n_total) {
  alert_txt <- if (n_poor > 0) {
    sprintf("%d param%s RSE > 20%%", n_poor, if (n_poor > 1) "s" else "")
  } else NULL

  tagList(
    div(class = "kpi-run",
      div(class = "kpi-dot"),
      run_name,
      tags$span(style = "font-weight:400; color:#64748b; font-size:.76rem;",
                file_name)
    ),
    span(class = "kpi-sep", "|"),
    div(class = "kpi-metric", tags$strong("D-crit"), round(d_crit, 3)),
    span(class = "kpi-sep", "|"),
    div(class = "kpi-metric",
        tags$strong("RSE moy"),
        sprintf("%.1f%%", rse_mean)),
    span(class = "kpi-sep", "|"),
    div(class = "kpi-metric",
        tags$strong(sprintf("%d/%d", n_poor, n_total)),
        "> 20%"),
    if (!is.null(alert_txt))
      div(class = "kpi-alert", alert_txt)
  )
}

# -- RSE badge ---------------------------------------------------------------
rse_badge <- function(x) {
  if (is.na(x)) return(span("\u2014", class = "ri-na"))
  cls <- if (x < RSE_THRESHOLDS[1]) "rse-good" else if (x < RSE_THRESHOLDS[2]) "rse-moderate" else "rse-poor"
  span(sprintf("%.2f%%", x), class = cls)
}

# -- RelInf badge ------------------------------------------------------------
ri_badge <- function(x) {
  if (is.na(x)) return(span("\u2014", class = "ri-na"))
  cls <- if (x >= RELINF_THRESHOLDS[2]) "ri-good" else if (x >= RELINF_THRESHOLDS[1]) "ri-moderate" else "ri-poor"
  span(sprintf("%.2f%%", x), class = cls)
}

# -- Param type badge --------------------------------------------------------
param_type_badge <- function(param) {
  if (str_starts(param, "THETA")) return(span("THETA", class = "badge-theta"))
  if (str_detect(param, "^OMEGA")) return(span("OMEGA", class = "badge-omega"))
  if (str_detect(param, "^SIGMA")) return(span("SIGMA", class = "badge-sigma"))
  span(param)
}

# -- Run color palette (max 4 runs) -----------------------------------------
.RUN_COLORS <- c(
  "Run A" = "#2563eb",
  "Run B" = "#dc2626",
  "Run C" = "#16a34a",
  "Run D" = "#d97706"
)

run_color <- function(run_name) {
  .RUN_COLORS[run_name] %||% "#6b7280"
}

# -- Run pill (header/drawer) -----------------------------------------------
run_pill <- function(name, color) {
  span(
    style = sprintf(
      "background:%s22; color:%s; border:1px solid %s55;
       padding:2px 10px; border-radius:999px; font-size:.76rem; font-weight:600;",
      color, color, color
    ),
    span(style = sprintf("display:inline-block;width:7px;height:7px;
                          border-radius:50%;background:%s;margin-right:5px;
                          vertical-align:middle;", color)),
    name
  )
}

# -- Detect optimality criterion from .ext header lines --------------------
detect_criterion <- function(lines) {
  hdr <- lines[str_detect(lines, "TABLE NO\\..*OPTIMALITY|DESIGN")][1]
  if (is.na(hdr)) return("D-OPTIMALITY")
  if (str_detect(hdr, "A-OPT"))  return("A-OPTIMALITY")
  if (str_detect(hdr, "DS-OPT")) return("DS-OPTIMALITY")
  if (str_detect(hdr, "R-OPT"))  return("R-OPTIMALITY")
  "D-OPTIMALITY"
}
