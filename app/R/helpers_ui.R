# =============================================================================
# helpers_ui.R — Composants UI réutilisables V4
# =============================================================================

# -- Seuils qualite RSE / RELATIVEINF (utilises par rse_badge / ri_badge) ---
RSE_THRESHOLDS    <- c(20, 50, 100) # <20% bon, 20-50% modere, 50-100% mauvais, >100% tres mauvais
RELINF_THRESHOLDS <- c(20, 50)   # >=50% bon, 20-50% modere, <20% mauvais

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

# -- RSE badge ---------------------------------------------------------------
rse_badge <- function(x) {
  if (is.na(x)) return(span("\u2014", class = "ri-na"))
  cls <- if (x < RSE_THRESHOLDS[1]) "rse-good" else if (x < RSE_THRESHOLDS[2]) "rse-moderate" else if (x < RSE_THRESHOLDS[3]) "rse-poor" else "rse-very-poor"
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

# -- Run color palette (max 4 runs) — keyed by rid, not by editable name ----
.RUN_COLORS <- c(
  "primary" = "#2563eb",
  "run_1"   = "#dc2626",
  "run_2"   = "#16a34a",
  "run_3"   = "#d97706",
  "run_4"   = "#7c3aed",
  "run_5"   = "#db2777"
)

run_color <- function(rid) {
  .RUN_COLORS[rid] %||% "#6b7280"
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
                          border-radius:50%%;background:%s;margin-right:5px;
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

# -- Guide banner with optional step navigation ----------------------------
render_guide_banner <- function(ns, guide, paths,
                                step_idx = 1L, n_steps = 0L,
                                step_label = NULL) {
  if (is.null(guide)) return(NULL)

  # Read .ctl source for display
  ctl_content <- NULL
  if (!is.null(paths$ext)) {
    base <- tools::file_path_sans_ext(basename(paths$ext))
    ctl_path <- file.path(dirname(paths$ext), paste0(base, ".ctl"))
    if (file.exists(ctl_path)) {
      ctl_content <- paste(readLines(ctl_path, warn = FALSE),
                           collapse = "\n")
    }
  }

  # Step navigation bar (only for multi-step examples)
  has_steps <- isTRUE(n_steps > 1L)
  step_nav <- if (has_steps) {
    lbl <- if (!is.null(step_label)) {
      step_label
    } else {
      paste("Etape", step_idx, "/", n_steps)
    }
    prev_disabled <- step_idx <= 1L
    next_disabled <- step_idx >= n_steps
    tags$div(
      class = "guide-step-nav",
      tags$span(class = "guide-step-label", lbl),
      actionButton(
        ns("step_prev"), "Precedent",
        class = "btn-sm btn-default",
        disabled = if (prev_disabled) NA else NULL
      ),
      actionButton(
        ns("step_next"), "Suivant",
        class = "btn-sm btn-default",
        disabled = if (next_disabled) NA else NULL
      )
    )
  }

  tags$div(
    class = "guide-banner",
    actionButton(ns("dismiss_banner"), NULL,
                 class = "dismiss-btn",
                 style = "background:none; border:none;",
                 icon = NULL,
                 tags$span("\u00d7")),
    step_nav,
    tags$h6(guide$context),
    if (!is.null(ctl_content)) tags$code(ctl_content),
    tags$ul(lapply(guide$points, tags$li))
  )
}
