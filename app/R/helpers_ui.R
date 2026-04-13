# =============================================================================
# helpers_ui.R — Composants UI réutilisables V4
# =============================================================================

# -- Extraction fichiers NONMEM (tar.gz ou fichiers multiples) ----------------
# Shared by mod_upload and mod_compare to avoid duplicated logic.
# Returns named list: list(ext=, shk=, coi=, clt=, tab=, ctl=, bfm=, cpu=)
extract_design_files <- function(files, tmp_prefix = "design") {
  exts <- c("ext", "shk", "coi", "clt", "tab", "bfm", "cpu")
  paths <- stats::setNames(
    vector("list", length(exts) + 1L),
    c(exts, "ctl")
  )

  if (nrow(files) == 1L &&
      grepl("\\.(tar\\.gz|tgz)$", files$name, ignore.case = TRUE)) {
    tmp <- file.path(tempdir(),
                     paste0(tmp_prefix, "_", format(Sys.time(), "%H%M%S")))
    dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
    untar(files$datapath, exdir = tmp)

    all_f <- list.files(tmp, recursive = TRUE, full.names = TRUE)
    all_n <- basename(all_f)

    for (et in exts) {
      idx <- which(grepl(paste0("\\.", et, "$"), all_n, ignore.case = TRUE))[1]
      if (!is.na(idx)) paths[[et]] <- all_f[idx]
    }
    ctl_idx <- which(grepl("\\.(ctl|mod|con)$", all_n, ignore.case = TRUE))[1]
    if (!is.na(ctl_idx)) paths$ctl <- all_f[ctl_idx]
  } else {
    for (i in seq_len(nrow(files))) {
      nm <- files$name[i]
      dp <- files$datapath[i]
      for (et in exts) {
        if (grepl(paste0("\\.", et, "$"), nm, ignore.case = TRUE))
          paths[[et]] <- dp
      }
      if (grepl("\\.(ctl|mod|con)$", nm, ignore.case = TRUE))
        paths$ctl <- dp
    }
  }
  paths
}

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

# -- Run color palette (max 6 runs actifs, run_counter peut depasser 5) ------
.RUN_COLORS <- c(
  "primary" = "#2563eb",
  "run_1"   = "#dc2626",
  "run_2"   = "#16a34a",
  "run_3"   = "#d97706",
  "run_4"   = "#7c3aed",
  "run_5"   = "#db2777",
  "run_6"   = "#0891b2",
  "run_7"   = "#65a30d",
  "run_8"   = "#c2410c",
  "run_9"   = "#7c2d12"
)

run_color <- function(rid) {
  .RUN_COLORS[rid] %||% "#6b7280"
}

# -- Run pill (multi-run comparison) ----------------------------------------
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

# -- Detect estimation method from .ext header lines -----------------------
detect_method <- function(lines) {
  hdr <- lines[str_detect(lines, "^TABLE NO\\.")][1]
  if (is.na(hdr)) return(list(method = "Unknown", mode = "Evaluation"))

  # Mode: Evaluation vs Optimization
  mode <- if (str_detect(hdr, "\\(Evaluation\\)")) "Evaluation" else "Optimization"

  # Method abbreviation
  method <- if (str_detect(hdr, "Conditional.*Interaction")) {
    "FOCEI"
  } else if (str_detect(hdr, "Conditional")) {
    "FOCE"
  } else if (str_detect(hdr, "Laplace")) {
    "LAPLACE"
  } else if (str_detect(hdr, "First Order")) {
    "FO"
  } else {
    "FO"
  }

  list(method = method, mode = mode)
}

# =============================================================================
# V5 UI Components — PopkinR-inspired
# =============================================================================

# -- Settings bar (horizontal strip of controls at top of a module) ----------
settings_bar <- function(...) {
  tags$div(
    class = "settings-bar",
    ...
  )
}

# -- Plot export UI (below plotOutput) ----------------------------------------
plot_export_ui <- function(ns, id, default_fname = "plot") {
  tags$div(
    class = "plot-export-bar",
    selectInput(
      ns(paste0(id, "_format")), NULL,
      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
      width = "80px"
    ),
    numericInput(ns(paste0(id, "_width")),  "Width (in)",  value = 10, min = 2, max = 30, step = 1, width = "90px"),
    numericInput(ns(paste0(id, "_height")), "Height (in)", value = 6,  min = 2, max = 20, step = 1, width = "90px"),
    textInput(ns(paste0(id, "_fname")), NULL, value = default_fname, width = "140px"),
    downloadButton(ns(paste0(id, "_dl")), "Export", class = "btn btn-sm btn-default")
  )
}

# -- Plot export server (factory for downloadHandler + ggsave) ----------------
plot_export_server <- function(input, output, session, id, plot_fn) {
  output[[paste0(id, "_dl")]] <- downloadHandler(
    filename = function() {
      fmt   <- input[[paste0(id, "_format")]] %||% "png"
      fname <- input[[paste0(id, "_fname")]]  %||% "plot"
      paste0(fname, ".", fmt)
    },
    content = function(file) {
      p <- plot_fn()
      req(p)
      fmt <- input[[paste0(id, "_format")]] %||% "png"
      w   <- input[[paste0(id, "_width")]]  %||% 10
      h   <- input[[paste0(id, "_height")]] %||% 6
      ggplot2::ggsave(file, plot = p, device = fmt,
                      width = w, height = h, dpi = 300)
    }
  )
}

# -- Metric card V5 (PopkinR style: icon block + label/value horizontal) ------
metric_card_v5 <- function(label, value, icon_name = "chart-bar",
                           color = "#2563eb", sub = NULL) {
  tags$div(
    class = "metric-card-v5",
    tags$div(
      class = "metric-icon-block",
      style = sprintf("background:%s22; color:%s;", color, color),
      icon(icon_name)
    ),
    tags$div(
      class = "metric-body",
      tags$div(class = "metric-v5-label", label),
      tags$div(class = "metric-v5-value", value),
      if (!is.null(sub)) tags$div(class = "metric-v5-sub", sub)
    )
  )
}

# -- Section header (consistent section title) --------------------------------
section_header <- function(title, subtitle = NULL) {
  tags$div(
    class = "section-header-v5",
    tags$h4(class = "section-title", title),
    if (!is.null(subtitle)) tags$span(class = "section-subtitle", subtitle)
  )
}

# -- Popkin-style tabs (underline style tabsetPanel wrapper) -------------------
popkin_tabs <- function(ns, ..., id = "sub_tabs") {
  tags$div(
    class = "popkin-tabs",
    tabsetPanel(id = ns(id), ...)
  )
}

