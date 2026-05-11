# =============================================================================
# helpers_ui.R — Composants UI réutilisables V4
# =============================================================================

# -- Extraction fichiers NONMEM (tar.gz ou fichiers multiples) ----------------
# Shared by mod_upload and mod_compare to avoid duplicated logic.
# Returns named list: list(ext=, shk=, coi=, clt=, tab=, ctl=, bfm=, cpu=)
extract_design_files <- function(files, tmp_prefix = "design") {
  exts <- c("ext", "shk", "coi", "clt", "tab", "bfm", "cpu")
  paths <- stats::setNames(
    vector("list", length(exts) + 2L),
    c(exts, "ctl", "mrg_cpp")
  )

  if (nrow(files) == 1L &&
      grepl("\\.(tar\\.gz|tgz)$", files$name, ignore.case = TRUE)) {
    tmp <- file.path(tempdir(),
                     paste0(tmp_prefix, "_", format(Sys.time(), "%H%M%S")))
    dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
    tryCatch(
      untar(files$datapath, exdir = tmp),
      error = function(e) {
        stop("Unable to extract archive '", files$name, "': ",
             conditionMessage(e), call. = FALSE)
      }
    )

    all_f <- list.files(tmp, recursive = TRUE, full.names = TRUE)
    all_n <- basename(all_f)

    for (et in exts) {
      idx <- which(grepl(paste0("\\.", et, "$"), all_n, ignore.case = TRUE))[1]
      if (!is.na(idx)) paths[[et]] <- all_f[idx]
    }
    ctl_idx <- which(grepl("\\.(ctl|mod|con)$", all_n, ignore.case = TRUE))[1]
    if (!is.na(ctl_idx)) paths$ctl <- all_f[ctl_idx]
    cpp_idx <- which(grepl("\\.cpp$", all_n, ignore.case = TRUE))[1]
    if (!is.na(cpp_idx)) paths$mrg_cpp <- all_f[cpp_idx]
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
      if (grepl("\\.cpp$", nm, ignore.case = TRUE))
        paths$mrg_cpp <- dp
    }
  }
  paths
}

# -- Shared app data helpers --------------------------------------------------
parse_mapping_text <- function(raw) {
  raw <- trimws(raw %||% "")
  if (raw == "") return(NULL)

  out <- tibble::tibble(raw = strsplit(raw, "\n")[[1]]) |>
    dplyr::filter(stringr::str_detect(raw, "=")) |>
    tidyr::separate(raw, into = c("key", "val"), sep = "=", extra = "merge") |>
    dplyr::mutate(dplyr::across(dplyr::everything(), trimws)) |>
    dplyr::filter(nchar(key) > 0, nchar(val) > 0) |>
    tibble::deframe()

  if (length(out) == 0L) NULL else out
}

resolve_true_values <- function(shared_true_vals, shared_ctl_lines) {
  sv <- shared_true_vals()
  if (!is.null(sv) && length(sv) > 0L) return(sv)

  cl <- shared_ctl_lines()
  if (is.null(cl)) return(NULL)

  vals <- read_true_values(cl)
  if (length(vals) == 0L) NULL else vals
}

new_design_run <- function(name = "Primary", ext_data = NULL, shk_data = NULL,
                           coi_data = NULL, clt_data = NULL, tab_data = NULL,
                           cpu_data = NA_real_) {
  list(
    name = name,
    ext_data = ext_data,
    shk_data = shk_data,
    coi_data = coi_data,
    clt_data = clt_data,
    tab_data = tab_data,
    cpu_data = cpu_data %||% NA_real_
  )
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

# -- Page shell/header ---------------------------------------------------------
page_shell <- function(...) {
  tags$div(class = "page-shell", ...)
}

page_header <- function(title, subtitle = NULL, eyebrow = NULL) {
  tags$div(
    class = "page-header-v6",
    if (!is.null(eyebrow)) tags$div(class = "page-eyebrow", eyebrow),
    tags$h3(title),
    if (!is.null(subtitle)) tags$p(subtitle)
  )
}

page_section <- function(title = NULL, ..., subtitle = NULL, class = NULL) {
  tags$section(
    class = paste(c("page-section", class), collapse = " "),
    if (!is.null(title)) section_header(title, subtitle),
    ...
  )
}

control_panel <- function(..., label = NULL) {
  tags$div(
    class = "control-panel",
    if (!is.null(label)) tags$div(class = "control-panel__label", label),
    tags$div(class = "control-panel__body", ...)
  )
}

compact_param_filter_ui <- function(ns, input_id, choices, selected,
                                    summary, note = NULL,
                                    quick_actions = NULL, open = FALSE) {
  action_items <- NULL
  if (!is.null(quick_actions) && length(quick_actions) > 0L) {
    action_items <- tags$div(
      class = "param-filter__actions",
      tags$span(class = "param-filter__actions-label", "Quick select"),
      lapply(names(quick_actions), function(action_id) {
        actionButton(
          ns(action_id), quick_actions[[action_id]],
          class = "btn btn-default btn-xs param-filter__action"
        )
      })
    )
  }

  tags$details(
    class = "param-filter",
    if (isTRUE(open)) open = NA,
    tags$summary(
      class = "param-filter__summary",
      tags$span(class = "param-filter__title", "Parameter filter"),
      tags$span(class = "param-filter-summary", summary)
    ),
    tags$div(
      class = "param-filter__body",
      action_items,
      checkboxGroupInput(
        ns(input_id), label = NULL,
        choices = choices, selected = selected,
        inline = TRUE
      ),
      if (!is.null(note)) {
        tags$p(class = "param-filter__note", note)
      }
    )
  )
}

science_note <- function(title, ..., open = FALSE) {
  tags$details(
    class = "science-note",
    if (isTRUE(open)) open = NA,
    tags$summary(title),
    tags$div(class = "science-note__body", ...)
  )
}

analysis_workspace <- function(title, ..., subtitle = NULL, actions = NULL,
                               class = NULL) {
  tags$div(
    class = paste(c("analysis-workspace", class), collapse = " "),
    tags$div(
      class = "analysis-workspace__header",
      tags$div(
        tags$h4(title),
        if (!is.null(subtitle)) tags$p(subtitle)
      ),
      if (!is.null(actions)) tags$div(class = "analysis-workspace__actions", actions)
    ),
    tags$div(class = "analysis-workspace__body", ...)
  )
}

empty_state <- function(title, body, icon_name = "circle-info") {
  tags$div(
    class = "empty-state",
    tags$div(class = "empty-state__icon", icon(icon_name)),
    tags$h4(title),
    tags$p(body)
  )
}

status_panel <- function(title, ..., tone = "neutral", icon_name = NULL,
                         class = NULL) {
  tone <- tone %||% "neutral"
  tags$div(
    class = paste(c("status-panel", paste0("status-panel--", tone), class),
                  collapse = " "),
    if (!is.null(icon_name)) tags$div(class = "status-panel__icon", icon(icon_name)),
    tags$div(
      class = "status-panel__body",
      tags$strong(title),
      tags$div(class = "status-panel__content", ...)
    )
  )
}

doc_callout <- function(section, text, label = "Open documentation") {
  tags$div(
    class = "doc-callout",
    tags$span(class = "doc-callout__text", text),
    doc_link(section, label)
  )
}

fact_item <- function(label, value, sub = NULL, tone = NULL) {
  tags$div(
    class = paste(c("fact-item", if (!is.null(tone)) paste0("fact-item--", tone)),
                  collapse = " "),
    tags$dt(label),
    tags$dd(value),
    if (!is.null(sub)) tags$small(sub)
  )
}

fact_strip <- function(..., class = NULL) {
  tags$dl(
    class = paste(c("fact-strip", class), collapse = " "),
    ...
  )
}

plot_panel <- function(title, ..., subtitle = NULL, actions = NULL,
                       class = NULL) {
  analysis_workspace(
    title = title,
    subtitle = subtitle,
    actions = actions,
    class = paste(c("plot-panel", class), collapse = " "),
    ...
  )
}

table_panel <- function(title, ..., subtitle = NULL, actions = NULL,
                        class = NULL) {
  analysis_workspace(
    title = title,
    subtitle = subtitle,
    actions = actions,
    class = paste(c("table-panel", class), collapse = " "),
    ...
  )
}

doc_link <- function(section, label = "See Documentation") {
  tags$a(
    href = "#",
    class = "doc-link",
    onclick = sprintf(
      "Shiny.setInputValue('open_doc_section', '%s', {priority:'event'}); return false;",
      section
    ),
    icon("book-open"),
    span(label)
  )
}

documentation_section <- function(id, title, ..., eyebrow = NULL) {
  tags$section(
    id = paste0("doc-", id),
    class = "documentation-section",
    if (!is.null(eyebrow)) tags$div(class = "documentation-section__eyebrow", eyebrow),
    tags$h3(title),
    tags$div(class = "documentation-section__body", ...)
  )
}

# -- Popkin-style tabs (underline style tabsetPanel wrapper) -------------------
popkin_tabs <- function(ns, ..., id = "sub_tabs") {
  tags$div(
    class = "popkin-tabs",
    tabsetPanel(id = ns(id), ...)
  )
}

# =============================================================================
# Status banners — reusable across SSE modules
# =============================================================================

# -- .ctl status banner (green/yellow) ----------------------------------------
ctl_status_banner <- function(true_vals) {
  has_ctl <- !is.null(true_vals) && length(true_vals) > 0L
  if (has_ctl) {
    status_panel(
      sprintf("True values loaded (%d params)", length(true_vals)),
      tags$p("From control stream uploaded in the Home tab."),
      tone = "success",
      icon_name = "check-circle"
    )
  } else {
    status_panel(
      "No control stream loaded",
      tags$p(
        "Upload a .ctl/.mod/.con file in the ",
        tags$strong("Home"), " tab to extract true parameter values."
      ),
      tone = "warning",
      icon_name = "exclamation-triangle"
    )
  }
}

# -- SSE data status banner (green/yellow) ------------------------------------
sse_status_banner <- function(data, label = "SSE data") {
  has_data <- !is.null(data)
  if (has_data) {
    n_total   <- attr(data, "n_total") %||% nrow(data)
    n_success <- attr(data, "n_success") %||% sum(data$converged)
    status_panel(
      sprintf("%s loaded", label),
      tags$p(sprintf("%d runs, %d converged", n_total, n_success)),
      tone = "success",
      icon_name = "check-circle"
    )
  } else {
    status_panel(
      sprintf("No %s loaded", label),
      tags$p(
        "Upload a PsN raw_results CSV in the ",
        tags$strong("SSE Upload"), " tab."
      ),
      tone = "warning",
      icon_name = "exclamation-triangle"
    )
  }
}

# =============================================================================
# Run health pills — reusable across SSE modules
# =============================================================================

# Per-metric thresholds (pharmacometrics conventions)
SSE_HEALTH_THRESHOLDS <- list(
  "Total runs"            = c(green = 0,  amber = 0,  red = 0),
  "Minimization OK"       = c(green = 80, amber = 60, red = 0),
  "No boundary estimates" = c(green = 80, amber = 60, red = 0),
  "Covariance OK"         = c(green = 60, amber = 40, red = 0),
  "No rounding errors"    = c(green = 80, amber = 60, red = 0)
)

health_pill_color <- function(stage_name, pct) {
  th <- SSE_HEALTH_THRESHOLDS[[stage_name]]
  if (is.null(th)) th <- c(green = 80, amber = 60, red = 0)
  if (pct >= th[["green"]]) "#15803d"
  else if (pct >= th[["amber"]]) "#b45309"
  else "#dc2626"
}

health_pill_bg <- function(stage_name, pct) {
  th <- SSE_HEALTH_THRESHOLDS[[stage_name]]
  if (is.null(th)) th <- c(green = 80, amber = 60, red = 0)
  if (pct >= th[["green"]]) "#dcfce7"
  else if (pct >= th[["amber"]]) "#fef3c7"
  else "#fee2e2"
}

health_pill_tone <- function(stage_name, pct) {
  th <- SSE_HEALTH_THRESHOLDS[[stage_name]]
  if (is.null(th)) th <- c(green = 80, amber = 60, red = 0)
  if (pct >= th[["green"]]) "success"
  else if (pct >= th[["amber"]]) "warning"
  else "danger"
}

# Build a row of run-health pills from a compute_run_health() result.
# @param health  Result of compute_run_health() (list with $stages data.frame)
# @param label   Optional label displayed before the pills
# @return tagList (or NULL if health is NULL)
make_health_pills <- function(health, label = NULL) {
  if (is.null(health)) return(NULL)
  stages <- health$stages
  pills <- lapply(seq_len(nrow(stages)), function(i) {
    s <- stages[i, ]
    tags$span(
      class = paste("health-pill",
                    paste0("health-pill--", health_pill_tone(s$stage, s$pct))),
      sprintf("%s: %d/%d (%.0f%%)", s$stage, s$n, s$denom, s$pct)
    )
  })
  div(class = "run-health-pills",
    if (!is.null(label)) tags$strong(class = "run-health-pills__label", label),
    div(class = "run-health-pills__items", pills)
  )
}

