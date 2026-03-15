# =============================================================================
# app.R — NONMEM $DESIGN Post-Processing Shiny App  (v3.0)
#
# Dependances : shiny, bslib, DT, ggplot2, dplyr, tidyr, stringr, purrr, readr
# Usage       : shiny::runApp("app/")
# =============================================================================

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

# -- Source parsers / plots ---------------------------------------------------
source("../scripts/parse_design_outputs.R", local = TRUE)
source("../scripts/report_design.R",        local = TRUE)

# -- Source modules -----------------------------------------------------------
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}


# =============================================================================
# UI
# =============================================================================

ui <- bslib::page_navbar(
  title = "$DESIGN Explorer",
  id    = "navbar",
  theme = bslib::bs_theme(
    version    = 5,
    base_font  = font_google("Inter"),
    code_font  = font_google("JetBrains Mono"),
    primary    = "#2563eb",
    "navbar-bg" = "#1a237e"
  ),
  header = tagList(
    tags$head(includeCSS("www/styles.css")),
    uiOutput("guide_banner")
  ),

  # -- Sidebar (persistent) ---------------------------------------------------
  sidebar = bslib::sidebar(
    width = 280,

    # Examples
    mod_examples_ui("examples"),

    # Upload
    mod_upload_ui("upload"),

    # Comparison
    mod_compare_ui("compare"),

    # Options communes
    div(
      class = "upload-box",
      tags$h6("Options"),
      textAreaInput(
        "param_labels", "Labels parametres (THETA)",
        placeholder = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
        rows = 3
      ),
      helpText("Un label par ligne, format THETA1=CL"),
      selectInput("table_no", "Bloc $DESIGN (TABLE NO.)",
                  choices = "1", selected = "1")
    ),

    # Options conditionnelles
    conditionalPanel(
      condition = "input.navbar == 'RSE / SE'",
      div(class = "upload-box",
        radioButtons("se_mode", "Metrique affichee",
                     choices = c("RSE (%)", "SE absolues"),
                     selected = "RSE (%)", inline = TRUE)
      )
    ),
    conditionalPanel(
      condition = "input.navbar == 'Convergence'",
      div(class = "upload-box",
        checkboxInput("log_conv", "Axe X echelle log", FALSE)
      )
    ),

    # A propos
    div(
      class = "upload-box",
      tags$h6("Reference"),
      tags$p(style = "font-size:.78rem; color:#6b7280; margin:0;",
        "RSE = |SE_FIM / theta| x 100",
        tags$br(),
        "RELATIVEINF = reduction d'incertitude sur ETA (%).",
        tags$br(), tags$br(),
        "Seuils RSE : < 20% (bon) / 20-50% (acceptable) / > 50% (mediocre)",
        tags$br(),
        "Seuils RelInf : > 50% (bon) / 20-50% (acceptable) / < 20% (insuffisant)"
      )
    )
  ),

  # -- Tabs -------------------------------------------------------------------
  bslib::nav_panel("Parametres",      mod_params_ui("params")),
  bslib::nav_panel("RSE / SE",        mod_rse_ui("rse")),
  bslib::nav_panel("RELATIVEINF",     mod_relativeinf_ui("ri")),
  bslib::nav_panel("FIM & Criteres",  mod_fim_ui("fim")),
  bslib::nav_panel("Temps optimaux",  mod_times_ui("times")),
  bslib::nav_panel("Convergence",     mod_convergence_ui("conv")),
  bslib::nav_panel("Donnees brutes",  mod_raw_ui("raw"))
)


# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # -- Upload module ----------------------------------------------------------
  upload <- mod_upload_server("upload")

  # -- Compare module ---------------------------------------------------------
  compare <- mod_compare_server("compare")

  # -- Examples module --------------------------------------------------------
  examples <- mod_examples_server("examples", session)

  # -- Example data loading ---------------------------------------------------
  example_ext  <- reactiveVal(NULL)
  example_shk  <- reactiveVal(NULL)
  example_coi  <- reactiveVal(NULL)
  example_clt  <- reactiveVal(NULL)
  example_tab  <- reactiveVal(NULL)
  example_ctl  <- reactiveVal(NULL)
  example_comp_run <- reactiveVal(NULL)

  observeEvent(examples$file_paths(), {
    paths <- examples$file_paths()

    # Reset case: clear all example data
    if (is.null(paths)) {
      example_ext(NULL); example_shk(NULL); example_coi(NULL)
      example_clt(NULL); example_tab(NULL); example_ctl(NULL)
      example_comp_run(NULL)
      updateTextAreaInput(session, "param_labels", value = "")
      return()
    }

    if (!is.null(paths$ext)) example_ext(tryCatch(read_ext(paths$ext), error = function(e) NULL))
    if (!is.null(paths$shk)) example_shk(tryCatch(read_shk(paths$shk), error = function(e) NULL))
    if (!is.null(paths$coi)) example_coi(tryCatch(read_coi(paths$coi), error = function(e) NULL))
    if (!is.null(paths$clt)) example_clt(tryCatch(read_clt(paths$clt), error = function(e) NULL))
    if (!is.null(paths$tab)) example_tab(tryCatch(read_tab(paths$tab), error = function(e) NULL))
    # Read .ctl if present
    if (!is.null(paths$ext)) {
      ctl_path <- file.path(dirname(paths$ext),
        paste0(tools::file_path_sans_ext(basename(paths$ext)), ".ctl"))
      if (file.exists(ctl_path)) {
        example_ctl(tryCatch(read_prior_nwpri(ctl_path), error = function(e) NULL))
      }
    }
    # Set labels
    lbl <- examples$labels()
    if (!is.null(lbl)) updateTextAreaInput(session, "param_labels", value = lbl)
  })

  # -- Auto-load comparison run from example compare_with ---------------------
  observeEvent(examples$compare_paths(), {
    comp_paths <- examples$compare_paths()
    if (is.null(comp_paths)) return()

    # Parse comparison data and inject into compare module as a run
    comp_data <- list(
      id = "example_comp", name = examples$compare_name() %||% "Run B",
      file_paths = comp_paths,
      ext_data = if (!is.null(comp_paths$ext)) tryCatch(read_ext(comp_paths$ext), error = function(e) NULL) else NULL,
      shk_data = if (!is.null(comp_paths$shk)) tryCatch(read_shk(comp_paths$shk), error = function(e) NULL) else NULL,
      coi_data = if (!is.null(comp_paths$coi)) tryCatch(read_coi(comp_paths$coi), error = function(e) NULL) else NULL,
      clt_data = if (!is.null(comp_paths$clt)) tryCatch(read_clt(comp_paths$clt), error = function(e) NULL) else NULL,
      tab_data = if (!is.null(comp_paths$tab)) tryCatch(read_tab(comp_paths$tab), error = function(e) NULL) else NULL
    )
    example_comp_run(comp_data)
  })

  # Merge example data with upload data (example takes priority if set)
  merged_ext <- reactive({ example_ext() %||% upload$ext_data() })
  merged_shk <- reactive({ example_shk() %||% upload$shk_data() })
  merged_coi <- reactive({ example_coi() %||% upload$coi_data() })
  merged_clt <- reactive({ example_clt() %||% upload$clt_data() })
  merged_tab <- reactive({ example_tab() %||% upload$tab_data() })
  merged_ctl <- reactive({ example_ctl() %||% upload$ctl_data() })
  merged_summary <- reactive({ examples$summary_data() })

  # -- Build all_runs: primary + comparison runs ------------------------------
  all_runs <- reactive({
    primary <- list(
      name     = "Run A",
      ext_data = merged_ext(),
      shk_data = merged_shk(),
      coi_data = merged_coi(),
      clt_data = merged_clt(),
      tab_data = merged_tab()
    )
    runs <- list(primary = primary)

    # Example comparison run (from compare_with)
    ex_comp <- example_comp_run()
    if (!is.null(ex_comp) && !is.null(ex_comp$ext_data)) {
      runs[["example_comp"]] <- ex_comp
    }

    # User-added comparison runs
    comp <- compare$comp_runs()
    for (rid in names(comp)) {
      r <- comp[[rid]]
      runs[[rid]] <- list(
        name     = r$name,
        ext_data = r$ext_data,
        shk_data = r$shk_data,
        coi_data = r$coi_data,
        clt_data = r$clt_data,
        tab_data = r$tab_data
      )
    }
    runs
  })

  # -- Mise a jour TABLE NO. --------------------------------------------------
  observe({
    ext <- merged_ext()
    req(ext)
    tabs <- sort(unique(ext$table_no))
    updateSelectInput(session, "table_no",
                      choices  = setNames(as.character(tabs), paste("Bloc", tabs)),
                      selected = as.character(max(tabs)))
  })

  # -- Reactives communes -----------------------------------------------------
  tbl_no <- reactive({ as.integer(input$table_no) })

  param_labels_r <- reactive({
    raw <- trimws(input$param_labels)
    if (raw == "") return(NULL)
    lines <- strsplit(raw, "\n")[[1]]
    lines <- lines[str_detect(lines, "=")]
    parts <- str_split_fixed(lines, "=", 2)
    lbl   <- setNames(trimws(parts[, 2]), trimws(parts[, 1]))
    lbl[nchar(names(lbl)) > 0 & nchar(lbl) > 0]
  })

  se_mode_r <- reactive({ input$se_mode %||% "RSE (%)" })
  log_conv_r <- reactive({ input$log_conv })

  # -- Guide banner for examples ----------------------------------------------
  output$guide_banner <- renderUI({
    guide <- examples$guide()
    if (is.null(guide)) return(NULL)

    paths <- examples$file_paths()
    ctl_content <- NULL
    if (!is.null(paths$ext)) {
      ctl_path <- file.path(dirname(paths$ext),
        paste0(tools::file_path_sans_ext(basename(paths$ext)), ".ctl"))
      if (file.exists(ctl_path)) ctl_content <- paste(readLines(ctl_path), collapse = "\n")
    }

    div(class = "guide-banner",
      span(class = "dismiss-btn", onclick = "this.parentElement.style.display='none'", "x"),
      tags$h6(guide$context),
      if (!is.null(ctl_content)) tags$code(ctl_content),
      tags$ul(lapply(guide$points, tags$li))
    )
  })

  # -- Module servers ---------------------------------------------------------
  mod_params_server("params",
    ext_data     = merged_ext,
    shk_data     = merged_shk,
    ext_lines    = upload$ext_lines,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    ctl_data     = merged_ctl,
    all_runs     = all_runs
  )

  mod_rse_server("rse",
    ext_data     = merged_ext,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    se_mode      = se_mode_r,
    all_runs     = all_runs
  )

  mod_relativeinf_server("ri",
    shk_data     = merged_shk,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    all_runs     = all_runs
  )

  mod_fim_server("fim",
    ext_data     = merged_ext,
    coi_data     = merged_coi,
    clt_data     = merged_clt,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    all_runs     = all_runs
  )

  mod_times_server("times",
    tab_data     = merged_tab,
    all_runs     = all_runs,
    summary_data = merged_summary
  )

  mod_convergence_server("conv",
    ext_data = merged_ext,
    log_conv = log_conv_r,
    all_runs = all_runs
  )

  mod_raw_server("raw",
    ext_data = merged_ext,
    all_runs = all_runs
  )
}


# =============================================================================
shinyApp(ui = ui, server = server)
