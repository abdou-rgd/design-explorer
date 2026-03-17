# =============================================================================
# app.R — NONMEM $DESIGN Post-Processing Shiny App  (V4)
#
# Usage : shiny::runApp("app/")  depuis la racine ClaudeProjets/
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

source("../scripts/parse_design_outputs.R", local = TRUE)
source("../scripts/report_design.R",        local = TRUE)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}


# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  # -- Head ------------------------------------------------------------------
  tags$head(
    google_fonts_link(),
    includeCSS("www/styles.css"),
    # Keyboard shortcut: ESC closes drawer
    tags$script(HTML("
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') Shiny.setInputValue('drawer_close_trigger',
          Math.random());
      });
    "))
  ),

  # -- Guide banner (from examples) -----------------------------------------
  uiOutput("guide_banner"),

  # -- App shell -------------------------------------------------------------
  div(id = "app-shell",

    # -- Sidebar -------------------------------------------------------------
    tags$nav(id = "app-sidebar",

      div(id = "sidebar-logo",
        div(class = "app-title",   "$DESIGN Explorer"),
        div(class = "app-subtitle", "NONMEM 7.5+ · Post-processing")
      ),

      tags$button(id = "btn-runs", onclick = "Shiny.setInputValue('open_drawer', Math.random())",
        "\u25A3 Runs actifs"
      ),

      div(class = "nav-section-label", "Resultats"),
      tags$button(class = "nav-item active", id = "nav-params",
        onclick = "navTo('params', this)", "Parametres"),
      tags$button(class = "nav-item", id = "nav-rse",
        onclick = "navTo('rse', this)", "RSE / SE"),
      tags$button(class = "nav-item", id = "nav-ri",
        onclick = "navTo('ri', this)", "RELATIVEINF"),

      div(class = "nav-section-label", "Design"),
      tags$button(class = "nav-item", id = "nav-fim",
        onclick = "navTo('fim', this)", "FIM & Criteres"),
      tags$button(class = "nav-item", id = "nav-times",
        onclick = "navTo('times', this)", "Temps optimaux"),
      tags$button(class = "nav-item", id = "nav-prior",
        onclick = "navTo('prior', this)", "Design Robuste"),

      div(class = "nav-section-label", "Diagnostic"),
      tags$button(class = "nav-item", id = "nav-conv",
        onclick = "navTo('conv', this)", "Convergence"),
      tags$button(class = "nav-item", id = "nav-raw",
        onclick = "navTo('raw', this)", "Donnees brutes")
    ),

    # -- Main area -----------------------------------------------------------
    div(id = "app-main",

      # KPI bar
      div(id = "kpi-bar", uiOutput("kpi_bar_content")),

      # Tab content
      div(id = "tab-content",
        conditionalPanel("input.active_tab == 'params' || !input.active_tab",
          mod_params_ui("params")),
        conditionalPanel("input.active_tab == 'rse'",
          mod_rse_ui("rse")),
        conditionalPanel("input.active_tab == 'ri'",
          mod_relativeinf_ui("ri")),
        conditionalPanel("input.active_tab == 'fim'",
          mod_fim_ui("fim")),
        conditionalPanel("input.active_tab == 'times'",
          mod_times_ui("times")),
        conditionalPanel("input.active_tab == 'prior'",
          mod_prior_ui("prior")),
        conditionalPanel("input.active_tab == 'conv'",
          mod_convergence_ui("conv")),
        conditionalPanel("input.active_tab == 'raw'",
          mod_raw_ui("raw"))
      )
    )
  ),

  # -- Drawer overlay --------------------------------------------------------
  div(id = "drawer-overlay", class = "",
    onclick = "Shiny.setInputValue('drawer_close_trigger', Math.random())"
  ),

  # -- Drawer panel ----------------------------------------------------------
  div(id = "drawer-panel", class = "",
    tags$button(id = "drawer-close", onclick = "Shiny.setInputValue('drawer_close_trigger', Math.random())", "\u2715"),
    div(class = "drawer-title", "Gestion des runs"),
    mod_examples_ui("examples"),
    mod_upload_ui("upload"),
    mod_compare_ui("compare"),
    tags$hr(),
    div(class = "upload-box",
      tags$h6("Labels parametres (THETA)"),
      textAreaInput("param_labels", NULL,
        placeholder = "THETA1=CL\nTHETA2=V\nTHETA3=KA", rows = 3),
      helpText("Un label par ligne, format THETA1=CL")
    ),
    div(class = "upload-box",
      tags$h6("Bloc $DESIGN (TABLE NO.)"),
      selectInput("table_no", NULL, choices = "1", selected = "1")
    ),
    div(class = "upload-box",
      tags$h6("Metrique RSE"),
      radioButtons("se_mode", NULL,
        choices  = c("RSE (%)", "SE absolues"),
        selected = "RSE (%)", inline = TRUE)
    ),
    div(class = "upload-box",
      checkboxInput("log_conv", "Axe X log (Convergence)", FALSE)
    ),
    div(class = "upload-box",
      tags$p(style = "font-size:.76rem; color:#64748b; margin:0;",
        "RSE : < 20% bon / 20-50% acceptable / > 50% mediocre", tags$br(),
        "RelInf : > 50% bon / 20-50% acceptable / < 20% insuffisant"
      )
    )
  ),

  # -- JS: nav + drawer -------------------------------------------------------
  tags$script(HTML("
    function navTo(tab, el) {
      Shiny.setInputValue('active_tab', tab, {priority: 'event'});
      document.querySelectorAll('.nav-item').forEach(function(b) {
        b.classList.remove('active');
      });
      el.classList.add('active');
    }
  ")),

  # -- JS eval handler (MUST be in UI for Shiny to register it) --------------
  uiOutput("js_handler")
)


# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # -- Drawer open/close ------------------------------------------------------
  observeEvent(input$open_drawer, {
    session$sendCustomMessage("evalJS", "
      document.getElementById('drawer-panel').classList.add('open');
      document.getElementById('drawer-overlay').classList.add('open');
    ")
  }, ignoreNULL = TRUE)

  observeEvent(input$drawer_close_trigger, {
    session$sendCustomMessage("evalJS", "
      document.getElementById('drawer-panel').classList.remove('open');
      document.getElementById('drawer-overlay').classList.remove('open');
    ")
  }, ignoreNULL = TRUE)

  # -- Upload module ----------------------------------------------------------
  upload   <- mod_upload_server("upload")
  compare  <- mod_compare_server("compare")
  examples <- mod_examples_server("examples", session)

  # -- Example data loading --------------------------------------------------
  example_ext      <- reactiveVal(NULL)
  example_shk      <- reactiveVal(NULL)
  example_coi      <- reactiveVal(NULL)
  example_clt      <- reactiveVal(NULL)
  example_tab      <- reactiveVal(NULL)
  example_ctl      <- reactiveVal(NULL)
  example_comp_run <- reactiveVal(NULL)

  observeEvent(examples$file_paths(), {
    paths <- examples$file_paths()
    if (is.null(paths)) {
      example_ext(NULL); example_shk(NULL); example_coi(NULL)
      example_clt(NULL); example_tab(NULL); example_ctl(NULL)
      example_comp_run(NULL)
      updateTextAreaInput(session, "param_labels", value = "")
      return()
    }
    if (!is.null(paths$ext)) example_ext(tryCatch(read_ext(paths$ext),  error = function(e) NULL))
    if (!is.null(paths$shk)) example_shk(tryCatch(read_shk(paths$shk),  error = function(e) NULL))
    if (!is.null(paths$coi)) example_coi(tryCatch(read_coi(paths$coi),  error = function(e) NULL))
    if (!is.null(paths$clt)) example_clt(tryCatch(read_clt(paths$clt),  error = function(e) NULL))
    if (!is.null(paths$tab)) example_tab(tryCatch(read_tab(paths$tab),  error = function(e) NULL))
    if (!is.null(paths$ext)) {
      ctl_path <- file.path(dirname(paths$ext),
        paste0(tools::file_path_sans_ext(basename(paths$ext)), ".ctl"))
      if (file.exists(ctl_path))
        example_ctl(tryCatch(read_prior_nwpri(ctl_path), error = function(e) NULL))
    }
    lbl <- examples$labels()
    if (!is.null(lbl)) updateTextAreaInput(session, "param_labels", value = lbl)
  })

  observeEvent(examples$compare_paths(), {
    comp_paths <- examples$compare_paths()
    if (is.null(comp_paths)) return()
    comp_data <- list(
      id = "example_comp", name = examples$compare_name() %||% "Run B",
      ext_data = if (!is.null(comp_paths$ext)) tryCatch(read_ext(comp_paths$ext), error = function(e) NULL) else NULL,
      shk_data = if (!is.null(comp_paths$shk)) tryCatch(read_shk(comp_paths$shk), error = function(e) NULL) else NULL,
      coi_data = if (!is.null(comp_paths$coi)) tryCatch(read_coi(comp_paths$coi), error = function(e) NULL) else NULL,
      clt_data = if (!is.null(comp_paths$clt)) tryCatch(read_clt(comp_paths$clt), error = function(e) NULL) else NULL,
      tab_data = if (!is.null(comp_paths$tab)) tryCatch(read_tab(comp_paths$tab), error = function(e) NULL) else NULL
    )
    example_comp_run(comp_data)
  })

  # -- Merged reactives -------------------------------------------------------
  merged_ext     <- reactive({ example_ext() %||% upload$ext_data() })
  merged_shk     <- reactive({ example_shk() %||% upload$shk_data() })
  merged_coi     <- reactive({ example_coi() %||% upload$coi_data() })
  merged_clt     <- reactive({ example_clt() %||% upload$clt_data() })
  merged_tab     <- reactive({ example_tab() %||% upload$tab_data() })
  merged_ctl     <- reactive({ example_ctl() %||% upload$ctl_data() })
  merged_summary <- reactive({
    upload_summary  <- upload$summary_data()
    example_summary <- examples$summary_data()
    if (!is.null(upload_summary) && nrow(upload_summary) > 0) upload_summary
    else example_summary
  })

  # -- all_runs ---------------------------------------------------------------
  all_runs <- reactive({
    primary <- list(
      name = "Run A", ext_data = merged_ext(), shk_data = merged_shk(),
      coi_data = merged_coi(), clt_data = merged_clt(), tab_data = merged_tab()
    )
    runs <- list(primary = primary)
    ex_comp <- example_comp_run()
    if (!is.null(ex_comp) && !is.null(ex_comp$ext_data))
      runs[["example_comp"]] <- ex_comp
    comp <- compare$comp_runs()
    for (rid in names(comp)) {
      r <- comp[[rid]]
      runs[[rid]] <- list(
        name = r$name, ext_data = r$ext_data, shk_data = r$shk_data,
        coi_data = r$coi_data, clt_data = r$clt_data, tab_data = r$tab_data
      )
    }
    runs
  })

  # -- TABLE NO. update -------------------------------------------------------
  observe({
    ext <- merged_ext(); req(ext)
    tabs <- sort(unique(ext$table_no))
    updateSelectInput(session, "table_no",
      choices  = setNames(as.character(tabs), paste("Bloc", tabs)),
      selected = as.character(max(tabs)))
  })

  # -- Shared reactives -------------------------------------------------------
  tbl_no <- reactive({ as.integer(input$table_no) })
  param_labels_r <- reactive({
    raw <- trimws(input$param_labels)
    if (raw == "") return(NULL)
    lbl <- tibble::tibble(raw = stringr::str_split_1(raw, "\n")) |>
      dplyr::filter(stringr::str_detect(raw, "=")) |>
      tidyr::separate_wider_delim(raw, "=", names = c("key", "val"), too_many = "merge") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), trimws)) |>
      dplyr::filter(nchar(key) > 0, nchar(val) > 0) |>
      tibble::deframe()
    if (length(lbl) == 0L) return(NULL)
    lbl
  })
  se_mode_r  <- reactive({ input$se_mode  %||% "RSE (%)" })
  log_conv_r <- reactive({ input$log_conv })

  # -- KPI bar ----------------------------------------------------------------
  output$kpi_bar_content <- renderUI({
    ext <- merged_ext()
    if (is.null(ext)) {
      return(div(class = "kpi-metric", style = "color:#94a3b8;",
                 "Aucun run charge — ouvrir le drawer pour charger des fichiers"))
    }
    tbl      <- tbl_no()
    ofv      <- tryCatch(get_ofv(ext, tbl),    error = function(e) NA_real_)
    rse      <- tryCatch(get_rse(ext, tbl),    error = function(e) NULL)
    n_est    <- if (!is.null(rse)) nrow(rse)   else 0L
    n_poor   <- if (!is.null(rse)) sum(rse$rse_pct > 20, na.rm = TRUE) else 0L
    rse_mean <- if (!is.null(rse) && n_est > 0) mean(rse$rse_pct, na.rm = TRUE) else NA_real_
    n_params <- max(n_est, 1L)
    d_crit   <- if (!is.na(ofv)) exp(-ofv / n_params) else NA_real_

    render_kpi_bar(
      run_name  = "Run A",
      file_name = "",
      d_crit    = if (!is.na(d_crit)) d_crit else ifelse(!is.na(ofv), ofv, 0),
      rse_mean  = if (!is.na(rse_mean)) rse_mean else 0,
      n_poor    = n_poor,
      n_total   = n_est
    )
  })

  # -- Guide banner -----------------------------------------------------------
  output$guide_banner <- renderUI({
    guide <- examples$guide()
    if (is.null(guide)) return(NULL)
    paths <- examples$file_paths()
    ctl_content <- NULL
    if (!is.null(paths$ext)) {
      ctl_path <- file.path(dirname(paths$ext),
        paste0(tools::file_path_sans_ext(basename(paths$ext)), ".ctl"))
      if (file.exists(ctl_path))
        ctl_content <- paste(readLines(ctl_path), collapse = "\n")
    }
    div(class = "guide-banner",
      tags$button(class = "dismiss-btn",
        onclick = "this.parentElement.style.display='none'", "\u00d7"),
      tags$h6(guide$context),
      if (!is.null(ctl_content)) tags$code(ctl_content),
      tags$ul(lapply(guide$points, tags$li))
    )
  })

  # -- JS eval handler --------------------------------------------------------
  output$js_handler <- renderUI({
    tags$script(HTML("
      Shiny.addCustomMessageHandler('evalJS', function(code) { eval(code); });
    "))
  })
  outputOptions(output, "js_handler", suspendWhenHidden = FALSE)

  # -- Module servers ---------------------------------------------------------
  mod_params_server("params",
    ext_data = merged_ext, shk_data = merged_shk,
    ext_lines = upload$ext_lines, tbl_no = tbl_no,
    param_labels = param_labels_r, all_runs = all_runs)

  mod_rse_server("rse",
    ext_data = merged_ext, tbl_no = tbl_no,
    param_labels = param_labels_r, se_mode = se_mode_r, all_runs = all_runs)

  mod_relativeinf_server("ri",
    shk_data = merged_shk, tbl_no = tbl_no,
    param_labels = param_labels_r, all_runs = all_runs)

  mod_fim_server("fim",
    ext_data = merged_ext, coi_data = merged_coi, clt_data = merged_clt,
    tbl_no = tbl_no, param_labels = param_labels_r, all_runs = all_runs)

  mod_times_server("times",
    tab_data = merged_tab, all_runs = all_runs)

  mod_prior_server("prior",
    summary_data = merged_summary, ctl_data = merged_ctl)

  mod_convergence_server("conv",
    ext_data = merged_ext, log_conv = log_conv_r, all_runs = all_runs)

  mod_raw_server("raw",
    ext_data = merged_ext, all_runs = all_runs)
}


# =============================================================================
shinyApp(ui = ui, server = server)
