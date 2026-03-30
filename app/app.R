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

# =============================================================================
# Version info — affichée dans le sidebar
# =============================================================================
.APP_VERSION      <- "V4.3.0"
.APP_VERSION_NAME <- "Les sept exemples"

# =============================================================================
# Logger — écrit dans la console R et dans app/logs/app.log
# Usage : log_info("message"), log_warn("..."), log_error("...")
# =============================================================================
.LOG_DIR <- file.path(getwd(), "logs")
if (!dir.exists(.LOG_DIR)) dir.create(.LOG_DIR, recursive = TRUE)
.LOG_FILE <- file.path(.LOG_DIR, "app.log")

.log_write <- function(level, ...) {
  msg <- paste0(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    sprintf("%-5s ", level),
    paste0(..., collapse = "")
  )
  message(msg)
  cat(msg, "\n", file = .LOG_FILE, append = TRUE)
}

log_info  <- function(...) .log_write("INFO",  ...)
log_warn  <- function(...) .log_write("WARN",  ...)
log_error <- function(...) .log_write("ERROR", ...)

log_info("App demarree — R ", R.version.string,
         ", dplyr ", packageVersion("dplyr"),
         ", shiny ", packageVersion("shiny"))

source("../R/parse_design_outputs.R", local = TRUE)
source("../R/report_design.R",        local = TRUE)

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
        div(class = "app-version",  paste0(.APP_VERSION, " \u2014 ", .APP_VERSION_NAME)),
        div(class = "app-subtitle", "NONMEM 7.5+ \u00b7 Post-processing")
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
        onclick = "navTo('raw', this)", "Donnees brutes"),
      tags$button(class = "nav-item", id = "nav-ctl",
        onclick = "navTo('ctl', this)", "Control Stream")
    ),

    # -- Main area -----------------------------------------------------------
    div(id = "app-main",

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
          mod_raw_ui("raw")),
        conditionalPanel("input.active_tab == 'ctl'",
          mod_ctl_stream_ui("ctl"))
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
    uiOutput("reset_run_btn"),
    mod_examples_ui("examples"),
    mod_upload_ui("upload"),
    mod_compare_ui("compare"),
    tags$hr(),
    div(class = "upload-box",
      tags$h6("Nom du run principal"),
      textInput("primary_run_name", NULL, value = "Primary", width = "100%")
    ),
    div(class = "upload-box",
      tags$h6("Labels parametres (THETA)"),
      textAreaInput("param_labels", NULL,
        placeholder = "THETA1=CL\nTHETA2=V\nTHETA3=KA", rows = 3),
      helpText("Un label par ligne, format THETA1=CL")
    ),
    div(class = "upload-box",
      tags$h6("Labels compartiments (CMT)"),
      textAreaInput("cmt_labels", NULL,
        placeholder = "1=Depot\n2=Central (PK)\n3=Effet (PD)", rows = 3),
      helpText("Un label par ligne, format 1=Nom")
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
      tags$p(style = "font-size:.78rem; color:#64748b; margin:0;",
        tags$strong("Seuils indicatifs RSE :"), tags$br(),
        "< 20% bon / 20-50% acceptable / > 50% mediocre", tags$br(),
        tags$strong("RelInf :"),
        " > 50% bon / 20-50% acceptable / < 20% insuffisant", tags$br(),
        tags$br(),
        tags$em(style = "font-size:.72rem;",
          "Ref. : Bauer 2021, Ex. 5 : \u00ab RSE no larger than 20% \u00bb.",
          " Mentens (PFIM) : \u00ab SE < 20-30% pour chaque parametre cle \u00bb.",
          " Ces seuils sont indicatifs et dependent du contexte de l'etude."
        )
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

  # -- Helper: safe parse with logging ----------------------------------------
  .safe_load <- function(parser, path, label) {
    if (is.null(path)) return(NULL)
    tryCatch(parser(path), error = function(e) {
      log_error("Parse ", label, " [", basename(path), "]: ", e$message)
      showNotification(paste0("Erreur ", label, " : ", e$message), type = "error")
      NULL
    })
  }

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

  # -- Reset trigger (universal) ----------------------------------------------
  reset_trigger <- reactiveVal(0L)

  # -- Upload module ----------------------------------------------------------
  upload   <- mod_upload_server("upload",   reset_trigger = reset_trigger)
  compare  <- mod_compare_server("compare")
  examples <- mod_examples_server("examples", reset_trigger = reset_trigger)

  # -- Example data loading --------------------------------------------------
  example_ext      <- reactiveVal(NULL)
  example_shk      <- reactiveVal(NULL)
  example_coi      <- reactiveVal(NULL)
  example_clt      <- reactiveVal(NULL)
  example_tab      <- reactiveVal(NULL)
  example_ctl      <- reactiveVal(NULL)
  example_ctl_lines <- reactiveVal(NULL)
  example_comp_run <- reactiveVal(NULL)

  observeEvent(examples$file_paths(), ignoreNULL = FALSE, {
    paths <- examples$file_paths()
    if (is.null(paths)) {
      example_ext(NULL); example_shk(NULL); example_coi(NULL)
      example_clt(NULL); example_tab(NULL); example_ctl(NULL); example_ctl_lines(NULL)
      example_comp_run(NULL)
      updateTextAreaInput(session, "param_labels", value = "")
      updateTextAreaInput(session, "cmt_labels",   value = "")
      return()
    }
    example_ext(.safe_load(read_ext, paths$ext, ".ext"))
    example_shk(.safe_load(read_shk, paths$shk, ".shk"))
    example_coi(.safe_load(read_coi, paths$coi, ".coi"))
    example_clt(.safe_load(read_clt, paths$clt, ".clt"))
    example_tab(.safe_load(read_tab, paths$tab, ".tab"))
    if (!is.null(paths$ext)) {
      base_name <- tools::file_path_sans_ext(basename(paths$ext))
      for (ext_try in c(".ctl", ".mod", ".con")) {
        ctl_path <- file.path(dirname(paths$ext), paste0(base_name, ext_try))
        if (file.exists(ctl_path)) {
          example_ctl(.safe_load(read_prior_nwpri, ctl_path, ext_try))
          example_ctl_lines(tryCatch(readr::read_lines(ctl_path), error = function(e) NULL))
          break
        }
      }
    }
    lbl <- examples$labels()
    if (!is.null(lbl)) updateTextAreaInput(session, "param_labels", value = lbl)
  })

  observeEvent(examples$compare_paths(), {
    comp_paths <- examples$compare_paths()
    if (is.null(comp_paths)) return()
    comp_data <- list(
      id = "example_comp", name = examples$compare_name() %||% "Run B",
      ext_data = .safe_load(read_ext, comp_paths$ext, ".ext (comp)"),
      shk_data = .safe_load(read_shk, comp_paths$shk, ".shk (comp)"),
      coi_data = .safe_load(read_coi, comp_paths$coi, ".coi (comp)"),
      clt_data = .safe_load(read_clt, comp_paths$clt, ".clt (comp)"),
      tab_data = .safe_load(read_tab, comp_paths$tab, ".tab (comp)")
    )
    example_comp_run(comp_data)
  })

  # Quand l'user uploade un fichier → effacer les données exemple (upload a priorité)
  # + auto-remplir labels THETA et suggerer nom de run depuis le .ctl
  observeEvent(upload$file_paths(), ignoreNULL = FALSE, {
    fps <- upload$file_paths()
    if (!is.null(fps$ext)) {
      example_ext(NULL); example_shk(NULL); example_coi(NULL)
      example_clt(NULL); example_tab(NULL); example_ctl(NULL); example_ctl_lines(NULL)
      example_comp_run(NULL)
    }
    # Auto-remplissage depuis le .ctl uploade
    ctl_lines <- upload$ctl_lines()
    if (!is.null(ctl_lines)) {
      # Labels THETA -> textArea param_labels
      theta_lbl <- tryCatch(parse_theta_labels(ctl_lines), error = function(e) NULL)
      if (!is.null(theta_lbl)) {
        lbl_text <- paste(paste0(names(theta_lbl), "=", theta_lbl), collapse = "\n")
        updateTextAreaInput(session, "param_labels", value = lbl_text)
      }
      # Labels CMT -> textArea cmt_labels (depuis $MODEL COMP=(NOM))
      cmt_lbl <- tryCatch(parse_cmt_labels(ctl_lines), error = function(e) NULL)
      if (!is.null(cmt_lbl)) {
        cmt_text <- paste(paste0(names(cmt_lbl), "=", cmt_lbl), collapse = "\n")
        updateTextAreaInput(session, "cmt_labels", value = cmt_text)
      }
      # Suggestion nom de run -> textInput primary_run_name
      design_name <- tryCatch(parse_design_summary(ctl_lines), error = function(e) NULL)
      if (!is.null(design_name)) {
        updateTextInput(session, "primary_run_name", value = design_name)
      }
    }
  })

  # -- Merged reactives -------------------------------------------------------
  merged_ext     <- reactive({ example_ext() %||% upload$ext_data() })
  merged_shk     <- reactive({ example_shk() %||% upload$shk_data() })
  merged_coi     <- reactive({ example_coi() %||% upload$coi_data() })
  merged_clt     <- reactive({ example_clt() %||% upload$clt_data() })
  merged_tab     <- reactive({ example_tab() %||% upload$tab_data() })
  merged_ctl     <- reactive({ example_ctl() %||% upload$ctl_data() })
  merged_summary <- reactive({ examples$summary_data() })

  # -- all_runs ---------------------------------------------------------------
  primary_name <- reactive({ input$primary_run_name %||% "Primary" })

  all_runs <- reactive({
    primary <- list(
      name = primary_name(), ext_data = merged_ext(), shk_data = merged_shk(),
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
    lbl <- tibble::tibble(raw = strsplit(raw, "\n")[[1]]) |>
      dplyr::filter(stringr::str_detect(raw, "=")) |>
      tidyr::separate(raw, into = c("key", "val"), sep = "=", extra = "merge") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), trimws)) |>
      dplyr::filter(nchar(key) > 0, nchar(val) > 0) |>
      tibble::deframe()
    if (length(lbl) == 0L) return(NULL)
    lbl
  })
  cmt_labels_r <- reactive({
    raw <- trimws(input$cmt_labels)
    if (raw == "") return(NULL)
    lbl <- tibble::tibble(raw = strsplit(raw, "\n")[[1]]) |>
      dplyr::filter(stringr::str_detect(raw, "=")) |>
      tidyr::separate(raw, into = c("key", "val"), sep = "=", extra = "merge") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), trimws)) |>
      dplyr::filter(nchar(key) > 0, nchar(val) > 0) |>
      tibble::deframe()
    if (length(lbl) == 0L) return(NULL)
    lbl
  })
  se_mode_r  <- reactive({ input$se_mode  %||% "RSE (%)" })
  log_conv_r <- reactive({ input$log_conv })

  # -- KPI bar — pills runs actifs --------------------------------------------
  # -- Universal reset button -------------------------------------------------
  output$reset_run_btn <- renderUI({
    if (is.null(merged_ext())) return(NULL)
    actionButton("reset_run", "Retirer la run",
      icon  = icon("times"),
      class = "btn-sm btn-danger w-100",
      style = "margin-bottom: 8px;"
    )
  })

  observeEvent(input$reset_run, {
    reset_trigger(reset_trigger() + 1L)
    updateTextAreaInput(session, "param_labels", value = "")
    updateTextAreaInput(session, "cmt_labels",   value = "")
    updateTextInput(session, "primary_run_name", value = "Primary")
    showNotification("Run retiree", type = "message")
  })

  observeEvent(reset_trigger(), {
    updateSelectInput(session, "table_no", choices = "1", selected = "1")
  }, ignoreInit = TRUE)

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
    tab_data = merged_tab, all_runs = all_runs, cmt_labels = cmt_labels_r)

  mod_prior_server("prior",
    summary_data = merged_summary, ctl_data = merged_ctl)

  mod_convergence_server("conv",
    ext_data = merged_ext, log_conv = log_conv_r, all_runs = all_runs)

  mod_raw_server("raw",
    ext_data = merged_ext, all_runs = all_runs)

  mod_ctl_stream_server("ctl",
    ctl_lines = reactive({ example_ctl_lines() %||% upload$ctl_lines() }))
}


# =============================================================================
shinyApp(ui = ui, server = server)
