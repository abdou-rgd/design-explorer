# =============================================================================
# app.R — NONMEM $DESIGN Post-Processing Shiny App  (V5)
#
# Usage : shiny::runApp("app/", launch.browser = TRUE)  depuis la racine design-explorer/
#         launch.browser = TRUE force l'ouverture dans le navigateur externe
#         (contourne les bugs d'upload du viewer RStudio integre sur certaines versions)
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
.APP_VERSION      <- "V5.7"
.APP_VERSION_NAME <- "Terre des hommes"

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

source("../R/design_utils.R",   local = TRUE)
source("../R/design_io.R",      local = TRUE)
source("../R/design_metrics.R", local = TRUE)
source("../R/design_summary.R", local = TRUE)
source("../R/ctl_parsers.R",    local = TRUE)
source("../R/report_design.R",        local = TRUE)
source("../R/fim_metrics.R",          local = TRUE)
source("../R/sse_metrics.R",          local = TRUE)
source("../R/sse_diagnostics.R",      local = TRUE)
source("../R/sse_comparison.R",       local = TRUE)
source("../R/mrgsolve_bridge.R",      local = TRUE)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}


# =============================================================================
# UI
# =============================================================================

ui <- navbarPage(
  title = span(
    span("DE$IGN EXPLORER",
         style = "font-family:'JetBrains Mono',monospace; font-weight:700; letter-spacing:.04em;"),
    span(paste0(.APP_VERSION, " \u2014 ", .APP_VERSION_NAME),
         style = "font-size:.65rem; color:#93c5fd; font-style:italic; margin-left:10px; opacity:0.85;")
  ),
  id = "navbar",
  inverse = TRUE,
  collapsible = TRUE,

  header = tagList(
    tags$head(google_fonts_link(), includeCSS("www/styles.css")),
    div(style = "display:none;",
      textInput("primary_run_name", NULL, value = "Primary"),
      textAreaInput("param_labels", NULL, placeholder = "THETA1=CL\nTHETA2=V\nTHETA3=KA", rows = 3),
      textAreaInput("cmt_labels", NULL, placeholder = "1=Depot\n2=Central (PK)\n3=Effet (PD)", rows = 3)
    )
  ),

  # -- Home ------------------------------------------------------------------
  tabPanel("Home", value = "home",
    fluidRow(
      column(3,
        tags$div(class = "home-sidebar",
          tags$div(class = "home-upload-compact",
            tags$h6(class = "home-section-label", "Load Run"),
            mod_upload_ui("upload"),
            tags$hr(style = "margin:8px 0;"),
            mod_examples_ui("examples")
          ),
          tags$div(class = "home-upload-compact", style = "margin-top:12px;",
            mod_compare_ui("compare")
          )
        )
      ),
      column(9,
        mod_home_ui("home")
      )
    )
  ),

  # -- Results (dropdown) ----------------------------------------------------
  navbarMenu("Results",
    tabPanel("Parameters",  value = "params", mod_params_ui("params")),
    tabPanel("RSE / SE",    value = "rse",    mod_rse_ui("rse")),
    tabPanel("RELATIVEINF", value = "ri",     mod_relativeinf_ui("ri"))
  ),

  # -- Design (dropdown) -----------------------------------------------------
  navbarMenu("Design",
    tabPanel("FIM & Criteria", value = "fim",   mod_fim_ui("fim")),
    tabPanel("Optimal Times",  value = "times", mod_times_ui("times")),
    tabPanel("Robust Design",  value = "prior", mod_prior_ui("prior"))
  ),

  # -- Decision (dropdown) ---------------------------------------------------
  navbarMenu("Decision",
    tabPanel("Power / NSN", value = "power", mod_power_ui("power"))
  ),

  # -- Diagnostic (dropdown) -------------------------------------------------
  navbarMenu("Diagnostic",
    tabPanel("Convergence",    value = "conv", mod_convergence_ui("conv")),
    tabPanel("Control Stream", value = "ctl",  mod_ctl_stream_ui("ctl")),
    tabPanel("Raw Data",       value = "raw",  mod_raw_ui("raw"))
  ),

  # -- Validation (dropdown) -------------------------------------------------
  navbarMenu("Validation",
    tabPanel("SSE Upload",     value = "sse_upload",    mod_sse_upload_ui("sse_upload")),
    tabPanel("SSE Validation", value = "sse",           mod_sse_validation_ui("sse")),
    tabPanel("SSE Analysis",   value = "sse_analysis",  mod_sse_analysis_ui("sse_analysis")),
    tabPanel("SSE Comparison", value = "sse_comparison", mod_sse_comparison_ui("sse_comparison"))
  ),

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

  # -- Reset trigger (universal) ----------------------------------------------
  reset_trigger <- reactiveVal(0L)

  # -- Suggested groupsize (set by upload/examples, consumed by mod_params) ---
  suggested_gs <- reactiveVal(1L)

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
  example_cpu      <- reactiveVal(NA_real_)
  example_ctl      <- reactiveVal(NULL)
  example_ctl_lines <- reactiveVal(NULL)
  example_comp_run <- reactiveVal(NULL)

  observeEvent(examples$file_paths(), ignoreNULL = FALSE, {
    paths <- examples$file_paths()
    if (is.null(paths)) {
      example_ext(NULL); example_shk(NULL); example_coi(NULL)
      example_clt(NULL); example_tab(NULL); example_cpu(NA_real_)
      example_ctl(NULL); example_ctl_lines(NULL)
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
    example_cpu(read_cpu(paths$cpu %||% ""))
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
    # GROUPSIZE depuis le .ctl de l'exemple
    ctl_ex <- example_ctl_lines()
    if (!is.null(ctl_ex)) {
      gs <- tryCatch(parse_groupsize(ctl_ex), error = function(e) NA_integer_)
      if (!is.na(gs)) suggested_gs(gs)
    }
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
      tab_data = .safe_load(read_tab, comp_paths$tab, ".tab (comp)"),
      cpu_data = read_cpu(comp_paths$cpu %||% "")
    )
    example_comp_run(comp_data)
  })

  # Quand l'user uploade un fichier → effacer les données exemple (upload a priorité)
  # + auto-remplir labels THETA et suggerer nom de run depuis le .ctl
  observeEvent(upload$file_paths(), ignoreNULL = FALSE, {
    fps <- upload$file_paths()
    if (!is.null(fps$ext)) {
      example_ext(NULL); example_shk(NULL); example_coi(NULL)
      example_clt(NULL); example_tab(NULL); example_cpu(NA_real_)
      example_ctl(NULL); example_ctl_lines(NULL)
      example_comp_run(NULL)
    }
    # Auto-remplissage depuis le .ctl uploade
    ctl_lines <- upload$ctl_lines()
    if (!is.null(ctl_lines)) {
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
      # GROUPSIZE -> mod_params via suggested_gs
      gs <- tryCatch(parse_groupsize(ctl_lines), error = function(e) NA_integer_)
      if (!is.na(gs)) suggested_gs(gs)
    }
  })

  # -- Merged reactives -------------------------------------------------------
  merged_ext     <- reactive({ example_ext() %||% upload$ext_data() })
  merged_shk     <- reactive({ example_shk() %||% upload$shk_data() })
  merged_coi     <- reactive({ example_coi() %||% upload$coi_data() })
  merged_clt     <- reactive({ example_clt() %||% upload$clt_data() })
  merged_tab     <- reactive({ example_tab() %||% upload$tab_data() })
  merged_ctl     <- reactive({ example_ctl() %||% upload$ctl_data() })
  merged_cpu     <- reactive({
    val <- example_cpu()
    if (!is.na(val)) return(val)
    upload$cpu_data()
  })
  # Robust summary: built-in example (pre-computed) or computed from uploaded .tab
  upload_summary <- reactive({
    tab <- upload$tab_data()
    if (is.null(tab)) return(NULL)
    compute_robust_summary(tab)
  })
  merged_summary   <- reactive({ examples$summary_data() %||% upload_summary() })
  merged_ctl_lines <- reactive({ example_ctl_lines() %||% upload$ctl_lines() })
  merged_true_vals <- reactive({
    cl <- merged_ctl_lines()
    if (is.null(cl)) return(NULL)
    tryCatch(read_true_values(cl), error = function(e) NULL)
  })

  # -- all_runs ---------------------------------------------------------------
  primary_name <- reactive({ input$primary_run_name %||% "Primary" })

  all_runs <- reactive({
    primary <- list(
      name = primary_name(), ext_data = merged_ext(), shk_data = merged_shk(),
      coi_data = merged_coi(), clt_data = merged_clt(), tab_data = merged_tab(),
      cpu_data = merged_cpu()
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
        coi_data = r$coi_data, clt_data = r$clt_data, tab_data = r$tab_data,
        cpu_data = r$cpu_data %||% NA_real_
      )
    }
    # Deduplicate run names (e.g. two runs with same $DESIGN args)
    run_nms <- vapply(runs, function(r) r$name, character(1))
    counts <- table(run_nms)
    dupe_names <- names(counts[counts > 1L])
    for (dn in dupe_names) {
      idx <- which(run_nms == dn)
      for (k in seq_along(idx)[-1]) {
        runs[[idx[k]]]$name <- paste0(dn, " (", k, ")")
      }
    }
    runs
  })

  # -- Shared reactives -------------------------------------------------------
  # tbl_no and groupsize are now owned by mod_params (returned as reactives)
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
  # -- Reset handler (triggered by mod_home) -----------------------------------
  observeEvent(reset_trigger(), {
    updateTextAreaInput(session, "param_labels", value = "")
    updateTextAreaInput(session, "cmt_labels",   value = "")
    updateTextInput(session, "primary_run_name", value = "Primary")
    suggested_gs(1L)
  }, ignoreInit = TRUE)

  # -- Module servers ---------------------------------------------------------
  # mod_params owns table_no + groupsize — call first, capture return values
  params_out <- mod_params_server("params",
    ext_data = merged_ext, shk_data = merged_shk,
    ext_lines = upload$ext_lines,
    param_labels = param_labels_r,
    table_no_range = examples$table_no_range,
    suggested_groupsize = suggested_gs,
    reset_trigger = reset_trigger,
    all_runs = all_runs)

  tbl_no     <- params_out$tbl_no
  groupsize_r <- params_out$groupsize

  mod_rse_server("rse",
    ext_data = merged_ext, tbl_no = tbl_no,
    param_labels = param_labels_r, all_runs = all_runs)

  mod_relativeinf_server("ri",
    shk_data = merged_shk, tbl_no = tbl_no,
    param_labels = param_labels_r, all_runs = all_runs)

  mod_fim_server("fim",
    ext_data = merged_ext, coi_data = merged_coi, clt_data = merged_clt,
    tbl_no = tbl_no, param_labels = param_labels_r, all_runs = all_runs)

  mod_times_server("times",
    tab_data = merged_tab, all_runs = all_runs, cmt_labels = cmt_labels_r,
    ext_data = merged_ext, ctl_lines = merged_ctl_lines,
    theta_labels = param_labels_r)

  mod_prior_server("prior",
    summary_data = merged_summary, ctl_data = merged_ctl,
    all_runs = all_runs)

  mod_convergence_server("conv",
    ext_data = merged_ext, all_runs = all_runs,
    ctl_lines = merged_ctl_lines,
    cpu_secs = merged_cpu)

  mod_raw_server("raw",
    ext_data = merged_ext, all_runs = all_runs)

  mod_ctl_stream_server("ctl",
    ctl_lines = merged_ctl_lines)

  mod_power_server("power",
    ext_data     = merged_ext,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    groupsize    = groupsize_r,
    all_runs     = all_runs)

  # -- SSE centralized upload --
  sse_upload <- mod_sse_upload_server("sse_upload", reset_trigger = reset_trigger)

  mod_sse_validation_server("sse",
    ext_data         = merged_ext,
    param_labels     = param_labels_r,
    shared_ctl_lines = merged_ctl_lines,
    shared_true_vals = merged_true_vals,
    sse_a_data       = sse_upload$sse_a_data,
    sse_b_data       = sse_upload$sse_b_data,
    name_a           = sse_upload$name_a,
    name_b           = sse_upload$name_b,
    coi_data         = merged_coi,
    clt_data         = merged_clt)

  mod_sse_analysis_server("sse_analysis",
    sse_a_shared = sse_upload$sse_a_data,
    sse_b_shared = sse_upload$sse_b_data,
    name_a       = sse_upload$name_a,
    name_b       = sse_upload$name_b,
    true_vals    = merged_true_vals,
    param_labels = param_labels_r)

  mod_sse_comparison_server("sse_comparison",
    sse_orig         = sse_upload$sse_a_data,
    sse_opti         = sse_upload$sse_b_data,
    name_orig        = sse_upload$name_a,
    name_opti        = sse_upload$name_b,
    shared_ctl_lines = merged_ctl_lines,
    shared_true_vals = merged_true_vals,
    param_labels     = param_labels_r)

  mod_home_server("home",
    merged_ext   = merged_ext,
    merged_cpu   = merged_cpu,
    merged_tab   = merged_tab,
    ext_lines    = upload$ext_lines,
    primary_name = primary_name,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    groupsize    = groupsize_r,
    reset_trigger = reset_trigger)
}


# =============================================================================
shinyApp(ui = ui, server = server)
