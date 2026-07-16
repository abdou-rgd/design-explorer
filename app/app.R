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

options(shiny.maxRequestSize = 100 * 1024 * 1024)

# =============================================================================
# Version info — affichée dans le sidebar
# =============================================================================
.APP_VERSION      <- "V5.8"
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

source("../R/source_core.R", local = TRUE)
source_core("..")

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}


# =============================================================================
# UI
# =============================================================================

ui <- navbarPage(
  title = span(
    span(class = "brand-mark", "DE$IGN"),
    span(class = "brand-wordmark", "Explorer"),
    span(class = "brand-version", paste0(.APP_VERSION, " / ", .APP_VERSION_NAME))
  ),
  id = "navbar",
  inverse = TRUE,
  collapsible = TRUE,
  windowTitle = "DE$IGN Explorer",

  header = tagList(
    tags$head(
      google_fonts_link(),
      includeCSS("www/styles.css"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('scroll-doc-section', function(id) {
          setTimeout(function() {
            var el = document.getElementById('doc-' + id);
            if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }, 160);
        });
      "))
    ),
    div(class = "app-command-deck",
      div(class = "command-deck__intro",
        div(class = "command-deck__eyebrow", "NONMEM $DESIGN workspace"),
        div(class = "command-deck__title", "Load outputs, inspect the FIM, and validate precision from one run context."),
        div(class = "global-run-context",
          div(class = "global-run-context__item",
            span(class = "global-run-context__label", "Run"),
            textOutput("global_run_label", inline = TRUE)
          ),
          div(class = "global-run-context__control",
            span(class = "global-run-context__label", "Table"),
            selectInput("global_table_no", NULL,
                        choices = "1", selected = "1", width = "82px")
          )
        )
      ),
      div(class = "command-deck__panels",
        div(class = "command-panel command-panel--load",
          div(class = "command-panel__header",
            span(class = "command-panel__kicker", "Primary run"),
            span(class = "command-panel__icon", icon("upload"))
          ),
          mod_upload_ui("upload")
        ),
        div(class = "command-panel command-panel--examples",
          div(class = "command-panel__header",
            span(class = "command-panel__kicker", "Reference cases"),
            span(class = "command-panel__icon", icon("book-open"))
          ),
          mod_examples_ui("examples")
        ),
        div(class = "command-panel command-panel--compare",
          div(class = "command-panel__header",
            span(class = "command-panel__kicker", "Side-by-side"),
            span(class = "command-panel__icon", icon("exchange-alt"))
          ),
          mod_compare_ui("compare")
        )
      )
    )
  ),

  # -- Home ------------------------------------------------------------------
  tabPanel("Home", value = "home",
    mod_home_ui("home")
  ),

  # -- Documentation ---------------------------------------------------------
  tabPanel("Documentation", value = "documentation",
    mod_documentation_ui("documentation")
  ),

  # -- Results (dropdown) ----------------------------------------------------
  navbarMenu("Precision",
    tabPanel("Parameter Table", value = "params", mod_params_ui("params")),
    tabPanel("RSE / SE",       value = "rse",    mod_rse_ui("rse")),
    tabPanel("RELATIVEINF", value = "ri",     mod_relativeinf_ui("ri"))
  ),

  # -- Design (dropdown) -----------------------------------------------------
  navbarMenu("Design",
    tabPanel("FIM Diagnostics", value = "fim",   mod_fim_ui("fim")),
    tabPanel("Sampling Times",  value = "times", mod_times_ui("times")),
    tabPanel("Robust Design",   value = "prior", mod_prior_ui("prior")),
    tabPanel("Dataset Builder", value = "dataset_builder", mod_dataset_builder_ui("dataset_builder"))
  ),

  # -- Decision (dropdown) ---------------------------------------------------
  navbarMenu("Decision",
    tabPanel("Power / NSN", value = "power", mod_power_ui("power")),
    tabPanel("Covariates", value = "covariates", mod_covariates_ui("covariates"))
  ),

  # -- Diagnostic (dropdown) -------------------------------------------------
  navbarMenu("Diagnostic",
    tabPanel("Convergence",    value = "conv", mod_convergence_ui("conv")),
    tabPanel("Control Stream", value = "ctl",  mod_ctl_stream_ui("ctl")),
    tabPanel("Raw Data",       value = "raw",  mod_raw_ui("raw"))
  ),

  # -- Validation (dropdown) -------------------------------------------------
  navbarMenu("Validation",
    tabPanel("Upload SSE",        value = "sse_upload",    mod_sse_upload_ui("sse_upload")),
    tabPanel("FIM vs SSE",        value = "sse",           mod_sse_validation_ui("sse")),
    tabPanel("SSE Diagnostics",   value = "sse_analysis",  mod_sse_analysis_ui("sse_analysis")),
    tabPanel("Design Comparison", value = "sse_comparison", mod_sse_comparison_ui("sse_comparison"))
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

  # -- Shared session state ----------------------------------------------------
  reset_trigger <- reactiveVal(0L)
  selected_table_no <- reactiveVal(NULL)
  primary_context <- reactiveVal(empty_primary_run_context())

  observeEvent(input$open_doc_section, {
    updateNavbarPage(session, "navbar", selected = "documentation")
    session$sendCustomMessage("scroll-doc-section", input$open_doc_section)
  }, ignoreInit = TRUE)

  # -- Upload module ----------------------------------------------------------
  upload   <- mod_upload_server("upload",   reset_trigger = reset_trigger)
  compare  <- mod_compare_server("compare", reset_trigger = reset_trigger)
  examples <- mod_examples_server("examples", reset_trigger = reset_trigger)

  .context_source_id <- function(paths) {
    if (!primary_run_paths_present(paths)) return(NULL)
    present <- unlist(paths[!vapply(paths, is.null, logical(1L))], use.names = FALSE)
    paste(sort(unique(basename(present))), collapse = "|")
  }

  .example_ctl_path <- function(paths) {
    if (is.null(paths) || is.null(paths$ext)) return(NULL)
    stem <- tools::file_path_sans_ext(basename(paths$ext))
    candidates <- file.path(
      dirname(paths$ext),
      paste0(stem, c(".ctl", ".mod", ".con"))
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) NULL else candidates[[1L]]
  }

  .context_cmt_labels <- function(ctl_lines) {
    if (is.null(ctl_lines)) return(NULL)
    tryCatch(parse_cmt_labels(ctl_lines), error = function(e) NULL)
  }

  .context_groupsize <- function(ctl_lines) {
    if (is.null(ctl_lines)) return(1L)
    value <- tryCatch(parse_groupsize(ctl_lines), error = function(e) NA_integer_)
    if (is.na(value) || value < 1L) 1L else as.integer(value)
  }

  .example_comparison <- function(paths, name) {
    if (is.null(paths)) return(NULL)
    list(
      id = "example_comp",
      name = name %||% "Run B",
      ext_data = .safe_load(read_ext, paths$ext, ".ext (comp)"),
      shk_data = .safe_load(read_shk, paths$shk, ".shk (comp)"),
      coi_data = .safe_load(read_coi, paths$coi, ".coi (comp)"),
      clt_data = .safe_load(read_clt, paths$clt, ".clt (comp)"),
      tab_data = .safe_load(read_tab, paths$tab, ".tab (comp)"),
      cpu_data = read_cpu(paths$cpu %||% "")
    )
  }

  upload_context <- reactive({
    paths <- upload$file_paths()
    if (!primary_run_paths_present(paths) || is.null(paths$ext)) return(NULL)

    ext_data <- upload$ext_data()
    if (is.null(ext_data)) return(NULL)
    ctl_lines <- upload$ctl_lines()
    tab_data <- upload$tab_data()
    run_name <- tryCatch(parse_design_summary(ctl_lines), error = function(e) NULL)
    summary_data <- if (is.null(tab_data)) {
      NULL
    } else {
      tryCatch(compute_robust_summary(tab_data), error = function(e) NULL)
    }

    new_primary_run_context(
      source = "upload",
      source_id = .context_source_id(paths),
      name = run_name %||% "Primary",
      file_paths = paths,
      ext_data = ext_data,
      shk_data = upload$shk_data(),
      coi_data = upload$coi_data(),
      clt_data = upload$clt_data(),
      tab_data = tab_data,
      ctl_data = upload$ctl_data(),
      cpu_data = upload$cpu_data(),
      ext_lines = upload$ext_lines(),
      ctl_lines = ctl_lines,
      summary_data = summary_data,
      comparison = NULL,
      param_labels = NULL,
      cmt_labels = .context_cmt_labels(ctl_lines),
      groupsize = .context_groupsize(ctl_lines)
    )
  })

  example_context <- reactive({
    selection <- examples$selection()
    if (is.null(selection)) return(NULL)

    paths <- selection$file_paths
    if (!primary_run_paths_present(paths) || is.null(paths$ext)) return(NULL)

    ctl_path <- .example_ctl_path(paths)
    ctl_lines <- if (is.null(ctl_path)) {
      NULL
    } else {
      tryCatch(readr::read_lines(ctl_path), error = function(e) NULL)
    }
    ext_lines <- if (is.null(paths$ext)) {
      NULL
    } else {
      tryCatch(
        readr::read_lines(paths$ext, progress = FALSE),
        error = function(e) NULL
      )
    }

    ext_data <- .safe_load(read_ext, paths$ext, ".ext")
    if (is.null(ext_data)) return(NULL)

    new_primary_run_context(
      source = "example",
      source_id = paste0(.context_source_id(paths), "@", selection$revision),
      name = "Primary",
      file_paths = paths,
      ext_data = ext_data,
      shk_data = .safe_load(read_shk, paths$shk, ".shk"),
      coi_data = .safe_load(read_coi, paths$coi, ".coi"),
      clt_data = .safe_load(read_clt, paths$clt, ".clt"),
      tab_data = .safe_load(read_tab, paths$tab, ".tab"),
      ctl_data = .safe_load(read_prior_nwpri, ctl_path, ".ctl/.mod/.con"),
      cpu_data = read_cpu(paths$cpu %||% ""),
      ext_lines = ext_lines,
      ctl_lines = ctl_lines,
      summary_data = selection$summary_data,
      comparison = .example_comparison(
        selection$compare_paths,
        selection$compare_name
      ),
      table_no_range = selection$table_no_range,
      param_labels = parse_mapping_text(selection$labels),
      cmt_labels = .context_cmt_labels(ctl_lines),
      groupsize = .context_groupsize(ctl_lines)
    )
  })

  observeEvent(examples$selection(), {
    if (is.null(examples$selection())) return()

    context <- example_context()
    if (is.null(context)) {
      showNotification(
        "Example not activated: its primary .ext file could not be read.",
        type = "error"
      )
      return()
    }
    primary_context(activate_primary_run_context(
      isolate(primary_context()),
      context
    ))
    selected_table_no(NULL)
  }, ignoreNULL = FALSE)

  observeEvent(upload$file_paths(), {
    paths <- upload$file_paths()
    if (!primary_run_paths_present(paths)) return()

    context <- upload_context()
    if (is.null(context)) {
      showNotification(
        "Upload not activated: a readable primary .ext file is required.",
        type = "warning"
      )
      return()
    }
    primary_context(activate_primary_run_context(
      isolate(primary_context()),
      context
    ))
    selected_table_no(NULL)
  }, ignoreNULL = FALSE)

  suggested_gs <- reactive(primary_context()$groupsize)
  merged_ext <- reactive(primary_context()$ext_data)
  merged_shk <- reactive(primary_context()$shk_data)
  merged_coi <- reactive(primary_context()$coi_data)
  merged_clt <- reactive(primary_context()$clt_data)
  merged_tab <- reactive(primary_context()$tab_data)
  merged_ctl <- reactive(primary_context()$ctl_data)
  merged_cpu <- reactive(primary_context()$cpu_data)
  merged_summary <- reactive(primary_context()$summary_data)
  merged_ctl_lines <- reactive(primary_context()$ctl_lines)
  merged_ext_lines <- reactive(primary_context()$ext_lines)
  merged_true_vals <- reactive({
    tryCatch(
      resolve_true_values(
        shared_true_vals = reactive(NULL),
        shared_ctl_lines = merged_ctl_lines
      ),
      error = function(e) NULL
    )
  })

  output$global_run_label <- renderText({
    primary_name()
  })

  observe({
    ext <- merged_ext()
    if (is.null(ext) || !"table_no" %in% names(ext)) {
      updateSelectInput(session, "global_table_no",
                        choices = "1", selected = "1")
      selected_table_no(NULL)
      return()
    }

    tabs <- sort(unique(ext$table_no))
    tnr  <- primary_context()$table_no_range
    if (!is.null(tnr) && length(tnr) == 2L) {
      tabs <- tabs[tabs >= tnr[1L] & tabs <= tnr[2L]]
      if (length(tabs) == 0L) tabs <- sort(unique(ext$table_no))
    }

    current <- isolate(selected_table_no())
    selected <- if (!is.null(current) && current %in% tabs) current else max(tabs)
    updateSelectInput(session, "global_table_no",
      choices  = setNames(as.character(tabs), as.character(tabs)),
      selected = as.character(selected))
    selected_table_no(selected)
  })

  observeEvent(input$global_table_no, {
    val <- as.integer(input$global_table_no)
    if (!is.na(val)) selected_table_no(val)
  }, ignoreInit = TRUE)

  # -- all_runs ---------------------------------------------------------------
  primary_name <- reactive(primary_context()$name %||% "Primary")

  all_runs <- reactive({
    primary <- new_design_run(
      name = primary_name(),
      ext_data = merged_ext(),
      shk_data = merged_shk(),
      coi_data = merged_coi(),
      clt_data = merged_clt(),
      tab_data = merged_tab(),
      cpu_data = merged_cpu()
    )
    runs <- list(primary = primary)
    ex_comp <- primary_context()$comparison
    if (!is.null(ex_comp) && !is.null(ex_comp$ext_data))
      runs[["example_comp"]] <- ex_comp
    comp <- compare$comp_runs()
    for (rid in names(comp)) {
      r <- comp[[rid]]
      runs[[rid]] <- new_design_run(
        name = r$name,
        ext_data = r$ext_data,
        shk_data = r$shk_data,
        coi_data = r$coi_data,
        clt_data = r$clt_data,
        tab_data = r$tab_data,
        cpu_data = r$cpu_data
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
  # tbl_no is app/run context; groupsize remains exposed by mod_power.
  tbl_no <- reactive(selected_table_no())
  param_labels_r <- reactive(primary_context()$param_labels)
  cmt_labels_r <- reactive(primary_context()$cmt_labels)
  # -- Reset handler (triggered by mod_home) -----------------------------------
  observeEvent(reset_trigger(), {
    primary_context(empty_primary_run_context())
    selected_table_no(NULL)
  }, ignoreInit = TRUE)

  # -- Module servers ---------------------------------------------------------
  # mod_power renders the TABLE NO. / GROUPSIZE controls. TABLE NO. is stored
  # in app-level run context so sibling modules consume a neutral reactive.
  power_out <- mod_power_server("power",
    ext_data            = merged_ext,
    param_labels        = param_labels_r,
    tbl_no              = tbl_no,
    suggested_groupsize = suggested_gs,
    reset_trigger       = reset_trigger,
    all_runs            = all_runs)

  groupsize_r <- power_out$groupsize

  mod_params_server("params",
    ext_data = merged_ext, shk_data = merged_shk,
    ext_lines = merged_ext_lines,
    param_labels = param_labels_r,
    tbl_no = tbl_no,
    all_runs = all_runs)

  mod_rse_server("rse",
    ext_data = merged_ext, tbl_no = tbl_no,
    param_labels = param_labels_r, all_runs = all_runs)

  mod_relativeinf_server("ri",
    shk_data = merged_shk, tbl_no = tbl_no,
    param_labels = param_labels_r, all_runs = all_runs)

  mod_fim_server("fim",
    ext_data = merged_ext, coi_data = merged_coi, clt_data = merged_clt,
    tbl_no = tbl_no, param_labels = param_labels_r, all_runs = all_runs,
    ctl_lines = merged_ctl_lines)

  mod_covariates_server("covariates",
    ext_data = merged_ext, tbl_no = tbl_no, param_labels = param_labels_r)

  shared_mrgsolve <- mod_mrgsolve_server("times-mrgsolve",
    ext_data     = merged_ext,
    tab_data     = merged_tab,
    ctl_lines    = merged_ctl_lines,
    theta_labels = param_labels_r)

  mod_times_server("times",
    tab_data = merged_tab, all_runs = all_runs, cmt_labels = cmt_labels_r,
    ext_data = merged_ext, ctl_lines = merged_ctl_lines,
    theta_labels = param_labels_r,
    mrgsolve_state = shared_mrgsolve)

  mod_prior_server("prior",
    summary_data = merged_summary, ctl_data = merged_ctl,
    all_runs = all_runs)

  mod_dataset_builder_server("dataset_builder", reset_trigger = reset_trigger)

  mod_convergence_server("conv",
    ext_data = merged_ext, all_runs = all_runs,
    ctl_lines = merged_ctl_lines,
    cpu_secs = merged_cpu)

  mod_raw_server("raw",
    ext_data = merged_ext, all_runs = all_runs)

  mod_ctl_stream_server("ctl",
    ctl_lines = merged_ctl_lines)

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
    clt_data         = merged_clt,
    tbl_no           = tbl_no)

  mod_sse_analysis_server("sse_analysis",
    design_a_shared = sse_upload$design_a,
    design_b_shared = sse_upload$design_b,
    true_vals    = merged_true_vals,
    param_labels = param_labels_r,
    mrgsolve_state = shared_mrgsolve)

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
    merged_shk   = merged_shk,
    merged_coi   = merged_coi,
    merged_clt   = merged_clt,
    merged_cpu   = merged_cpu,
    merged_tab   = merged_tab,
    merged_ctl_lines = merged_ctl_lines,
    merged_true_vals = merged_true_vals,
    ext_lines    = merged_ext_lines,
    primary_name = primary_name,
    tbl_no       = tbl_no,
    param_labels = param_labels_r,
    groupsize    = groupsize_r,
    all_runs     = all_runs,
    sse_a_data   = sse_upload$sse_a_data,
    sse_b_data   = sse_upload$sse_b_data,
    sse_name_a   = sse_upload$name_a,
    sse_name_b   = sse_upload$name_b,
    reset_trigger = reset_trigger)
}


# =============================================================================
shinyApp(ui = ui, server = server)
