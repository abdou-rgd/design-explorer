# =============================================================================
# mod_upload.R — Module upload (tar.gz ou fichiers multiples)
# =============================================================================

mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "upload-box",
      tags$h6("NONMEM Files"),
      fileInput(ns("upload"), "tar.gz archive or individual files",
                multiple = TRUE,
                accept   = c(".ext", ".shk", ".coi", ".clt", ".tab", ".bfm", ".cpu",
                             ".ctl", ".mod", ".con", ".tar.gz", ".tgz"),
                buttonLabel = "Browse"),
      helpText("Upload a .tar.gz (nrm workflow) or multiple individual files."),
      helpText(style = "font-size:0.82em; color:#854d0e;",
        "Tip: load NONMEM outputs (.ext, .ctl, ...) here first,",
        " then upload SSE results in the Validation tab."
      ),
      uiOutput(ns("file_status"))
    )
  )
}

mod_upload_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {

    # Chemins fichiers detectes
    file_paths <- reactiveVal(list(
      ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, ctl = NULL, bfm = NULL, cpu = NULL
    ))

    # Lignes brutes .ext (pour detect_criterion) et .ctl (pour labels/design summary)
    ext_lines_raw <- reactiveVal(NULL)
    ctl_lines_raw <- reactiveVal(NULL)

    # Reset universel
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        file_paths(list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, ctl = NULL, bfm = NULL, cpu = NULL))
        ext_lines_raw(NULL)
        ctl_lines_raw(NULL)
      }, ignoreInit = TRUE)
    }

    observe({
      files <- input$upload
      req(files)

      paths <- extract_design_files(files, "design")

      file_paths(paths)

      # Lire les lignes brutes .ext (detect_criterion) et .ctl (labels/design)
      if (!is.null(paths$ext)) {
        ext_lines_raw(readr::read_lines(paths$ext, progress = FALSE))
      }
      if (!is.null(paths$ctl)) {
        ctl_lines_raw(readr::read_lines(paths$ctl, progress = FALSE))
      } else {
        ctl_lines_raw(NULL)
      }
    })

    # Donnees parsees (reactives)
    .safe_parse <- function(parser, path, label) {
      if (is.null(path)) return(NULL)
      tryCatch(parser(path), error = function(e) {
        log_error("Parse ", label, " [", basename(path), "]: ", e$message)
        showNotification(paste0("Error ", label, ": ", e$message), type = "error")
        NULL
      })
    }

    ext_data <- reactive(.safe_parse(read_ext,          file_paths()$ext, ".ext"))
    shk_data <- reactive(.safe_parse(read_shk,          file_paths()$shk, ".shk"))
    coi_data <- reactive(.safe_parse(read_coi,          file_paths()$coi, ".coi"))
    clt_data <- reactive(.safe_parse(read_clt,          file_paths()$clt, ".clt"))
    tab_data <- reactive(.safe_parse(read_tab,          file_paths()$tab, ".tab"))
    ctl_data <- reactive(.safe_parse(read_prior_nwpri,  file_paths()$ctl, ".ctl"))
    # bfm: pas encore consomme — schema ETC different de .ext, attente read_bfm()
    bfm_data <- reactive(NULL)
    cpu_data <- reactive(read_cpu(file_paths()$cpu %||% ""))

    # Status fichiers
    output$file_status <- renderUI({
      p <- file_paths()
      status_line <- function(ext_name, detected, required = FALSE) {
        if (!is.null(detected)) {
          div(span(class = "status-ok", paste(ext_name, "detected")))
        } else if (required) {
          div(span(class = "status-miss", paste(ext_name, "required")))
        } else {
          div(span(class = "status-miss", paste(ext_name, "optional")))
        }
      }
      tagList(
        status_line(".ext", p$ext, required = TRUE),
        status_line(".shk", p$shk),
        status_line(".coi", p$coi),
        status_line(".clt", p$clt),
        status_line(".tab", p$tab),
        status_line(".ctl/.mod", p$ctl),
        status_line(".bfm", p$bfm),
        status_line(".cpu", p$cpu)
      )
    })

    # Retourner les reactives
    list(
      ext_data   = ext_data,
      shk_data   = shk_data,
      coi_data   = coi_data,
      clt_data   = clt_data,
      tab_data   = tab_data,
      ctl_data   = ctl_data,
      bfm_data   = bfm_data,
      cpu_data   = cpu_data,
      ext_lines  = ext_lines_raw,
      ctl_lines  = ctl_lines_raw,
      file_paths = file_paths
    )
  })
}
