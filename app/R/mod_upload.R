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

    empty_paths <- function() {
      stats::setNames(
        vector("list", 9L),
        c("ext", "shk", "coi", "clt", "tab", "bfm", "cpu", "ctl", "mrg_cpp")
      )
    }

    empty_state <- function() {
      list(
        file_paths = empty_paths(),
        ext_lines = NULL,
        ctl_lines = NULL,
        ext_data = NULL,
        shk_data = NULL,
        coi_data = NULL,
        clt_data = NULL,
        tab_data = NULL,
        ctl_data = NULL,
        bfm_data = NULL,
        cpu_data = NA_real_
      )
    }

    upload_state <- reactiveVal(empty_state())

    file_paths <- reactive(upload_state()$file_paths)
    ext_lines_raw <- reactive(upload_state()$ext_lines)
    ctl_lines_raw <- reactive(upload_state()$ctl_lines)

    # Reset universel
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        upload_state(empty_state())
      }, ignoreInit = TRUE)
    }

    parse_file <- function(parser, path, label) {
      if (is.null(path)) return(NULL)

      tryCatch(
        parser(path),
        error = function(e) {
          stop(
            "Error ", label, " [", basename(path), "]: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
    }

    build_upload_state <- function(files) {
      paths <- extract_design_files(files, "design")

      list(
        file_paths = paths,
        ext_lines = parse_file(
          function(path) readr::read_lines(path, progress = FALSE),
          paths$ext,
          ".ext"
        ),
        ctl_lines = parse_file(
          function(path) readr::read_lines(path, progress = FALSE),
          paths$ctl,
          ".ctl"
        ),
        ext_data = parse_file(read_ext, paths$ext, ".ext"),
        shk_data = parse_file(read_shk, paths$shk, ".shk"),
        coi_data = parse_file(read_coi, paths$coi, ".coi"),
        clt_data = parse_file(read_clt, paths$clt, ".clt"),
        tab_data = parse_file(read_tab, paths$tab, ".tab"),
        ctl_data = parse_file(read_prior_nwpri, paths$ctl, ".ctl"),
        # bfm: pas encore consomme — schema ETC different de .ext, attente read_bfm()
        bfm_data = NULL,
        cpu_data = read_cpu(paths$cpu %||% "")
      )
    }

    observeEvent(input$upload, {
      files <- input$upload
      req(files)

      next_state <- tryCatch(
        build_upload_state(files),
        error = function(e) {
          if (exists("log_error", mode = "function", inherits = TRUE)) {
            log_error("Upload rejected: ", conditionMessage(e))
          }
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      if (!is.null(next_state)) upload_state(next_state)
    }, ignoreNULL = TRUE)

    ext_data <- reactive(upload_state()$ext_data)
    shk_data <- reactive(upload_state()$shk_data)
    coi_data <- reactive(upload_state()$coi_data)
    clt_data <- reactive(upload_state()$clt_data)
    tab_data <- reactive(upload_state()$tab_data)
    ctl_data <- reactive(upload_state()$ctl_data)
    bfm_data <- reactive(upload_state()$bfm_data)
    cpu_data <- reactive(upload_state()$cpu_data)

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
