# =============================================================================
# mod_upload.R — Module upload (tar.gz ou fichiers multiples)
# =============================================================================

mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "upload-box",
      tags$h6("Fichiers NONMEM"),
      fileInput(ns("upload"), "Dossier tar.gz ou fichiers",
                multiple = TRUE,
                accept   = c(".ext", ".shk", ".coi", ".clt", ".tab", ".bfm",
                             ".ctl", ".mod", ".con", ".tar.gz", ".tgz", ".gz"),
                buttonLabel = "Parcourir"),
      helpText("Upload un .tar.gz (workflow nrm) ou plusieurs fichiers individuels."),
      uiOutput(ns("file_status"))
    )
  )
}

mod_upload_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {

    # Chemins fichiers detectes
    file_paths <- reactiveVal(list(
      ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, ctl = NULL, bfm = NULL
    ))

    # Lignes brutes .ext (pour detect_criterion) et .ctl (pour labels/design summary)
    ext_lines_raw <- reactiveVal(NULL)
    ctl_lines_raw <- reactiveVal(NULL)

    # Reset universel
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        file_paths(list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, ctl = NULL, bfm = NULL))
        ext_lines_raw(NULL)
        ctl_lines_raw(NULL)
      }, ignoreInit = TRUE)
    }

    observe({
      files <- input$upload
      req(files)

      paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, ctl = NULL, bfm = NULL)

      if (nrow(files) == 1L && grepl("\\.(tar\\.gz|tgz)$", files$name, ignore.case = TRUE)) {
        # tar.gz : extraire dans un dossier temporaire
        tmp <- file.path(tempdir(), paste0("design_", format(Sys.time(), "%H%M%S")))
        dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
        untar(files$datapath, exdir = tmp)

        all_files <- list.files(tmp, recursive = TRUE, full.names = TRUE)
        all_names <- basename(all_files)

        ext_i <- which(grepl("\\.ext$", all_names, ignore.case = TRUE))[1]
        shk_i <- which(grepl("\\.shk$", all_names, ignore.case = TRUE))[1]
        coi_i <- which(grepl("\\.coi$", all_names, ignore.case = TRUE))[1]
        clt_i <- which(grepl("\\.clt$", all_names, ignore.case = TRUE))[1]
        tab_i <- which(grepl("\\.tab$", all_names, ignore.case = TRUE))[1]
        ctl_i <- which(grepl("\\.(ctl|mod|con)$", all_names, ignore.case = TRUE))[1]
        bfm_i <- which(grepl("\\.bfm$", all_names, ignore.case = TRUE))[1]

        if (!is.na(ext_i)) paths$ext <- all_files[ext_i]
        if (!is.na(shk_i)) paths$shk <- all_files[shk_i]
        if (!is.na(coi_i)) paths$coi <- all_files[coi_i]
        if (!is.na(clt_i)) paths$clt <- all_files[clt_i]
        if (!is.na(tab_i)) paths$tab <- all_files[tab_i]
        if (!is.na(ctl_i)) paths$ctl <- all_files[ctl_i]
        if (!is.na(bfm_i)) paths$bfm <- all_files[bfm_i]

      } else {
        # Fichiers multiples : matcher par nom original
        for (i in seq_len(nrow(files))) {
          nm <- files$name[i]
          dp <- files$datapath[i]
          if (grepl("\\.ext$", nm, ignore.case = TRUE)) paths$ext <- dp
          if (grepl("\\.shk$", nm, ignore.case = TRUE)) paths$shk <- dp
          if (grepl("\\.coi$", nm, ignore.case = TRUE)) paths$coi <- dp
          if (grepl("\\.clt$", nm, ignore.case = TRUE)) paths$clt <- dp
          if (grepl("\\.tab$", nm, ignore.case = TRUE)) paths$tab <- dp
          if (grepl("\\.(ctl|mod|con)$", nm, ignore.case = TRUE)) paths$ctl <- dp
          if (grepl("\\.bfm$", nm, ignore.case = TRUE)) paths$bfm <- dp
        }
      }

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
        showNotification(paste0("Erreur ", label, " : ", e$message), type = "error")
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

    # Status fichiers
    output$file_status <- renderUI({
      p <- file_paths()
      status_line <- function(ext_name, detected, required = FALSE) {
        if (!is.null(detected)) {
          div(span(class = "status-ok", paste(ext_name, "detecte")))
        } else if (required) {
          div(span(class = "status-miss", paste(ext_name, "requis")))
        } else {
          div(span(class = "status-miss", paste(ext_name, "optionnel")))
        }
      }
      tagList(
        status_line(".ext", p$ext, required = TRUE),
        status_line(".shk", p$shk),
        status_line(".coi", p$coi),
        status_line(".clt", p$clt),
        status_line(".tab", p$tab),
        status_line(".ctl/.mod", p$ctl),
        status_line(".bfm", p$bfm)
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
      ext_lines  = ext_lines_raw,
      ctl_lines  = ctl_lines_raw,
      file_paths = file_paths
    )
  })
}
