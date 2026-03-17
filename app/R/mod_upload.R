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
                accept   = c(".ext", ".shk", ".coi", ".clt", ".tab",
                             ".ctl", ".tar.gz", ".tgz", ".gz"),
                buttonLabel = "Parcourir"),
      helpText("Upload un .tar.gz (workflow nrm) ou plusieurs fichiers individuels."),
      uiOutput(ns("file_status"))
    )
  )
}

mod_upload_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {

    # Limite de taille de requete : 100 MB
    options(shiny.maxRequestSize = 100 * 1024^2)

    # Chemins fichiers detectes
    file_paths <- reactiveVal(list(
      ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, ctl = NULL,
      summary_tab = NULL
    ))

    # Lignes brutes .ext (pour detect_criterion)
    ext_lines_raw <- reactiveVal(NULL)

    # Universal reset from app.R
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        file_paths(list(ext = NULL, shk = NULL, coi = NULL, clt = NULL,
                        tab = NULL, ctl = NULL, summary_tab = NULL))
        ext_lines_raw(NULL)
      })
    }

    observe({
      files <- input$upload
      req(files)

      # Validation taille fichier (50 MB max par fichier)
      oversized <- files$name[file.info(files$datapath)$size > 50 * 1024^2]
      if (length(oversized) > 0) {
        showNotification(
          paste("Fichier(s) trop volumineux (max 50 MB) :", paste(oversized, collapse = ", ")),
          type = "error"
        )
        return()
      }

      showNotification("Chargement en cours...", id = "upload_loading",
                       duration = NULL, type = "message")
      on.exit(removeNotification("upload_loading"), add = TRUE)

      paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, ctl = NULL,
                    summary_tab = NULL)

      if (nrow(files) == 1L && grepl("\\.(tar\\.gz|tgz)$", files$name, ignore.case = TRUE)) {
        # tar.gz : extraire dans un dossier temporaire
        tmp <- file.path(tempdir(), paste0("design_", format(Sys.time(), "%H%M%S")))
        dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
        untar(files$datapath, exdir = tmp)

        all_files <- list.files(tmp, recursive = TRUE, full.names = TRUE)
        all_names <- basename(all_files)

        ext_i     <- which(grepl("\\.ext$", all_names, ignore.case = TRUE))[1]
        shk_i     <- which(grepl("\\.shk$", all_names, ignore.case = TRUE))[1]
        coi_i     <- which(grepl("\\.coi$", all_names, ignore.case = TRUE))[1]
        clt_i     <- which(grepl("\\.clt$", all_names, ignore.case = TRUE))[1]
        ctl_i     <- which(grepl("\\.ctl$", all_names, ignore.case = TRUE))[1]
        # summary.tab detecte en priorite, sinon premier .tab
        sum_i     <- which(grepl("summary.*\\.tab$", all_names, ignore.case = TRUE))[1]
        tab_i     <- if (!is.na(sum_i)) NA_integer_ else which(grepl("\\.tab$", all_names, ignore.case = TRUE))[1]

        if (!is.na(ext_i)) paths$ext <- all_files[ext_i]
        if (!is.na(shk_i)) paths$shk <- all_files[shk_i]
        if (!is.na(coi_i)) paths$coi <- all_files[coi_i]
        if (!is.na(clt_i)) paths$clt <- all_files[clt_i]
        if (!is.na(tab_i)) paths$tab <- all_files[tab_i]
        if (!is.na(ctl_i)) paths$ctl <- all_files[ctl_i]
        if (!is.na(sum_i)) paths$summary_tab <- all_files[sum_i]

      } else {
        # Fichiers multiples : matcher par nom original
        for (i in seq_len(nrow(files))) {
          nm <- files$name[i]
          dp <- files$datapath[i]
          if (grepl("\\.ext$", nm, ignore.case = TRUE)) paths$ext <- dp
          if (grepl("\\.shk$", nm, ignore.case = TRUE)) paths$shk <- dp
          if (grepl("\\.coi$", nm, ignore.case = TRUE)) paths$coi <- dp
          if (grepl("\\.clt$", nm, ignore.case = TRUE)) paths$clt <- dp
          if (grepl("\\.ctl$", nm, ignore.case = TRUE)) paths$ctl <- dp
          # summary.tab prioritaire sur .tab ordinaire
          if (grepl("summary.*\\.tab$", nm, ignore.case = TRUE)) {
            paths$summary_tab <- dp
          } else if (grepl("\\.tab$", nm, ignore.case = TRUE)) {
            paths$tab <- dp
          }
        }
      }

      file_paths(paths)

      # Lire les lignes brutes .ext pour detect_criterion
      if (!is.null(paths$ext)) {
        ext_lines_raw(readr::read_lines(paths$ext, progress = FALSE))
      }
    })

    # Donnees parsees (reactives)
    ext_data <- reactive({
      p <- file_paths()$ext
      if (is.null(p)) return(NULL)
      tryCatch(read_ext(p), error = function(e) {
        showNotification(paste("Erreur .ext :", e$message), type = "error"); NULL
      })
    })

    shk_data <- reactive({
      p <- file_paths()$shk
      if (is.null(p)) return(NULL)
      tryCatch(read_shk(p), error = function(e) {
        showNotification(paste("Erreur .shk :", e$message), type = "error"); NULL
      })
    })

    coi_data <- reactive({
      p <- file_paths()$coi
      if (is.null(p)) return(NULL)
      tryCatch(read_coi(p), error = function(e) {
        showNotification(paste("Erreur .coi :", e$message), type = "error"); NULL
      })
    })

    clt_data <- reactive({
      p <- file_paths()$clt
      if (is.null(p)) return(NULL)
      tryCatch(read_clt(p), error = function(e) {
        showNotification(paste("Erreur .clt :", e$message), type = "error"); NULL
      })
    })

    tab_data <- reactive({
      p <- file_paths()$tab
      if (is.null(p)) return(NULL)
      tryCatch(read_tab(p), error = function(e) {
        showNotification(paste("Erreur .tab :", e$message), type = "error"); NULL
      })
    })

    ctl_data <- reactive({
      p <- file_paths()$ctl
      if (is.null(p)) return(NULL)
      tryCatch(read_prior_nwpri(p), error = function(e) {
        showNotification(paste("Erreur .ctl :", e$message), type = "error"); NULL
      })
    })

    summary_data <- reactive({
      p <- file_paths()$summary_tab
      if (is.null(p)) return(NULL)
      tryCatch(read_summary_tab(p), error = function(e) {
        showNotification(paste("Erreur summary.tab :", e$message), type = "error"); NULL
      })
    })

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
        status_line(".ctl", p$ctl)
      )
    })

    # Retourner les reactives
    list(
      ext_data     = ext_data,
      shk_data     = shk_data,
      coi_data     = coi_data,
      clt_data     = clt_data,
      tab_data     = tab_data,
      ctl_data     = ctl_data,
      summary_data = summary_data,
      ext_lines    = ext_lines_raw,
      file_paths   = file_paths
    )
  })
}
