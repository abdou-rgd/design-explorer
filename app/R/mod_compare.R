# =============================================================================
# mod_compare.R -- Multi-run comparison management
# =============================================================================

mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "upload-box",
      tags$h6("Comparaison"),
      actionButton(ns("add_run"), "Ajouter un run", icon = icon("plus"),
                   class = "btn-sm btn-outline-primary w-100"),
      uiOutput(ns("run_list"))
    )
  )
}

mod_compare_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Separate UI state from data state to avoid fileInput reset
    run_ids   <- reactiveVal(character())   # drives UI rendering
    run_names <- reactiveValues()            # editable names
    run_data  <- reactiveValues()            # parsed data (does NOT drive UI)
    id_counter  <- reactiveVal(0L)   # always-increasing ID generator (never decremented)
    run_count   <- reactiveVal(0L)   # number of currently active runs (increments/decrements)

    # Mutable list of observers keyed by run_id (use <<- from handlers)
    run_observers <- list()

    observeEvent(input$add_run, {
      if (run_count() >= 3L) {
        showNotification("Maximum 3 runs de comparaison (4 total)",
                         type = "warning")
        return()
      }
      new_id <- id_counter() + 1L
      id_counter(new_id)
      run_count(run_count() + 1L)
      rid <- paste0("run_", new_id)
      default_name <- c("Run B", "Run C", "Run D")[run_count()]

      run_names[[rid]] <- default_name
      run_data[[rid]] <- list(
        ext_data = NULL, shk_data = NULL, coi_data = NULL,
        clt_data = NULL, tab_data = NULL
      )

      # Append to IDs (this triggers UI render)
      run_ids(c(run_ids(), rid))
    })

    # Render run list -- depends ONLY on run_ids(), not on run_data
    output$run_list <- renderUI({
      ids <- run_ids()
      if (length(ids) == 0L) return(NULL)

      run_uis <- lapply(ids, function(rid) {
        rname <- isolate(run_names[[rid]])
        color <- run_color(rname)
        has_data <- !is.null(isolate(run_data[[rid]]$ext_data))

        div(class = "upload-box",
            style = paste0("border-left: 3px solid ", color, ";"),
          fluidRow(
            column(8, textInput(ns(paste0("name_", rid)), NULL,
                                value = rname, width = "100%")),
            column(4, actionButton(ns(paste0("rm_", rid)), NULL,
                                   icon = icon("xmark"),
                                   class = "btn-sm btn-outline-danger"))
          ),
          fileInput(ns(paste0("upload_", rid)), NULL, multiple = TRUE,
                    accept = c(".ext", ".shk", ".coi", ".clt", ".tab",
                               ".tar.gz", ".tgz", ".gz"),
                    buttonLabel = "Fichiers"),
          uiOutput(ns(paste0("status_", rid)))
        )
      })
      tagList(run_uis)
    })

    # Track which run IDs have had observers created
    observed_ids <- reactiveVal(character())

    observe({
      current <- run_ids()
      already <- observed_ids()
      new_ids <- setdiff(current, already)

      for (rid in new_ids) {
        local({
          local_rid <- rid

          # File upload observer
          obs_upload <- observeEvent(input[[paste0("upload_", local_rid)]], {
            files <- input[[paste0("upload_", local_rid)]]
            req(files)
            paths <- list(ext = NULL, shk = NULL, coi = NULL,
                          clt = NULL, tab = NULL)

            if (nrow(files) == 1L &&
                grepl("\\.(tar\\.gz|tgz)$", files$name,
                      ignore.case = TRUE)) {
              tmp <- file.path(tempdir(),
                paste0("comp_", local_rid, "_",
                       format(Sys.time(), "%H%M%S")))
              dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
              untar(files$datapath, exdir = tmp)
              all_f <- list.files(tmp, recursive = TRUE,
                                  full.names = TRUE)
              all_n <- basename(all_f)
              for (et in c("ext", "shk", "coi", "clt", "tab")) {
                idx <- which(grepl(paste0("\\.", et, "$"), all_n,
                                   ignore.case = TRUE))[1]
                if (!is.na(idx)) paths[[et]] <- all_f[idx]
              }
            } else {
              for (i in seq_len(nrow(files))) {
                nm <- files$name[i]
                dp <- files$datapath[i]
                for (et in c("ext", "shk", "coi", "clt", "tab")) {
                  if (grepl(paste0("\\.", et, "$"), nm,
                            ignore.case = TRUE))
                    paths[[et]] <- dp
                }
              }
            }

            # Parse and store in run_data (does NOT trigger UI re-render)
            parsed <- list(
              ext_data = NULL, shk_data = NULL, coi_data = NULL,
              clt_data = NULL, tab_data = NULL
            )
            if (!is.null(paths$ext))
              parsed$ext_data <- tryCatch(read_ext(paths$ext),
                                          error = function(e) NULL)
            if (!is.null(paths$shk))
              parsed$shk_data <- tryCatch(read_shk(paths$shk),
                                          error = function(e) NULL)
            if (!is.null(paths$coi))
              parsed$coi_data <- tryCatch(read_coi(paths$coi),
                                          error = function(e) NULL)
            if (!is.null(paths$clt))
              parsed$clt_data <- tryCatch(read_clt(paths$clt),
                                          error = function(e) NULL)
            if (!is.null(paths$tab))
              parsed$tab_data <- tryCatch(read_tab(paths$tab),
                                          error = function(e) NULL)
            run_data[[local_rid]] <- parsed

            n_loaded <- sum(!sapply(parsed, is.null))
            showNotification(
              paste0(run_names[[local_rid]], " : ", n_loaded,
                     " fichier(s) charge(s)"),
              type = "message"
            )
          }, ignoreInit = TRUE)

          # Name edit observer
          obs_name <- observeEvent(input[[paste0("name_", local_rid)]], {
            run_names[[local_rid]] <-
              input[[paste0("name_", local_rid)]]
          }, ignoreInit = TRUE)

          # Remove observer
          obs_remove <- observeEvent(input[[paste0("rm_", local_rid)]], {
            # Destroy observers for this run before removing it
            if (!is.null(run_observers[[local_rid]])) {
              run_observers[[local_rid]]$upload$destroy()
              run_observers[[local_rid]]$name$destroy()
              run_observers[[local_rid]]$remove$destroy()
              run_observers[[local_rid]] <<- NULL
            }
            run_ids(setdiff(run_ids(), local_rid))
            run_data[[local_rid]] <- NULL
            run_names[[local_rid]] <- NULL
            run_count(run_count() - 1L)
          }, ignoreInit = TRUE)

          # Register observers so they can be destroyed later
          run_observers[[local_rid]] <<- list(
            upload = obs_upload,
            name   = obs_name,
            remove = obs_remove
          )
        })
      }

      observed_ids(union(already, new_ids))
    })

    # Return: build list from run_data + run_names
    list(
      comp_runs = reactive({
        ids <- run_ids()
        if (length(ids) == 0L) return(list())
        result <- list()
        for (rid in ids) {
          d <- run_data[[rid]]
          if (is.null(d)) next
          result[[rid]] <- list(
            name     = run_names[[rid]] %||% rid,
            ext_data = d$ext_data,
            shk_data = d$shk_data,
            coi_data = d$coi_data,
            clt_data = d$clt_data,
            tab_data = d$tab_data
          )
        }
        result
      })
    )
  })
}
