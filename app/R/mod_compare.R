# =============================================================================
# mod_compare.R -- Multi-run comparison management
# =============================================================================

mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "upload-box",
      tags$h6("Comparison"),
      actionButton(ns("add_run"), "Add a run", icon = icon("plus"),
                   class = "btn-sm btn-default w-100"),
      uiOutput(ns("run_list"))
    )
  )
}

mod_compare_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Separate UI state from data state to avoid fileInput reset
    run_ids   <- reactiveVal(character())   # drives UI rendering
    run_names <- reactiveValues()            # editable names
    run_data  <- reactiveValues()            # parsed data (does NOT drive UI)
    run_counter <- reactiveVal(0L)

    observeEvent(input$add_run, {
      if (length(run_ids()) >= 5L) {
        showNotification("Maximum 5 comparison runs (6 total)",
                         type = "warning")
        return()
      }
      n <- run_counter() + 1L
      run_counter(n)
      rid <- paste0("run_", n)
      default_name <- paste0("Run ", LETTERS[n + 1L])

      run_names[[rid]] <- default_name
      run_data[[rid]] <- list(
        ext_data = NULL, shk_data = NULL, coi_data = NULL,
        clt_data = NULL, tab_data = NULL, cpu_data = NA_real_
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
        color <- run_color(rid)
        has_data <- !is.null(isolate(run_data[[rid]]$ext_data))

        div(class = "upload-box",
            style = paste0("border-left: 3px solid ", color, ";"),
          fluidRow(
            column(8, textInput(ns(paste0("name_", rid)), NULL,
                                value = rname, width = "100%")),
            column(4, actionButton(ns(paste0("rm_", rid)), NULL,
                                   icon = icon("times"),
                                   class = "btn-sm btn-danger"))
          ),
          fileInput(ns(paste0("upload_", rid)), NULL, multiple = TRUE,
                    accept = c(".ext", ".shk", ".coi", ".clt", ".tab",
                               ".ctl", ".mod", ".con",
                               ".tar.gz", ".tgz"),
                    buttonLabel = "Files"),
          uiOutput(ns(paste0("status_", rid)))
        )
      })
      tagList(run_uis)
    })

    # Track which run IDs have had observers created
    observed_ids <- reactiveVal(character())

    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        for (rid in run_ids()) {
          run_data[[rid]] <- NULL
          run_names[[rid]] <- NULL
        }
        run_ids(character())
      }, ignoreInit = TRUE)
    }

    observe({
      current <- run_ids()
      already <- observed_ids()
      new_ids <- setdiff(current, already)

      for (rid in new_ids) {
        local({
          local_rid <- rid

          # File upload observer
          observeEvent(input[[paste0("upload_", local_rid)]], {
            files <- input[[paste0("upload_", local_rid)]]
            req(files)
            paths <- extract_design_files(files, paste0("comp_", local_rid))

            # Parse and store in run_data (does NOT trigger UI re-render)
            parsed <- list(
              ext_data = NULL, shk_data = NULL, coi_data = NULL,
              clt_data = NULL, tab_data = NULL, cpu_data = NA_real_
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
            if (!is.null(paths$cpu))
              parsed$cpu_data <- read_cpu(paths$cpu)
            run_data[[local_rid]] <- parsed

            # Auto-fill run name from .ctl if available
            if (!is.null(paths$ctl)) {
              ctl_lines <- tryCatch(readLines(paths$ctl, warn = FALSE),
                                   error = function(e) NULL)
              if (!is.null(ctl_lines)) {
                design_name <- tryCatch(parse_design_summary(ctl_lines),
                                        error = function(e) NULL)
                if (!is.null(design_name)) {
                  run_names[[local_rid]] <- design_name
                  updateTextInput(session, paste0("name_", local_rid),
                                  value = design_name)
                }
              }
            }

            n_loaded <- sum(!sapply(parsed, is.null))
            showNotification(
              paste0(run_names[[local_rid]], " : ", n_loaded,
                     " file(s) loaded"),
              type = "message"
            )
          }, ignoreInit = TRUE)

          # Name edit observer — debounced to avoid cascade on every keystroke
          name_input_d <- debounce(
            reactive(input[[paste0("name_", local_rid)]]), 500
          )
          observeEvent(name_input_d(), {
            run_names[[local_rid]] <- name_input_d()
          }, ignoreInit = TRUE, ignoreNULL = TRUE)

          # Remove observer
          observeEvent(input[[paste0("rm_", local_rid)]], {
            run_ids(setdiff(run_ids(), local_rid))
            run_data[[local_rid]] <- NULL
            run_names[[local_rid]] <- NULL
            # run_counter stays monotone — never decremented
          }, ignoreInit = TRUE)
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
            tab_data = d$tab_data,
            cpu_data = d$cpu_data %||% NA_real_
          )
        }
        result
      })
    )
  })
}
