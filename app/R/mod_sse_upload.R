# =============================================================================
# mod_sse_upload.R — Centralized SSE file upload
#
# Single upload point for PsN raw_results CSVs. All SSE modules (Validation,
# Analysis, Comparison) consume shared reactives from this module.
#
# Returns: sse_a_data, name_a, sse_b_data, name_b
# =============================================================================

mod_sse_upload_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    page_header(
      "SSE Upload",
      "Central upload point for PsN raw_results CSV files consumed by SSE Validation, Analysis, and Comparison.",
      eyebrow = "Validation"
    ),
    doc_callout(
      "sse-validation",
      "Load NONMEM outputs in Home first when possible, then upload Design A and optional Design B raw_results CSV files here.",
      "Open SSE workflow documentation"
    ),
    page_section(
      "SSE data sources",
      subtitle = "Design A is required. Design B enables side-by-side comparison.",
      tags$div(class = "sse-upload-grid",
        tags$div(class = "upload-panel upload-panel--primary",
          tags$h4("Design A"),
          fileInput(ns("sse_a"), "SSE results (raw_results_*.csv)",
                    accept = ".csv", width = "100%"),
          textInput(ns("name_a"), "Design name", value = "Original",
                    width = "100%")
        ),
        tags$div(class = "upload-panel",
          tags$h4("Design B"),
          fileInput(ns("sse_b"), "SSE results (optional)",
                    accept = ".csv", width = "100%"),
          textInput(ns("name_b"), "Design name", value = "Optimized",
                    width = "100%")
        ),
        tags$div(class = "upload-panel upload-panel--status",
          uiOutput(ns("upload_status"))
        )
      )
    ),
    uiOutput(ns("status_banner"))
  )
}

mod_sse_upload_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {

    # Internal storage
    sse_a_raw <- reactiveVal(NULL)
    sse_b_raw <- reactiveVal(NULL)

    # Reset
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        sse_a_raw(NULL)
        sse_b_raw(NULL)
        updateTextInput(session, "name_a", value = "Original")
        updateTextInput(session, "name_b", value = "Optimized")
      }, ignoreInit = TRUE)
    }

    # --- Parse Design A ---
    observeEvent(input$sse_a, {
      req(input$sse_a)
      tryCatch({
        # Check for summary format (not supported in centralized path)
        first_line <- readLines(input$sse_a$datapath, n = 1L, warn = FALSE)
        if (grepl("^SSE run info", first_line, ignore.case = TRUE)) {
          showNotification(
            paste0("Summary format detected (sse_results.csv). ",
                   "Please upload the raw_results_*.csv file instead."),
            type = "warning", duration = 10
          )
          sse_a_raw(NULL)
          return()
        }
        dat <- read_sse_raw_all(input$sse_a$datapath)
        sse_a_raw(dat)
        showNotification("Design A loaded.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("SSE read error (Design A):", conditionMessage(e)),
                         type = "error", duration = 8)
        sse_a_raw(NULL)
      })
    })

    # --- Parse Design B ---
    observeEvent(input$sse_b, {
      req(input$sse_b)
      tryCatch({
        first_line <- readLines(input$sse_b$datapath, n = 1L, warn = FALSE)
        if (grepl("^SSE run info", first_line, ignore.case = TRUE)) {
          showNotification(
            paste0("Summary format detected (sse_results.csv). ",
                   "Please upload the raw_results_*.csv file instead."),
            type = "warning", duration = 10
          )
          sse_b_raw(NULL)
          return()
        }
        dat <- read_sse_raw_all(input$sse_b$datapath)
        sse_b_raw(dat)
        showNotification("Design B loaded.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("SSE read error (Design B):", conditionMessage(e)),
                         type = "error", duration = 8)
        sse_b_raw(NULL)
      })
    })

    # --- Per-file status ---
    output$upload_status <- renderUI({
      a <- sse_a_raw()
      b <- sse_b_raw()

      make_line <- function(data, label) {
        if (!is.null(data)) {
          n_total   <- attr(data, "n_total") %||% nrow(data)
          n_success <- attr(data, "n_success") %||% sum(data$converged)
          tags$div(
            class = "upload-status-line upload-status-line--ready",
            icon("check-circle"),
            tags$span(sprintf("%s: %d runs, %d converged", label, n_total, n_success))
          )
        } else {
          tags$div(
            class = "upload-status-line",
            icon("circle"),
            tags$span(sprintf("%s: not loaded", label))
          )
        }
      }

      tagList(
        tags$h4("Status"),
        make_line(a, "Design A"),
        make_line(b, "Design B (optional)")
      )
    })

    # --- Summary banner ---
    output$status_banner <- renderUI({
      a <- sse_a_raw()
      b <- sse_b_raw()
      if (is.null(a) && is.null(b)) return(NULL)

      msgs <- character(0)
      if (!is.null(a)) {
        msgs <- c(msgs, sprintf(
          "Design A (%s): %d runs, %d converged",
          input$name_a %||% "Original",
          attr(a, "n_total") %||% nrow(a),
          attr(a, "n_success") %||% sum(a$converged)
        ))
      }
      if (!is.null(b)) {
        msgs <- c(msgs, sprintf(
          "Design B (%s): %d runs, %d converged",
          input$name_b %||% "Optimized",
          attr(b, "n_total") %||% nrow(b),
          attr(b, "n_success") %||% sum(b$converged)
        ))
      }

      status_panel(
        "SSE data ready",
        tags$p(paste(msgs, collapse = " | ")),
        tone = "success",
        icon_name = "bar-chart"
      )
    })

    # --- Return shared reactives ---
    list(
      sse_a_data = reactive(sse_a_raw()),
      name_a     = reactive(input$name_a %||% "Original"),
      sse_b_data = reactive(sse_b_raw()),
      name_b     = reactive(input$name_b %||% "Optimized")
    )
  })
}
