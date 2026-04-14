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
  tagList(
    # --- Info banner ---
    div(class = "alert alert-info", style = "border-radius:10px; margin-bottom:12px;",
      tags$strong("SSE Upload"),
      tags$p(style = "margin:6px 0 0; font-size:0.9em;",
        "Upload your PsN SSE results (raw_results_*.csv) here. ",
        "All SSE tabs (Validation, Analysis, Comparison) will consume this data. ",
        "Design A is required; Design B is optional (enables side-by-side comparison)."
      ),
      tags$p(style = "margin:4px 0 0; font-size:0.85em; color:#854d0e;",
        tags$strong("Recommended:"),
        " load your NONMEM outputs (.ext, .ctl, ...) in the Home tab first, ",
        "then upload SSE results here. This ensures true parameter values ",
        "and FIM RSE are available for all SSE diagnostics."
      )
    ),

    # --- Typical workflow (collapsible) ---
    tags$details(
      style = paste0(
        "border:1px solid #ccc; border-radius:8px; padding:10px 14px;",
        " margin-bottom:14px; background:#f9f9fb;"
      ),
      tags$summary(style = "cursor:pointer; font-weight:600; font-size:0.95em;",
        "Typical optimal design workflow"
      ),
      div(style = "margin-top:10px; font-size:0.88em; line-height:1.7;",
        tags$ol(style = "margin:0; padding-left:20px;",
          tags$li(tags$strong("FIM evaluation"),
            " of a candidate design (NONMEM $DESIGN, MAXEVAL=0)"),
          tags$li(tags$strong("SSE"),
            " to confirm the FIM predictions and analyze estimability"),
          tags$li(tags$strong("FIM optimization"),
            " to find better sampling times/doses (MAXEVAL>0)"),
          tags$li(tags$strong("SSE on the optimized design"),
            " to confirm the improvement"),
          tags$li(tags$strong("Compare the two SSEs"),
            " (original vs optimized) to quantify the gain")
        ),
        tags$p(style = "margin:8px 0 0; color:#555;",
          "In this app: load the FIM results in the Home tab (step 1 or 3), ",
          "then upload the corresponding SSE results here (step 2 or 4). ",
          "Use Design A for the original and Design B for the optimized to ",
          "enable side-by-side comparison (step 5)."
        )
      )
    ),

    # --- File uploads + design names + status ---
    fluidRow(
      column(4,
        fileInput(ns("sse_a"), "Design A - SSE results (raw_results_*.csv)",
                  accept = ".csv", width = "100%"),
        textInput(ns("name_a"), "Design name", value = "Original",
                  width = "100%")
      ),
      column(4,
        fileInput(ns("sse_b"), "Design B - SSE results (optional)",
                  accept = ".csv", width = "100%"),
        textInput(ns("name_b"), "Design name", value = "Optimized",
                  width = "100%")
      ),
      column(4,
        uiOutput(ns("upload_status"))
      )
    ),

    # --- Summary banner ---
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
          div(
            style = paste0(
              "padding:8px 12px; border-radius:8px; margin-bottom:6px;",
              " background:#f0fdf4; border:1px solid #bbf7d0; color:#166534;"
            ),
            icon("check-circle"),
            tags$strong(sprintf(" %s: %d runs, %d converged", label, n_total, n_success))
          )
        } else {
          div(
            style = paste0(
              "padding:8px 12px; border-radius:8px; margin-bottom:6px;",
              " background:#f8fafc; border:1px solid #e2e8f0; color:#64748b;"
            ),
            icon("circle"),
            sprintf(" %s: not loaded", label)
          )
        }
      }

      tagList(
        tags$p(tags$strong("Status"), style = "margin-bottom:6px; margin-top:25px;"),
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

      div(
        class = "alert",
        style = paste0(
          "border-radius:10px; margin-top:6px; padding:10px 14px;",
          " background:#f8fafc; border:1px solid #e2e8f0;"
        ),
        icon("bar-chart"),
        tags$strong(" SSE data ready. "),
        tags$span(paste(msgs, collapse = "  |  "), style = "font-size:0.9em;")
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
