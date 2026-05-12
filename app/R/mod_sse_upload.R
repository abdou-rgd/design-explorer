# =============================================================================
# mod_sse_upload.R — Centralized SSE file upload
#
# Single upload point for PsN raw_results CSVs plus optional keep_tables
# archives. All SSE modules consume shared reactives from this module.
#
# Returns: sse_a_data, name_a, sse_b_data, name_b, individual_pk_data
# =============================================================================

mod_sse_upload_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    page_header(
      "SSE Upload",
      "Central upload point for PsN raw_results CSV files and optional keep_tables individual PK archives.",
      eyebrow = "Validation"
    ),
    doc_callout(
      "sse-validation",
      "Load NONMEM outputs in Home first when possible, then upload raw_results CSV files and optional PsN individual PK archives here.",
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
    page_section(
      "Individual PK tables",
      subtitle = "Optional PsN keep_tables archive. Recommended table name: pk_individuals.tab.",
      tags$div(class = "upload-panel upload-panel--primary",
        tags$h4("PsN archive"),
        fileInput(ns("patab_zip"), "PsN output archive (.zip) with pk_individuals.tab-* or patab*.tab-* files",
                  accept = ".zip", width = "100%"),
        tags$div(
          class = "upload-status-line upload-status-line--warning",
          icon("info-circle"),
          tags$span(
            "Recommended NONMEM convention: ",
            tags$code("FILE=pk_individuals.tab"),
            ". The app also accepts legacy PsN names like ",
            tags$code("patab1.tab"), "."
          )
        ),
        uiOutput(ns("patab_status"))
      )
    ),
    science_note(
      "PsN SSE command for shrinkage",
      tags$p(
        "Upload ", tags$code("raw_results_*.csv"),
        " here. The SSE shrinkage plots use the ",
        tags$code("shrinkage_eta*(%)"),
        " columns from that file; ", tags$code("sse_results.csv"),
        " is not enough for these diagnostics."
      ),
      tags$pre(
        paste0(
          "# Full diagnostic run\n",
          "wrapsn 10 sse model.mod -samples=200 -seed=12345 -shrinkage\n",
          "# Quick smoke test\n",
          "wrapsn 10 sse model.mod -samples=20 -seed=12345 -shrinkage\n",
          "# Also keep NONMEM $TABLE files for individual PK outputs\n",
          "wrapsn 10 sse model.mod -samples=200 -seed=12345 -shrinkage -keep_tables\n\n",
          "$TABLE ID TIME CL VC Q VP KA F1 IPRED ETA(1) ETA(2) ETA(3) ETA(4) ETA(5) ETA(6) FILE=pk_individuals.tab NOPRINT ONEHEADER"
        )
      ),
      tags$ul(
        tags$li(
          tags$code("raw_results_*.csv"),
          " stores run-level population estimates only. If ",
          tags$code("-keep_tables"),
          " was used, individual PK outputs are separate table files in the ",
          "PsN SSE run directory, not columns in ", tags$code("raw_results_*.csv"),
          "."
        ),
        tags$li(
          "If ", tags$code("shrinkage_eta*(%)"),
          " columns are present but all ", tags$code("NA"),
          ", check that the estimation model computes post-hoc ETAs ",
          "(", tags$code("POSTHOC"), ", no ", tags$code("MAXEVAL=0"),
          ") and that PsN was not run with ", tags$code("-no_shrinkage"), "."
        ),
        tags$li(
          tags$code("-keep_tables"),
          " keeps subject/record-level NONMEM ", tags$code("$TABLE"),
          " outputs. The app convention is ",
          tags$code("FILE=pk_individuals.tab"), ". Existing PsN names like ",
          tags$code("patab1.tab"), " are accepted only for backwards ",
          "compatibility. This table is not required for the SSE shrinkage plots."
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
    individual_pk_raw <- reactiveVal(NULL)

    # Reset
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        sse_a_raw(NULL)
        sse_b_raw(NULL)
        individual_pk_raw(NULL)
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

    # --- Parse optional PsN keep_tables archive ---
    observeEvent(input$patab_zip, {
      req(input$patab_zip)
      tryCatch({
        dat <- read_sse_patab_outputs(input$patab_zip$datapath)
        if (nrow(dat) == 0L) {
          showNotification(
            paste0(
              "No individual PK table files found. Expected files like ",
              "pk_individuals.tab-1 and pk_individuals.tab-sim-1 ",
              "(legacy patab*.tab-* is also accepted)."
            ),
            type = "warning", duration = 8
          )
          individual_pk_raw(NULL)
          return()
        }
        individual_pk_raw(dat)
        sumry <- summarize_individual_pk_archive(dat)
        showNotification(
          sprintf("Individual PK tables loaded: %d samples, %d subjects.",
                  sumry$n_samples, sumry$n_ids),
          type = "message", duration = 4
        )
      }, error = function(e) {
        showNotification(paste("patab read error:", conditionMessage(e)),
                         type = "error", duration = 8)
        individual_pk_raw(NULL)
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
          pk_cols <- detect_individual_pk_columns(data)
          tagList(
            tags$div(
              class = "upload-status-line upload-status-line--ready",
              icon("check-circle"),
              tags$span(sprintf("%s: %d runs, %d converged", label, n_total, n_success))
            ),
            if (length(pk_cols) == 0L) {
              tags$div(
                class = "upload-status-line upload-status-line--warning",
                icon("table"),
                tags$span(
                  "This uploaded raw_results file has run-level estimates only; ",
                  "individual PK tables may exist separately if -keep_tables was used"
                )
              )
            } else {
              tags$div(
                class = "upload-status-line upload-status-line--ready",
                icon("table"),
                tags$span(sprintf(
                  "%s: %d individual PK/table columns detected",
                  label, length(pk_cols)
                ))
              )
            }
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

    output$patab_status <- renderUI({
      dat <- individual_pk_raw()
      if (is.null(dat)) {
        return(tags$div(
          class = "upload-status-line",
          icon("circle"),
          tags$span(
            "No individual PK archive loaded. Expected ",
            tags$code("pk_individuals.tab-*"),
            " / ",
            tags$code("pk_individuals.tab-sim-*"),
            "."
          )
        ))
      }
      sumry <- summarize_individual_pk_archive(dat)
      tagList(
        tags$div(
          class = "upload-status-line upload-status-line--ready",
          icon("table"),
          tags$span(sprintf(
            "%d patab files: %d estimation, %d simulation",
            sumry$n_files, sumry$n_est, sumry$n_sim
          ))
        ),
        tags$div(
          class = "upload-status-line upload-status-line--ready",
          icon("users"),
          tags$span(sprintf(
            "%d samples, %d subjects, columns: %s",
            sumry$n_samples, sumry$n_ids,
            paste(sumry$columns, collapse = ", ")
          ))
        )
      )
    })

    # --- Return shared reactives ---
    list(
      sse_a_data = reactive(sse_a_raw()),
      name_a     = reactive(input$name_a %||% "Original"),
      sse_b_data = reactive(sse_b_raw()),
      name_b     = reactive(input$name_b %||% "Optimized"),
      individual_pk_data = reactive(individual_pk_raw())
    )
  })
}
