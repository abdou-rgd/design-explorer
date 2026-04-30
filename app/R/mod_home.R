# =============================================================================
# mod_home.R — Home / Landing page (V5)
#
# Displays run info cards after data is loaded.
# Upload + Examples UIs are placed directly in app.R's conditionalPanel
# (same namespace as before — no module wrapping needed).
# =============================================================================

mod_home_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    uiOutput(ns("run_info")),
    uiOutput(ns("reset_btn"))
  )
}

mod_home_server <- function(id, merged_ext,
                            merged_shk = reactive(NULL),
                            merged_coi = reactive(NULL),
                            merged_clt = reactive(NULL),
                            merged_cpu, merged_tab,
                            merged_ctl_lines = reactive(NULL),
                            merged_true_vals = reactive(NULL),
                            ext_lines, primary_name, tbl_no,
                            param_labels, groupsize,
                            all_runs = reactive(list()),
                            sse_a_data = reactive(NULL),
                            sse_b_data = reactive(NULL),
                            sse_name_a = reactive("Design A"),
                            sse_name_b = reactive("Design B"),
                            reset_trigger) {
  moduleServer(id, function(input, output, session) {

    output$run_info <- renderUI({
      ext <- merged_ext()
      if (is.null(ext)) {
        return(tags$div(
          class = "home-dashboard home-dashboard--empty",
          page_header(
            "NONMEM DESIGN Explorer",
            "Load a primary NONMEM run, then add comparison runs when you want side-by-side scientific outputs.",
            eyebrow = "Run workspace"
          ),
          empty_state(
            "No primary run loaded",
            "Upload NONMEM output files in the left panel or load a built-in example to start exploring parameters, design criteria, optimal times, and validation diagnostics.",
            icon_name = "folder-open"
          )
        ))
      }

      # Extract key metrics
      tabs   <- sort(unique(ext$table_no))
      n_tabs <- length(tabs)
      tno    <- tbl_no()
      final  <- ext |> dplyr::filter(table_no == tno, type == "final")

      # OFV
      ofv_val <- if (nrow(final) > 0 && "OBJ" %in% names(final)) {
        signif(final$OBJ[1], 6)
      } else {
        "N/A"
      }

      # Number of estimated (non-fixed) params — use get_rse which filters fixed
      n_params <- tryCatch({
        nrow(get_rse(ext, tno))
      }, error = function(e) "N/A")

      # Criterion type + method from .ext header
      lines <- ext_lines()
      criterion <- if (!is.null(lines)) detect_criterion(lines) else "D-OPTIMALITY"
      method_info <- if (!is.null(lines)) detect_method(lines) else list(method = "FO", mode = "Evaluation")
      method_sub <- paste0(method_info$mode, " | Table ", tno, "/", n_tabs)

      # CPU time
      cpu <- merged_cpu()
      cpu_txt <- if (!is.na(cpu) && cpu > 0) {
        if (cpu > 3600) {
          sprintf("%.1f h", cpu / 3600)
        } else if (cpu > 60) {
          sprintf("%.1f min", cpu / 60)
        } else {
          sprintf("%.0f s", cpu)
        }
      } else {
        "N/A"
      }

      # GROUPSIZE
      gs <- groupsize()

      file_tile <- function(label, loaded, detail = NULL) {
        tags$div(
          class = paste("run-file-tile",
                        if (isTRUE(loaded)) "run-file-tile--loaded"
                        else "run-file-tile--missing"),
          tags$span(class = "run-file-tile__name", label),
          tags$strong(if (isTRUE(loaded)) "Loaded" else "Missing"),
          if (!is.null(detail)) tags$small(detail)
        )
      }

      runs <- all_runs()
      comp_runs <- if (length(runs) > 1L) runs[-1] else list()
      true_vals <- merged_true_vals()
      ctl_loaded <- !is.null(merged_ctl_lines()) && length(merged_ctl_lines()) > 0L
      sse_a <- sse_a_data()
      sse_b <- sse_b_data()
      sse_tile <- function(label, data) {
        if (is.null(data)) {
          return(tags$div(class = "run-state-card run-state-card--missing",
            tags$span(label),
            tags$strong("Not loaded"),
            tags$small("Upload in SSE Upload")
          ))
        }
        n_total <- attr(data, "n_total") %||% nrow(data)
        n_success <- attr(data, "n_success") %||% sum(data$converged)
        tags$div(class = "run-state-card run-state-card--ready",
          tags$span(label),
          tags$strong(sprintf("%d/%d converged", n_success, n_total)),
          tags$small("raw_results CSV")
        )
      }

      tags$div(
        class = "home-dashboard",
        page_header(
          primary_name() %||% "Primary run",
          "Primary run overview, loaded file context, comparison runs, and validation readiness.",
          eyebrow = "Run workspace"
        ),
        page_section(
          "Primary run summary",
          subtitle = "Key metadata for the selected TABLE NO.",
          tags$div(class = "run-summary-grid",
            tags$div(class = "run-summary-cell run-summary-cell--wide",
              tags$span("Method"),
              tags$strong(method_info$method),
              tags$small(method_sub)
            ),
            tags$div(class = "run-summary-cell run-summary-cell--wide",
              tags$span("Criterion"),
              tags$strong(criterion)
            ),
            tags$div(class = "run-summary-cell",
              tags$span("OFV"),
              tags$strong(ofv_val),
              tags$small("-log(det(FIM))")
            ),
            tags$div(class = "run-summary-cell",
              tags$span("Parameters"),
              tags$strong(n_params),
              tags$small("estimated")
            ),
            tags$div(class = "run-summary-cell",
              tags$span("Sample size"),
              tags$strong(gs),
              tags$small("GROUPSIZE")
            ),
            tags$div(class = "run-summary-cell",
              tags$span("CPU time"),
              tags$strong(cpu_txt)
            )
          )
        ),
        page_section(
          "Loaded files",
          subtitle = "Operational data sources available to downstream tabs.",
          tags$div(class = "run-file-grid",
            file_tile(".ext", !is.null(merged_ext()), "criteria, RSE, convergence"),
            file_tile(".shk", !is.null(merged_shk()), "shrinkage, RELATIVEINF"),
            file_tile(".coi", !is.null(merged_coi()), "FIM matrix"),
            file_tile(".clt", !is.null(merged_clt()), "FIM fallback"),
            file_tile(".tab", !is.null(merged_tab()), "optimal times"),
            file_tile(".ctl/.mod", ctl_loaded,
                      if (!is.null(true_vals)) sprintf("%d true values", length(true_vals))
                      else "true values, GROUPSIZE")
          )
        ),
        page_section(
          "Comparison and validation state",
          subtitle = "Secondary design runs and SSE data are first-class analysis inputs.",
          tags$div(class = "run-state-grid",
            tags$div(class = "run-state-card",
              tags$span("Comparison runs"),
              tags$strong(length(comp_runs)),
              if (length(comp_runs) > 0L) {
                tags$small(paste(vapply(comp_runs, function(r) r$name %||% "Run",
                                        character(1)), collapse = ", "))
              } else {
                tags$small("Add runs in the left panel")
              }
            ),
            sse_tile(sse_name_a(), sse_a),
            sse_tile(sse_name_b(), sse_b)
          )
        ),
        page_section(
          "Next checks",
          subtitle = "Suggested reading order for this run.",
          tags$div(class = "run-next-grid",
            tags$div(class = "run-next-item",
              icon("table"),
              tags$div(tags$strong("Parameters"), tags$span("Inspect OFV, RSE, shrinkage, and comparison rows."))
            ),
            tags$div(class = "run-next-item",
              icon("chart-bar"),
              tags$div(tags$strong("FIM & Criteria"), tags$span("Review D-criterion and matrix diagnostics."))
            ),
            tags$div(class = "run-next-item",
              icon("clock"),
              tags$div(tags$strong("Optimal Times"), tags$span("Check optimized sampling schedules when .tab is available."))
            ),
            tags$div(class = "run-next-item",
              icon("vial"),
              tags$div(tags$strong("Validation"), tags$span("Upload SSE outputs to compare empirical and FIM precision."))
            ),
            tags$div(class = "run-next-item",
              icon("book-open"),
              tags$div(tags$strong("Documentation"), doc_link("overview", "Open methods and references"))
            )
          )
        )
      )
    })

    output$reset_btn <- renderUI({
      if (is.null(merged_ext())) return(NULL)
      tags$div(
        class = "home-actions",
        actionButton(session$ns("reset_run"), "Remove run",
          icon  = icon("times"),
          class = "btn-sm btn-danger")
      )
    })

    observeEvent(input$reset_run, {
      reset_trigger(reset_trigger() + 1L)
      showNotification("Run removed", type = "message")
    })
  })
}
