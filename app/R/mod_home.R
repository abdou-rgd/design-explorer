# =============================================================================
# mod_home.R — Home / Landing page (V5)
#
# Displays run info cards after data is loaded.
# Upload + Examples UIs are placed directly in app.R's conditionalPanel
# (same namespace as before — no module wrapping needed).
# =============================================================================

mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("run_info")),
    uiOutput(ns("reset_btn"))
  )
}

mod_home_server <- function(id, merged_ext, merged_cpu, merged_tab,
                            ext_lines, primary_name, tbl_no,
                            param_labels, groupsize,
                            reset_trigger) {
  moduleServer(id, function(input, output, session) {

    output$run_info <- renderUI({
      ext <- merged_ext()
      if (is.null(ext)) {
        return(tags$div(
          class = "surface-card",
          style = "text-align:center; padding:40px 20px; color:var(--text-muted);",
          tags$h4("No run loaded"),
          tags$p("Upload NONMEM output files or load a built-in example to get started.")
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

      tagList(
        section_header("Run Summary"),
        tags$div(class = "home-cards-grid",
          metric_card_v5("Method",       method_info$method, "flask",    "#2563eb", sub = method_sub),
          metric_card_v5("Criterion",    criterion,   "bullseye",       "#7c3aed"),
          metric_card_v5("OFV",          ofv_val,     "chart-line",     "#16a34a", sub = "-log(det(FIM))"),
          metric_card_v5("Parameters",   n_params,    "list-ol",        "#d97706", sub = "estimated"),
          metric_card_v5("Sample Size",  gs,          "users",          "#dc2626", sub = "GROUPSIZE"),
          metric_card_v5("CPU Time",     cpu_txt,     "clock",          "#0891b2")
        )
      )
    })

    output$reset_btn <- renderUI({
      if (is.null(merged_ext())) return(NULL)
      tags$div(
        style = "margin-top: 8px;",
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
