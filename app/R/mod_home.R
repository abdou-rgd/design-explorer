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

      # Number of estimated params
      n_params <- if (nrow(final) > 0) {
        ncol(final) - 3L  # minus ITERATION, table_no, type
      } else {
        "N/A"
      }

      # Criterion type
      criterion <- if (!is.null(ext_lines())) {
        detect_criterion(ext_lines())
      } else {
        "D-OPTIMALITY"
      }

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

      # D-criterion
      d_crit <- tryCatch({
        signif(get_d_criterion(ext, tno), 4)
      }, error = function(e) "N/A")

      tagList(
        section_header("Run Summary", primary_name()),
        tags$div(class = "home-cards-grid",
          metric_card_v5("Criterion",    criterion,   "bullseye",       "#2563eb"),
          metric_card_v5("OFV",          ofv_val,     "chart-line",     "#7c3aed", sub = paste0("Table ", tno, "/", n_tabs)),
          metric_card_v5("D-criterion",  d_crit,      "chart-bar",      "#16a34a", sub = "exp(-OFV/p)"),
          metric_card_v5("Parameters",   n_params,    "list-ol",        "#d97706"),
          metric_card_v5("Sample Size",  gs,          "users",          "#dc2626", sub = "N total (IDs x GROUPSIZE)"),
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
