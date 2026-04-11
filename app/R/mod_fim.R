# =============================================================================
# mod_fim.R — Onglet FIM & Criteres (+ multi-run + correlation)
# =============================================================================

mod_fim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content"))
  )
}

mod_fim_server <- function(id, ext_data, coi_data, clt_data, tbl_no, param_labels, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ext_data du run actuellement selectionne dans le sélecteur heatmap
    selected_ext <- reactive({
      runs <- all_runs()
      if (length(runs) <= 1L) return(ext_data())
      rid <- input$heatmap_run %||% names(runs)[1]
      r <- runs[[rid]]
      if (!is.null(r)) r$ext_data else ext_data()
    })

    # FIM : preferer .coi, sinon .clt
    fim_matrix <- reactive({
      coi <- coi_data()
      if (!is.null(coi)) return(coi)
      clt <- clt_data()
      if (!is.null(clt)) return(clt)
      NULL
    })

    output$content <- renderUI({
      ext <- ext_data()
      fim <- fim_matrix()

      if (is.null(ext)) {
        return(div(class = "alert alert-info",
                   "Load a .ext file to see FIM criteria."))
      }

      popkin_tabs(ns,
        tabPanel("Criteria Summary",
          uiOutput(ns("cards")),
          br(),
          if (length(all_runs()) > 1) {
            div(class = "surface-card",
              p(class = "section-title", "FIM multi-run comparison"),
              DTOutput(ns("compare_table"))
            )
          }
        ),
        tabPanel("Eigenvalues",
          div(class = "plot-card", style = "margin-top:12px;",
            p(class = "section-title", "Correlation matrix eigenvalues"),
            DTOutput(ns("eigen_table"))
          )
        ),
        tabPanel("Correlation Heatmap",
          div(class = "plot-card", style = "margin-top:12px;",
            p(class = "section-title", "Correlation matrix (FIM)"),
            uiOutput(ns("heatmap_run_selector")),
            if (!is.null(fim)) {
              tagList(
                plotOutput(ns("heatmap"), height = "400px"),
                plot_export_ui(ns, "heatmap_export", default_fname = "fim_heatmap")
              )
            } else {
              div(class = "alert alert-warning", style = "margin-top:10px;",
                  ".coi or .clt file required for the correlation heatmap.")
            }
          )
        ),
        id = "fim_tabs"
      )
    })

    # Metric cards FIM — synchronisees avec le run selectionne
    output$cards <- renderUI({
      ext <- selected_ext()
      if (is.null(ext)) return(NULL)
      ofv      <- get_ofv(ext, tbl_no())
      if (length(ofv) != 1L) ofv <- NA_real_
      rse      <- get_rse(ext, tbl_no())
      n_params <- nrow(rse)
      d_crit   <- get_d_criterion(ofv, n_params)
      det_fim  <- if (!is.na(ofv)) exp(-ofv) else NA_real_
      cn       <- get_condition_number(ext, tbl_no())

      fluidRow(
        column(3, metric_card_v5("D-criterion", signif(d_crit, 4),
                                 icon_name = "chart-bar", color = "#2563eb",
                                 sub = "exp(-OFV/p)")),
        column(3, metric_card_v5("Determinant",
                                 if (!is.na(det_fim)) formatC(det_fim, format = "e", digits = 3) else "N/A",
                                 icon_name = "calculator", color = "#7c3aed",
                                 sub = "exp(-OFV)")),
        column(3, metric_card_v5("Cond. # (FE)",
                                 if (!is.na(cn$condition_number)) signif(cn$condition_number, 4) else "N/A",
                                 icon_name = "balance-scale", color = "#16a34a",
                                 sub = "Fixed effects")),
        column(3, metric_card_v5("Eigenvalues",
                                 if (!is.na(cn$min_eigenvalue))
                                   sprintf("%.3g - %.3g", cn$min_eigenvalue, cn$max_eigenvalue)
                                 else "N/A",
                                 icon_name = "sort-amount-down", color = "#d97706",
                                 sub = "min - max"))
      )
    })

    # Multi-run comparison table
    output$compare_table <- renderDT({
      runs <- all_runs(); req(length(runs) > 1)
      comp_df <- purrr::imap(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        ext <- r$ext_data
        ofv <- get_ofv(ext, tbl_no())
        if (length(ofv) != 1L || is.na(ofv)) ofv <- NA_real_
        rse <- get_rse(ext, tbl_no())
        n_params <- nrow(rse)
        d_crit <- get_d_criterion(ofv, n_params)
        cn <- get_condition_number(ext, tbl_no())
        cpu_secs <- r$cpu_data %||% NA_real_
        tibble(
          Run = r$name,
          OFV = if (!is.na(ofv)) round(ofv, 4) else NA_real_,
          `D-criterion` = if (!is.na(d_crit)) signif(d_crit, 4) else NA_real_,
          `Params` = n_params,
          `Cond. #` = if (!is.na(cn$condition_number)) signif(cn$condition_number, 4) else NA,
          `RSE mean (%)` = if (n_params > 0L) round(mean(rse$rse_pct, na.rm = TRUE), 2) else NA_real_,
          `RSE med. (%)` = if (n_params > 0L) round(median(rse$rse_pct, na.rm = TRUE), 2) else NA_real_,
          `RSE max (%)` = if (n_params > 0L) round(max(rse$rse_pct, na.rm = TRUE), 2) else NA_real_,
          CPU = format_cpu(cpu_secs)
        )
      }) |> dplyr::bind_rows()
      datatable(comp_df, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 5, dom = "t"))
    })

    # Eigenvalues table — synchronisee avec le run selectionne
    output$eigen_table <- renderDT({
      ext <- selected_ext()
      if (is.null(ext)) return(NULL)
      eig <- get_eigenvalues(ext, tbl_no())
      if (nrow(eig) == 0L) return(NULL)

      eig <- eig |> filter(eigenvalue != 0)

      datatable(
        eig |> mutate(eigenvalue = signif(eigenvalue, 5)),
        rownames = FALSE, class = "stripe hover compact",
        options = list(pageLength = 20, dom = "tip")
      )
    })

    # Heatmap run selector (multi-run only)
    output$heatmap_run_selector <- renderUI({
      runs <- all_runs()
      if (length(runs) <= 1L) return(NULL)
      run_choices <- setNames(names(runs),
                              vapply(runs, function(r) r$name %||% "?", character(1L)))
      selectInput(ns("heatmap_run"), "Displayed run:",
                  choices = run_choices, selected = names(runs)[1],
                  width = "100%")
    })

    # FIM heatmap reactive (shared between renderPlot and export)
    heatmap_plot <- reactive({
      runs <- all_runs()
      first_rid <- names(runs)[1]
      rid  <- if (length(runs) > 1L) input$heatmap_run %||% first_rid else first_rid
      r    <- runs[[rid]]
      fim  <- if (!is.null(r)) {
        r$coi_data %||% r$clt_data
      } else {
        fim_matrix()
      }
      if (is.null(fim)) return(NULL)
      plot_fim_heatmap(fim, labels = param_labels())
    })

    output$heatmap <- renderPlot({ heatmap_plot() }, res = 110)
    plot_export_server(input, output, session, "heatmap_export", heatmap_plot)

  })
}
