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

    criteria_cards_ui <- function(ext, tbl) {
      tryCatch({
        ofv      <- get_ofv(ext, tbl)
        if (length(ofv) != 1L) ofv <- NA_real_
        rse      <- get_rse(ext, tbl)
        n_params <- nrow(rse)
        d_crit   <- get_d_criterion(ofv, n_params)
        det_fim  <- if (!is.na(ofv)) exp(-ofv) else NA_real_
        cn       <- get_condition_number(ext, tbl)

        fact_strip(
          fact_item("D-criterion", signif(d_crit, 4), "exp(-OFV/p)", "primary"),
          fact_item(
            "Determinant",
            if (!is.na(det_fim)) formatC(det_fim, format = "e", digits = 3) else "N/A",
            "exp(-OFV)"
          ),
          fact_item(
            "Cond. # (FE)",
            if (!is.na(cn$condition_number)) signif(cn$condition_number, 4) else "N/A",
            "Fixed effects"
          ),
          fact_item(
            "Eigenvalues",
            if (!is.na(cn$min_eigenvalue))
              sprintf("%.3g - %.3g", cn$min_eigenvalue, cn$max_eigenvalue)
            else "N/A",
            "min - max"
          )
        )
      }, error = function(e) {
        div(class = "alert alert-warning", style = "border-radius:8px; margin:12px 0;",
            tags$strong("FIM criteria unavailable"),
            tags$p(style = "margin:4px 0 0;", conditionMessage(e)))
      })
    }

    robust_cards_ui <- function(ext, tbl) {
      tryCatch({
        if (is.null(ext) || !"table_no" %in% names(ext)) return(NULL)
        tbls <- sort(unique(ext$table_no))
        if (length(tbls) <= 1L) return(NULL)

        n_params <- tryCatch(nrow(get_rse(ext, tbl)), error = function(e) 0L)
        if (is.na(n_params) || n_params <= 0L) {
          for (tbl_i in tbls) {
            n_params <- tryCatch(nrow(get_rse(ext, tbl_i)), error = function(e) 0L)
            if (!is.na(n_params) && n_params > 0L) break
          }
        }
        if (is.na(n_params) || n_params <= 0L) return(NULL)

        rdc <- get_robust_d_criterion(ext, n_params)
        if (is.null(rdc)) return(NULL)

        tagList(
          tags$p(
            class = "workspace-note",
            "Computed over all ", rdc$n_subprob,
            " TABLE NO. blocks. Use the TABLE selector for individual prior realizations."
          ),
          fact_strip(
            fact_item("Robust D", signif(rdc$d_robust, 4), "geometric mean", "primary"),
            fact_item("P10 - P90", sprintf("%.4g - %.4g", rdc$d_p10, rdc$d_p90),
                      "prior spread"),
            fact_item("OFV mean", sprintf("%.4f", rdc$ofv_mean),
                      paste0("SD ", sprintf("%.3f", rdc$ofv_sd))),
            fact_item("Sub-problems", rdc$n_subprob,
                      paste0(n_params, " estimable params"))
          )
        )
      }, error = function(e) {
        div(class = "alert alert-warning", style = "border-radius:8px; margin:12px 0;",
            tags$strong("Robust design summary unavailable"),
            tags$p(style = "margin:4px 0 0;", conditionMessage(e)))
      })
    }

    output$content <- renderUI({
      ext <- ext_data()
      fim <- fim_matrix()

      if (is.null(ext)) {
        return(page_shell(
          page_header(
            "FIM & Criteria",
            "Optimality criteria, matrix diagnostics, and multi-run FIM comparison.",
            eyebrow = "Design"
          ),
          empty_state("Load a .ext file", "FIM criteria are available after the primary .ext file is loaded.", "chart-bar")
        ))
      }

      page_shell(
        page_header(
          "FIM & Criteria",
          "Optimality criteria, matrix diagnostics, and multi-run FIM comparison.",
          eyebrow = "Design"
        ),
        doc_callout(
          "fim",
          "D-criterion, robust D, determinant, and conditioning notes are collected in Documentation.",
          "Open FIM documentation"
        ),
        popkin_tabs(ns,
          tabPanel("Criteria Summary",
            uiOutput(ns("cards")),
            if (length(all_runs()) > 1) {
              table_panel(
                "FIM multi-run comparison",
                DTOutput(ns("compare_table"))
              )
            }
          ),
          tabPanel("Eigenvalues",
            table_panel(
              "Correlation matrix eigenvalues",
              DTOutput(ns("eigen_table"))
            )
          ),
          tabPanel("Correlation Heatmap",
            plot_panel(
              "Correlation matrix (FIM)",
              uiOutput(ns("heatmap_run_selector")),
              if (!is.null(fim)) {
                tagList(
                  plotOutput(ns("heatmap"), height = "400px"),
                  plot_export_ui(ns, "heatmap_export", default_fname = "fim_heatmap")
                )
              } else {
                status_panel(
                  "Matrix source missing",
                  tags$p(".coi or .clt file required for the correlation heatmap."),
                  tone = "warning",
                  icon_name = "exclamation-triangle"
                )
              }
            )
          ),
          id = "fim_tabs"
        )
      )
    })

    # Metric cards FIM — synchronisees avec le run selectionne
    output$cards <- renderUI({
      ext <- selected_ext()
      if (is.null(ext)) return(NULL)
      robust_cards_ui(ext, tbl_no()) %||% criteria_cards_ui(ext, tbl_no())
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
