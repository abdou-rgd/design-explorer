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
                   "Chargez un fichier .ext pour voir les criteres FIM."))
      }

      compare_ui <- NULL
      if (length(all_runs()) > 1) {
        compare_ui <- tagList(
          div(class = "surface-card",
            p(class = "section-title", "Comparaison FIM multi-runs"),
            DTOutput(ns("compare_table"))
          ),
          br()
        )
      }

      tagList(
        uiOutput(ns("cards")),
        br(),
        compare_ui,
        fluidRow(
          column(6,
            div(class = "plot-card",
              p(class = "section-title", "Eigenvalues de la matrice de correlation"),
              DTOutput(ns("eigen_table"))
            )
          ),
          column(6,
            div(class = "plot-card",
              p(class = "section-title", "Matrice de correlation (FIM)"),
              if (!is.null(fim)) {
                plotOutput(ns("heatmap"), height = "400px")
              } else {
                div(class = "alert alert-warning", style = "margin-top:10px;",
                    "Fichier .coi ou .clt requis pour la heatmap de correlation.")
              }
            )
          )
        ),
      )
    })

    # Metric cards FIM
    output$cards <- renderUI({
      ext <- ext_data()
      if (is.null(ext)) return(NULL)
      ofv      <- get_ofv(ext, tbl_no())
      if (length(ofv) != 1L) ofv <- NA_real_
      rse      <- get_rse(ext, tbl_no())
      n_params <- nrow(rse)
      d_crit   <- get_d_criterion(ofv, n_params)
      det_fim  <- if (!is.na(ofv)) exp(-ofv) else NA_real_
      cn       <- get_condition_number(ext, tbl_no())

      fluidRow(
        column(3, metric_card("D-critere", signif(d_crit, 4),
                              "exp(-OFV/p)", "blue")),
        column(3, metric_card("Determinant", if (!is.na(det_fim)) formatC(det_fim, format = "e", digits = 3) else "N/A",
                              "exp(-OFV)", "purple")),
        column(3, metric_card("Cond. # (FE)",
                              if (!is.na(cn$condition_number)) signif(cn$condition_number, 4) else "N/A",
                              "effets fixes", "green")),
        column(3, metric_card("Eigenvalues",
                              if (!is.na(cn$min_eigenvalue))
                                sprintf("%.3g - %.3g", cn$min_eigenvalue, cn$max_eigenvalue)
                              else "N/A",
                              "min - max", "orange"))
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
        tibble(
          Run = r$name,
          OFV = if (!is.na(ofv)) round(ofv, 4) else NA_real_,
          `D-critere` = if (!is.na(d_crit)) signif(d_crit, 4) else NA_real_,
          `Params` = n_params,
          `Cond. #` = if (!is.na(cn$condition_number)) signif(cn$condition_number, 4) else NA,
          `RSE moy. (%)` = if (n_params > 0L) round(mean(rse$rse_pct, na.rm = TRUE), 2) else NA_real_,
          `RSE max (%)` = if (n_params > 0L) round(max(rse$rse_pct, na.rm = TRUE), 2) else NA_real_
        )
      }) |> dplyr::bind_rows()
      datatable(comp_df, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 5, dom = "t"))
    })

    # Eigenvalues table
    output$eigen_table <- renderDT({
      ext <- ext_data()
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

    # FIM heatmap
    output$heatmap <- renderPlot({
      fim <- fim_matrix()
      if (is.null(fim)) return(NULL)
      plot_fim_heatmap(fim, labels = param_labels())
    }, res = 110)


  })
}
