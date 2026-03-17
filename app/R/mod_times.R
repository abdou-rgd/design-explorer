# =============================================================================
# mod_times.R — Onglet Temps optimaux (+ multi-run + model prediction + summary)
# =============================================================================

mod_times_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content"))
  )
}

mod_times_server <- function(id, tab_data, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$content <- renderUI({
      tab <- tab_data()

      if (is.null(tab)) {
        return(div(class = "alert alert-warning",
                   "Fichier .tab requis pour cet onglet."))
      }

      # Standard .tab section
      tagList(
        fluidRow(
          column(12,
            div(class = "plot-card",
              p(class = "section-title",
                "Prediction du modele et temps de sampling"),
              plotOutput(ns("prediction"), height = "350px")
            )
          )
        ),
        br(),
        fluidRow(
          column(7,
            div(class = "plot-card",
              p(class = "section-title",
                "Temps d'echantillonnage optimaux par groupe"),
              plotOutput(ns("gantt"), height = "380px")
            )
          ),
          column(5,
            div(class = "param-table-wrap",
              p(class = "section-title", "Donnees temps optimaux"),
              DTOutput(ns("times_table"))
            )
          )
        )
      )
    })

    # -- Standard .tab outputs ------------------------------------------------
    output$prediction <- renderPlot({
      tab <- tab_data()
      if (is.null(tab)) return(NULL)
      plot_model_prediction(tab)
    }, res = 110)

    output$gantt <- renderPlot({
      tab <- tab_data()
      if (is.null(tab)) return(NULL)
      runs <- all_runs()
      if (length(runs) <= 1) return(plot_optimal_times(tab))

      combined <- purrr::imap(runs, function(r, idx) {
        if (is.null(r$tab_data)) return(NULL)
        obs <- prepare_tab_obs(r$tab_data)
        obs |> mutate(run = r$name) |>
          select(any_of(c("TSTRAT", "TIME", "run")))
      }) |> purrr::list_rbind()
      if (nrow(combined) == 0) return(plot_optimal_times(tab))

      combined <- combined |>
        mutate(group = factor(paste0("Groupe ", TSTRAT)))

      ggplot(combined, aes(x = TIME, y = group,
                           color = run, shape = run)) +
        geom_point(size = 3, alpha = 0.85,
                   position = position_dodge(width = 0.4)) +
        scale_color_manual(values = .RUN_COLORS, name = NULL) +
        labs(title = "Temps d'echantillonnage -- Comparaison multi-runs",
             x = "Temps (source : .tab)", y = "Strate",
             caption = "Source : .tab") +
        .theme_design() +
        theme(panel.grid.major.y = element_blank())
    }, res = 110)

    output$times_table <- renderDT({
      tab <- tab_data()
      if (is.null(tab)) return(NULL)
      obs <- prepare_tab_obs(tab)

      cols_show <- intersect(
        c("TSTRAT", "TIME", "IPRED", "CONC", "STRAT", "CMT"),
        names(obs)
      )
      if (length(cols_show) == 0) {
        cols_show <- names(obs)[!names(obs) %in% c("table_no")]
      }

      obs_display <- obs |>
        select(all_of(cols_show)) |>
        mutate(across(where(is.double), ~ round(.x, 4)))

      datatable(obs_display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(pageLength = 20, dom = "tip",
                               scrollX = TRUE))
    })
  })
}
