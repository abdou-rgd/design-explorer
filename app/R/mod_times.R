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
        return(div(class = "alert alert-info", style = "border-radius:10px; margin:16px 0;",
          tags$strong("Onglet Temps optimaux"),
          tags$p(style = "margin:6px 0 0;",
            "Disponible quand :",
            tags$ul(style = "margin:4px 0;",
              tags$li("Un fichier ", tags$code(".tab"), " est charge (genere par ",
                      tags$code("$TABLE"), " dans le fichier de controle)"),
              tags$li("Le .tab contient les temps de sampling optimises (",
                      tags$code("TIME"), ", ", tags$code("TSTRAT"), ")")
            )
          )
        ))
      }

      # Standard .tab section
      tagList(
        fluidRow(
          column(12,
            div(class = "plot-card",
              p(class = "section-title",
                "Courbe predite et points de sampling"),
              plotOutput(ns("prediction"), height = "400px")
            )
          )
        ),
        br(),
        fluidRow(
          column(7,
            div(class = "plot-card",
              p(class = "section-title",
                "Temps de sampling optimaux par strate (TSTRAT)"),
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
      tab <- tab_data(); req(tab)
      plot_model_prediction(tab)
    }, res = 110)

    output$gantt <- renderPlot({
      tab <- tab_data(); req(tab)
      runs <- all_runs()
      if (length(runs) <= 1) return(plot_optimal_times(tab))

      combined <- purrr::map_dfr(runs, function(r) {
        if (is.null(r$tab_data)) return(NULL)
        obs <- r$tab_data
        if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
        if (nrow(obs) > 1L) obs <- obs[-1L, , drop = FALSE]  # exclure ligne dose (TIME=0, AMT>0)
        if (!"TSTRAT" %in% names(obs)) obs$TSTRAT <- 1
        obs |> mutate(run = r$name) |>
          select(any_of(c("TSTRAT", "TIME", "run")))
      })
      if (nrow(combined) == 0) return(plot_optimal_times(tab))

      combined <- combined |>
        mutate(group = factor(paste0("Strate ", TSTRAT)))

      ggplot(combined, aes(x = TIME, y = group,
                           color = run, shape = run)) +
        geom_point(size = 3, alpha = 0.85,
                   position = position_dodge(width = 0.4)) +
        scale_color_manual(values = .RUN_COLORS, name = NULL) +
        labs(title = "Temps de sampling -- Comparaison multi-runs",
             x = "Temps (h)", y = NULL) +
        theme_bw(base_size = 12) +
        theme(legend.position = "bottom",
              panel.grid.major.y = element_blank())
    }, res = 110)

    output$times_table <- renderDT({
      tab <- tab_data(); req(tab)
      obs <- tab
      if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
      if (nrow(obs) > 1L) obs <- obs[-1L, , drop = FALSE]  # exclure ligne dose (TIME=0, AMT>0)

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
