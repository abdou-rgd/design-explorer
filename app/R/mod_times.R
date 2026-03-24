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

    # -- Detection robust design (multi-table) ---------------------------------
    is_robust <- reactive({
      tab <- tab_data()
      if (is.null(tab)) return(FALSE)
      "table_no" %in% names(tab) && n_distinct(tab$table_no) > 1L
    })

    # Table 1 uniquement (pour courbe predite + table d'affichage)
    tab_single <- reactive({
      tab <- tab_data()
      req(tab)
      if (is_robust()) filter(tab, table_no == 1L) else tab
    })

    # -- UI dynamique ----------------------------------------------------------
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

      robust_banner <- if (is_robust()) {
        n_tabs <- n_distinct(tab$table_no)
        div(class = "alert alert-info",
            style = "border-radius:8px; margin-bottom:12px; padding:10px 14px;",
          tags$strong(paste0("Design robuste detecte (", n_tabs, " sous-problemes)")),
          tags$p(style = "margin:4px 0 0; font-size:0.9em;",
            "La distribution des temps optimaux est calculee sur l'ensemble des realisations du prior. ",
            "La courbe predite utilise uniquement la premiere realisation."
          )
        )
      } else {
        NULL
      }

      tagList(
        robust_banner,
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
                if (is_robust())
                  "Distribution des temps optimaux par strate (design robuste)"
                else
                  "Temps de sampling optimaux par strate (TSTRAT)"
              ),
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

    # -- Courbe predite --------------------------------------------------------
    output$prediction <- renderPlot({
      req(tab_single())
      plot_model_prediction(tab_single())
    }, res = 110)

    # -- Gantt / distribution --------------------------------------------------
    output$gantt <- renderPlot({
      tab <- tab_data(); req(tab)
      runs <- all_runs()

      # Cas robust design : boxplot de la distribution des temps
      if (is_robust()) {
        obs <- tab
        if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
        if (!"TSTRAT" %in% names(obs)) obs$TSTRAT <- 1
        obs <- obs |> mutate(group = factor(paste0("Strate ", TSTRAT)))
        n_tabs <- n_distinct(tab$table_no)

        return(
          ggplot(obs, aes(x = TIME, y = group, fill = group)) +
            geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.alpha = 0.4) +
            scale_fill_brewer(palette = "Set2", guide = "none") +
            labs(
              title = "Distribution des temps optimaux par strate",
              x     = "Temps (h)",
              y     = NULL,
              caption = paste0(
                "N = ", n_tabs,
                " realisations du prior | Boite = mediane + IQR | ",
                "Un point hors boite = realisation atypique"
              )
            ) +
            theme_bw(base_size = 12) +
            theme(
              panel.grid.major.y = element_blank(),
              plot.caption = element_text(size = 8, color = "#6b7280")
            )
        )
      }

      # Cas multi-run : comparaison des temps par run (comportement existant)
      if (length(runs) > 1L) {
        combined <- purrr::map_dfr(runs, function(r) {
          if (is.null(r$tab_data)) return(NULL)
          obs <- r$tab_data
          if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
          if (nrow(obs) > 1L) obs <- obs[-1L, , drop = FALSE]
          if (!"TSTRAT" %in% names(obs)) obs$TSTRAT <- 1
          obs |> mutate(run = r$name) |>
            select(any_of(c("TSTRAT", "TIME", "run")))
        })
        if (nrow(combined) == 0) {
          return(plot_optimal_times(tab_single()))
        }

        combined <- combined |>
          mutate(group = factor(paste0("Strate ", TSTRAT)))

        return(
          ggplot(combined, aes(x = TIME, y = group,
                               color = run, shape = run)) +
            geom_point(size = 3, alpha = 0.85,
                       position = position_dodge(width = 0.4)) +
            scale_color_manual(values = .RUN_COLORS, name = NULL) +
            labs(title = "Temps de sampling -- Comparaison multi-runs",
                 x = "Temps (h)", y = NULL,
                 caption = "Un point = temps de prelevement optimal pour ce groupe de patients (TSTRAT)") +
            theme_bw(base_size = 12) +
            theme(legend.position = "bottom",
                  panel.grid.major.y = element_blank(),
                  plot.caption = element_text(size = 8, color = "#6b7280"))
        )
      }

      # Cas mono-run : detecter PK-PD (CMT multiple)
      obs_single <- tab_single()
      cmt_col <- if ("CMT" %in% names(obs_single) &&
                     n_distinct(obs_single$CMT) > 1L) "CMT" else NULL
      plot_optimal_times(obs_single, cmt_col = cmt_col)

    }, res = 110)

    # -- Table -----------------------------------------------------------------
    output$times_table <- renderDT({
      tab <- tab_single(); req(tab)
      obs <- tab
      if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
      if (nrow(obs) > 1L) obs <- obs[-1L, , drop = FALSE]

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
