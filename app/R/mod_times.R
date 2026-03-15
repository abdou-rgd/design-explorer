# =============================================================================
# mod_times.R — Onglet Temps optimaux (+ multi-run + model prediction + summary)
# =============================================================================

mod_times_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content"))
  )
}

mod_times_server <- function(id, tab_data, all_runs = reactive(list()),
                             summary_data = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$content <- renderUI({
      tab <- tab_data()
      summ <- summary_data()

      if (is.null(tab) && is.null(summ)) {
        return(div(class = "alert alert-warning",
                   "Fichier .tab requis pour cet onglet."))
      }

      ui_parts <- list()

      # Summary.tab section (robust design)
      if (!is.null(summ)) {
        # Build tables for TIME and IPRED
        summ_panels <- list()
        for (var_name in c("TIME", "IPRED")) {
          if (var_name %in% names(summ)) {
            summ_panels <- c(summ_panels, list(
              div(class = "param-table-wrap", style = "margin-bottom:14px;",
                p(class = "section-title",
                  paste0("Distribution robuste : ", var_name,
                         " (", nrow(summ[[var_name]]), " points)")),
                DTOutput(ns(paste0("summary_", tolower(var_name))))
              )
            ))
          }
        }
        # Also show other variables if present
        other_vars <- setdiff(names(summ), c("TIME", "IPRED", "ID", "EVID", "MDV"))
        for (var_name in other_vars) {
          summ_panels <- c(summ_panels, list(
            div(class = "param-table-wrap", style = "margin-bottom:14px;",
              p(class = "section-title",
                paste0("Distribution robuste : ", var_name)),
              DTOutput(ns(paste0("summary_", tolower(var_name))))
            )
          ))
        }

        ui_parts <- c(ui_parts, list(
          div(class = "plot-card",
            p(class = "section-title",
              "Robust Design -- Distribution des temps et predictions"),
            plotOutput(ns("summary_plot"), height = "380px")
          ),
          br()
        ), summ_panels, list(br()))
      }

      # Standard .tab section
      if (!is.null(tab)) {
        ui_parts <- c(ui_parts, list(
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
        ))
      }

      do.call(tagList, ui_parts)
    })

    # -- Summary plot: TIME distribution with CI ribbons ----------------------
    output$summary_plot <- renderPlot({
      summ <- summary_data(); req(summ)
      time_df <- summ[["TIME"]]
      ipred_df <- summ[["IPRED"]]
      req(time_df, ipred_df)

      # Build plot data: for each row, show IPRED mean vs TIME mean with CI
      plot_df <- tibble(
        row = time_df$Row,
        time_mean = time_df$Mean,
        time_lo = if ("2.50%" %in% names(time_df)) time_df[["2.50%"]] else time_df$Low,
        time_hi = if ("97.50%" %in% names(time_df)) time_df[["97.50%"]] else time_df$High,
        ipred_mean = ipred_df$Mean,
        ipred_lo = if ("2.50%" %in% names(ipred_df)) ipred_df[["2.50%"]] else ipred_df$Low,
        ipred_hi = if ("97.50%" %in% names(ipred_df)) ipred_df[["97.50%"]] else ipred_df$High
      )

      # Filter out dose row (EVID=1, typically row 1 with TIME=0, IPRED=0)
      obs_df <- plot_df |> filter(ipred_mean > 0 | time_mean > 0)

      ggplot(obs_df, aes(x = time_mean, y = ipred_mean)) +
        geom_ribbon(aes(ymin = ipred_lo, ymax = ipred_hi),
                    fill = "#3b82f6", alpha = 0.15) +
        geom_errorbarh(aes(xmin = time_lo, xmax = time_hi),
                       height = 0, color = "#6b7280", linewidth = 0.4) +
        geom_point(size = 3.5, color = "#2563eb") +
        geom_line(linewidth = 0.6, color = "#2563eb", alpha = 0.5) +
        labs(title = "Prediction moyenne et IC 95% (1000 replications prior)",
             subtitle = "Barres horizontales = IC 95% temps, ruban = IC 95% IPRED",
             x = "Temps (Mean +/- IC 95%)",
             y = "IPRED (Mean +/- IC 95%)") +
        theme_bw(base_size = 11) +
        theme(panel.grid.minor = element_blank())
    }, res = 110)

    # -- Summary DT tables ----------------------------------------------------
    observe({
      summ <- summary_data()
      if (is.null(summ)) return()

      show_vars <- setdiff(names(summ), c("ID", "EVID", "MDV"))
      for (var_name in show_vars) {
        local({
          local_var <- var_name
          output_id <- paste0("summary_", tolower(local_var))
          output[[output_id]] <- renderDT({
            df <- summ[[local_var]]
            req(df)
            df <- df |> mutate(across(where(is.double), ~ round(.x, 4)))
            datatable(df, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = 20, dom = "tip",
                                     scrollX = TRUE))
          })
        })
      }
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

      combined <- purrr::imap_dfr(runs, function(r, idx) {
        if (is.null(r$tab_data)) return(NULL)
        obs <- r$tab_data
        if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
        if (!"TSTRAT" %in% names(obs)) obs$TSTRAT <- 1
        obs |> mutate(run = r$name) |>
          select(any_of(c("TSTRAT", "TIME", "run")))
      })
      if (nrow(combined) == 0) return(plot_optimal_times(tab))

      combined <- combined |>
        mutate(group = factor(paste0("Groupe ", TSTRAT)))

      ggplot(combined, aes(x = TIME, y = group,
                           color = run, shape = run)) +
        geom_point(size = 3, alpha = 0.85,
                   position = position_dodge(width = 0.4)) +
        scale_color_manual(values = .RUN_COLORS, name = NULL) +
        labs(title = "Temps d'echantillonnage -- Comparaison multi-runs",
             x = "Temps", y = NULL) +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom",
              panel.grid.major.y = element_blank())
    }, res = 110)

    output$times_table <- renderDT({
      tab <- tab_data(); req(tab)
      obs <- tab
      if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)

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
