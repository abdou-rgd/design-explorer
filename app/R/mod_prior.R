# =============================================================================
# mod_prior.R — Onglet Design Robuste ($PRIOR / $SIM TRUE=PRIOR)
# =============================================================================

mod_prior_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("content")))
}

mod_prior_server <- function(id, summary_data, ctl_data = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$content <- renderUI({
      summ  <- summary_data()
      prior <- ctl_data()

      has_summ  <- !is.null(summ) && !is.null(summ[["TIME"]])
      has_prior <- !is.null(prior) && isTRUE(prior$has_prior)

      if (!has_summ && !has_prior) {
        return(div(class = "alert alert-info", style = "border-radius:10px; margin:16px 0;",
          tags$strong("Onglet Design Robuste"),
          tags$p(style = "margin:6px 0 0;",
            "Disponible quand :",
            tags$ul(style = "margin:4px 0;",
              tags$li(tags$code("$PRIOR NWPRI"), " present dans le fichier .ctl"),
              tags$li(tags$code("summary.tab"), " genere par ",
                      tags$code("$SIM TRUE=PRIOR SUBPROB=1000"))
            )
          )
        ))
      }

      parts <- list()

      # -- Bandeau pedagogique ------------------------------------------------
      parts <- c(parts, list(
        div(class = "guide-banner", style = "margin-bottom:16px;",
          tags$strong("Principe du design robuste"),
          tags$p(style = "margin:6px 0 0; font-size:.88rem;",
            "Les temps de prelevement sont optimises independamment pour",
            tags$strong("1000 jeux de parametres"), "tires du prior ($SIM TRUE=PRIOR SUBPROB=1000).",
            "Les plages [2.5% - 97.5%] ci-dessous indiquent les temps qui restent",
            "informatifs quelle que soit l'incertitude parametrique.",
            "La procedure recommandee : choisir des temps dans ces plages, puis",
            "re-evaluer le design resultant pour verifier la perte d'efficience."
          )
        )
      ))

      # -- Section A : Prior utilise ------------------------------------------
      if (has_prior) {
        parts <- c(parts, list(
          div(class = "param-table-wrap",
            p(class = "section-title", "Prior utilise ($PRIOR NWPRI)"),
            uiOutput(ns("prior_cards")),
            br(),
            DTOutput(ns("prior_thetap"))
          ),
          br()
        ))
      }

      # -- Section B : Distribution des temps robustes ------------------------
      if (has_summ) {
        parts <- c(parts, list(
          div(class = "plot-card",
            p(class = "section-title",
              "Distribution des temps optimaux sur 1000 replications prior"),
            plotOutput(ns("robust_plot"), height = "380px")
          ),
          br(),
          div(class = "param-table-wrap",
            p(class = "section-title",
              HTML("Plages robustes des temps d'echantillonnage &mdash;
                    <span style='color:#6b7280; font-size:.85em;'>
                    Colonnes : Mean | STD | RSTD(%) | 2.5% | 97.5%</span>")),
            p(style = "font-size:.82rem; color:#6b7280; margin:-4px 0 8px;",
              "Ligne 1 = TSTRAT 1, ligne 2 = TSTRAT 2, etc. (heure 0 / dose exclue)."),
            DTOutput(ns("robust_table"))
          )
        ))
      }

      do.call(tagList, parts)
    })

    # -------------------------------------------------------------------------
    # Cartes Prior
    # -------------------------------------------------------------------------
    output$prior_cards <- renderUI({
      prior <- ctl_data(); req(prior, prior$has_prior)
      fluidRow(
        column(4, metric_card("Type", "NWPRI", prior$raw_prior_line %||% "$PRIOR", "blue")),
        column(4, metric_card("PLEV",
                              if (!is.null(prior$plev) && !is.na(prior$plev)) prior$plev else "N/A",
                              "Niveau d'acceptation", "purple")),
        column(4, metric_card("THETAP", length(prior$thetap),
                              "parametres avec prior", "green"))
      )
    })

    # -------------------------------------------------------------------------
    # Table THETAP
    # -------------------------------------------------------------------------
    output$prior_thetap <- renderDT({
      prior <- ctl_data(); req(prior, prior$has_prior, length(prior$thetap) > 0)
      tp <- tibble(
        Parametre = paste0("THETA", seq_along(prior$thetap)),
        `Prior (THETAP)` = prior$thetap,
        `Prior SD`       = if (!is.null(prior$thetapv)) round(sqrt(diag(prior$thetapv)), 4) else NA_real_
      )
      datatable(tp, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 15, dom = "t"))
    })

    # -------------------------------------------------------------------------
    # Plot robuste (TIME mean +/- IC 95% vs IPRED mean +/- IC 95%)
    # -------------------------------------------------------------------------
    output$robust_plot <- renderPlot({
      summ <- summary_data(); req(summ)
      time_df  <- summ[["TIME"]];  req(time_df)
      ipred_df <- summ[["IPRED"]]; req(ipred_df)

      # Exclure la ligne dose (row 1, TIME=0)
      time_df  <- time_df[-1, , drop = FALSE]
      ipred_df <- ipred_df[-1, , drop = FALSE]

      lo_col_t <- if ("2.50%"  %in% names(time_df))  "2.50%"  else "Low"
      hi_col_t <- if ("97.50%" %in% names(time_df))  "97.50%" else "High"
      lo_col_i <- if ("2.50%"  %in% names(ipred_df)) "2.50%"  else "Low"
      hi_col_i <- if ("97.50%" %in% names(ipred_df)) "97.50%" else "High"

      plot_df <- tibble(
        tstrat     = paste0("TSTRAT ", seq_len(nrow(time_df))),
        time_mean  = time_df$Mean,
        time_lo    = time_df[[lo_col_t]],
        time_hi    = time_df[[hi_col_t]],
        ipred_mean = ipred_df$Mean,
        ipred_lo   = ipred_df[[lo_col_i]],
        ipred_hi   = ipred_df[[hi_col_i]]
      )

      ggplot(plot_df, aes(x = time_mean, y = ipred_mean, label = tstrat)) +
        geom_ribbon(aes(ymin = ipred_lo, ymax = ipred_hi),
                    fill = "#3b82f6", alpha = 0.13) +
        geom_errorbarh(aes(xmin = time_lo, xmax = time_hi),
                       height = 0, color = "#6b7280", linewidth = 0.5) +
        geom_point(size = 4, color = "#2563eb") +
        geom_line(linewidth = 0.6, color = "#2563eb", alpha = 0.45) +
        geom_text(size = 3.2, color = "#374151", vjust = -1) +
        labs(
          title    = "Prediction moyenne et IC 95% (1000 replications prior)",
          subtitle = "Barres horizontales = IC 95% des temps  |  Ruban = IC 95% IPRED",
          x        = "Temps (h) — Mean +/- IC 95%",
          y        = "IPRED — Mean +/- IC 95%"
        ) +
        theme_bw(base_size = 11) +
        theme(panel.grid.minor = element_blank())
    }, res = 110)

    # -------------------------------------------------------------------------
    # Table des plages robustes (TIME uniquement, filtre dose)
    # -------------------------------------------------------------------------
    output$robust_table <- renderDT({
      summ <- summary_data(); req(summ)
      time_df <- summ[["TIME"]]; req(time_df)

      # Exclure la ligne dose (row 1)
      df <- time_df[-1, , drop = FALSE]

      lo_col <- if ("2.50%"  %in% names(df)) "2.50%"  else "Low"
      hi_col <- if ("97.50%" %in% names(df)) "97.50%" else "High"

      display <- tibble(
        TSTRAT        = seq_len(nrow(df)),
        `Mean (h)`    = round(df$Mean, 3),
        `STD`         = round(df$STD,  3),
        `RSTD (%)`    = round(df$RSTD, 1),
        `2.5% (h)`    = round(df[[lo_col]], 3),
        `97.5% (h)`   = round(df[[hi_col]], 3),
        `Plage suggeree` = paste0(
          "[", round(floor(df[[lo_col]] * 4) / 4, 2),
          " \u2013 ", round(ceiling(df[[hi_col]] * 4) / 4, 2), "]"
        )
      )

      datatable(display, rownames = FALSE, class = "stripe hover compact",
                options = list(dom = "t", ordering = FALSE)) |>
        formatStyle("RSTD (%)",
          color = styleInterval(c(25, 50), c("#16a34a", "#d97706", "#dc2626")),
          fontWeight = "bold"
        ) |>
        formatStyle("Plage suggeree",
          fontWeight = "bold", color = "#2563eb"
        )
    })
  })
}
