# =============================================================================
# mod_times.R — Onglet Temps optimaux (+ multi-run + model prediction + summary)
# =============================================================================

mod_times_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content"))
  )
}

mod_times_server <- function(id, tab_data, all_runs = reactive(list()), cmt_labels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Tab effectif : tab_data() ou premier run secondaire ayant un .tab ------
    # Permet d'afficher l'onglet Temps optimaux meme si le run principal n'a
    # pas de fichier .tab (ex : run A = evaluation, run B = optimisation).
    effective_tab_data <- reactive({
      tab <- tab_data()
      if (!is.null(tab)) return(tab)
      for (r in all_runs()) {
        if (!is.null(r$tab_data)) return(r$tab_data)
      }
      NULL
    })

    # -- Detection robust design (multi-table) ---------------------------------
    is_robust <- reactive({
      tab <- effective_tab_data()
      if (is.null(tab)) return(FALSE)
      "table_no" %in% names(tab) && n_distinct(tab$table_no) > 1L
    })

    # Table 1 uniquement (pour courbe predite + table d'affichage)
    tab_single <- reactive({
      tab <- effective_tab_data()
      req(tab)
      if (is_robust()) filter(tab, table_no == 1L) else tab
    })

    # Helper: agregation robuste pour un seul run (tab = data.frame brut)
    .robust_summary_one <- function(tab) {
      obs <- tab
      if ("EVID" %in% names(obs)) obs <- dplyr::filter(obs, EVID == 0)
      if (!"TSTRAT" %in% names(obs)) {
        obs <- obs |>
          dplyr::group_by(table_no) |>
          dplyr::mutate(TSTRAT = dplyr::row_number()) |>
          dplyr::ungroup()
      }
      obs <- obs |>
        dplyr::group_by(table_no, TSTRAT) |>
        dplyr::mutate(obs_idx = dplyr::row_number()) |>
        dplyr::ungroup()
      obs |>
        dplyr::group_by(TSTRAT, obs_idx) |>
        dplyr::summarise(
          N_subprob = dplyr::n_distinct(table_no),
          P10       = round(quantile(TIME, 0.10, na.rm = TRUE), 2),
          Mediane   = round(median(TIME,          na.rm = TRUE), 2),
          P90       = round(quantile(TIME, 0.90,  na.rm = TRUE), 2),
          .groups   = "drop"
        ) |>
        dplyr::arrange(TSTRAT, obs_idx) |>
        dplyr::rename(Strate = TSTRAT, Obs = obs_idx, N = N_subprob)
    }

    # Agregation robuste partagee (renderDT + downloadHandler)
    # Multi-run : long format avec colonne Run en tete
    robust_summary <- reactive({
      runs <- all_runs()
      if (length(runs) <= 1L) {
        tab <- tab_data(); req(tab)
        return(.robust_summary_one(tab))
      }
      result <- purrr::imap(runs, function(r, rid) {
        if (is.null(r$tab_data)) return(NULL)
        tab <- r$tab_data
        if (!("table_no" %in% names(tab)) || dplyr::n_distinct(tab$table_no) <= 1L)
          return(NULL)
        .robust_summary_one(tab) |>
          dplyr::mutate(Run = r$name %||% rid, .before = 1)
      }) |> dplyr::bind_rows()
      req(nrow(result) > 0L)
      result
    })

    # -- UI dynamique ----------------------------------------------------------
    output$content <- renderUI({
      tab <- effective_tab_data()

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

      export_btn <- div(style = "text-align: right; margin-bottom: 6px;",
        downloadButton(ns("export_csv"), "Exporter CSV",
                       class = "btn-sm btn-default")
      )

      if (is_robust()) {
        # Design robuste : pas de courbe predite, boxplot central + table resume
        tagList(
          export_btn,
          robust_banner,
          fluidRow(
            column(8,
              div(class = "plot-card",
                p(class = "section-title",
                  "Distribution des temps optimaux par strate (design robuste)"),
                plotOutput(ns("gantt"), height = "420px")
              )
            ),
            column(4,
              div(class = "param-table-wrap",
                p(class = "section-title", "Resume statistique (P10 / mediane / P90)"),
                DTOutput(ns("times_table"))
              )
            )
          )
        )
      } else {
        tagList(
          export_btn,
          fluidRow(
            column(12,
              div(class = "plot-card",
                p(class = "section-title", "Courbe predite et points de sampling"),
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
      }
    })

    # -- Courbe predite --------------------------------------------------------
    output$prediction <- renderPlot({
      runs <- all_runs()
      if (length(runs) <= 1L) {
        req(tab_single())
        return(plot_model_prediction(tab_single()))
      }

      # Multi-run : overlay des courbes predites par run
      combined <- purrr::imap(runs, function(r, rid) {
        if (is.null(r$tab_data)) return(NULL)
        tab <- r$tab_data
        # For multi-subproblem runs, use only table_no 1
        if ("table_no" %in% names(tab) && dplyr::n_distinct(tab$table_no) > 1L)
          tab <- dplyr::filter(tab, table_no == 1L)
        if ("EVID" %in% names(tab)) tab <- dplyr::filter(tab, EVID == 0)
        if (nrow(tab) > 1L) tab <- tab[-1L, , drop = FALSE]
        if (nrow(tab) == 0L) return(NULL)
        pred_col <- intersect(c("IPRED", "PRED", "DV"), names(tab))[1]
        if (is.na(pred_col)) return(NULL)
        tab |>
          dplyr::mutate(IPRED = .data[[pred_col]], run = rid) |>
          dplyr::select(any_of(c("TIME", "IPRED", "TSTRAT", "run")))
      }) |> dplyr::bind_rows()

      if (nrow(combined) == 0L) {
        req(tab_single())
        return(plot_model_prediction(tab_single()))
      }

      run_labels <- setNames(vapply(runs, function(r) r$name %||% "?", character(1L)),
                             names(runs))
      run_colors <- setNames(vapply(names(runs), run_color, character(1L)),
                             names(runs))

      ggplot(combined, aes(x = TIME, y = IPRED, color = run, group = run)) +
        geom_line(linetype = "dashed", alpha = 0.6, size = 0.8) +
        geom_point(size = 2.5, alpha = 0.9) +
        scale_color_manual(values = run_colors, labels = run_labels,
                           name = NULL) +
        .theme_design() +
        labs(
          title    = "Predictions (IPRED) aux temps de sampling optimaux",
          subtitle = "Comparaison multi-runs",
          x = "Temps (h)", y = "IPRED",
          caption  = paste0(
            "Chaque point = prediction du modele a un temps optimal",
            " | Tirets = connexion des points (pas une courbe PK continue)"
          )
        )
    }, res = 110)

    # -- Gantt / distribution --------------------------------------------------
    output$gantt <- renderPlot({
      tab <- effective_tab_data()
      runs <- all_runs()

      # Cas robust design : boxplot + annotations medianes + multi-run overlay
      if (is_robust()) {
        req(tab)
        obs <- tab
        if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
        # Use obs position within each subproblem as strate when TSTRAT absent
        if (!"TSTRAT" %in% names(obs)) {
          obs <- obs |>
            dplyr::group_by(table_no) |>
            dplyr::mutate(TSTRAT = dplyr::row_number()) |>
            dplyr::ungroup()
        }
        obs <- obs |> mutate(group = factor(paste0("Strate ", TSTRAT)))
        n_tabs <- n_distinct(tab$table_no)

        med_labels <- obs |>
          dplyr::group_by(group) |>
          dplyr::summarise(med = median(TIME), .groups = "drop")

        p <- ggplot(obs, aes(x = TIME, y = group, fill = group)) +
          geom_boxplot(alpha = 0.7, outlier.size = 0.8,
                       outlier.alpha = 0.4) +
          geom_text(data = med_labels,
                    aes(x = med, y = group,
                        label = sprintf("%.1fh", med)),
                    inherit.aes = FALSE,
                    vjust = -0.6, size = 3.2, fontface = "bold",
                    color = "#1e3a5f") +
          scale_fill_brewer(palette = "Set2", guide = "none")

        # Multi-run overlay: comparison runs' optimal times as colored points
        if (length(runs) > 1L) {
          comp_pts <- purrr::imap(runs[-1], function(r, rid) {
            if (is.null(r$tab_data)) return(NULL)
            comp_tab <- r$tab_data
            if ("table_no" %in% names(comp_tab) &&
                dplyr::n_distinct(comp_tab$table_no) > 1L)
              comp_tab <- dplyr::filter(comp_tab, table_no == 1L)
            if ("EVID" %in% names(comp_tab))
              comp_tab <- dplyr::filter(comp_tab, EVID == 0)
            if (nrow(comp_tab) == 0L) return(NULL)
            if (!"TSTRAT" %in% names(comp_tab))
              comp_tab$TSTRAT <- seq_len(nrow(comp_tab))
            comp_tab |>
              dplyr::mutate(group = factor(paste0("Strate ", TSTRAT)),
                            run = rid) |>
              dplyr::select(dplyr::any_of(c("TIME", "group", "run")))
          }) |> dplyr::bind_rows()

          if (nrow(comp_pts) > 0L) {
            run_labels <- setNames(
              vapply(runs[-1], function(r) r$name %||% "?", character(1L)),
              names(runs[-1]))
            run_colors <- setNames(
              vapply(names(runs[-1]), run_color, character(1L)),
              names(runs[-1]))
            p <- p +
              geom_point(data = comp_pts,
                         aes(x = TIME, y = group, color = run),
                         inherit.aes = FALSE, size = 3, alpha = 0.85,
                         position = position_dodge(width = 0.3)) +
              scale_color_manual(values = run_colors, labels = run_labels,
                                 name = NULL)
          }
        }

        caption_txt <- paste0(
          "N = ", n_tabs,
          " realisations du prior | Boite = mediane + IQR | ",
          "Chiffre = mediane")
        if (length(runs) > 1L)
          caption_txt <- paste0(caption_txt,
                                " | Points colores = runs de comparaison")

        return(
          p +
            labs(
              title = "Distribution des temps optimaux par strate",
              x     = "Temps (h)",
              y     = NULL,
              caption = caption_txt
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
        combined <- purrr::imap(runs, function(r, rid) {
          if (is.null(r$tab_data)) return(NULL)
          obs <- r$tab_data
          if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
          if (nrow(obs) > 1L) obs <- obs[-1L, , drop = FALSE]
          if (!"TSTRAT" %in% names(obs)) obs$TSTRAT <- 1
          obs |> mutate(run = rid) |>
            select(any_of(c("TSTRAT", "TIME", "run")))
        }) |> dplyr::bind_rows()
        if (nrow(combined) == 0) {
          return(plot_optimal_times(tab_single()))
        }

        combined <- combined |>
          mutate(group = factor(paste0("Strate ", TSTRAT)))

        run_labels <- setNames(vapply(runs, function(r) r$name %||% "?", character(1L)),
                               names(runs))
        run_colors <- setNames(vapply(names(runs), run_color, character(1L)),
                               names(runs))
        return(
          ggplot(combined, aes(x = TIME, y = group,
                               color = run, shape = run)) +
            geom_point(size = 3, alpha = 0.85,
                       position = position_dodge(width = 0.4)) +
            scale_color_manual(values = run_colors, labels = run_labels,
                               name = NULL) +
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

      # Appliquer labels CMT dans le df avant le plot
      lbls <- cmt_labels()
      obs_plot <- obs_single
      if (!is.null(lbls) && !is.null(cmt_col) && "CMT" %in% names(obs_plot)) {
        obs_plot <- obs_plot |>
          mutate(CMT = ifelse(
            as.character(CMT) %in% names(lbls),
            paste0(lbls[as.character(CMT)], " (CMT=", CMT, ")"),
            as.character(CMT)
          ))
      }
      plot_optimal_times(obs_plot, cmt_col = cmt_col)

    }, res = 110)

    # -- Table -----------------------------------------------------------------
    output$times_table <- renderDT({
      tab <- effective_tab_data(); req(tab)

      if (is_robust()) {
        return(datatable(robust_summary(), rownames = FALSE,
                         class = "stripe hover compact",
                         options = list(pageLength = 30, dom = "t",
                                        scrollX = TRUE)))
      }

      # Cas normal : table des temps individuels
      obs <- tab_single()
      if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
      if (nrow(obs) > 1L) obs <- obs[-1L, , drop = FALSE]

      cols_show <- intersect(
        c("TSTRAT", "TIME", "IPRED", "CONC", "STRAT", "CMT"),
        names(obs)
      )
      if (length(cols_show) == 0L) {
        cols_show <- names(obs)[!names(obs) %in% c("table_no")]
      }

      obs_display <- obs |>
        select(all_of(cols_show)) |>
        mutate(across(where(is.double), ~ round(.x, 4)))

      # Appliquer les labels CMT si disponibles
      lbls <- cmt_labels()
      if (!is.null(lbls) && "CMT" %in% names(obs_display)) {
        obs_display <- obs_display |>
          mutate(CMT = ifelse(
            as.character(CMT) %in% names(lbls),
            paste0(lbls[as.character(CMT)], " (CMT=", CMT, ")"),
            as.character(CMT)
          ))
      }

      datatable(obs_display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(pageLength = 20, dom = "tip",
                               scrollX = TRUE))
    })

    # -- Export CSV ------------------------------------------------------------
    output$export_csv <- downloadHandler(
      filename = function() {
        paste0("temps_optimaux_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        tab <- effective_tab_data()
        req(tab)

        if (is_robust()) {
          export_df <- robust_summary()
        } else {
          obs <- tab_single()
          if ("EVID" %in% names(obs)) obs <- dplyr::filter(obs, EVID == 0)
          if (nrow(obs) > 1L) obs <- obs[-1L, , drop = FALSE]
          cols_show <- intersect(
            c("TSTRAT", "TIME", "IPRED", "CONC", "STRAT", "CMT"),
            names(obs)
          )
          if (length(cols_show) == 0L) {
            cols_show <- names(obs)[!names(obs) %in% c("table_no")]
          }
          lbls <- cmt_labels()
          export_df <- obs |>
            dplyr::select(dplyr::all_of(cols_show)) |>
            dplyr::mutate(dplyr::across(where(is.double), ~ round(.x, 4)))
          if (!is.null(lbls) && "CMT" %in% names(export_df)) {
            export_df <- export_df |>
              dplyr::mutate(CMT = ifelse(
                as.character(CMT) %in% names(lbls),
                paste0(lbls[as.character(CMT)], " (CMT=", CMT, ")"),
                as.character(CMT)
              ))
          }
        }

        tryCatch(
          readr::write_csv(export_df, file),
          error = function(e) warning("CSV export failed: ", conditionMessage(e))
        )
      }
    )
  })
}
