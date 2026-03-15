# =============================================================================
# mod_params.R — Onglet Parametres
# =============================================================================

mod_params_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    br(),
    div(
      class = "param-table-wrap",
      p(class = "section-title", "Table des parametres du design"),
      DTOutput(ns("param_table"))
    ),
    br(),
    div(
      class = "plot-card",
      p(class = "section-title", "Shrinkage EBV par ETA"),
      uiOutput(ns("shrinkage_content"))
    ),
    br(),
    div(
      class = "param-table-wrap",
      p(class = "section-title", "Priors ($PRIOR NWPRI)"),
      uiOutput(ns("prior_content"))
    )
  )
}

mod_params_server <- function(id, ext_data, shk_data, ext_lines, tbl_no, param_labels, ctl_data = reactive(NULL), all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {

    rse_r <- reactive({
      ext <- ext_data(); req(ext)
      get_rse(ext, tbl_no())
    })

    ri_r <- reactive({
      shk <- shk_data()
      if (is.null(shk)) return(tibble(eta = character(), relativeinf_pct = numeric()))
      get_relativeinf(shk, tbl_no())
    })

    # Metric cards
    output$cards <- renderUI({
      ext <- ext_data()
      if (is.null(ext)) {
        return(div(class = "alert alert-info", style = "border-radius:10px;",
                   "Chargez un fichier .ext pour commencer l'analyse."))
      }

      ofv  <- get_ofv(ext, tbl_no())
      rse  <- rse_r()
      lines <- ext_lines()
      crit <- if (!is.null(lines)) detect_criterion(lines) else "D-OPTIMALITY"
      n_blocs <- n_distinct(ext$table_no)

      n_good <- sum(rse$rse_pct < 20,  na.rm = TRUE)
      n_mod  <- sum(rse$rse_pct >= 20 & rse$rse_pct < 50, na.rm = TRUE)
      n_poor <- sum(rse$rse_pct >= 50, na.rm = TRUE)
      n_tot  <- nrow(rse)

      fluidRow(
        column(3, metric_card("Critere OFV", round(ofv, 4), crit, "blue")),
        column(3, metric_card("Parametres estimes", n_tot,
                              sprintf("%d blocs $DESIGN", n_blocs), "purple")),
        column(3, metric_card("RSE < 20%", n_good,
                              sprintf("sur %d params", n_tot), "green")),
        column(3, metric_card("RSE > 50%", n_poor,
                              if (n_mod > 0) sprintf("+ %d entre 20-50%%", n_mod) else "tous estimables",
                              "orange"))
      )
    })

    # Table parametres DT
    output$param_table <- renderDT({
      ext <- ext_data(); req(ext)
      rse  <- rse_r()
      ri   <- ri_r()
      lbls <- param_labels()

      rse <- rse |>
        mutate(
          param_type = case_when(
            str_starts(param, "THETA")                    ~ "THETA",
            str_detect(param, "^OMEGA\\((\\d+),\\1\\)$") ~ "OMEGA (diag.)",
            str_starts(param, "OMEGA")                   ~ "OMEGA (off-diag.)",
            str_detect(param, "^SIGMA\\((\\d+),\\1\\)$") ~ "SIGMA (diag.)",
            str_starts(param, "SIGMA")                   ~ "SIGMA (off-diag.)",
            TRUE                                         ~ "Autre"
          ),
          sort_key = case_when(
            str_starts(param, "THETA") ~ 1L,
            str_starts(param, "OMEGA") ~ 2L,
            str_starts(param, "SIGMA") ~ 3L,
            TRUE                       ~ 4L
          )
        ) |>
        arrange(sort_key, param)

      if (nrow(ri) > 0) {
        ri_indexed <- ri |>
          mutate(eta_idx = as.integer(str_extract(eta, "\\d+")))
        rse <- rse |>
          mutate(eta_idx = if_else(
            str_detect(param, "^OMEGA\\((\\d+),\\1\\)$"),
            as.integer(str_extract(str_extract(param, "\\d+"), "\\d+")),
            NA_integer_
          )) |>
          left_join(ri_indexed |> select(eta_idx, relativeinf_pct), by = "eta_idx") |>
          select(-eta_idx)
      } else {
        rse <- rse |> mutate(relativeinf_pct = NA_real_)
      }

      if (!is.null(lbls)) {
        rse <- rse |> mutate(label = if_else(param %in% names(lbls), lbls[param], param))
      } else {
        rse <- rse |> mutate(label = param)
      }

      tab <- rse |>
        transmute(
          Type            = param_type,
          Parametre       = label,
          Estime          = signif(estimate, 4),
          `SE (FIM)`      = signif(se, 3),
          `RSE (%)`       = round(rse_pct, 2),
          `RelInf (%)`    = if_else(!is.na(relativeinf_pct), round(relativeinf_pct, 2), NA_real_)
        )

      datatable(
        tab, rownames = FALSE, class = "stripe hover compact",
        options = list(
          pageLength = 25, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = 2:5))
        )
      ) |>
        formatStyle("RSE (%)",
          color = styleInterval(c(20, 50), c("#16a34a", "#d97706", "#dc2626")),
          fontWeight = "bold"
        ) |>
        formatStyle("RelInf (%)",
          color = styleInterval(c(20, 50), c("#dc2626", "#d97706", "#16a34a")),
          fontWeight = "bold"
        ) |>
        formatStyle("Type",
          backgroundColor = styleEqual(
            c("THETA", "OMEGA (diag.)", "OMEGA (off-diag.)", "SIGMA (diag.)", "SIGMA (off-diag.)"),
            c("#dbeafe", "#ede9fe", "#f3e8ff", "#fce7f3", "#fff1f2")
          )
        )
    })

    # Shrinkage EBV (TYPE 4)
    shrk_r <- reactive({
      shk <- shk_data()
      if (is.null(shk)) return(tibble(eta = character(), shrinkage_pct = numeric()))
      get_shrinkage(shk, tbl_no())
    })

    output$shrinkage_content <- renderUI({
      shrk <- shrk_r()
      ns <- session$ns
      if (nrow(shrk) == 0L) {
        return(div(class = "alert alert-info",
                   "Fichier .shk requis pour afficher les shrinkages."))
      }
      tagList(
        plotOutput(ns("shrinkage_plot"), height = "320px"),
        br(),
        DTOutput(ns("shrinkage_table"))
      )
    })

    output$shrinkage_plot <- renderPlot({
      shrk <- shrk_r(); req(nrow(shrk) > 0)
      lbls <- param_labels()
      if (!is.null(lbls)) {
        eta_labels <- setNames(lbls, paste0("ETA", seq_along(lbls)))
        shrk <- shrk |>
          mutate(eta = if_else(eta %in% names(eta_labels), eta_labels[eta], eta))
      }
      shrk <- shrk |>
        mutate(
          quality = factor(
            if_else(shrinkage_pct > 30, "> 30% (elevee)", "<= 30% (acceptable)"),
            levels = c("<= 30% (acceptable)", "> 30% (elevee)")
          )
        )
      ggplot(shrk, aes(x = reorder(eta, shrinkage_pct), y = shrinkage_pct, fill = quality)) +
        geom_col(width = 0.65, color = "white", linewidth = 0.3) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "grey40", linewidth = 0.45) +
        geom_text(aes(label = sprintf("%.2f%%", shrinkage_pct)),
                  hjust = -0.12, size = 3.2, color = "grey25") +
        scale_fill_manual(
          values = c("<= 30% (acceptable)" = "#4CAF50", "> 30% (elevee)" = "#F44336"),
          name = NULL, drop = FALSE
        ) +
        coord_flip() +
        labs(title = "Shrinkage EBV (%) par ETA", x = NULL, y = "Shrinkage (%)") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank())
    }, res = 110)

    output$shrinkage_table <- renderDT({
      shk <- shk_data(); req(shk)
      tbl <- tbl_no()
      shk_tbl <- shk |>
        filter(.data$table_no == tbl) |>
        mutate(type_label = case_when(
          type_id == 4L  ~ "EBV Shrinkage SD (%)",
          type_id == 5L  ~ "EBV Shrinkage VR (%)",
          type_id == 8L  ~ "EPS Shrinkage SD (%)",
          type_id == 11L ~ "RELATIVEINF (%)",
          TRUE           ~ paste("Type", type_id)
        )) |>
        select(-c(table_no, subpop)) |>
        select(type_label, type_id, everything()) |>
        mutate(across(where(is.double), ~ round(.x, 2)))

      datatable(shk_tbl, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 15, dom = "tip", scrollX = TRUE))
    })

    # Prior display
    output$prior_content <- renderUI({
      prior <- ctl_data()
      if (is.null(prior) || !prior$has_prior) {
        return(div(class = "alert alert-info",
                   "Uploadez un fichier .ctl contenant $PRIOR NWPRI pour afficher les priors."))
      }
      ns <- session$ns
      tagList(
        fluidRow(
          column(4, metric_card("Type", "NWPRI", prior$raw_prior_line, "blue")),
          column(4, metric_card("PLEV", if (!is.na(prior$plev)) prior$plev else "N/A",
                                "Niveau d'acceptation", "purple")),
          column(4, metric_card("THETAP", length(prior$thetap),
                                "parametres avec prior", "green"))
        ),
        br(),
        DTOutput(ns("prior_thetap_table")),
        if (!is.null(prior$thetapv)) {
          tagList(
            br(),
            p(class = "section-title", "Matrice variance-covariance des priors (THETAPV)"),
            DTOutput(ns("prior_thetapv_table"))
          )
        }
      )
    })

    output$prior_thetap_table <- renderDT({
      prior <- ctl_data(); req(prior, prior$has_prior, length(prior$thetap) > 0)
      lbls <- param_labels()
      tp <- tibble(
        Parametre = paste0("THETA", seq_along(prior$thetap)),
        `Prior (THETAP)` = prior$thetap,
        `Prior SD` = if (!is.null(prior$thetapv)) sqrt(diag(prior$thetapv)) else NA_real_
      )
      if (!is.null(lbls)) {
        tp <- tp |> mutate(Label = if_else(Parametre %in% names(lbls), lbls[Parametre], ""))
      }
      datatable(tp, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 15, dom = "t"))
    })

    output$prior_thetapv_table <- renderDT({
      prior <- ctl_data(); req(prior, !is.null(prior$thetapv))
      mat <- prior$thetapv
      n <- nrow(mat)
      rnames <- paste0("THETA", seq_len(n))
      df <- as.data.frame(mat)
      names(df) <- rnames
      df <- cbind(data.frame(Parametre = rnames), df)
      df[-1] <- round(df[-1], 6)
      datatable(df, rownames = FALSE, class = "stripe hover compact",
                options = list(pageLength = 15, dom = "t", scrollX = TRUE))
    })
  })
}
