# =============================================================================
# mod_times.R — Onglet Temps optimaux (+ multi-run + model prediction + summary)
# =============================================================================

mod_times_ui <- function(id) {
  ns <- NS(id)
  tagList(
    settings_bar(
      radioButtons(ns("time_unit"), "Time unit",
        choices = c("Hours" = "hours", "Days" = "days"),
        selected = "hours", inline = TRUE),
      tags$div(style = "display:inline-flex; gap:6px; align-items:center; margin-left:16px;",
        tags$span("Zoom:"),
        numericInput(ns("x_min"), NULL, value = NA, width = "90px"),
        numericInput(ns("x_max"), NULL, value = NA, width = "90px"),
        actionLink(ns("reset_zoom"), "Reset")
      ),
      tags$div(style = "display:inline-flex; gap:12px; margin-left:16px;",
        checkboxInput(ns("show_ctp"),   "CTP",   TRUE),
        checkboxInput(ns("show_doses"), "Doses", TRUE)
        # Rug toggle deferred — geom_rug not yet wired into plot_pk_profile
      )
    ),
    mod_mrgsolve_ui(ns("mrgsolve")),
    uiOutput(ns("tier_badge")),
    uiOutput(ns("content"))
  )
}

mod_times_server <- function(id, tab_data, all_runs = reactive(list()),
                             cmt_labels = reactive(NULL),
                             ext_data = reactive(NULL),
                             ctl_lines = reactive(NULL),
                             theta_labels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Helper: robust distribution boxplot + optional multi-run overlay ------
    .robust_plot_impl <- function(tab, runs, time_unit = "hours") {
      obs <- tab
      if ("EVID" %in% names(obs)) obs <- dplyr::filter(obs, EVID == 0)
      if (!"TSTRAT" %in% names(obs)) {
        obs <- obs |>
          dplyr::group_by(table_no) |>
          dplyr::mutate(TSTRAT = dplyr::row_number()) |>
          dplyr::ungroup()
      }
      obs <- obs |> dplyr::mutate(group = factor(paste0("Stratum ", TSTRAT)))
      n_tabs <- dplyr::n_distinct(tab$table_no)

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
            dplyr::mutate(group = factor(paste0("Stratum ", TSTRAT)),
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
        " prior realizations | Box = median + IQR | ",
        "Number = median")
      if (length(runs) > 1L)
        caption_txt <- paste0(caption_txt,
                              " | Colored points = comparison runs")

      p +
        labs(
          title = "Optimal times distribution by stratum",
          x     = "Time (h)",
          y     = NULL,
          caption = caption_txt
        ) +
        theme_bw(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          plot.caption = element_text(size = 8, color = "#6b7280")
        )
    }

    # -- mrgsolve sub-module (nested) ------------------------------------------
    mrg_sim <- mod_mrgsolve_server("mrgsolve",
      ext_data     = ext_data,
      tab_data     = tab_data,
      ctl_lines    = ctl_lines,
      theta_labels = theta_labels
    )

    # -- Tab effectif : tab_data() ou premier run secondaire ayant un .tab ------
    effective_tab_data <- reactive({
      tab <- tab_data()
      if (!is.null(tab)) return(tab)
      for (r in all_runs()) {
        if (!is.null(r$tab_data)) return(r$tab_data)
      }
      NULL
    })

    # -- Detection robust design (multi-table) — kept for robust_summary -------
    is_robust <- reactive({
      tab <- effective_tab_data()
      if (is.null(tab)) return(FALSE)
      "table_no" %in% names(tab) && dplyr::n_distinct(tab$table_no) > 1L
    })

    # Table 1 uniquement (pour robust_summary)
    tab_single <- reactive({
      tab <- effective_tab_data()
      req(tab)
      if (is_robust()) dplyr::filter(tab, table_no == 1L) else tab
    })

    # Helper: agregation robuste pour un seul run
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
          Median    = round(median(TIME,          na.rm = TRUE), 2),
          P90       = round(quantile(TIME, 0.90,  na.rm = TRUE), 2),
          .groups   = "drop"
        ) |>
        dplyr::arrange(TSTRAT, obs_idx) |>
        dplyr::rename(Stratum = TSTRAT, Obs = obs_idx, N = N_subprob)
    }

    # Agregation robuste partagee
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

    # -- Dispatcher pattern ----------------------------------------------------
    pattern <- reactive({
      tab <- effective_tab_data()
      if (is.null(tab)) return("unknown")
      cl <- tryCatch(ctl_lines(), error = function(e) NULL)
      detect_tab_pattern(tab, cl)
    })

    smooth <- reactive({
      req(effective_tab_data())
      thetas <- tryCatch({
        ext <- ext_data()
        if (is.null(ext)) stats::setNames(numeric(), character())
        else {
          finals <- dplyr::filter(ext, type == "final")
          if (nrow(finals) == 0L) stats::setNames(numeric(), character())
          else {
            last <- finals[nrow(finals), ]
            theta_cols <- grep("^THETA", names(last), value = TRUE)
            vals <- as.numeric(last[, theta_cols])
            nms  <- theta_cols
            lbls <- tryCatch(theta_labels(), error = function(e) NULL)
            if (!is.null(lbls)) nms <- ifelse(nms %in% names(lbls), lbls[nms], nms)
            stats::setNames(vals, nms)
          }
        }
      }, error = function(e) stats::setNames(numeric(), character()))

      pick_smooth_curve_engine(
        tab                = effective_tab_data(),
        ctl_lines          = tryCatch(ctl_lines(), error = function(e) NULL),
        theta_values       = thetas,
        mrgsolve_sim       = if (isTRUE(mrg_sim$is_available())) mrg_sim$sim_data() else NULL,
        mrgsolve_available = isTRUE(mrg_sim$is_available())
      )
    })

    # -- Zoom state ------------------------------------------------------------
    zoom_xlim <- reactiveVal(NULL)

    observeEvent(input$plot_brush, {
      b <- input$plot_brush
      if (!is.null(b)) {
        zoom_xlim(c(b$xmin, b$xmax))
        updateNumericInput(session, "x_min", value = round(b$xmin, 2))
        updateNumericInput(session, "x_max", value = round(b$xmax, 2))
      }
    })

    observeEvent(input$plot_dblclick, {
      zoom_xlim(NULL)
      updateNumericInput(session, "x_min", value = NA)
      updateNumericInput(session, "x_max", value = NA)
    })

    observeEvent(input$reset_zoom, {
      zoom_xlim(NULL)
      updateNumericInput(session, "x_min", value = NA)
      updateNumericInput(session, "x_max", value = NA)
    }, ignoreInit = TRUE)

    observeEvent(c(input$x_min, input$x_max), {
      xm <- input$x_min; xM <- input$x_max
      if (is.finite(xm) && is.finite(xM) && xM > xm) zoom_xlim(c(xm, xM))
    }, ignoreInit = TRUE)

    # -- Traffic-light badge ---------------------------------------------------
    output$tier_badge <- renderUI({
      pat <- pattern()
      # No badge for patterns where smooth curve is not applicable
      if (pat %in% c("robust_subprob", "classical", "stratified",
                     "discrete", "unknown")) {
        return(tagList())
      }

      s <- smooth()
      tier <- s$tier
      color <- switch(tier,
        mrgsolve = "#16a34a",
        template = "#d97706",
        "#dc2626"
      )
      label <- switch(tier,
        mrgsolve = "Smooth curve: mrgsolve (user model)",
        template = "Smooth curve: closed-form ADVAN template",
        "Smooth curve: unavailable — showing IPRED points only"
      )
      warn <- s$warning %||% mrg_sim$warning_reason()
      tags$div(style = "margin: 6px 0 10px; font-size: 0.85em;",
        tags$span(style = sprintf(
          "display:inline-block; width:8px; height:8px; border-radius:50%%; background:%s; margin-right:6px;",
          color)),
        label,
        if (!is.null(warn) && nzchar(warn))
          tags$span(style = "color:#92400e; margin-left:8px;",
                    paste0("(", warn, ")"))
      )
    })

    # -- UI dynamique ----------------------------------------------------------
    output$content <- renderUI({
      tab <- effective_tab_data()

      if (is.null(tab)) {
        return(div(class = "alert alert-info", style = "border-radius:10px; margin:16px 0;",
          tags$strong("Optimal Times"),
          tags$p(style = "margin:6px 0 0;",
            "Available when a ", tags$code(".tab"), " file is loaded.")
        ))
      }

      pat <- pattern()

      if (identical(pat, "robust_subprob")) {
        n_tabs <- dplyr::n_distinct(tab$table_no)
        robust_banner <- div(class = "alert alert-info",
            style = "border-radius:8px; margin-bottom:12px; padding:10px 14px;",
          tags$strong(paste0("Robust design detected (", n_tabs, " sub-problems)")),
          tags$p(style = "margin:4px 0 0; font-size:0.9em;",
            "The optimal times distribution is computed over all prior realizations. ",
            "The predicted curve uses only the first realization."
          )
        )
        return(tagList(
          robust_banner,
          fluidRow(
            column(8,
              div(class = "plot-card",
                p(class = "section-title",
                  "Optimal times distribution by stratum (robust design)"),
                plotOutput(ns("prediction"), height = "420px"),
                plot_export_ui(ns, "pred_export", default_fname = "robust_times_dist")
              )
            ),
            column(4,
              div(class = "param-table-wrap",
                p(class = "section-title", "Summary statistics (P10 / median / P90)"),
                DTOutput(ns("times_table"))
              )
            )
          )
        ))
      }

      # Elementary / focei_repl / pkpd_multi / dose_time_opt / other
      tagList(
        fluidRow(column(12,
          div(class = "plot-card",
            p(class = "section-title", "Predicted curve and sampling points"),
            plotOutput(ns("prediction"), height = "420px",
                       brush = brushOpts(id = ns("plot_brush"),
                                          direction = "x",
                                          resetOnNew = TRUE),
                       dblclick = ns("plot_dblclick")),
            plot_export_ui(ns, "pred_export", default_fname = "prediction_plot")
          )
        )),
        br(),
        fluidRow(column(12,
          div(class = "param-table-wrap",
            p(class = "section-title", "Optimal times data"),
            DTOutput(ns("times_table"))
          )
        ))
      )
    })

    # -- Predicted plot (reactive for export) ----------------------------------
    pred_plot <- reactive({
      req(effective_tab_data())
      tab <- effective_tab_data()
      pat <- pattern()
      tu  <- input$time_unit %||% "hours"
      xlim_val <- zoom_xlim()

      if (identical(pat, "robust_subprob")) {
        return(.robust_plot_impl(tab, all_runs(), tu))
      }

      if (pat %in% c("classical", "stratified", "discrete", "unknown")) {
        return(render_empty_state(pat))
      }

      s <- smooth()
      if (identical(s$tier, "dots")) {
        return(render_fallback_dot_plot(tab, time_unit = tu,
                                        cmt_labels = cmt_labels()))
      }

      obs <- prepare_tab_obs(tab)
      ctp <- NULL
      if (isTRUE(input$show_ctp) && length(all_runs()) > 1L) {
        run_keys <- names(all_runs())
        for (rk in run_keys[-1]) {
          if (!is.null(all_runs()[[rk]]$tab_data)) {
            ctp <- all_runs()[[rk]]$tab_data
            break
          }
        }
      }
      dose_t <- if (isTRUE(input$show_doses)) mrg_sim$dose_times() else NULL

      render_pk_timeline(
        smooth         = s,
        obs_points     = obs,
        compare_points = ctp,
        dose_times     = dose_t,
        time_unit      = tu,
        cmt_labels     = cmt_labels(),
        xlim           = xlim_val
      )
    })

    output$prediction <- renderPlot({ pred_plot() }, res = 110)
    plot_export_server(input, output, session, "pred_export", pred_plot)

    # -- Row-click zoom on times table ----------------------------------------
    observeEvent(input$times_table_rows_selected, {
      sel <- input$times_table_rows_selected
      req(length(sel) == 1L)
      obs <- prepare_tab_obs(effective_tab_data())
      if (is.null(obs) || nrow(obs) == 0L || sel > nrow(obs)) return()
      t_sel <- obs$TIME[sel]
      d_times <- mrg_sim$dose_times()
      half_window <- if (!is.null(d_times) && length(d_times) > 1L) {
        median(diff(sort(d_times))) / 2
      } else {
        max(2, abs(t_sel) * 0.1)
      }
      if (identical(input$time_unit, "days")) {
        t_disp <- t_sel / 24
        half_disp <- half_window / 24
      } else {
        t_disp <- t_sel
        half_disp <- half_window
      }
      zoom_xlim(c(t_disp - half_disp, t_disp + half_disp))
      updateNumericInput(session, "x_min", value = round(t_disp - half_disp, 2))
      updateNumericInput(session, "x_max", value = round(t_disp + half_disp, 2))
    })

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
      if ("EVID" %in% names(obs)) obs <- dplyr::filter(obs, EVID == 0)

      # Elementary datasets: collapse to one row per unique sampling point
      if ("ID" %in% names(obs) && dplyr::n_distinct(obs$ID) > 4L &&
          "TSTRAT" %in% names(obs)) {
        arm_sig <- obs |>
          dplyr::group_by(ID) |>
          dplyr::summarise(sig = paste(sort(unique(TSTRAT)), collapse = ","),
                           .groups = "drop")
        rep_ids <- arm_sig |>
          dplyr::group_by(sig) |>
          dplyr::slice_min(ID, n = 1L) |>
          dplyr::ungroup() |>
          dplyr::pull(ID)
        obs <- obs |> dplyr::filter(ID %in% rep_ids)
      }

      cols_show <- intersect(
        c("ID", "TSTRAT", "TMIN", "TIME", "TMAX", "IPRED", "CONC", "STRAT", "CMT"),
        names(obs)
      )
      if (length(cols_show) == 0L) {
        cols_show <- names(obs)[!names(obs) %in% c("table_no")]
      }

      obs_display <- obs |>
        dplyr::select(dplyr::all_of(cols_show)) |>
        dplyr::mutate(dplyr::across(where(is.double), ~ round(.x, 4)))

      lbls <- cmt_labels()
      if (!is.null(lbls) && "CMT" %in% names(obs_display)) {
        obs_display <- obs_display |>
          dplyr::mutate(CMT = ifelse(
            as.character(CMT) %in% names(lbls),
            paste0(lbls[as.character(CMT)], " (CMT=", CMT, ")"),
            as.character(CMT)
          ))
      }

      datatable(obs_display, rownames = FALSE,
                class = "stripe hover compact",
                selection = "single",
                options = list(pageLength = nrow(obs_display), dom = "t",
                               scrollX = TRUE))
    })

  })
}
