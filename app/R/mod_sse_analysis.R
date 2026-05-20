# =============================================================================
# mod_sse_analysis.R — SSE Intrinsic Analysis Tab
#
# Analyzes the SSE results themselves: run health, parameter distributions,
# OFV distribution, empirical correlations, individual PK patab outputs,
# per-parameter diagnostics.
# Consumes shared SSE data from mod_sse_upload (centralized upload).
#
# Inputs: sse_a_shared, sse_b_shared, individual_pk_shared (reactives),
#         name_a, name_b, true_vals, param_labels
# =============================================================================

mod_sse_analysis_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    page_header(
      "SSE Diagnostics",
      tagList(
        "Analyze the SSE results themselves: convergence quality, parameter ",
        "estimability, OFV distribution, and empirical correlations. ",
        "Load SSE data in the SSE Upload tab first."
      ),
      eyebrow = "Validation"
    ),

    # --- Design selector (A/B) ---
    uiOutput(ns("design_selector")),

    # --- Run health banner (always visible) ---
    uiOutput(ns("run_health_banner")),

    doc_callout(
      "sse-analysis",
      "Run-health, shrinkage, OFV, and parameter-distribution guidance is collected in Documentation.",
      "Open SSE analysis documentation"
    ),

    page_section(
      "Analysis workspace",
      subtitle = "Choose one diagnostic view at a time. Parameter filters apply to parameter-based views.",
      control_panel(
        uiOutput(ns("hypothesis_filter_ui")),
        checkboxInput(
          ns("show_failed"),
          "Include failed runs in plots",
          value = FALSE
        ),
        checkboxInput(
          ns("color_ofv"),
          "Color OFV by convergence status",
          value = TRUE
        ),
        checkboxInput(
          ns("pk_log_axes"),
          "Log scale for Individual PK recovery",
          value = FALSE
        ),
        radioButtons(
          ns("pk_run_filter"),
          "Individual PK run set",
          choices = c(
            "Minimization OK" = "minimization_successful",
            "Strict QC" = "strict_qc_ok",
            "All runs" = "all_runs"
          ),
          selected = "minimization_successful",
          inline = TRUE
        ),
        radioButtons(
          ns("active_view"),
          "Active view",
          choices = c(
            "Parameter distributions" = "distributions",
            "SSE reliability map" = "reliability",
            "Run composition" = "run_composition",
            "PsN summary stats" = "psn_summary",
            "dOFV diagnostics" = "dofv",
            "Individual PK recovery" = "indiv_pk_recovery",
            "Individual PK errors" = "indiv_pk_errors",
            "Individual PK intervals" = "indiv_pk_intervals",
            "Individual PK outliers" = "indiv_pk_outliers",
            "Individual PK heatmap" = "indiv_pk_heatmap",
            "OFV distribution" = "ofv",
            "Shrinkage boxplot" = "shrink_box",
            "Exploratory RSE-shrinkage map" = "shrink_scatter",
            "Heuristic ETA review ranking" = "eta_risk",
            "Shrinkage summary table" = "shrink_table",
            "Parameter diagnostics table" = "diagnostics"
          ),
          selected = "distributions",
          inline = TRUE
        ),
        radioButtons(
          ns("diagnostic_param_group"),
          "Parameter set",
          choices = c(
            "Top issues" = "top",
            "Fixed effects" = "theta",
            "Variability" = "omega",
            "Residual" = "sigma",
            "All" = "all",
            "Custom" = "custom"
          ),
          selected = "top",
          inline = TRUE
        ),
        uiOutput(ns("param_filter_ui"))
      ),
      uiOutput(ns("active_view_ui"))
    )
  )
}


mod_sse_analysis_server <- function(
  id,
  sse_a_shared = reactive(NULL),
  sse_b_shared = reactive(NULL),
  name_a = reactive("Design A"),
  name_b = reactive("Design B"),
  true_vals,
  param_labels = reactive(NULL),
  individual_pk_shared = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    # --- Design selector (show only when B is loaded) ---
    output$design_selector <- renderUI({
      b <- sse_b_shared()
      if (is.null(b)) {
        return(NULL)
      }

      choices <- c("a" = "a", "b" = "b")
      names(choices) <- c(name_a(), name_b())

      control_panel(
        label = "Analyze",
        radioButtons(
          session$ns("which_design"),
          label = NULL,
          choices = choices,
          selected = "a",
          inline = TRUE
        )
      )
    })

    # --- SSE data (switches between A and B) ---
    sse_raw_all <- reactive({
      sel <- input$which_design %||% "a"
      dat <- if (sel == "b") sse_b_shared() else sse_a_shared()
      req(dat)
      dat
    })

    output$hypothesis_filter_ui <- renderUI({
      dat <- tryCatch(sse_raw_all(), error = function(e) NULL)
      if (is.null(dat) || !"hypothesis" %in% names(dat)) {
        return(NULL)
      }
      choices <- sse_hypothesis_choices(dat)
      selectInput(
        session$ns("hypothesis_filter"),
        "PsN hypothesis",
        choices = stats::setNames(choices$value, choices$label),
        selected = "auto"
      )
    })

    sse_all <- reactive({
      dat <- sse_raw_all()
      filter_sse_hypothesis(dat, input$hypothesis_filter %||% "auto")
    })

    # --- Run health ---
    run_health <- reactive({
      dat <- sse_all()
      req(dat)
      compute_run_health(dat)
    })

    run_composition_data <- reactive({
      dat <- sse_raw_all()
      req(dat)
      compute_sse_run_composition(dat)
    })

    psn_summary_data <- reactive({
      dat <- sse_all()
      tv <- true_vals()
      req(dat, tv)
      compute_sse_psn_parameter_summary(
        dat,
        tv,
        param_labels(),
        only_converged = !isTRUE(input$show_failed)
      )
    })

    dofv_diagnostics <- reactive({
      dat <- sse_raw_all()
      req(dat)
      compute_sse_dofv_diagnostics(dat)
    })

    output$run_health_banner <- renderUI({
      rh <- tryCatch(run_health(), error = function(e) NULL)
      if (is.null(rh)) {
        return(status_panel(
          "No SSE data loaded",
          tags$p("Upload a PsN raw_results CSV in the SSE Upload tab."),
          tone = "warning",
          icon_name = "exclamation-triangle"
        ))
      }

      status_panel(
        "Run Health",
        make_health_funnel(rh),
        tone = "neutral",
        icon_name = "heartbeat"
      )
    })

    # --- Param distributions (long-format data) ---
    dist_data <- reactive({
      dat <- sse_all()
      tv <- true_vals()
      req(dat, tv)
      compute_param_distributions(dat, tv, param_labels())
    })

    diagnostic_param_sets <- reactive({
      dd <- dist_data()
      req(dd)

      meta_rows <- lapply(
        split(dd, list(dd$param_label, dd$param_type), drop = TRUE),
        function(group) {
          true_value <- group$true_value[[1]]
          med <- stats::median(group$estimate, na.rm = TRUE)
          iqr <- stats::IQR(group$estimate, na.rm = TRUE)
          data.frame(
            param_label = group$param_label[[1]],
            param_type = group$param_type[[1]],
            true_value = true_value,
            med = med,
            iqr = iqr,
            score = abs(med - true_value) /
              pmax(abs(true_value), 1e-12) +
              iqr / pmax(abs(true_value), 1e-12),
            stringsAsFactors = FALSE
          )
        }
      )
      meta <- tibble::as_tibble(dplyr::bind_rows(meta_rows))

      top <- meta |>
        dplyr::arrange(dplyr::desc(score)) |>
        dplyr::slice_head(n = 12) |>
        dplyr::pull(param_label)

      list(
        all = unique(dd$param_label),
        top = top,
        theta = unique(dd$param_label[dd$param_type == "THETA"]),
        omega = unique(dd$param_label[grepl("^OMEGA", dd$param_type)]),
        sigma = unique(dd$param_label[grepl("^SIGMA", dd$param_type)])
      )
    })

    selected_diagnostic_group <- reactive({
      sets <- diagnostic_param_sets()
      group <- input$diagnostic_param_group %||% "top"
      if (identical(group, "custom")) {
        selected <- isolate(input$selected_params)
        if (is.null(selected)) {
          selected <- sets$top
        }
        return(selected)
      }
      selected <- sets[[group]]
      if (is.null(selected) || length(selected) == 0L) sets$all else selected
    })

    updating_param_selection <- reactiveVal(FALSE)
    set_selected_params <- function(selected) {
      updating_param_selection(TRUE)
      on.exit(updating_param_selection(FALSE), add = TRUE)
      freezeReactiveValue(input, "selected_params")
      updateCheckboxGroupInput(session, "selected_params", selected = selected)
    }
    set_param_group <- function(group) {
      updating_param_selection(TRUE)
      on.exit(updating_param_selection(FALSE), add = TRUE)
      freezeReactiveValue(input, "diagnostic_param_group")
      updateRadioButtons(session, "diagnostic_param_group", selected = group)
    }

    observeEvent(
      input$diagnostic_param_group,
      {
        if (identical(input$diagnostic_param_group, "custom")) {
          return()
        }
        set_selected_params(selected_diagnostic_group())
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$selected_params,
      {
        if (isTRUE(updating_param_selection())) {
          return()
        }
        group <- input$diagnostic_param_group %||% "top"
        if (!identical(group, "custom")) {
          set_param_group("custom")
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$params_all, {
      sets <- diagnostic_param_sets()
      set_param_group("all")
      set_selected_params(sets$all)
    })
    observeEvent(input$params_none, {
      set_param_group("custom")
      set_selected_params(character())
    })
    observeEvent(input$params_top, {
      sets <- diagnostic_param_sets()
      set_param_group("top")
      set_selected_params(sets$top)
    })
    observeEvent(input$params_theta, {
      sets <- diagnostic_param_sets()
      set_param_group("theta")
      set_selected_params(sets$theta)
    })
    observeEvent(input$params_omega, {
      sets <- diagnostic_param_sets()
      set_param_group("omega")
      set_selected_params(sets$omega)
    })
    observeEvent(input$params_sigma, {
      sets <- diagnostic_param_sets()
      set_param_group("sigma")
      set_selected_params(sets$sigma)
    })

    # --- Param filter UI ---
    output$param_filter_ui <- renderUI({
      dd <- dist_data()
      if (is.null(dd) || nrow(dd) == 0L) {
        return(NULL)
      }

      all_params <- unique(dd$param_label)
      selected <- isolate(input$selected_params) %||%
        selected_diagnostic_group()
      summary <- sprintf(
        "%d selected of %d",
        length(selected),
        length(all_params)
      )

      compact_param_filter_ui(
        session$ns,
        "selected_params",
        choices = all_params,
        selected = selected,
        summary = summary,
        note = "Use Parameter set for readable defaults, then expand this filter for hand-picked plots.",
        quick_actions = c(
          params_top = "Top issues",
          params_theta = "Fixed",
          params_omega = "IIV",
          params_sigma = "Residual",
          params_all = "All",
          params_none = "None"
        )
      )
    })

    # --- Filtered distribution data ---
    dist_data_filtered <- reactive({
      dd <- dist_data()
      if (is.null(dd)) {
        return(NULL)
      }
      sel <- input$selected_params
      if (is.null(sel)) {
        sel <- selected_diagnostic_group()
      }
      if (length(sel) == 0L) {
        return(dd[0, ])
      }
      dd[dd$param_label %in% sel, ]
    })

    dist_plot_height <- reactive({
      dd <- dist_data_filtered()
      n_params <- if (is.null(dd) || nrow(dd) == 0L) {
        1L
      } else {
        length(unique(dd$param_label))
      }
      paste0(max(520L, min(980L, 260L * ceiling(n_params / 3))), "px")
    })

    # --- Distribution plot ---
    dist_plot_fn <- reactive({
      dd <- dist_data_filtered()
      if (is.null(dd) || nrow(dd) == 0L) {
        return(
          ggplot() +
            labs(title = "Load SSE data in SSE Upload tab") +
            .theme_design()
        )
      }
      plot_param_distributions(dd, show_failed = isTRUE(input$show_failed))
    })
    output$dist_plot <- renderPlot(
      {
        dist_plot_fn()
      },
      res = 110
    )
    plot_export_server(input, output, session, "dist_export", dist_plot_fn)

    output$active_view_ui <- renderUI({
      active <- input$active_view %||% "distributions"

      if (active == "reliability") {
        return(analysis_workspace(
          "SSE reliability map",
          plotOutput(session$ns("reliability_map"), height = "560px"),
          plot_export_ui(
            session$ns,
            "reliability_export",
            default_fname = "sse_reliability_map"
          )
        ))
      }
      if (active == "run_composition") {
        return(analysis_workspace(
          "Run composition",
          DTOutput(session$ns("run_composition_table")),
          actions = downloadButton(
            session$ns("export_run_composition_csv"),
            "Export CSV",
            class = "btn-sm btn-default"
          )
        ))
      }
      if (active == "psn_summary") {
        return(analysis_workspace(
          "PsN summary stats",
          DTOutput(session$ns("psn_summary_table")),
          actions = downloadButton(
            session$ns("export_psn_summary_csv"),
            "Export CSV",
            class = "btn-sm btn-default"
          )
        ))
      }
      if (active == "dofv") {
        return(analysis_workspace(
          "dOFV diagnostics",
          plotOutput(session$ns("dofv_plot"), height = "420px"),
          DTOutput(session$ns("dofv_table")),
          actions = tagList(
            plot_export_ui(
              session$ns,
              "dofv_export",
              default_fname = "sse_dofv_diagnostics"
            ),
            downloadButton(
              session$ns("export_dofv_csv"),
              "Export CSV",
              class = "btn-sm btn-default"
            )
          )
        ))
      }
      if (active == "indiv_pk_recovery") {
        return(analysis_workspace(
          "Individual PK recovery",
          plotOutput(session$ns("individual_pk_recovery"), height = "620px"),
          plot_export_ui(
            session$ns,
            "individual_pk_recovery_export",
            default_fname = "individual_pk_recovery"
          )
        ))
      }
      if (active == "indiv_pk_errors") {
        return(analysis_workspace(
          "Individual PK relative errors",
          plotOutput(session$ns("individual_pk_errors"), height = "460px"),
          plot_export_ui(
            session$ns,
            "individual_pk_errors_export",
            default_fname = "individual_pk_relative_errors"
          )
        ))
      }
      if (active == "indiv_pk_intervals") {
        return(analysis_workspace(
          "Individual PK intervals",
          plotOutput(session$ns("individual_pk_intervals"), height = "520px"),
          plot_export_ui(
            session$ns,
            "individual_pk_intervals_export",
            default_fname = "individual_pk_error_intervals"
          )
        ))
      }
      if (active == "indiv_pk_outliers") {
        return(analysis_workspace(
          "Individual PK outliers",
          plotOutput(session$ns("individual_pk_outliers"), height = "640px"),
          DTOutput(session$ns("individual_pk_outliers_table")),
          actions = tagList(
            plot_export_ui(
              session$ns,
              "individual_pk_outliers_export",
              default_fname = "individual_pk_outliers"
            ),
            downloadButton(
              session$ns("export_individual_pk_outliers_csv"),
              "Export CSV",
              class = "btn-sm btn-default"
            )
          )
        ))
      }
      if (active == "indiv_pk_heatmap") {
        return(analysis_workspace(
          "Individual PK heatmap",
          plotOutput(session$ns("individual_pk_heatmap"), height = "820px"),
          plot_export_ui(
            session$ns,
            "individual_pk_heatmap_export",
            default_fname = "individual_pk_error_heatmap"
          )
        ))
      }
      if (active == "ofv") {
        return(analysis_workspace(
          "OFV distribution",
          plotOutput(session$ns("ofv_plot"), height = "420px"),
          plot_export_ui(
            session$ns,
            "ofv_export",
            default_fname = "sse_ofv_distribution"
          )
        ))
      }
      if (active == "shrink_box") {
        return(analysis_workspace(
          "Shrinkage distribution per ETA",
          plotOutput(session$ns("shrink_box"), height = "460px"),
          plot_export_ui(
            session$ns,
            "shrink_box_export",
            default_fname = "sse_shrinkage_boxplot"
          )
        ))
      }
      if (active == "shrink_scatter") {
        return(analysis_workspace(
          "Exploratory precision vs shrinkage map",
          plotOutput(session$ns("shrink_scatter"), height = "560px"),
          plot_export_ui(
            session$ns,
            "shrink_scatter_export",
            default_fname = "sse_shrinkage_rse_scatter"
          )
        ))
      }
      if (active == "eta_risk") {
        return(analysis_workspace(
          "Heuristic ETA review ranking",
          DTOutput(session$ns("eta_risk_table")),
          actions = downloadButton(
            session$ns("export_eta_risk_csv"),
            "Export CSV",
            class = "btn-sm btn-default"
          )
        ))
      }
      if (active == "shrink_table") {
        return(analysis_workspace(
          "Shrinkage summary table",
          DTOutput(session$ns("shrink_table")),
          actions = downloadButton(
            session$ns("export_shrink_csv"),
            "Export CSV",
            class = "btn-sm btn-default"
          )
        ))
      }
      if (active == "diagnostics") {
        return(analysis_workspace(
          "Per-parameter diagnostics",
          DTOutput(session$ns("diag_table")),
          actions = downloadButton(
            session$ns("export_diag_csv"),
            "Export CSV",
            class = "btn-sm btn-default"
          )
        ))
      }

      analysis_workspace(
        "Parameter estimate distributions",
        plotOutput(session$ns("dist_plot"), height = dist_plot_height()),
        plot_export_ui(
          session$ns,
          "dist_export",
          default_fname = "sse_param_distributions"
        )
      )
    })

    # --- PsN-oriented run and hypothesis diagnostics ---
    output$run_composition_table <- renderDT({
      comp <- run_composition_data()
      req(comp)
      if (nrow(comp) == 0L) {
        return(NULL)
      }

      display <- comp |>
        dplyr::select(
          Hypothesis = hypothesis,
          Type = hypothesis_type,
          `N runs` = n_runs,
          `N samples` = n_samples,
          `Minimization OK` = n_minimization_ok,
          `Covariance OK` = n_covariance_ok,
          `Near boundary` = n_boundary,
          `Rounding errors` = n_rounding_errors,
          `Condition number >1000` = n_high_condition_number,
          `Median OFV` = ofv_median,
          `P5 OFV` = ofv_p5,
          `P95 OFV` = ofv_p95
        )
      numeric_cols <- vapply(display, is.numeric, logical(1))
      display[numeric_cols] <- lapply(display[numeric_cols], round, digits = 2)

      datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(pageLength = 20, dom = "tip", scrollX = TRUE)
      )
    })

    output$export_run_composition_csv <- downloadHandler(
      filename = function() {
        paste0(
          "sse_run_composition_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        comp <- run_composition_data()
        req(comp)
        tryCatch(
          write.csv(comp, file, row.names = FALSE),
          error = function(e) {
            warning("CSV export failed: ", conditionMessage(e))
          }
        )
      }
    )

    output$psn_summary_table <- renderDT({
      psn <- psn_summary_data()
      req(psn)
      if (nrow(psn) == 0L) {
        return(NULL)
      }

      display <- psn |>
        dplyr::select(
          Parameter = param_label,
          Type = param_type,
          `True value` = true_value,
          N = n,
          Mean = mean_estimate,
          Median = median_estimate,
          SD = sd_estimate,
          Min = min_estimate,
          Max = max_estimate,
          Skewness = skewness,
          Kurtosis = kurtosis,
          RMSE = rmse,
          `Rel. RMSE (%)` = relative_rmse,
          Bias = bias,
          `Rel. bias (%)` = relative_bias,
          `Rel. abs. bias (%)` = relative_absolute_bias,
          `RSE (%)` = rse
        )
      numeric_cols <- vapply(display, is.numeric, logical(1))
      display[numeric_cols] <- lapply(display[numeric_cols], round, digits = 3)

      datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(
          pageLength = 20,
          dom = "tip",
          scrollX = TRUE,
          order = list(list(12, "desc"))
        )
      ) |>
        formatStyle(
          "Rel. RMSE (%)",
          backgroundColor = styleInterval(
            c(20, 50),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          )
        )
    })

    output$export_psn_summary_csv <- downloadHandler(
      filename = function() {
        paste0("sse_psn_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        psn <- psn_summary_data()
        req(psn)
        tryCatch(
          write.csv(psn, file, row.names = FALSE),
          error = function(e) {
            warning("CSV export failed: ", conditionMessage(e))
          }
        )
      }
    )

    dofv_plot_fn <- reactive({
      diag <- dofv_diagnostics()
      plot_sse_dofv_distribution(diag$long)
    })
    output$dofv_plot <- renderPlot(
      {
        dofv_plot_fn()
      },
      res = 110
    )
    plot_export_server(input, output, session, "dofv_export", dofv_plot_fn)

    output$dofv_table <- renderDT({
      diag <- dofv_diagnostics()
      summary <- diag$summary
      req(summary)
      if (nrow(summary) == 0L) {
        return(NULL)
      }

      display <- summary |>
        dplyr::select(
          Hypothesis = hypothesis,
          Reference = reference_hypothesis,
          N = n,
          `N dOFV < 0` = n_negative,
          `% dOFV < 0` = pct_negative,
          `Mean dOFV` = mean_dofv,
          `Median dOFV` = median_dofv,
          `P5 dOFV` = p5_dofv,
          `P95 dOFV` = p95_dofv,
          `% dOFV > 3.84` = pct_dofv_gt_3_84
        )
      numeric_cols <- vapply(display, is.numeric, logical(1))
      display[numeric_cols] <- lapply(display[numeric_cols], round, digits = 2)

      datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(pageLength = 10, dom = "tip", scrollX = TRUE)
      )
    })

    output$export_dofv_csv <- downloadHandler(
      filename = function() {
        paste0("sse_dofv_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        diag <- dofv_diagnostics()
        req(diag$long)
        tryCatch(
          write.csv(diag$long, file, row.names = FALSE),
          error = function(e) {
            warning("CSV export failed: ", conditionMessage(e))
          }
        )
      }
    )

    # --- OFV plot ---
    ofv_plot_fn <- reactive({
      dat <- sse_all()
      if (is.null(dat)) {
        return(ggplot() + labs(title = "No SSE data") + .theme_design())
      }
      plot_ofv_distribution(dat, color_by_status = isTRUE(input$color_ofv))
    })
    output$ofv_plot <- renderPlot(
      {
        ofv_plot_fn()
      },
      res = 110
    )
    plot_export_server(input, output, session, "ofv_export", ofv_plot_fn)

    # --- Reliability map ---
    reliability_data <- reactive({
      dat <- sse_all()
      tv <- true_vals()
      req(dat, tv)
      compute_sse_reliability_map(dat, tv, param_labels())
    })

    reliability_filtered <- reactive({
      rel <- reliability_data()
      if (is.null(rel) || nrow(rel) == 0L) {
        return(rel)
      }
      sel <- input$selected_params
      if (is.null(sel)) {
        sel <- selected_diagnostic_group()
      }
      if (length(sel) == 0L) {
        return(rel[0, ])
      }
      rel[rel$param_label %in% sel, ]
    })

    reliability_plot_fn <- reactive({
      rel <- reliability_filtered()
      plot_sse_reliability_map(rel)
    })
    output$reliability_map <- renderPlot(
      {
        reliability_plot_fn()
      },
      res = 110
    )
    plot_export_server(
      input,
      output,
      session,
      "reliability_export",
      reliability_plot_fn
    )

    # --- Individual PK patab outputs ---
    individual_pk_diagnostics_data <- reactive({
      dat <- individual_pk_shared()
      if (is.null(dat) || nrow(dat) == 0L) {
        return(build_individual_pk_diagnostics(NULL, tibble::tibble()))
      }
      sse <- tryCatch(sse_all(), error = function(e) NULL)
      build_individual_pk_diagnostics(sse, dat)
    })

    individual_pk_diagnostics_recovery <- reactive({
      diag <- individual_pk_diagnostics_data()
      filter_individual_pk_recovery(
        diag$individual_pk_recovery_long,
        status_filter = input$pk_run_filter %||% "minimization_successful"
      )
    })

    individual_pk_diagnostics_by_id <- reactive({
      summarize_individual_pk_by_id_param(individual_pk_diagnostics_recovery())
    })

    individual_pk_recovery_data <- reactive({
      dat <- individual_pk_shared()
      if (is.null(dat) || nrow(dat) == 0L) {
        return(tibble::tibble())
      }
      compute_individual_pk_recovery(dat)
    })

    individual_pk_recovery_plot <- reactive({
      rec <- individual_pk_recovery_data()
      plot_individual_pk_recovery(rec, log_axes = isTRUE(input$pk_log_axes))
    })
    output$individual_pk_recovery <- renderPlot(
      {
        individual_pk_recovery_plot()
      },
      res = 110
    )
    plot_export_server(
      input,
      output,
      session,
      "individual_pk_recovery_export",
      individual_pk_recovery_plot
    )

    individual_pk_errors_plot <- reactive({
      rec <- individual_pk_recovery_data()
      plot_individual_pk_error_distribution(rec)
    })
    output$individual_pk_errors <- renderPlot(
      {
        individual_pk_errors_plot()
      },
      res = 110
    )
    plot_export_server(
      input,
      output,
      session,
      "individual_pk_errors_export",
      individual_pk_errors_plot
    )

    individual_pk_intervals_plot <- reactive({
      diag <- individual_pk_diagnostics_data()
      plot_individual_pk_error_forest(
        diag$individual_pk_summary_by_param,
        status_filter = input$pk_run_filter %||% "minimization_successful"
      )
    })
    output$individual_pk_intervals <- renderPlot(
      {
        individual_pk_intervals_plot()
      },
      res = 110
    )
    plot_export_server(
      input,
      output,
      session,
      "individual_pk_intervals_export",
      individual_pk_intervals_plot
    )

    individual_pk_outliers_plot <- reactive({
      plot_individual_pk_outliers(
        individual_pk_diagnostics_by_id(),
        top_n = 25L
      )
    })
    output$individual_pk_outliers <- renderPlot(
      {
        individual_pk_outliers_plot()
      },
      res = 110
    )
    plot_export_server(
      input,
      output,
      session,
      "individual_pk_outliers_export",
      individual_pk_outliers_plot
    )

    output$individual_pk_outliers_table <- renderDT({
      outliers <- individual_pk_diagnostics_by_id()
      req(outliers)
      if (nrow(outliers) == 0L) {
        return(NULL)
      }
      display <- outliers |>
        dplyr::slice_head(n = 50) |>
        dplyr::select(
          Rank = outlier_rank,
          ID,
          ARM,
          Parameter = param,
          `N runs` = n_samples,
          `Median error (%)` = median_relative_error,
          `P5 error (%)` = p5_relative_error,
          `P95 error (%)` = p95_relative_error,
          `Median |error| (%)` = median_abs_relative_error,
          `P95 |error| (%)` = p95_abs_relative_error,
          `% |error| >20` = pct_abs_error_over_20,
          `% |error| >50` = pct_abs_error_over_50
        )
      numeric_cols <- vapply(display, is.numeric, logical(1))
      display[numeric_cols] <- lapply(display[numeric_cols], round, digits = 2)

      datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(
          pageLength = 15,
          dom = "tip",
          scrollX = TRUE,
          order = list(list(0, "asc"))
        )
      ) |>
        formatStyle(
          "Median |error| (%)",
          backgroundColor = styleInterval(
            c(20, 40),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          )
        ) |>
        formatStyle(
          "P95 |error| (%)",
          backgroundColor = styleInterval(
            c(50, 100),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          )
        )
    })

    output$export_individual_pk_outliers_csv <- downloadHandler(
      filename = function() {
        paste0(
          "individual_pk_outliers_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".csv"
        )
      },
      content = function(file) {
        outliers <- individual_pk_diagnostics_by_id()
        req(outliers)
        tryCatch(
          write.csv(outliers, file, row.names = FALSE),
          error = function(e) {
            warning("CSV export failed: ", conditionMessage(e))
          }
        )
      }
    )

    individual_pk_heatmap_plot <- reactive({
      plot_individual_pk_error_heatmap(individual_pk_diagnostics_by_id())
    })
    output$individual_pk_heatmap <- renderPlot(
      {
        individual_pk_heatmap_plot()
      },
      res = 110
    )
    plot_export_server(
      input,
      output,
      session,
      "individual_pk_heatmap_export",
      individual_pk_heatmap_plot
    )

    # --- Shrinkage: long-format data per replicate ---
    shrink_long <- reactive({
      dat <- sse_all()
      req(dat)
      compute_shrinkage_long(
        dat,
        param_labels = param_labels(),
        only_converged = !isTRUE(input$show_failed)
      )
    })

    # --- Shrinkage: summary per ETA ---
    shrink_summary <- reactive({
      dat <- sse_all()
      req(dat)
      compute_shrinkage_summary(
        dat,
        param_labels = param_labels(),
        only_converged = !isTRUE(input$show_failed)
      )
    })

    # --- Boxplot ---
    shrink_box_fn <- reactive({
      sl <- shrink_long()
      plot_shrinkage_boxplot(sl)
    })
    output$shrink_box <- renderPlot(
      {
        shrink_box_fn()
      },
      res = 110
    )
    plot_export_server(
      input,
      output,
      session,
      "shrink_box_export",
      shrink_box_fn
    )

    # --- Scatter RSE vs shrinkage ---
    shrink_scatter_fn <- reactive({
      dat <- sse_all()
      tv <- true_vals()
      if (is.null(dat) || is.null(tv)) {
        return(
          ggplot() +
            labs(
              title = "Load SSE data and .ctl to see identifiability scatter"
            ) +
            .theme_design()
        )
      }
      plot_shrinkage_rse_scatter(
        dat,
        tv,
        param_labels(),
        shrink_sum = shrink_summary()
      )
    })
    output$shrink_scatter <- renderPlot(
      {
        shrink_scatter_fn()
      },
      res = 110
    )
    plot_export_server(
      input,
      output,
      session,
      "shrink_scatter_export",
      shrink_scatter_fn
    )

    # --- Shrinkage summary table ---
    output$shrink_table <- renderDT({
      ss <- shrink_summary()
      req(ss)
      if (nrow(ss) == 0L) {
        return(NULL)
      }

      display <- ss |>
        dplyr::select(
          Parameter = param_label,
          ETA = eta,
          `N runs` = n,
          `Mean (%)` = mean_shrink,
          `Median (%)` = median_shrink,
          `SD (%)` = sd_shrink,
          `P5 (%)` = p5,
          `P95 (%)` = p95
        )

      datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(pageLength = 20, dom = "t", scrollX = TRUE)
      ) |>
        formatStyle(
          "Mean (%)",
          backgroundColor = styleInterval(
            c(20, 30),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          )
        )
    })

    output$export_shrink_csv <- downloadHandler(
      filename = function() {
        paste0("sse_shrinkage_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        ss <- shrink_summary()
        req(ss)
        tryCatch(
          write.csv(ss, file, row.names = FALSE),
          error = function(e) {
            warning("CSV export failed: ", conditionMessage(e))
          }
        )
      }
    )

    # --- ETA risk ranking table ---
    eta_risk_data <- reactive({
      rel <- reliability_data()
      if (is.null(rel) || nrow(rel) == 0L) {
        return(tibble::tibble())
      }
      rel |>
        dplyr::filter(grepl("^OMEGA\\(", param)) |>
        dplyr::arrange(dplyr::desc(risk_score))
    })

    output$eta_risk_table <- renderDT({
      eta <- eta_risk_data()
      req(eta)
      if (nrow(eta) == 0L) {
        return(NULL)
      }

      display <- eta |>
        dplyr::select(
          Parameter = param_label,
          Type = param_type,
          `Empirical RSE (%)` = rse_empirical,
          `Rel. Bias (%)` = relative_bias,
          `Mean shrinkage (%)` = mean_shrinkage,
          `% SE NA` = pct_se_na,
          `% RSE>100` = pct_rse_over_100,
          `Heuristic review score` = risk_score
        )
      numeric_cols <- vapply(display, is.numeric, logical(1))
      display[numeric_cols] <- lapply(display[numeric_cols], round, digits = 2)

      datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(
          pageLength = 20,
          dom = "t",
          scrollX = TRUE,
          order = list(list(7, "desc"))
        )
      ) |>
        formatStyle(
          "Mean shrinkage (%)",
          backgroundColor = styleInterval(
            c(20, 30),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          )
        ) |>
        formatStyle(
          "Empirical RSE (%)",
          backgroundColor = styleInterval(
            c(30, 50, 100),
            c("#dcfce7", "#fef3c7", "#fed7aa", "#fee2e2")
          )
        )
    })

    output$export_eta_risk_csv <- downloadHandler(
      filename = function() {
        paste0("sse_eta_risk_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        eta <- eta_risk_data()
        req(eta)
        tryCatch(
          write.csv(eta, file, row.names = FALSE),
          error = function(e) {
            warning("CSV export failed: ", conditionMessage(e))
          }
        )
      }
    )

    # --- Per-parameter diagnostics table ---
    diag_data <- reactive({
      dat <- sse_all()
      tv <- true_vals()
      req(dat, tv)
      compute_param_diagnostics(dat, tv, param_labels())
    })

    output$diag_table <- renderDT({
      diag <- diag_data()
      req(diag)

      display <- diag |>
        dplyr::select(
          Parameter = param_label,
          Type = param_type,
          `N runs` = n_runs,
          `SE = NA` = n_se_na,
          `% SE NA` = pct_se_na,
          `RSE > 100%` = n_rse_over_100,
          `% RSE>100` = pct_rse_over_100,
          `Est. = 0` = n_zero_estimate,
          `% Zero` = pct_zero_estimate
        )

      datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(
          pageLength = 20,
          dom = "t",
          scrollX = TRUE,
          order = list(list(4, "desc"))
        )
      ) |>
        formatStyle(
          "% SE NA",
          backgroundColor = styleInterval(
            c(10, 30),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          )
        ) |>
        formatStyle(
          "% RSE>100",
          backgroundColor = styleInterval(
            c(10, 30),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          )
        ) |>
        formatStyle(
          "% Zero",
          backgroundColor = styleInterval(
            c(5, 20),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          )
        )
    })

    # --- Export diagnostics CSV ---
    output$export_diag_csv <- downloadHandler(
      filename = function() {
        paste0("sse_diagnostics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        diag <- diag_data()
        req(diag)
        tryCatch(
          write.csv(diag, file, row.names = FALSE),
          error = function(e) {
            warning("CSV export failed: ", conditionMessage(e))
          }
        )
      }
    )
  })
}
