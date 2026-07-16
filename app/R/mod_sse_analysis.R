# =============================================================================
# mod_sse_analysis.R — SSE Intrinsic Analysis Tab
#
# Analyzes the SSE results themselves: run health, parameter distributions,
# OFV distribution, empirical correlations, individual PK patab outputs,
# per-parameter diagnostics.
# Consumes shared SSE data from mod_sse_upload (centralized upload).
#
# Inputs: design_a_shared and design_b_shared reactive bundles containing id,
#         name, raw_results, and individual_pk; true_vals; param_labels.
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
            "PK Exposure" = "pk_exposure",
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
  design_a_shared = reactive(list(
    id = "a",
    name = "Design A",
    raw_results = NULL,
    individual_pk = NULL
  )),
  design_b_shared = reactive(list(
    id = "b",
    name = "Design B",
    raw_results = NULL,
    individual_pk = NULL
  )),
  true_vals,
  param_labels = reactive(NULL),
  mrgsolve_state = NULL
) {
  moduleServer(id, function(input, output, session) {
    read_design_bundle <- function(shared, expected_id) {
      bundle <- shared()
      required_fields <- c("id", "name", "raw_results", "individual_pk")
      if (!is.list(bundle) || !all(required_fields %in% names(bundle))) {
        stop(
          sprintf(
            "SSE design %s must provide: %s",
            toupper(expected_id),
            paste(required_fields, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      if (!identical(bundle$id, expected_id)) {
        stop(
          sprintf("SSE design bundle id must be '%s'", expected_id),
          call. = FALSE
        )
      }
      bundle
    }

    design_a_bundle <- reactive(read_design_bundle(design_a_shared, "a"))
    design_b_bundle <- reactive(read_design_bundle(design_b_shared, "b"))

    # --- Design selector (show only when B is loaded) ---
    output$design_selector <- renderUI({
      a <- design_a_bundle()
      b <- design_b_bundle()
      if (is.null(b$raw_results)) {
        return(NULL)
      }

      choices <- c("a" = "a", "b" = "b")
      names(choices) <- c(a$name, b$name)
      selected <- isolate(input$which_design) %||% "a"
      if (!selected %in% unname(choices)) selected <- "a"

      control_panel(
        label = "Analyze",
        radioButtons(
          session$ns("which_design"),
          label = NULL,
          choices = choices,
          selected = selected,
          inline = TRUE
        )
      )
    })

    selected_design_id <- reactive({
      selection <- input$which_design %||% "a"
      if (!selection %in% c("a", "b")) {
        return("a")
      }
      if (
        identical(selection, "b") &&
          is.null(design_b_bundle()$raw_results)
      ) {
        return("a")
      }
      selection
    })

    observeEvent(design_b_bundle()$raw_results, {
      if (
        is.null(design_b_bundle()$raw_results) &&
          identical(isolate(input$which_design), "b")
      ) {
        updateRadioButtons(session, "which_design", selected = "a")
      }
    }, ignoreInit = FALSE)

    selected_design_bundle <- reactive({
      if (identical(selected_design_id(), "b")) {
        design_b_bundle()
      } else {
        design_a_bundle()
      }
    })

    # --- SSE data (switches between A and B) ---
    sse_raw_all <- reactive({
      dat <- selected_design_bundle()$raw_results
      req(dat)
      dat
    })

    selected_individual_pk <- reactive({
      selected_design_bundle()$individual_pk
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
      if (active == "pk_exposure") {
        return(analysis_workspace(
          "PK Exposure",
          uiOutput(session$ns("pk_exposure_status")),
          uiOutput(session$ns("pk_exposure_controls")),
          uiOutput(session$ns("pk_exposure_mapping_controls")),
          DTOutput(session$ns("pk_exposure_mapping_table")),
          uiOutput(session$ns("pk_exposure_preflight_summary")),
          uiOutput(session$ns("pk_exposure_sanity_check")),
          DTOutput(session$ns("pk_exposure_sanity_table")),
          uiOutput(session$ns("pk_exposure_compute_controls")),
          DTOutput(session$ns("pk_exposure_results_table"))
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
      dat <- selected_individual_pk()
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
      dat <- selected_individual_pk()
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

    pk_exposure_model_meta <- reactive({
      if (is.null(mrgsolve_state)) return(NULL)
      compiled <- tryCatch(
        isTRUE(mrgsolve_state$is_compiled()),
        error = function(e) FALSE
      )
      if (!compiled) return(NULL)

      list(
        param_names = tryCatch(mrgsolve_state$param_names(), error = function(e) character()),
        capture_names = tryCatch(mrgsolve_state$capture_names(), error = function(e) character()),
        cmt_names = tryCatch(mrgsolve_state$cmt_names(), error = function(e) character())
      )
    })

    pk_exposure_patab <- reactive({
      dat <- selected_individual_pk()
      if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) {
        return(NULL)
      }
      dat
    })

    pk_exposure_auto_mapping <- reactive({
      meta <- pk_exposure_model_meta()
      dat <- pk_exposure_patab()
      if (is.null(meta) || is.null(dat)) {
        return(tibble::tibble(
          model_param = character(),
          patab_column = character(),
          status = character(),
          source = character()
        ))
      }
      build_sse_mrgsolve_parameter_mapping(
        individual_pk_data = dat,
        model_param_names = meta$param_names
      )
    })

    pk_exposure_selected_mapping <- reactive({
      params <- input$pk_exposure_params %||% character()
      if (length(params) == 0L) {
        return(stats::setNames(character(), character()))
      }

      values <- vapply(params, function(param) {
        input[[.sse_mrgsolve_mapping_input_id(param)]] %||% ""
      }, character(1L))
      keep <- nzchar(values)
      stats::setNames(values[keep], params[keep])
    })

    pk_exposure_preflight <- reactive({
      meta <- pk_exposure_model_meta()
      if (is.null(meta)) return(NULL)

      build_sse_mrgsolve_exposure_preflight(
        individual_pk_data = pk_exposure_patab(),
        model_param_names = meta$param_names,
        capture_names = meta$capture_names,
        concentration_output = input$pk_exposure_output,
        selected_mapping = pk_exposure_selected_mapping(),
        required_params = input$pk_exposure_params %||% character()
      )
    })

    pk_exposure_cache <- reactiveVal(NULL)

    pk_exposure_key_context <- reactive({
      preflight <- pk_exposure_preflight()
      dat <- pk_exposure_patab()
      if (is.null(preflight) || is.null(dat)) return(NULL)

      list(
        model_hash = tryCatch(
          mrgsolve_state$model_hash(),
          error = function(e) NA_character_
        ),
        preflight = preflight,
        individual_pk_data = dat,
        dosing_events = tryCatch(
          mrgsolve_state$dose_events(),
          error = function(e) NULL
        )
      )
    })

    output$pk_exposure_status <- renderUI({
      if (is.null(mrgsolve_state)) {
        return(status_panel(
          "PK exposure unavailable",
          tags$p("No shared mrgsolve model state is registered for this session."),
          tone = "warning",
          icon_name = "exclamation-triangle"
        ))
      }

      compiled <- tryCatch(
        isTRUE(mrgsolve_state$is_compiled()),
        error = function(e) FALSE
      )
      if (!compiled) {
        err <- tryCatch(mrgsolve_state$compile_error(), error = function(e) NULL)
        return(status_panel(
          "PK exposure requires a compiled mrgsolve model",
          tagList(
            tags$p("Compile the shared mrgsolve model from the Sampling Times tab before running exposure diagnostics."),
            if (!is.null(err) && nzchar(err)) {
              tags$p(tags$strong("Last error: "), err)
            }
          ),
          tone = "warning",
          icon_name = "exclamation-triangle"
        ))
      }

      meta <- pk_exposure_model_meta()
      if (is.null(meta)) {
        return(NULL)
      }
      patab <- pk_exposure_patab()
      selected_bundle <- selected_design_bundle()
      if (is.null(patab)) {
        return(status_panel(
          "PK exposure requires individual PK tables",
          tags$p(sprintf(
            paste(
              "Upload a PsN output archive with pk_individuals/patab tables",
              "for %s in SSE Upload before configuring exposure diagnostics."
            ),
            selected_bundle$name
          )),
          tone = "warning",
          icon_name = "exclamation-triangle"
        ))
      }

      status_panel(
        "mrgsolve model ready for PK exposure setup",
        tagList(
          tags$p(sprintf(
            "Compiled model detected: %d parameters, %d outputs, %d compartments.",
            length(meta$param_names), length(meta$capture_names), length(meta$cmt_names)
          )),
          tags$p(sprintf(
            "Individual PK archive for %s: %d rows, %d samples, %d IDs.",
            selected_bundle$name,
            nrow(patab),
            if ("sample" %in% names(patab)) length(unique(stats::na.omit(patab$sample))) else 0L,
            if ("ID" %in% names(patab)) length(unique(stats::na.omit(patab$ID))) else 0L
          ))
        ),
        tone = "success",
        icon_name = "check-circle"
      )
    })

    output$pk_exposure_controls <- renderUI({
      meta <- pk_exposure_model_meta()
      patab <- pk_exposure_patab()
      if (is.null(meta) || is.null(patab)) return(NULL)

      auto_mapping <- pk_exposure_auto_mapping()
      mapped_params <- auto_mapping$model_param[auto_mapping$status == "mapped"]
      selected_params <- isolate(input$pk_exposure_params) %||% mapped_params
      selected_params <- selected_params[selected_params %in% meta$param_names]

      selected_output <- isolate(input$pk_exposure_output)
      if (is.null(selected_output) || !selected_output %in% meta$capture_names) {
        selected_output <- build_sse_mrgsolve_exposure_preflight(
          individual_pk_data = patab,
          model_param_names = meta$param_names,
          capture_names = meta$capture_names
        )$concentration_output
      }

      tagList(
        fluidRow(
          column(
            4,
            selectInput(
              session$ns("pk_exposure_output"),
              "Concentration output",
              choices = meta$capture_names,
              selected = selected_output
            )
          ),
          column(
            8,
            selectizeInput(
              session$ns("pk_exposure_params"),
              "Individual model inputs to map",
              choices = meta$param_names,
              selected = selected_params,
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )
          )
        )
      )
    })

    output$pk_exposure_mapping_controls <- renderUI({
      meta <- pk_exposure_model_meta()
      patab <- pk_exposure_patab()
      if (is.null(meta) || is.null(patab)) return(NULL)

      params <- input$pk_exposure_params %||% character()
      if (length(params) == 0L) {
        return(status_panel(
          "No individual model inputs selected",
          tags$p("Select the mrgsolve parameters that should receive individual values from the PsN tables."),
          tone = "neutral",
          icon_name = "info-circle"
        ))
      }

      candidate_columns <- sse_mrgsolve_candidate_columns(patab)
      auto_mapping <- pk_exposure_auto_mapping()

      tags$div(
        style = "margin-top: 12px;",
        tags$p(
          style = "font-weight: 600; margin-bottom: 8px;",
          "patab to mrgsolve mapping"
        ),
        fluidRow(lapply(seq_along(params), function(i) {
          param <- params[[i]]
          input_id <- .sse_mrgsolve_mapping_input_id(param)
          auto_col <- auto_mapping$patab_column[match(param, auto_mapping$model_param)]
          selected <- input[[input_id]] %||% auto_col
          if (is.na(selected) || !selected %in% candidate_columns) selected <- ""
          column(
            4,
            selectInput(
              session$ns(input_id),
              param,
              choices = c("Unmapped" = "", candidate_columns),
              selected = selected
            )
          )
        }))
      )
    })

    output$pk_exposure_mapping_table <- renderDT({
      preflight <- pk_exposure_preflight()
      req(preflight)

      mapping <- preflight$mapping
      selected <- input$pk_exposure_params %||% character()
      mapping$required <- mapping$model_param %in% selected
      mapping <- mapping[mapping$required | mapping$status == "mapped", , drop = FALSE]
      if (nrow(mapping) == 0L) {
        mapping <- preflight$mapping
      }

      datatable(
        mapping,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(pageLength = 15, dom = "t", scrollX = TRUE)
      )
    })

    output$pk_exposure_preflight_summary <- renderUI({
      preflight <- pk_exposure_preflight()
      if (is.null(preflight)) return(NULL)

      if (identical(preflight$status, "ready")) {
        return(status_panel(
          "PK exposure preflight ready",
          tags$p(sprintf(
            "%d required inputs mapped. Concentration output: %s.",
            preflight$n_mapped_required,
            preflight$concentration_output
          )),
          tone = "success",
          icon_name = "check-circle"
        ))
      }

      if (identical(preflight$status, "needs_mapping")) {
        return(status_panel(
          "PK exposure mapping incomplete",
          tags$p(paste(
            "Missing required inputs:",
            paste(preflight$missing_required_params, collapse = ", ")
          )),
          tone = "warning",
          icon_name = "exclamation-triangle"
        ))
      }

      if (identical(preflight$status, "needs_output")) {
        return(status_panel(
          "PK exposure output not selected",
          tags$p("Select a valid mrgsolve output column to use as concentration."),
          tone = "warning",
          icon_name = "exclamation-triangle"
        ))
      }

      status_panel(
        "PK exposure preflight unavailable",
        tags$p("Compile a model and upload individual PK tables before configuring exposure diagnostics."),
        tone = "warning",
        icon_name = "exclamation-triangle"
      )
    })

    pk_exposure_sanity_key <- reactive({
      context <- pk_exposure_key_context()
      if (is.null(context)) return(NULL)

      build_sse_mrgsolve_exposure_cache_key(
        model_hash = context$model_hash,
        preflight = context$preflight,
        individual_pk_data = context$individual_pk_data,
        dosing_events = context$dosing_events,
        max_ids = 3L,
        delta = 1
      )
    })

    pk_exposure_sanity <- eventReactive(input$pk_exposure_run_sanity, {
      key <- pk_exposure_sanity_key()
      result <- run_sse_mrgsolve_sanity_check(
        mod = tryCatch(mrgsolve_state$model(), error = function(e) NULL),
        individual_pk_data = pk_exposure_patab(),
        preflight = pk_exposure_preflight(),
        dosing_events = tryCatch(mrgsolve_state$dose_events(), error = function(e) NULL),
        kind = "estimation",
        max_ids = 3L,
        delta = 1
      )
      result$context_key <- key
      result
    }, ignoreInit = TRUE)

    pk_exposure_current_sanity <- reactive({
      result <- tryCatch(pk_exposure_sanity(), error = function(e) NULL)
      if (is.null(result)) return(NULL)

      key <- pk_exposure_sanity_key()
      if (is.null(key) || !identical(result$context_key, key)) return(NULL)
      result
    })

    output$pk_exposure_sanity_check <- renderUI({
      preflight <- pk_exposure_preflight()
      if (is.null(preflight) || !identical(preflight$status, "ready")) {
        return(NULL)
      }

      result <- pk_exposure_current_sanity()
      tone <- "neutral"
      title <- "mrgsolve sanity check"
      body <- tags$p("Run a small simulation on the first sample and up to three IDs before computing full SSE exposures.")

      if (!is.null(result)) {
        if (identical(result$status, "ok")) {
          tone <- "success"
          title <- "mrgsolve sanity check passed"
          body <- tags$p(sprintf(
            "%d rows simulated for %d IDs. %s range: %.4g to %.4g.",
            result$n_rows,
            result$n_ids,
            result$output,
            result$value_range[[1]],
            result$value_range[[2]]
          ))
        } else {
          tone <- "warning"
          title <- "mrgsolve sanity check needs attention"
          body <- tags$p(result$message %||% result$status)
        }
      }

      status_panel(
        title,
        tagList(
          body,
          actionButton(
            session$ns("pk_exposure_run_sanity"),
            "Run sanity check",
            class = "btn-sm btn-primary"
          )
        ),
        tone = tone,
        icon_name = if (identical(tone, "success")) "check-circle" else "info-circle"
      )
    })

    output$pk_exposure_sanity_table <- renderDT({
      result <- pk_exposure_current_sanity()
      req(result, identical(result$status, "ok"), result$preview)

      datatable(
        result$preview,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(pageLength = 10, dom = "t", scrollX = TRUE)
      )
    })

    .pk_exposure_positive_limit <- function(x) {
      if (is.null(x) || is.na(x) || x <= 0) return(NULL)
      as.integer(x)
    }

    .pk_exposure_trough_times <- function(x) {
      if (is.null(x) || is.na(x) || !is.finite(x)) return(numeric(0L))
      as.numeric(x)
    }

    pk_exposure_compute_key <- reactive({
      context <- pk_exposure_key_context()
      if (is.null(context)) return(NULL)

      build_sse_mrgsolve_exposure_cache_key(
        model_hash = context$model_hash,
        preflight = context$preflight,
        individual_pk_data = context$individual_pk_data,
        dosing_events = context$dosing_events,
        max_samples = .pk_exposure_positive_limit(input$pk_exposure_max_samples),
        max_ids = .pk_exposure_positive_limit(input$pk_exposure_max_ids),
        delta = input$pk_exposure_delta %||% 1,
        trough_times = .pk_exposure_trough_times(input$pk_exposure_trough_time)
      )
    })

    pk_exposure_current_cache <- reactive({
      cache <- pk_exposure_cache()
      if (is.null(cache)) return(NULL)

      key <- pk_exposure_compute_key()
      if (is.null(key) || !identical(cache$key, key)) return(NULL)
      cache
    })

    observeEvent(
      pk_exposure_compute_key(),
      {
        key <- pk_exposure_compute_key()
        cache <- isolate(pk_exposure_cache())
        if (!is.null(cache) &&
            (is.null(key) || !identical(cache$key, key))) {
          pk_exposure_cache(NULL)
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    output$pk_exposure_compute_controls <- renderUI({
      preflight <- pk_exposure_preflight()
      sanity <- pk_exposure_current_sanity()
      if (is.null(preflight) || !identical(preflight$status, "ready")) {
        return(NULL)
      }
      if (is.null(sanity) || !identical(sanity$status, "ok")) {
        return(status_panel(
          "Run sanity check before full exposure computation",
          tags$p("The full SSE exposure computation is available after the mini-simulation passes."),
          tone = "neutral",
          icon_name = "info-circle"
        ))
      }

      tagList(
        tags$hr(),
        fluidRow(
          column(
            3,
            numericInput(
              session$ns("pk_exposure_max_samples"),
              "Max samples",
              value = 20,
              min = 0,
              step = 1
            )
          ),
          column(
            3,
            numericInput(
              session$ns("pk_exposure_max_ids"),
              "Max IDs",
              value = 0,
              min = 0,
              step = 1
            )
          ),
          column(
            3,
            numericInput(
              session$ns("pk_exposure_delta"),
              "Time step",
              value = 1,
              min = 0.001,
              step = 0.5
            )
          ),
          column(
            3,
            numericInput(
              session$ns("pk_exposure_trough_time"),
              "Ctrough time",
              value = NA,
              min = 0,
              step = 1
            )
          )
        ),
        tags$div(
          style = "display:flex; gap:8px; align-items:center; margin: 8px 0 12px;",
          actionButton(
            session$ns("pk_exposure_compute"),
            "Compute exposures",
            class = "btn-sm btn-primary"
          ),
          downloadButton(
            session$ns("export_pk_exposure_csv"),
            "Export CSV",
            class = "btn-sm btn-default"
          )
        ),
        uiOutput(session$ns("pk_exposure_compute_status"))
      )
    })

    observeEvent(input$pk_exposure_compute, {
      key <- pk_exposure_compute_key()
      sanity <- pk_exposure_current_sanity()
      req(key, sanity, identical(sanity$status, "ok"))
      if (!is.null(pk_exposure_current_cache())) return()

      withProgress(message = "Computing PK exposures", value = 0, {
        incProgress(0.2, detail = "Preparing individual simulations")
        result <- compute_sse_mrgsolve_exposure_recovery(
          mod = tryCatch(mrgsolve_state$model(), error = function(e) NULL),
          individual_pk_data = pk_exposure_patab(),
          preflight = pk_exposure_preflight(),
          dosing_events = tryCatch(mrgsolve_state$dose_events(), error = function(e) NULL),
          max_samples = .pk_exposure_positive_limit(input$pk_exposure_max_samples),
          max_ids = .pk_exposure_positive_limit(input$pk_exposure_max_ids),
          delta = input$pk_exposure_delta %||% 1,
          trough_times = .pk_exposure_trough_times(input$pk_exposure_trough_time)
        )
        incProgress(0.9, detail = "Caching exposure table")
        pk_exposure_cache(list(
          key = key,
          result = result,
          created = Sys.time()
        ))
      })
    }, ignoreInit = TRUE)

    output$pk_exposure_compute_status <- renderUI({
      cache <- pk_exposure_current_cache()
      if (is.null(cache)) {
        return(NULL)
      }
      status_panel(
        "PK exposure table ready",
        tags$p(sprintf(
          "%d rows computed at %s.",
          nrow(cache$result),
          format(cache$created, "%H:%M:%S")
        )),
        tone = "success",
        icon_name = "check-circle"
      )
    })

    output$pk_exposure_results_table <- renderDT({
      cache <- pk_exposure_current_cache()
      req(cache, cache$result)

      datatable(
        cache$result,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(pageLength = 20, dom = "tip", scrollX = TRUE)
      )
    })

    output$export_pk_exposure_csv <- downloadHandler(
      filename = function() {
        paste0("sse_pk_exposure_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        cache <- pk_exposure_current_cache()
        req(cache, cache$result)
        write.csv(cache$result, file, row.names = FALSE)
      }
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
