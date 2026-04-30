# =============================================================================
# mod_sse_analysis.R — SSE Intrinsic Analysis Tab
#
# Analyzes the SSE results themselves: run health, parameter distributions,
# OFV distribution, empirical correlations, per-parameter diagnostics.
# Consumes shared SSE data from mod_sse_upload (centralized upload).
#
# Inputs: sse_a_shared, sse_b_shared (reactives), name_a, name_b, true_vals, param_labels
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

    status_panel(
      "SSE diagnostic definitions",
      tags$p("Run-health, shrinkage, OFV, and parameter-distribution guidance is collected in Documentation."),
      doc_link("sse-analysis", "Open SSE analysis documentation"),
      tone = "info",
      icon_name = "book-open"
    ),

    page_section(
      "Analysis workspace",
      subtitle = "Choose one diagnostic view at a time. Parameter filters apply to parameter-based views.",
      control_panel(
        checkboxInput(ns("show_failed"), "Include failed runs in plots",
                      value = FALSE),
        checkboxInput(ns("color_ofv"), "Color OFV by convergence status",
                      value = TRUE),
        radioButtons(
          ns("active_view"), "Active view",
          choices = c(
            "Parameter distributions" = "distributions",
            "OFV distribution" = "ofv",
            "Shrinkage boxplot" = "shrink_box",
            "Shrinkage vs RSE" = "shrink_scatter",
            "Shrinkage summary table" = "shrink_table",
            "Parameter diagnostics table" = "diagnostics"
          ),
          selected = "distributions",
          inline = TRUE
        ),
        uiOutput(ns("param_filter_ui"))
      ),
      uiOutput(ns("active_view_ui"))
    )
  )
}


mod_sse_analysis_server <- function(id,
                                    sse_a_shared = reactive(NULL),
                                    sse_b_shared = reactive(NULL),
                                    name_a = reactive("Design A"),
                                    name_b = reactive("Design B"),
                                    true_vals,
                                    param_labels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # --- Design selector (show only when B is loaded) ---
    output$design_selector <- renderUI({
      b <- sse_b_shared()
      if (is.null(b)) return(NULL)

      choices <- c("a" = "a", "b" = "b")
      names(choices) <- c(name_a(), name_b())

      control_panel(
        label = "Analyze",
        radioButtons(session$ns("which_design"), label = NULL,
                     choices = choices, selected = "a", inline = TRUE)
      )
    })

    # --- SSE data (switches between A and B) ---
    sse_all <- reactive({
      sel <- input$which_design %||% "a"
      dat <- if (sel == "b") sse_b_shared() else sse_a_shared()
      req(dat)
      dat
    })

    # --- Run health ---
    run_health <- reactive({
      dat <- sse_all()
      req(dat)
      compute_run_health(dat)
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
        make_health_pills(rh),
        tone = "neutral",
        icon_name = "heartbeat"
      )
    })

    # --- Param distributions (long-format data) ---
    dist_data <- reactive({
      dat <- sse_all()
      tv  <- true_vals()
      req(dat, tv)
      compute_param_distributions(dat, tv, param_labels())
    })

    # --- Param filter UI ---
    output$param_filter_ui <- renderUI({
      dd <- dist_data()
      if (is.null(dd) || nrow(dd) == 0L) return(NULL)

      all_params <- unique(dd$param_label)
      control_panel(
        label = "Parameters",
          checkboxGroupInput(
            session$ns("selected_params"), label = NULL,
            choices = all_params, selected = all_params,
            inline = TRUE
          )
      )
    })

    # --- Filtered distribution data ---
    dist_data_filtered <- reactive({
      dd <- dist_data()
      if (is.null(dd)) return(NULL)
      sel <- input$selected_params
      if (is.null(sel) || length(sel) == 0L) return(dd)
      dd[dd$param_label %in% sel, ]
    })

    # --- Distribution plot ---
    dist_plot_fn <- reactive({
      dd <- dist_data_filtered()
      if (is.null(dd) || nrow(dd) == 0L) {
        return(ggplot() +
          labs(title = "Load SSE data in SSE Upload tab") +
          .theme_design())
      }
      plot_param_distributions(dd, show_failed = isTRUE(input$show_failed))
    })
    output$dist_plot <- renderPlot({ dist_plot_fn() }, res = 110)
    plot_export_server(input, output, session, "dist_export", dist_plot_fn)

    output$active_view_ui <- renderUI({
      active <- input$active_view %||% "distributions"

      if (active == "ofv") {
        return(analysis_workspace(
          "OFV distribution",
          plotOutput(session$ns("ofv_plot"), height = "420px"),
          plot_export_ui(session$ns, "ofv_export",
                         default_fname = "sse_ofv_distribution")
        ))
      }
      if (active == "shrink_box") {
        return(analysis_workspace(
          "Shrinkage distribution per ETA",
          plotOutput(session$ns("shrink_box"), height = "460px"),
          plot_export_ui(session$ns, "shrink_box_export",
                         default_fname = "sse_shrinkage_boxplot")
        ))
      }
      if (active == "shrink_scatter") {
        return(analysis_workspace(
          "Identifiability: empirical RSE vs mean shrinkage",
          plotOutput(session$ns("shrink_scatter"), height = "560px"),
          plot_export_ui(session$ns, "shrink_scatter_export",
                         default_fname = "sse_shrinkage_rse_scatter")
        ))
      }
      if (active == "shrink_table") {
        return(analysis_workspace(
          "Shrinkage summary table",
          DTOutput(session$ns("shrink_table")),
          actions = downloadButton(session$ns("export_shrink_csv"),
                                   "Export CSV", class = "btn-sm btn-default")
        ))
      }
      if (active == "diagnostics") {
        return(analysis_workspace(
          "Per-parameter diagnostics",
          DTOutput(session$ns("diag_table")),
          actions = downloadButton(session$ns("export_diag_csv"),
                                   "Export CSV", class = "btn-sm btn-default")
        ))
      }

      analysis_workspace(
        "Parameter estimate distributions",
        plotOutput(session$ns("dist_plot"), height = "620px"),
        plot_export_ui(session$ns, "dist_export",
                       default_fname = "sse_param_distributions")
      )
    })

    # --- OFV plot ---
    ofv_plot_fn <- reactive({
      dat <- sse_all()
      if (is.null(dat)) {
        return(ggplot() + labs(title = "No SSE data") + .theme_design())
      }
      plot_ofv_distribution(dat, color_by_status = isTRUE(input$color_ofv))
    })
    output$ofv_plot <- renderPlot({ ofv_plot_fn() }, res = 110)
    plot_export_server(input, output, session, "ofv_export", ofv_plot_fn)

    # --- Shrinkage: long-format data per replicate ---
    shrink_long <- reactive({
      dat <- sse_all()
      req(dat)
      compute_shrinkage_long(
        dat, param_labels = param_labels(),
        only_converged = !isTRUE(input$show_failed)
      )
    })

    # --- Shrinkage: summary per ETA ---
    shrink_summary <- reactive({
      dat <- sse_all()
      req(dat)
      compute_shrinkage_summary(
        dat, param_labels = param_labels(),
        only_converged = !isTRUE(input$show_failed)
      )
    })

    # --- Boxplot ---
    shrink_box_fn <- reactive({
      sl <- shrink_long()
      plot_shrinkage_boxplot(sl)
    })
    output$shrink_box <- renderPlot({ shrink_box_fn() }, res = 110)
    plot_export_server(input, output, session, "shrink_box_export",
                       shrink_box_fn)

    # --- Scatter RSE vs shrinkage ---
    shrink_scatter_fn <- reactive({
      dat <- sse_all()
      tv  <- true_vals()
      if (is.null(dat) || is.null(tv)) {
        return(ggplot() +
          labs(title = "Load SSE data and .ctl to see identifiability scatter") +
          .theme_design())
      }
      plot_shrinkage_rse_scatter(dat, tv, param_labels(),
                                 shrink_sum = shrink_summary())
    })
    output$shrink_scatter <- renderPlot({ shrink_scatter_fn() }, res = 110)
    plot_export_server(input, output, session, "shrink_scatter_export",
                       shrink_scatter_fn)

    # --- Shrinkage summary table ---
    output$shrink_table <- renderDT({
      ss <- shrink_summary()
      req(ss)
      if (nrow(ss) == 0L) return(NULL)

      display <- ss |>
        dplyr::select(
          Parameter   = param_label,
          ETA         = eta,
          `N runs`    = n,
          `Mean (%)`  = mean_shrink,
          `Median (%)` = median_shrink,
          `SD (%)`    = sd_shrink,
          `P5 (%)`    = p5,
          `P95 (%)`   = p95
        )

      datatable(display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(pageLength = 20, dom = "t",
                               scrollX = TRUE)) |>
        formatStyle("Mean (%)",
          backgroundColor = styleInterval(
            c(30, 50), c("#dcfce7", "#fef3c7", "#fee2e2")
          ))
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
          error = function(e) warning("CSV export failed: ", conditionMessage(e))
        )
      }
    )

    # --- Per-parameter diagnostics table ---
    diag_data <- reactive({
      dat <- sse_all()
      tv  <- true_vals()
      req(dat, tv)
      compute_param_diagnostics(dat, tv, param_labels())
    })

    output$diag_table <- renderDT({
      diag <- diag_data()
      req(diag)

      display <- diag |>
        dplyr::select(
          Parameter    = param_label,
          Type         = param_type,
          `N runs`     = n_runs,
          `SE = NA`    = n_se_na,
          `% SE NA`    = pct_se_na,
          `RSE > 100%` = n_rse_over_100,
          `% RSE>100`  = pct_rse_over_100,
          `Est. = 0`   = n_zero_estimate,
          `% Zero`     = pct_zero_estimate
        )

      datatable(display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(
                  pageLength = 20, dom = "t",
                  scrollX = TRUE,
                  order = list(list(4, "desc"))
                )) |>
        formatStyle("% SE NA",
          backgroundColor = styleInterval(
            c(10, 30), c("#dcfce7", "#fef3c7", "#fee2e2")
          )) |>
        formatStyle("% RSE>100",
          backgroundColor = styleInterval(
            c(10, 30), c("#dcfce7", "#fef3c7", "#fee2e2")
          )) |>
        formatStyle("% Zero",
          backgroundColor = styleInterval(
            c(5, 20), c("#dcfce7", "#fef3c7", "#fee2e2")
          ))
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
          error = function(e) warning("CSV export failed: ", conditionMessage(e))
        )
      }
    )
  })
}
