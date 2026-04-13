# =============================================================================
# mod_sse_analysis.R — SSE Intrinsic Analysis Tab
#
# Analyzes the SSE results themselves: run health, parameter distributions,
# OFV distribution, empirical correlations, per-parameter diagnostics.
# Consumes shared SSE data from mod_sse_validation (uploaded once, used by both).
#
# Inputs: sse_file_path (reactive), true_vals (reactive), param_labels (reactive)
# =============================================================================

mod_sse_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Info banner ---
    div(class = "alert alert-info", style = "border-radius:10px; margin-bottom:12px;",
      tags$strong("SSE Diagnostics"),
      tags$p(style = "margin:6px 0 0; font-size:0.9em;",
        "Analyze the SSE results themselves: convergence quality, parameter ",
        "estimability, OFV distribution, and empirical correlations. ",
        "Upload your PsN raw_results CSV in the SSE Validation tab first."
      )
    ),

    # --- Run health banner (always visible) ---
    uiOutput(ns("run_health_banner")),

    # --- Toggle + param filter ---
    fluidRow(
      column(6,
        checkboxInput(ns("show_failed"), "Include failed runs in plots",
                      value = FALSE)
      ),
      column(6,
        checkboxInput(ns("color_ofv"), "Color OFV by convergence status",
                      value = TRUE)
      )
    ),
    fluidRow(
      column(12, uiOutput(ns("param_filter_ui")))
    ),

    # --- Plot selector ---
    fluidRow(
      column(12,
        div(style = paste0(
          "border:1px solid #ddd; border-radius:8px; padding:8px 12px;",
          " margin-bottom:10px; background:#fafafa;"
        ),
          div(style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
            tags$strong("Sections to display:", style = "white-space:nowrap;"),
            checkboxGroupInput(
              ns("visible_sections"), label = NULL,
              choices = c("Parameter Distributions" = "distributions",
                          "OFV Distribution" = "ofv",
                          "Empirical Correlations" = "correlations",
                          "Parameter Diagnostics" = "diagnostics"),
              selected = c("distributions", "ofv", "correlations", "diagnostics"),
              inline = TRUE
            )
          )
        )
      )
    ),

    # --- Parameter distributions ---
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('distributions') > -1", ns("visible_sections")
      ),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "Parameter Estimate Distributions"),
            plotOutput(ns("dist_plot"), height = "600px"),
            plot_export_ui(ns, "dist_export",
                           default_fname = "sse_param_distributions")
          )
        )
      ),
      br()
    ),

    # --- OFV distribution ---
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('ofv') > -1", ns("visible_sections")
      ),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "OFV Distribution"),
            plotOutput(ns("ofv_plot"), height = "350px"),
            plot_export_ui(ns, "ofv_export",
                           default_fname = "sse_ofv_distribution")
          )
        )
      ),
      br()
    ),

    # --- Empirical correlation heatmap ---
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('correlations') > -1", ns("visible_sections")
      ),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "Empirical Correlation Heatmap"),
            plotOutput(ns("cor_plot"), height = "500px"),
            plot_export_ui(ns, "cor_export",
                           default_fname = "sse_empirical_correlations")
          )
        )
      ),
      br()
    ),

    # --- Per-parameter diagnostics table ---
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('diagnostics') > -1", ns("visible_sections")
      ),
      fluidRow(
        column(12,
          div(class = "param-table-wrap",
            div(style = paste0(
              "display:flex; justify-content:space-between;",
              " align-items:center;"
            ),
              p(class = "section-title", style = "margin:0;",
                "Per-Parameter Diagnostics"),
              downloadButton(ns("export_diag_csv"), "Export CSV",
                             class = "btn-sm btn-default")
            ),
            DTOutput(ns("diag_table"))
          )
        )
      )
    )
  )
}


mod_sse_analysis_server <- function(id, sse_file_path,
                                    sse_all_shared = reactive(NULL),
                                    true_vals,
                                    param_labels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # --- Read unfiltered SSE data (prefer pre-parsed from mod_sse_validation) ---
    sse_all <- reactive({
      pre <- sse_all_shared()
      if (!is.null(pre)) return(pre)
      fp <- sse_file_path()
      req(fp)
      tryCatch(
        read_sse_raw_all(fp),
        error = function(e) {
          showNotification(paste("SSE read error:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
    })

    # --- Run health ---
    run_health <- reactive({
      dat <- sse_all()
      req(dat)
      compute_run_health(dat)
    })

    output$run_health_banner <- renderUI({
      rh <- run_health()
      if (is.null(rh)) {
        return(div(class = "alert alert-warning",
                   style = "border-radius:8px;",
          tags$strong("No SSE data loaded."),
          " Upload a PsN raw_results CSV in the SSE Validation tab."
        ))
      }

      stages <- rh$stages

      # Per-metric thresholds (pharmacometrics conventions)
      # Minimization/rounding: higher is better
      # Covariance: relaxed (FOCEI failures are endemic on complex models)
      # No boundary: inverted (higher % without boundary = better)
      thresholds <- list(
        "Total runs"            = c(green = 0,  amber = 0,  red = 0),
        "Minimization OK"       = c(green = 80, amber = 60, red = 0),
        "No boundary estimates" = c(green = 80, amber = 60, red = 0),
        "Covariance OK"         = c(green = 60, amber = 40, red = 0),
        "No rounding errors"    = c(green = 80, amber = 60, red = 0)
      )

      pill_color <- function(stage_name, pct) {
        th <- thresholds[[stage_name]]
        if (is.null(th)) th <- c(green = 80, amber = 60, red = 0)
        if (pct >= th[["green"]]) "#15803d"
        else if (pct >= th[["amber"]]) "#b45309"
        else "#dc2626"
      }
      pill_bg <- function(stage_name, pct) {
        th <- thresholds[[stage_name]]
        if (is.null(th)) th <- c(green = 80, amber = 60, red = 0)
        if (pct >= th[["green"]]) "#dcfce7"
        else if (pct >= th[["amber"]]) "#fef3c7"
        else "#fee2e2"
      }

      # Build pills
      pills <- lapply(seq_len(nrow(stages)), function(i) {
        s <- stages[i, ]
        tags$span(
          style = sprintf(
            paste0(
              "display:inline-block; padding:4px 10px; border-radius:6px;",
              " margin:2px 4px; font-size:0.88em; font-weight:600;",
              " background:%s; color:%s;"
            ),
            pill_bg(s$stage, s$pct), pill_color(s$stage, s$pct)
          ),
          sprintf("%s: %d/%d (%.0f%%)", s$stage, s$n, s$denom, s$pct)
        )
      })

      div(class = "alert",
          style = paste0(
            "border-radius:10px; margin-bottom:12px; padding:10px 14px;",
            " background:#f8fafc; border:1px solid #e2e8f0;"
          ),
        tags$strong("Run Health", style = "font-size:1em;"),
        div(style = "margin-top:6px; display:flex; flex-wrap:wrap; gap:2px;",
          pills
        )
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
      div(style = paste0(
        "border:1px solid #ddd; border-radius:8px; padding:8px 12px;",
        " margin-bottom:10px; background:#fafafa;"
      ),
        div(style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
          tags$strong("Parameters:", style = "white-space:nowrap;"),
          checkboxGroupInput(
            session$ns("selected_params"), label = NULL,
            choices = all_params, selected = all_params,
            inline = TRUE
          )
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
          labs(title = "Upload SSE data in SSE Validation tab") +
          .theme_design())
      }
      plot_param_distributions(dd, show_failed = isTRUE(input$show_failed))
    })
    output$dist_plot <- renderPlot({ dist_plot_fn() }, res = 110)
    plot_export_server(input, output, session, "dist_export", dist_plot_fn)

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

    # --- Correlation heatmap ---
    cor_matrix <- reactive({
      dat <- sse_all()
      tv  <- true_vals()
      req(dat, tv)
      compute_empirical_correlations(
        dat, tv,
        only_converged = !isTRUE(input$show_failed),
        param_labels = param_labels()
      )
    })

    cor_plot_fn <- reactive({
      cm <- cor_matrix()
      if (is.null(cm)) {
        return(ggplot() + labs(title = "Not enough data for correlations") +
               .theme_design())
      }
      plot_empirical_cor_heatmap(cm)
    })
    output$cor_plot <- renderPlot({ cor_plot_fn() }, res = 110)
    plot_export_server(input, output, session, "cor_export", cor_plot_fn)

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
