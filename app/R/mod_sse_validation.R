# =============================================================================
# mod_sse_validation.R — SSE Validation Tab
#
# Compares FIM-predicted RSE with empirical RSE from SSE (Stochastic Simulation
# and Estimation). Includes methodology panel with formulas and references.
# Inputs: PsN raw CSV + .ctl (true values) + .ext already loaded (FIM RSE)
# =============================================================================

mod_sse_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Info banner ---
    div(class = "alert alert-info", style = "border-radius:10px; margin-bottom:12px;",
      tags$strong("FIM vs SSE Validation"),
      tags$p(style = "margin:6px 0 0; font-size:0.9em;",
        "Compare FIM-predicted RSE (from the loaded .ext) ",
        "with empirical RSE computed from SSE estimation results. ",
        "A point on the diagonal = perfect prediction."
      )
    ),

    # --- File upload + .ctl status ---
    fluidRow(
      column(6,
        fileInput(ns("sse_file"), "SSE results (raw_results_*.csv)",
                  accept = ".csv", width = "100%")
      ),
      column(6,
        uiOutput(ns("ctl_status"))
      )
    ),

    # --- Status banner ---
    uiOutput(ns("status_banner")),

    # --- Methodology panel (collapsible) ---
    tags$details(
      style = paste0(
        "border:1px solid #ccc; border-radius:8px; padding:10px 14px;",
        " margin-bottom:14px; background:#f9f9fb;"
      ),
      tags$summary(style = "cursor:pointer; font-weight:600; font-size:0.95em;",
        "Methodology and formulas"
      ),
      div(style = "margin-top:10px; font-size:0.88em; line-height:1.6;",

        tags$h5("Empirical metrics from SSE", style = "margin-top:6px;"),
        tags$p(
          "For each model parameter x, over K successful SSE runs ",
          "(filtered on minimization_successful = 1), ",
          "the following metrics are computed:"
        ),
        tags$p(
          "Note: by default, PsN does ", tags$strong("not"),
          " filter on minimization success (PsN SSE User Guide, v5.7.0). ",
          "This app applies the filter minimization_successful = 1 to exclude ",
          "runs with convergence issues (e.g. rounding errors)."
        ),

        tags$table(
          style = paste0(
            "border-collapse:collapse; width:100%%; margin:8px 0;",
            " font-size:0.92em;"
          ),
          tags$thead(
            tags$tr(style = "border-bottom:2px solid #999;",
              tags$th(style = "text-align:left; padding:4px 8px;", "Metric"),
              tags$th(style = "text-align:left; padding:4px 8px;", "Formula"),
              tags$th(style = "text-align:left; padding:4px 8px;", "Interpretation")
            )
          ),
          tags$tbody(
            tags$tr(style = "border-bottom:1px solid #ddd;",
              tags$td(style = "padding:4px 8px;", "REE"),
              tags$td(style = "padding:4px 8px; font-family:monospace;",
                      HTML("REE<sub>k</sub> = (&hat;x<sub>k</sub> &minus; x*) / x* &times; 100")),
              tags$td(style = "padding:4px 8px;",
                      "Relative Estimation Error for run k")
            ),
            tags$tr(style = "border-bottom:1px solid #ddd;",
              tags$td(style = "padding:4px 8px;", "RB (%)"),
              tags$td(style = "padding:4px 8px; font-family:monospace;",
                      HTML("RB = (1/K) &sum; REE<sub>k</sub>")),
              tags$td(style = "padding:4px 8px;",
                      "Relative Bias (accuracy)")
            ),
            tags$tr(style = "border-bottom:1px solid #ddd;",
              tags$td(style = "padding:4px 8px;", "95% CI of RB"),
              tags$td(style = "padding:4px 8px; font-family:monospace;",
                      HTML("RB &plusmn; 1.96 &times; sd(REE) / &radic;K")),
              tags$td(style = "padding:4px 8px;",
                      "If CI excludes 0, bias is significant")
            ),
            tags$tr(style = "border-bottom:1px solid #ddd;",
              tags$td(style = "padding:4px 8px;", "RRMSE (%)"),
              tags$td(style = "padding:4px 8px; font-family:monospace;",
                      HTML("RRMSE = &radic;((1/K) &sum; REE<sub>k</sub>&sup2;)")),
              tags$td(style = "padding:4px 8px;",
                      "Relative Root Mean Squared Error (precision + bias)")
            ),
            tags$tr(style = "border-bottom:1px solid #ddd;",
              tags$td(style = "padding:4px 8px;", "Empirical RSE (%)"),
              tags$td(style = "padding:4px 8px; font-family:monospace;",
                      HTML("RSE = 100 &times; sd(&hat;x) / |x*|")),
              tags$td(style = "padding:4px 8px;",
                      "Empirical precision (cf. FIM-predicted RSE)")
            ),
            tags$tr(
              tags$td(style = "padding:4px 8px;", HTML("Empirical D-criterion")),
              tags$td(style = "padding:4px 8px; font-family:monospace;",
                      HTML("&phi;<sub>D</sub> = det(VarCov)^(1/p)")),
              tags$td(style = "padding:4px 8px;",
                      "Global summary of estimation uncertainty")
            )
          )
        ),

        tags$h5("D-criterion warning", style = "margin-top:10px;"),
        tags$p(
          "The empirical D-criterion requires the full variance-covariance ",
          "matrix of the estimated parameters to be well-conditioned. ",
          "When the matrix is ill-conditioned (near-singular), the determinant ",
          "is unreliable and the D-criterion cannot be estimated. ",
          "This was observed by Fayette et al. (2026) in the crossover example ",
          "with NONMEM-SAEM and NONMEM-FOCE."
        ),

        tags$h5("Which file to upload", style = "margin-top:10px;"),
        tags$p(
          "Upload the ", tags$strong("raw_results_*.csv"),
          " file from your PsN SSE output directory. ",
          "This file contains one row per simulated dataset with individual ",
          "parameter estimates, which allows this app to compute all metrics ",
          "(including the empirical D-criterion) and filter on successful runs."
        ),
        tags$p(style = "font-size:0.88em; color:#555;",
          "The PsN summary file (sse_results.csv) is also accepted as a ",
          "fallback, but the raw_results file is preferred."
        ),

        tags$h5("Running SSE with PsN", style = "margin-top:10px;"),
        tags$p("Command line to run an SSE on a Sanofi-type cluster with wrapsn:"),
        tags$pre(style = paste0(
          "font-size:0.85em; background:#f0f0f0; padding:8px;",
          " border-radius:4px; overflow-x:auto;"
        ),
          paste0(
            "# Syntax: wrapsn <ncpu> sse <model> [options]\n",
            "wrapsn 10 sse model.mod -samples=200 -seed=12345"
          )
        ),
        tags$p(style = "font-size:0.88em;",
          "If wrapsn is not in your PATH, use the full path ",
          "(e.g. /apps/wrapsn/wrapsn)."
        ),

        tags$h5("Recommendations", style = "margin-top:6px;"),
        tags$ul(style = "margin:4px 0; font-size:0.9em;",
          tags$li(
            tags$strong("Number of samples:"), " at least 200 ",
            "(Fayette et al. 2026 used K=200). More samples (500-1000) ",
            "give more precise percentile estimates but take longer."
          ),
          tags$li(
            tags$strong("Filtering:"),
            " this app automatically filters on minimization_successful = 1. ",
            "You do ", tags$strong("not"), " need to add -out_filter to your ",
            "PsN command. The number of excluded runs is shown in the status ",
            "banner above."
          ),
          tags$li(
            tags$strong("Seed:"), " use -seed=N for reproducibility."
          ),
          tags$li(
            tags$strong("Estimation method:"),
            " use the same method as your $DESIGN evaluation ",
            "(e.g. FOCE/FOCEI). Fayette et al. 2026 compared SAEM vs FOCE ",
            "and found consistent results across methods."
          )
        ),

        tags$h5("References", style = "margin-top:10px;"),
        tags$ul(style = "margin:4px 0;",
          tags$li(
            "Fayette L, Brendel K, Mentre F. ",
            tags$em(paste0(
              "Advances and Further Comparison of Software Tools for ",
              "Fisher Information Matrix-Based Design Evaluation ",
              "in Pharmacometrics."
            )),
            " Pharm Res. 2026. doi:10.1007/s11095-026-04024-4"
          ),
          tags$li(
            "PsN SSE User Guide v5.7.0. ",
            tags$em(
              "\"SSE does not check whether minimization was successful or not. ",
              "Statistical computations include also parameter estimates from ",
              "NONMEM runs terminated with e.g. rounding errors, unless the ",
              "option -out_filter is used.\""
            )
          )
        )
      )
    ),

    # --- Parameter filter + plot selector ---
    fluidRow(
      column(12,
        uiOutput(ns("param_filter_ui"))
      )
    ),
    fluidRow(
      column(12,
        div(style = paste0(
          "border:1px solid #ddd; border-radius:8px; padding:8px 12px;",
          " margin-bottom:10px; background:#fafafa;"
        ),
          div(style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
            tags$strong("Plots to display:", style = "white-space:nowrap;"),
            checkboxGroupInput(
              ns("visible_plots"), label = NULL,
              choices = c("Scatter" = "scatter",
                          "REE Boxplot" = "ree",
                          "RSE Bar Chart" = "rse"),
              selected = c("scatter", "ree", "rse"),
              inline = TRUE
            )
          )
        )
      )
    ),

    # --- Scatter plot ---
    conditionalPanel(
      condition = sprintf("input['%s'].indexOf('scatter') > -1", ns("visible_plots")),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "FIM RSE vs SSE RSE Scatter"),
            plotOutput(ns("scatter"), height = "650px"),
            plot_export_ui(ns, "scatter_export", default_fname = "fim_vs_sse_scatter")
          )
        )
      ),
      br()
    ),

    # --- REE Boxplot ---
    conditionalPanel(
      condition = sprintf("input['%s'].indexOf('ree') > -1", ns("visible_plots")),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "REE Distribution by Parameter"),
            plotOutput(ns("ree_boxplot"), height = "450px"),
            plot_export_ui(ns, "ree_export", default_fname = "ree_boxplot")
          )
        )
      ),
      br()
    ),

    # --- RSE Bar Chart ---
    conditionalPanel(
      condition = sprintf("input['%s'].indexOf('rse') > -1", ns("visible_plots")),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "FIM vs SSE: RSE Comparison"),
            plotOutput(ns("rse_bar"), height = "400px"),
            plot_export_ui(ns, "rse_bar_export", default_fname = "rse_comparison")
          )
        )
      ),
      br()
    ),

    # --- D-criterion card ---
    uiOutput(ns("d_criterion_card")),

    # --- Comparison table ---
    fluidRow(
      column(12,
        div(class = "param-table-wrap",
          div(style = "display:flex; justify-content:space-between; align-items:center;",
            p(class = "section-title", style = "margin:0;", "Comparison table"),
            downloadButton(ns("export_csv"), "Export CSV",
                           class = "btn-sm btn-default")
          ),
          DTOutput(ns("comp_table"))
        )
      )
    )
  )
}


mod_sse_validation_server <- function(id, ext_data,
                                      param_labels = reactive(NULL),
                                      shared_ctl_lines = reactive(NULL),
                                      shared_true_vals = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # --- Parse SSE file (auto-detect format) ---
    sse_parsed <- reactive({
      req(input$sse_file)
      tryCatch(
        read_sse_auto(input$sse_file$datapath),
        error = function(e) {
          showNotification(paste("SSE read error:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
    })

    # --- .ctl status indicator ---
    output$ctl_status <- renderUI({
      has_ctl <- !is.null(shared_true_vals()) && length(shared_true_vals()) > 0L
      if (has_ctl) {
        div(
          style = paste0(
            "padding:10px 14px; border-radius:8px; margin-top:25px;",
            " background:#f0fdf4; border:1px solid #bbf7d0; color:#166534;"
          ),
          icon("check-circle"),
          tags$strong(sprintf(" True values loaded (%d params)",
                              length(shared_true_vals()))),
          tags$p(style = "margin:4px 0 0; font-size:0.82em; color:#555;",
            "From control stream uploaded in the Home tab.")
        )
      } else {
        div(
          style = paste0(
            "padding:10px 14px; border-radius:8px; margin-top:25px;",
            " background:#fefce8; border:1px solid #fde68a; color:#854d0e;"
          ),
          icon("exclamation-triangle"),
          tags$strong(" No control stream loaded"),
          tags$p(style = "margin:4px 0 0; font-size:0.82em; color:#555;",
            "Upload a .ctl/.mod/.con file in the ",
            tags$strong("Home"), " tab to extract true parameter values.")
        )
      }
    })

    # --- Parse true values from .ctl ---
    # For "summary" format: true values come from the file itself
    # For "raw" format: need .ctl from Home tab via shared_true_vals
    true_vals <- reactive({
      parsed <- sse_parsed()
      if (!is.null(parsed) && parsed$format == "summary") {
        return(parsed$data$true_values)
      }

      # Pre-computed from main upload
      sv <- shared_true_vals()
      if (!is.null(sv) && length(sv) > 0L) return(sv)

      # Fallback: parse shared_ctl_lines directly
      cl <- shared_ctl_lines()
      if (is.null(cl)) return(NULL)
      vals <- read_true_values(cl)
      if (length(vals) == 0L) return(NULL)
      vals
    })

    # --- Compute SSE metrics ---
    sse_metrics <- reactive({
      parsed <- sse_parsed()
      req(parsed)

      if (parsed$format == "summary") {
        return(parsed$data$metrics)
      }

      # Raw format: compute from individual estimates
      req(true_vals())
      compute_sse_metrics(parsed$data, true_vals(), param_labels())
    })

    # --- Empirical D-criterion ---
    d_criterion <- reactive({
      parsed <- sse_parsed()
      req(parsed, true_vals())

      if (parsed$format == "summary") {
        # Cannot compute D-criterion from summary (no individual estimates)
        return(NULL)
      }

      compute_empirical_d_criterion(parsed$data, true_vals())
    })

    # --- Get FIM RSE (from already-loaded .ext, last table) ---
    fim_rse <- reactive({
      ext <- ext_data()
      req(ext)
      last_tbl <- max(ext$table_no)
      get_rse(ext, table_no = last_tbl)
    })

    # --- Compare ---
    comparison <- reactive({
      req(sse_metrics(), fim_rse())
      compare_fim_sse(sse_metrics(), fim_rse())
    })

    # --- Parameter filter UI (dynamic) ---
    output$param_filter_ui <- renderUI({
      comp <- comparison()
      if (is.null(comp) || nrow(comp) == 0L) return(NULL)

      matched <- comp |> dplyr::filter(status == "matched")
      if (nrow(matched) == 0L) return(NULL)

      all_params <- matched$param_label %||% matched$param
      # Pre-select: exclude params with RSE > 100% (outliers that distort scale)
      rse_max <- pmax(matched$rse_fim, matched$rse_sse, na.rm = TRUE)
      default_selected <- all_params[is.na(rse_max) | rse_max <= 100]
      # If that removes everything, keep all
      if (length(default_selected) == 0L) default_selected <- all_params

      has_outliers <- any(!is.na(rse_max) & rse_max > 100)

      tagList(
        div(style = paste0(
          "border:1px solid #ddd; border-radius:8px; padding:8px 12px;",
          " margin-bottom:10px; background:#fafafa;"
        ),
          div(style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
            tags$strong("Parameters to display:", style = "white-space:nowrap;"),
            checkboxGroupInput(
              session$ns("selected_params"), label = NULL,
              choices = all_params, selected = default_selected,
              inline = TRUE
            )
          ),
          if (has_outliers) {
            tags$p(style = "margin:4px 0 0; font-size:0.82em; color:#888;",
              "Parameters with RSE > 100% are hidden by default to improve ",
              "readability. Check them above to include them."
            )
          }
        )
      )
    })

    # --- Filtered comparison (for plot) ---
    comparison_filtered <- reactive({
      comp <- comparison()
      req(comp)
      sel <- input$selected_params
      if (is.null(sel) || length(sel) == 0L) return(comp)
      # Filter on param_label (display name)
      comp |> dplyr::filter(
        status != "matched" | param_label %in% sel | param %in% sel
      )
    })

    # --- Status banner ---
    output$status_banner <- renderUI({
      parsed <- sse_parsed()
      if (is.null(parsed)) return(NULL)

      tv <- true_vals()
      n_params <- if (!is.null(tv)) length(tv) else 0L

      if (parsed$format == "summary") {
        n_samples <- parsed$data$n_samples %||% "?"
        sim_model <- parsed$data$sim_model %||% ""
        filter_msg <- sprintf(
          "PsN summary format detected: %s runs | Model: %s",
          n_samples, sim_model
        )
        ctl_source <- " (from sse_results.csv)"
      } else {
        raw <- parsed$data
        n_total <- attr(raw, "n_total") %||% "?"
        n_success <- attr(raw, "n_success") %||% nrow(raw)
        pre_filtered <- isTRUE(attr(raw, "pre_filtered"))

        filter_msg <- if (pre_filtered) {
          sprintf(
            paste0("SSE: %s runs loaded (no minimization_successful column ",
                   "-- assuming pre-filtered, e.g. via PsN -out_filter)"),
            n_success
          )
        } else {
          sprintf("SSE: %s/%s valid runs (minimization_successful = 1)",
                  n_success, n_total)
        }

        ctl_source <- if (!is.null(shared_ctl_lines())) {
          " (from Home upload)"
        } else {
          ""
        }
      }

      div(class = "alert alert-success",
          style = "border-radius:8px; margin-bottom:10px; padding:8px 14px;",
        tags$strong(filter_msg),
        if (n_params > 0L) {
          tags$span(style = "margin-left:16px;",
            sprintf("| %d parameters%s", n_params, ctl_source))
        }
      )
    })

    # --- D-criterion card ---
    output$d_criterion_card <- renderUI({
      dc <- d_criterion()
      if (is.null(dc)) return(NULL)

      if (dc$ill_conditioned) {
        div(class = "alert alert-warning",
            style = "border-radius:8px; margin-bottom:10px; padding:8px 14px;",
          tags$strong("Empirical D-criterion: not estimable"),
          tags$p(style = "margin:4px 0 0; font-size:0.9em;",
            sprintf(
              paste0(
                "The empirical variance-covariance matrix is ill-conditioned ",
                "(rcond = %.2e, p = %d parameters). The determinant is ",
                "unreliable and the D-criterion cannot be computed. ",
                "This may indicate near-collinear or non-identifiable parameters ",
                "(Fayette et al. 2026)."
              ),
              dc$rcond, dc$p
            )
          )
        )
      } else if (!is.na(dc$d_criterion)) {
        div(class = "alert alert-success",
            style = "border-radius:8px; margin-bottom:10px; padding:8px 14px;",
          tags$strong(sprintf(
            "Empirical D-criterion: %.4g  (p = %d parameters, rcond = %.2e)",
            dc$d_criterion, dc$p, dc$rcond
          )),
          tags$p(style = "margin:4px 0 0; font-size:0.85em; color:#555;",
            HTML(paste0(
              "&phi;<sub>D</sub> = det(VarCov)<sup>1/p</sup> ",
              "&mdash; lower values indicate better estimation precision"
            ))
          )
        )
      }
    })

    # --- Scatter plot ---
    scatter_plot <- reactive({
      comp <- comparison_filtered()
      if (is.null(comp) || nrow(comp) == 0L) {
        return(ggplot() +
          labs(title = "Upload an SSE CSV and a .ctl to see the scatter plot") +
          .theme_design())
      }
      plot_fim_vs_sse(comp)
    })
    output$scatter <- renderPlot({ scatter_plot() }, res = 110)
    plot_export_server(input, output, session, "scatter_export", scatter_plot)

    # --- REE distribution (reactive) ---
    ree_dist <- reactive({
      parsed <- sse_parsed()
      req(parsed, true_vals())
      if (parsed$format == "summary") return(NULL)
      compute_ree_distribution(parsed$data, true_vals(), param_labels())
    })

    # --- REE distribution filtered by selected params ---
    ree_dist_filtered <- reactive({
      rd <- ree_dist()
      if (is.null(rd)) return(NULL)
      sel <- input$selected_params
      if (is.null(sel) || length(sel) == 0L) return(rd)
      list(
        individual = rd$individual |>
          dplyr::filter(param_label %in% sel | param %in% sel),
        summary = rd$summary |>
          dplyr::filter(param_label %in% sel | param %in% sel)
      )
    })

    # --- REE Boxplot ---
    ree_plot <- reactive({
      rd <- ree_dist_filtered()
      if (is.null(rd)) {
        return(ggplot() +
          labs(title = "REE boxplot requires raw_results CSV (not summary)") +
          .theme_design())
      }
      plot_ree_boxplot(rd)
    })
    output$ree_boxplot <- renderPlot({ ree_plot() }, res = 110)
    plot_export_server(input, output, session, "ree_export", ree_plot)

    # --- RSE Bar Chart ---
    rse_bar_plot <- reactive({
      comp <- comparison_filtered()
      if (is.null(comp) || nrow(comp) == 0L) {
        return(ggplot() +
          labs(title = "Upload SSE data to see RSE comparison") +
          .theme_design())
      }
      plot_rse_bar(comp)
    })
    output$rse_bar <- renderPlot({ rse_bar_plot() }, res = 110)
    plot_export_server(input, output, session, "rse_bar_export", rse_bar_plot)

    # --- Comparison table ---
    output$comp_table <- renderDT({
      comp <- comparison()
      req(comp)

      display <- comp |>
        dplyr::filter(status == "matched") |>
        dplyr::select(
          Parameter = param_label,
          Type = param_type,
          `RSE FIM (%)` = rse_fim,
          `RSE SSE (%)` = rse_sse,
          `RRMSE SSE (%)` = rmse_sse,
          `Rel. Bias (%)` = relative_bias,
          `Bias CI low` = rb_ci_lower,
          `Bias CI high` = rb_ci_upper,
          Ratio = ratio,
          `+/-20%` = pass_20pct
        ) |>
        dplyr::mutate(
          `RSE FIM (%)` = round(`RSE FIM (%)`, 2),
          Ratio = round(Ratio, 2),
          `+/-20%` = ifelse(`+/-20%`, "OK", "Out of band")
        )

      datatable(display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(
                  pageLength = 20, dom = "t",
                  scrollX = TRUE,
                  columnDefs = list(
                    list(className = "dt-center", targets = 8:9)
                  )
                )) |>
        formatStyle("+/-20%",
          backgroundColor = styleEqual(
            c("OK", "Out of band"),
            c("#d4edda", "#f8d7da")
          ))
    })

    # --- Export CSV ---
    output$export_csv <- downloadHandler(
      filename = function() {
        paste0("validation_fim_vs_sse_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        comp <- comparison()
        req(comp)
        export <- comp |>
          dplyr::select(param, param_type, status,
                        rse_fim, rse_sse, rmse_sse,
                        relative_bias, rb_ci_lower, rb_ci_upper,
                        ratio, pass_20pct)
        tryCatch(
          write.csv(export, file, row.names = FALSE),
          error = function(e) warning("CSV export failed: ", conditionMessage(e))
        )
      }
    )
    # --- Pre-parsed unfiltered SSE data (avoids double read in mod_sse_analysis) ---
    sse_all_raw <- reactive({
      req(input$sse_file)
      tryCatch(read_sse_raw_all(input$sse_file$datapath),
               error = function(e) NULL)
    })

    # --- Return shared reactives for mod_sse_analysis ---
    list(
      sse_file_path = reactive(input$sse_file$datapath),
      sse_all       = sse_all_raw,
      true_vals     = true_vals
    )
  })
}
