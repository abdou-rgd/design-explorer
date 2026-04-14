# =============================================================================
# mod_sse_validation.R — SSE Validation Tab
#
# Compares FIM-predicted RSE with empirical RSE from SSE (Stochastic Simulation
# and Estimation). Includes methodology panel with formulas and references.
# Inputs: sse_a_data, sse_b_data (shared reactives from mod_sse_upload)
#         + .ctl (true values) + .ext already loaded (FIM RSE)
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

    # --- Design selector (A/B) ---
    uiOutput(ns("design_selector")),

    # --- SSE + .ctl status banners ---
    fluidRow(
      column(6,
        uiOutput(ns("sse_status"))
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
          "Upload files in the ", tags$strong("SSE Upload"),
          " tab (Validation dropdown)."
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
                                      shared_true_vals = reactive(NULL),
                                      sse_a_data = reactive(NULL),
                                      sse_b_data = reactive(NULL),
                                      name_a = reactive("Design A"),
                                      name_b = reactive("Design B")) {
  moduleServer(id, function(input, output, session) {

    # --- Design selector (show only when B is loaded) ---
    output$design_selector <- renderUI({
      b <- sse_b_data()
      if (is.null(b)) return(NULL)

      choices <- c("a" = "a", "b" = "b")
      names(choices) <- c(name_a(), name_b())

      div(
        style = paste0(
          "border:1px solid #ddd; border-radius:8px; padding:8px 12px;",
          " margin-bottom:10px; background:#fafafa;"
        ),
        div(style = "display:flex; align-items:center; gap:12px;",
          tags$strong("Validate:", style = "white-space:nowrap;"),
          div(style = "margin-bottom:-15px;",
            radioButtons(session$ns("which_design"), label = NULL,
                         choices = choices, selected = "a", inline = TRUE)
          )
        )
      )
    })

    # --- Active SSE data (switches between A and B) ---
    sse_data <- reactive({
      sel <- input$which_design %||% "a"
      if (sel == "b") sse_b_data() else sse_a_data()
    })

    # --- Converged subset (for metrics that need filtered data) ---
    sse_converged <- reactive({
      dat <- sse_data()
      req(dat)
      dat[dat$converged, , drop = FALSE]
    })

    # --- SSE status banner ---
    output$sse_status <- renderUI({
      sse_status_banner(sse_data(), "SSE data")
    })

    # --- .ctl status indicator ---
    output$ctl_status <- renderUI({
      ctl_status_banner(shared_true_vals())
    })

    # --- True values from .ctl (Home tab) ---
    true_vals <- reactive({
      sv <- shared_true_vals()
      if (!is.null(sv) && length(sv) > 0L) return(sv)

      cl <- shared_ctl_lines()
      if (is.null(cl)) return(NULL)
      vals <- read_true_values(cl)
      if (length(vals) == 0L) return(NULL)
      vals
    })

    # --- Compute SSE metrics ---
    sse_metrics <- reactive({
      dat <- sse_converged()
      req(dat, true_vals())
      compute_sse_metrics(dat, true_vals(), param_labels())
    })

    # --- Empirical D-criterion ---
    d_criterion <- reactive({
      dat <- sse_converged()
      req(dat, true_vals())
      compute_empirical_d_criterion(dat, true_vals())
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
      dat <- sse_data()
      if (is.null(dat)) return(NULL)

      tv <- true_vals()
      n_params <- if (!is.null(tv)) length(tv) else 0L

      n_total <- attr(dat, "n_total") %||% nrow(dat)
      n_success <- attr(dat, "n_success") %||% sum(dat$converged)
      pre_filtered <- isTRUE(attr(dat, "pre_filtered"))

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
      dat <- sse_converged()
      req(dat, true_vals())
      compute_ree_distribution(dat, true_vals(), param_labels())
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
  })
}
