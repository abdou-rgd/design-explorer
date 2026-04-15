# =============================================================================
# mod_sse_comparison.R — SSE Comparison Tab (original vs optimized design)
#
# Compares two SSE results side by side: run health, RSE, distributions,
# empirical correlations. Consumes shared SSE data from mod_sse_upload.
#
# Inputs: sse_orig, sse_opti (shared reactives), shared_ctl_lines, param_labels
# =============================================================================

mod_sse_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Info banner ---
    div(class = "alert alert-info", style = "border-radius:10px; margin-bottom:12px;",
      tags$strong("Two-SSE Comparison"),
      tags$p(style = "margin:6px 0 0; font-size:0.9em;",
        "Compare an original design SSE with an optimized design SSE. ",
        "Upload two PsN raw_results CSVs in the SSE Upload tab. ",
        "True parameter values are extracted from the .ctl loaded in the Home tab."
      )
    ),

    # --- SSE + .ctl status banners ---
    fluidRow(
      column(4, uiOutput(ns("sse_a_status"))),
      column(4, uiOutput(ns("sse_b_status"))),
      column(4, uiOutput(ns("ctl_status")))
    ),

    # --- Status banner ---
    uiOutput(ns("status_banner")),

    # --- Run health side-by-side ---
    uiOutput(ns("run_health_banner")),

    # --- Controls ---
    fluidRow(
      column(4,
        checkboxInput(ns("show_failed"), "Include failed runs in plots",
                      value = FALSE)
      ),
      column(8,
        div(style = paste0(
          "border:1px solid #ddd; border-radius:8px; padding:8px 12px;",
          " background:#fafafa;"
        ),
          div(style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
            tags$strong("Sections:", style = "white-space:nowrap;"),
            checkboxGroupInput(
              ns("visible_sections"), label = NULL,
              choices = c("RSE Comparison" = "rse",
                          "Distributions" = "distributions",
                          "Correlations" = "correlations"),
              selected = c("rse", "distributions", "correlations"),
              inline = TRUE
            )
          )
        )
      )
    ),

    # --- Parameter filter ---
    fluidRow(column(12, uiOutput(ns("param_filter_ui")))),

    # --- RSE Comparison ---
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('rse') > -1", ns("visible_sections")
      ),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "Empirical RSE Comparison"),
            plotOutput(ns("rse_plot"), height = "500px"),
            plot_export_ui(ns, "rse_export",
                           default_fname = "sse_rse_comparison")
          )
        )
      ),
      br(),
      fluidRow(
        column(12,
          div(class = "param-table-wrap",
            div(style = paste0(
              "display:flex; justify-content:space-between;",
              " align-items:center;"
            ),
              p(class = "section-title", style = "margin:0;",
                "RSE Comparison Table"),
              downloadButton(ns("export_csv"), "Export CSV",
                             class = "btn-sm btn-default")
            ),
            DTOutput(ns("rse_table"))
          )
        )
      ),
      br()
    ),

    # --- Distribution overlay ---
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('distributions') > -1", ns("visible_sections")
      ),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "Parameter Distributions Overlay"),
            plotOutput(ns("dist_plot"), height = "600px"),
            plot_export_ui(ns, "dist_export",
                           default_fname = "sse_distributions_overlay")
          )
        )
      ),
      br()
    ),

    # --- Correlation delta ---
    conditionalPanel(
      condition = sprintf(
        "input['%s'].indexOf('correlations') > -1", ns("visible_sections")
      ),
      fluidRow(
        column(12,
          div(class = "plot-card",
            p(class = "section-title", "Correlation Change (Optimized - Original)"),
            plotOutput(ns("cor_delta_plot"), height = "500px"),
            plot_export_ui(ns, "cor_export",
                           default_fname = "sse_correlation_delta")
          )
        )
      )
    )
  )
}


mod_sse_comparison_server <- function(id,
                                      sse_orig = reactive(NULL),
                                      sse_opti = reactive(NULL),
                                      name_orig = reactive("Original"),
                                      name_opti = reactive("Optimized"),
                                      shared_ctl_lines = reactive(NULL),
                                      shared_true_vals = reactive(NULL),
                                      param_labels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # --- SSE data (from centralized upload) ---
    sse_all_orig <- reactive({ sse_orig() })
    sse_all_opti <- reactive({ sse_opti() })

    # --- Status banners ---
    output$sse_a_status <- renderUI({
      sse_status_banner(sse_all_orig(), sprintf("Design A (%s)", name_orig()))
    })
    output$sse_b_status <- renderUI({
      sse_status_banner(sse_all_opti(), sprintf("Design B (%s)", name_opti()))
    })
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

    # --- Status banner ---
    output$status_banner <- renderUI({
      orig <- sse_all_orig()
      opti <- sse_all_opti()
      tv   <- true_vals()

      if (is.null(orig) && is.null(opti)) {
        return(div(class = "alert alert-warning",
                   style = "border-radius:8px; margin-bottom:10px;",
          tags$strong("Upload two SSE CSV files in the SSE Upload tab to begin.")
        ))
      }

      parts <- c()
      if (!is.null(orig)) {
        n_t <- attr(orig, "n_total") %||% nrow(orig)
        n_s <- attr(orig, "n_success") %||% sum(orig$converged)
        parts <- c(parts, sprintf("%s: %d/%d runs OK", name_orig(), n_s, n_t))
      }
      if (!is.null(opti)) {
        n_t <- attr(opti, "n_total") %||% nrow(opti)
        n_s <- attr(opti, "n_success") %||% sum(opti$converged)
        parts <- c(parts, sprintf("%s: %d/%d runs OK", name_opti(), n_s, n_t))
      }
      if (!is.null(tv)) {
        parts <- c(parts, sprintf("%d parameters", length(tv)))
      }

      missing <- c()
      if (is.null(orig)) missing <- c(missing, "Design A SSE")
      if (is.null(opti)) missing <- c(missing, "Design B SSE")
      if (is.null(tv))   missing <- c(missing, ".ctl for true values")

      cls <- if (length(missing) == 0L) "alert-success" else "alert-info"

      div(class = paste("alert", cls),
          style = "border-radius:8px; margin-bottom:10px; padding:8px 14px;",
        tags$strong(paste(parts, collapse = " | ")),
        if (length(missing) > 0L) {
          tags$span(style = "margin-left:12px; color:#666;",
            paste("Still needed:", paste(missing, collapse = ", ")))
        }
      )
    })

    # --- Run health ---
    health_orig <- reactive({
      dat <- sse_all_orig()
      req(dat)
      compute_run_health(dat)
    })
    health_opti <- reactive({
      dat <- sse_all_opti()
      req(dat)
      compute_run_health(dat)
    })

    output$run_health_banner <- renderUI({
      ho <- tryCatch(health_orig(), error = function(e) NULL)
      hp <- tryCatch(health_opti(), error = function(e) NULL)
      if (is.null(ho) && is.null(hp)) return(NULL)

      div(class = "alert",
          style = paste0(
            "border-radius:10px; margin-bottom:12px; padding:10px 14px;",
            " background:#f8fafc; border:1px solid #e2e8f0;"
          ),
        tags$strong("Run Health Comparison", style = "font-size:1em;"),
        div(style = "margin-top:6px;",
          make_health_pills(ho, name_orig()),
          make_health_pills(hp, name_opti())
        )
      )
    })

    # --- SSE metrics (converged runs only) ---
    metrics_orig <- reactive({
      dat <- sse_all_orig()
      tv  <- true_vals()
      req(dat, tv)
      converged <- dat[dat$converged, , drop = FALSE]
      compute_sse_metrics(converged, tv, param_labels())
    })

    metrics_opti <- reactive({
      dat <- sse_all_opti()
      tv  <- true_vals()
      req(dat, tv)
      converged <- dat[dat$converged, , drop = FALSE]
      compute_sse_metrics(converged, tv, param_labels())
    })

    # --- RSE comparison ---
    rse_comp <- reactive({
      req(metrics_orig(), metrics_opti())
      compare_rse(metrics_orig(), metrics_opti())
    })

    # --- Parameter filter ---
    output$param_filter_ui <- renderUI({
      rc <- tryCatch(rse_comp(), error = function(e) NULL)
      if (is.null(rc) || nrow(rc) == 0L) return(NULL)

      all_params <- rc$param_label
      # Pre-exclude params with RSE > 100% on either side
      rse_max <- pmax(rc$rse_orig, rc$rse_opti, na.rm = TRUE)
      default_sel <- all_params[is.na(rse_max) | rse_max <= 100]
      if (length(default_sel) == 0L) default_sel <- all_params

      div(style = paste0(
        "border:1px solid #ddd; border-radius:8px; padding:8px 12px;",
        " margin-bottom:10px; background:#fafafa;"
      ),
        div(style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
          tags$strong("Parameters:", style = "white-space:nowrap;"),
          checkboxGroupInput(
            session$ns("selected_params"), label = NULL,
            choices = all_params, selected = default_sel,
            inline = TRUE
          )
        )
      )
    })

    # --- Filtered RSE comparison ---
    rse_comp_filtered <- reactive({
      rc <- rse_comp()
      if (is.null(rc)) return(NULL)
      sel <- input$selected_params
      if (is.null(sel) || length(sel) == 0L) return(rc)
      rc[rc$param_label %in% sel, ]
    })

    # --- RSE plot ---
    rse_plot_fn <- reactive({
      rc <- rse_comp_filtered()
      if (is.null(rc) || nrow(rc) == 0L) {
        return(ggplot() +
          labs(title = "Upload two SSE CSVs and a .ctl") +
          .theme_design())
      }
      plot_rse_comparison(rc, name_orig = name_orig(), name_opti = name_opti())
    })
    output$rse_plot <- renderPlot({ rse_plot_fn() }, res = 110)
    plot_export_server(input, output, session, "rse_export", rse_plot_fn)

    # --- RSE table ---
    output$rse_table <- renderDT({
      rc <- rse_comp()
      req(rc)

      display <- data.frame(
        Parameter    = rc$param_label,
        Type         = rc$param_type,
        `RSE Orig`   = rc$rse_orig,
        `RSE Opti`   = rc$rse_opti,
        Delta        = round(rc$delta_rse, 2),
        `Change`     = ifelse(is.na(rc$pct_change), "\u2014",
                              paste0(ifelse(rc$pct_change > 0, "+", ""),
                                     rc$pct_change, "%")),
        `RRMSE Orig` = rc$rrmse_orig,
        `RRMSE Opti` = rc$rrmse_opti,
        `Bias Orig`  = rc$bias_orig,
        `Bias Opti`  = rc$bias_opti,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      datatable(display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(
                  pageLength = 20, dom = "t",
                  scrollX = TRUE,
                  order = list(list(4, "asc"))
                )) |>
        formatStyle("Delta",
          color = styleInterval(c(-0.01, 0.01),
                                c("#15803d", "#666", "#dc2626")),
          fontWeight = "bold") |>
        formatStyle("RSE Orig",
          backgroundColor = styleInterval(
            c(20, 50), c("#dcfce7", "#fef3c7", "#fee2e2")
          )) |>
        formatStyle("RSE Opti",
          backgroundColor = styleInterval(
            c(20, 50), c("#dcfce7", "#fef3c7", "#fee2e2")
          ))
    })

    # --- Export CSV ---
    output$export_csv <- downloadHandler(
      filename = function() {
        paste0("sse_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        rc <- rse_comp()
        req(rc)
        tryCatch(
          write.csv(rc, file, row.names = FALSE),
          error = function(e) warning("CSV export failed: ", conditionMessage(e))
        )
      }
    )

    # --- Distribution overlay ---
    dist_orig <- reactive({
      dat <- sse_all_orig()
      tv  <- true_vals()
      req(dat, tv)
      compute_param_distributions(dat, tv, param_labels())
    })
    dist_opti <- reactive({
      dat <- sse_all_opti()
      tv  <- true_vals()
      req(dat, tv)
      compute_param_distributions(dat, tv, param_labels())
    })

    # Filtered distributions
    dist_orig_filtered <- reactive({
      dd <- dist_orig()
      if (is.null(dd)) return(NULL)
      sel <- input$selected_params
      if (is.null(sel) || length(sel) == 0L) return(dd)
      dd[dd$param_label %in% sel, ]
    })
    dist_opti_filtered <- reactive({
      dd <- dist_opti()
      if (is.null(dd)) return(NULL)
      sel <- input$selected_params
      if (is.null(sel) || length(sel) == 0L) return(dd)
      dd[dd$param_label %in% sel, ]
    })

    dist_plot_fn <- reactive({
      d1 <- dist_orig_filtered()
      d2 <- dist_opti_filtered()
      if (is.null(d1) || is.null(d2)) {
        return(ggplot() +
          labs(title = "Upload two SSE CSVs and a .ctl") +
          .theme_design())
      }
      plot_distribution_overlay(d1, d2,
        name_orig = name_orig(), name_opti = name_opti(),
        show_failed = isTRUE(input$show_failed))
    })
    output$dist_plot <- renderPlot({ dist_plot_fn() }, res = 110)
    plot_export_server(input, output, session, "dist_export", dist_plot_fn)

    # --- Correlation delta ---
    cor_orig <- reactive({
      dat <- sse_all_orig()
      tv  <- true_vals()
      req(dat, tv)
      compute_empirical_correlations(
        dat, tv, only_converged = !isTRUE(input$show_failed),
        param_labels = param_labels())
    })
    cor_opti <- reactive({
      dat <- sse_all_opti()
      tv  <- true_vals()
      req(dat, tv)
      compute_empirical_correlations(
        dat, tv, only_converged = !isTRUE(input$show_failed),
        param_labels = param_labels())
    })

    cor_delta_fn <- reactive({
      c1 <- cor_orig()
      c2 <- cor_opti()
      if (is.null(c1) || is.null(c2)) {
        return(ggplot() +
          labs(title = "Not enough data for correlation comparison") +
          .theme_design())
      }

      # Intersect parameters (same model, but guard against edge cases)
      common <- intersect(colnames(c1), colnames(c2))
      if (length(common) < 2L) {
        return(ggplot() +
          labs(title = "Not enough shared parameters") +
          .theme_design())
      }
      m1 <- c1[common, common]
      m2 <- c2[common, common]
      delta <- m2 - m1

      # Long format
      df <- expand.grid(Var1 = common, Var2 = common,
                        stringsAsFactors = FALSE)
      df$value <- as.vector(delta)
      df$Var1 <- factor(df$Var1, levels = common)
      df$Var2 <- factor(df$Var2, levels = rev(common))

      ggplot(df, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = ifelse(abs(value) > 0.1,
                                     sprintf("%+.2f", value), "")),
                  size = 2.8, color = "black") +
        scale_fill_gradient2(low = "#2563eb", mid = "white", high = "#dc2626",
                             midpoint = 0, limits = c(-1, 1),
                             name = "Delta r") +
        labs(title = "Correlation Change (Optimized - Original)",
             subtitle = "Blue = decreased | Red = increased | Values for |delta| > 0.1",
             x = NULL, y = NULL) +
        .theme_design() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey50"),
          legend.position = "right"
        )
    })
    output$cor_delta_plot <- renderPlot({ cor_delta_fn() }, res = 110)
    plot_export_server(input, output, session, "cor_export", cor_delta_fn)
  })
}
