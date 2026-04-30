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
  page_shell(
    page_header(
      "SSE Comparison",
      "Compare Design A and Design B SSE results side by side using empirical precision and distribution diagnostics.",
      eyebrow = "Validation"
    ),
    fluidRow(
      column(4, uiOutput(ns("sse_a_status"))),
      column(4, uiOutput(ns("sse_b_status"))),
      column(4, uiOutput(ns("ctl_status")))
    ),
    uiOutput(ns("status_banner")),
    uiOutput(ns("run_health_banner")),
    status_panel(
      "Comparison workflow",
      tags$p("Upload Design A and Design B in SSE Upload. True values come from the loaded control stream."),
      doc_link("sse-comparison", "Open SSE comparison documentation"),
      tone = "info",
      icon_name = "book-open"
    ),
    page_section(
      "Comparison workspace",
      subtitle = "Choose one active view; filters and exports stay tied to that view.",
      control_panel(
        checkboxInput(ns("show_failed"), "Include failed runs in plots",
                      value = FALSE),
        radioButtons(
          ns("active_view"), "Active view",
          choices = c(
            "RSE comparison plot" = "rse",
            "Distribution overlay" = "distributions",
            "RSE comparison table" = "table"
          ),
          selected = "rse",
          inline = TRUE
        ),
        uiOutput(ns("param_filter_ui"))
      ),
      uiOutput(ns("active_view_ui"))
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
      resolve_true_values(shared_true_vals, shared_ctl_lines)
    })

    # --- Status banner ---
    output$status_banner <- renderUI({
      orig <- sse_all_orig()
      opti <- sse_all_opti()
      tv   <- true_vals()

      if (is.null(orig) && is.null(opti)) {
        return(status_panel(
          "Upload SSE CSV files",
          tags$p("Upload Design A and Design B in the SSE Upload tab to begin."),
          tone = "warning",
          icon_name = "exclamation-triangle"
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

      status_panel(
        "Comparison readiness",
        tags$p(paste(parts, collapse = " | ")),
        if (length(missing) > 0L) {
          tags$p(class = "status-panel__muted",
            paste("Still needed:", paste(missing, collapse = ", ")))
        },
        tone = if (length(missing) == 0L) "success" else "info",
        icon_name = "clipboard-check"
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

      status_panel(
        "Run Health Comparison",
        make_health_pills(ho, name_orig()),
        make_health_pills(hp, name_opti()),
        tone = "neutral",
        icon_name = "heartbeat"
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

      checkboxGroupInput(
        session$ns("selected_params"), label = "Parameters",
        choices = all_params, selected = default_sel,
        inline = TRUE
      )
    })

    output$active_view_ui <- renderUI({
      active <- input$active_view %||% "rse"
      if (active == "distributions") {
        return(plot_panel(
          "Parameter distributions overlay",
          plotOutput(session$ns("dist_plot"), height = "600px"),
          plot_export_ui(session$ns, "dist_export",
                         default_fname = "sse_distributions_overlay")
        ))
      }
      if (active == "table") {
        return(table_panel(
          "RSE comparison table",
          DTOutput(session$ns("rse_table")),
          actions = downloadButton(session$ns("export_csv"), "Export CSV",
                                   class = "btn-sm btn-default")
        ))
      }
      plot_panel(
        "Empirical RSE comparison",
        plotOutput(session$ns("rse_plot"), height = "500px"),
        plot_export_ui(session$ns, "rse_export",
                       default_fname = "sse_rse_comparison")
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
      if (is.null(sse_all_orig()) || is.null(sse_all_opti()) ||
          is.null(true_vals())) {
        return(.empty_plot(
          "Upload Design B SSE (and a .ctl) to see the comparison"))
      }
      rc <- tryCatch(rse_comp_filtered(), error = function(e) NULL)
      if (is.null(rc) || nrow(rc) == 0L) {
        return(.empty_plot("No parameters selected"))
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

  })
}
