# =============================================================================
# mod_convergence.R — Onglet Convergence (+ multi-run overlay + step chart)
# =============================================================================

mod_convergence_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "plot-card",
      settings_bar(
        checkboxInput(ns("log_conv"), "Log X axis", FALSE),
        uiOutput(ns("toggle_ui"))
      ),
      p(class = "section-title", "Optimality criterion convergence (OFV)"),
      plotOutput(ns("plot"), height = "420px"),
      plot_export_ui(ns, "conv_export", default_fname = "convergence_plot")
    )
  )
}

mod_convergence_server <- function(id, ext_data,
                                   all_runs = reactive(list()),
                                   ctl_lines = reactive(NULL),
                                   cpu_secs = reactive(NA_real_)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Log X axis is now internal
    log_conv <- reactive({ isTRUE(input$log_conv) })

    # Detect whether step chart is available:
    # single-run, 2-5 table_nos (multi-step optimization, not robust)
    show_toggle <- reactive({
      runs <- all_runs()
      if (length(runs) > 1L) return(FALSE)
      ext <- ext_data()
      if (is.null(ext)) return(FALSE)
      n_blocs <- dplyr::n_distinct(ext$table_no)
      n_blocs >= 2L && n_blocs <= 5L
    })

    output$toggle_ui <- renderUI({
      if (!show_toggle()) return(NULL)
      div(style = "text-align: right; margin-bottom: 6px;",
        radioButtons(ns("conv_view"), NULL,
          choices = c("Detail" = "detail", "Resume" = "summary"),
          selected = "detail", inline = TRUE)
      )
    })

    # Current view mode (default = detail)
    view_mode <- reactive({
      if (!show_toggle()) return("detail")
      input$conv_view %||% "detail"
    })

    conv_plot <- reactive({
      runs <- all_runs()

      if (length(runs) <= 1) {
        ext <- ext_data()
        if (is.null(ext)) return(NULL)

        n_blocs <- dplyr::n_distinct(ext$table_no)

        if (n_blocs > 5L) {
          finals <- ext |>
            dplyr::filter(type == "final") |>
            dplyr::select(table_no, OBJ) |>
            dplyr::filter(!is.na(OBJ))

          if (nrow(finals) == 0L) {
            return(ggplot() +
              labs(title = paste0("Robust design (", n_blocs,
                                  " subproblems) -- no final OFV data")) +
              .theme_design())
          }

          med_val <- median(finals$OBJ)
          return(
            ggplot(finals, aes(x = OBJ)) +
              geom_histogram(bins = 40, fill = "#2563eb", alpha = 0.75,
                             color = "white", size = 0.2) +
              geom_vline(xintercept = med_val, linetype = "dashed",
                         color = "#dc2626", size = 0.8) +
              annotate("text", x = med_val, y = Inf, vjust = 1.5, hjust = -0.1,
                       label = sprintf("median = %.3f", med_val),
                       color = "#dc2626", size = 3.5) +
              labs(
                title = paste0("D-optimality criterion distribution",
                               " (N = ", nrow(finals), " subproblems)"),
                subtitle = "Robust design: OFV evaluated under each prior realization",
                x = "OFV  (-log det FIM)",
                y = "Number of subproblems",
                caption = "Dashed line = median | Spread = criterion sensitivity to prior uncertainty"
              ) +
              .theme_design()
          )
        }

        if (view_mode() == "summary" && n_blocs >= 2L) {
          method_labels <- parse_design_methods(ctl_lines())
          steps <- build_convergence_steps(ext, cpu_secs(), method_labels)
          return(plot_convergence_steps(steps))
        }

        return(plot_convergence(ext, log_iter = log_conv()))
      }

      run_labels <- setNames(vapply(runs, function(r) r$name %||% "?", character(1L)),
                             names(runs))
      run_colors <- setNames(vapply(names(runs), run_color, character(1L)),
                             names(runs))

      any_robust <- any(vapply(runs, function(r) {
        !is.null(r$ext_data) &&
        "table_no" %in% names(r$ext_data) &&
        dplyr::n_distinct(r$ext_data$table_no) > 1L
      }, logical(1L)))

      if (any_robust) {
        all_finals <- purrr::imap(runs, function(r, rid) {
          if (is.null(r$ext_data)) return(NULL)
          is_rob <- "table_no" %in% names(r$ext_data) &&
                    dplyr::n_distinct(r$ext_data$table_no) > 1L
          finals <- dplyr::filter(r$ext_data, type == "final", !is.na(OBJ))
          if (nrow(finals) == 0L) return(NULL)
          data.frame(OBJ = finals$OBJ, run = rid, robust = is_rob,
                     stringsAsFactors = FALSE)
        }) |> dplyr::bind_rows()

        if (nrow(all_finals) == 0L) {
          return(ggplot() + labs(title = "No final OFV data") + .theme_design())
        }

        rob_data    <- all_finals[all_finals$robust,  , drop = FALSE]
        nonrob_data <- all_finals[!all_finals$robust, , drop = FALSE]

        p <- ggplot()
        if (nrow(rob_data) > 0L) {
          p <- p + geom_density(data = rob_data,
                                aes(x = OBJ, fill = run, color = run),
                                alpha = 0.4, size = 0.7)
        }
        if (nrow(nonrob_data) > 0L) {
          vlines <- nonrob_data |>
            dplyr::group_by(run) |>
            dplyr::summarise(val = median(OBJ), .groups = "drop")
          p <- p + geom_vline(data = vlines,
                              aes(xintercept = val, color = run),
                              linetype = "solid", size = 1.1)
        }

        caption_txt <- "Curve = OFV distribution of robust subproblems"
        if (nrow(nonrob_data) > 0L)
          caption_txt <- paste0(caption_txt,
                                " | Vertical line = final OFV of non-robust run")

        return(
          p +
            scale_fill_manual(values  = run_colors, labels = run_labels, name = NULL) +
            scale_color_manual(values = run_colors, labels = run_labels, name = NULL) +
            labs(title   = "Final OFV distribution -- Multi-run comparison",
                 x       = "OFV (-log det FIM)",
                 y       = "Density",
                 caption = caption_txt) +
            .theme_design()
        )
      }

      combined <- purrr::imap(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        r$ext_data |>
          dplyr::filter(type == "iteration") |>
          dplyr::select(table_no, ITERATION, OBJ) |>
          dplyr::filter(!is.na(OBJ), !is.na(ITERATION)) |>
          dplyr::mutate(run = idx)
      }) |> dplyr::bind_rows()

      if (nrow(combined) == 0) return(ggplot() + labs(title = "No convergence data") + .theme_design())

      p <- ggplot(combined, aes(x = ITERATION, y = OBJ, color = run)) +
        geom_line(size = 0.75, alpha = 0.9) +
        scale_color_manual(values = run_colors, labels = run_labels,
                           name = NULL) +
        labs(title = "Convergence -- Multi-run comparison",
             x = "Iteration ($DESIGN)", y = "OFV (-log det FIM)",
             caption = "Source: .ext") +
        .theme_design()

      if (log_conv()) p <- p + scale_x_log10()
      p
    })

    output$plot <- renderPlot({ conv_plot() }, res = 110)
    plot_export_server(input, output, session, "conv_export", conv_plot)
  })
}
