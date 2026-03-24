# =============================================================================
# mod_rse.R — Onglet RSE / SE (avec toggle + multi-run + waterfall)
# =============================================================================

mod_rse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "plot-card",
      fluidRow(
        column(9, p(class = "section-title", "RSE / SE predits par la FIM -- par parametre")),
        column(3, radioButtons(ns("plot_style"), NULL,
                               choices = c("Barplot" = "bar", "Waterfall" = "waterfall"),
                               selected = "bar", inline = TRUE))
      ),
      plotOutput(ns("plot"), height = "420px")
    )
  )
}

mod_rse_server <- function(id, ext_data, tbl_no, param_labels, se_mode, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      ext <- ext_data()
      if (is.null(ext)) return(NULL)
      runs <- all_runs()
      mode <- se_mode()

      # If only primary run, use existing plots
      if (length(runs) <= 1) {
        plot_style <- input$plot_style %||% "bar"
        if (plot_style == "waterfall" && (is.null(mode) || mode != "SE absolues")) {
          return(plot_rse_waterfall(ext, table_no = tbl_no(), param_labels = param_labels()))
        }
        if (!is.null(mode) && mode == "SE absolues") {
          return(plot_se(ext, table_no = tbl_no(), param_labels = param_labels()))
        } else {
          return(plot_rse(ext, table_no = tbl_no(), param_labels = param_labels()))
        }
      }

      # Multi-run: build combined data
      show_se <- (!is.null(mode) && mode == "SE absolues")
      combined <- purrr::imap(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        rse <- get_rse(r$ext_data, tbl_no())
        if (nrow(rse) == 0) return(NULL)
        lbls <- param_labels()
        if (!is.null(lbls)) {
          rse <- rse |> mutate(param = ifelse(param %in% names(lbls), lbls[param], param))
        }
        rse |> mutate(run = idx)
      }) |> dplyr::bind_rows()

      if (nrow(combined) == 0) return(NULL)

      run_labels <- setNames(vapply(runs, function(r) r$name, character(1L)),
                             names(runs))
      y_var <- if (show_se) "se" else "rse_pct"
      y_lab <- if (show_se) "SE" else "RSE (%)"

      ggplot(combined, aes(x = param, y = .data[[y_var]], fill = run)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65,
                 color = "white", size = 0.3) +
        {if (!show_se) geom_hline(yintercept = c(20, 50), linetype = "dashed",
                                   color = "grey40", size = 0.45)} +
        scale_fill_manual(values = .RUN_COLORS, labels = run_labels,
                          name = NULL) +
        labs(title = paste(y_lab, "-- Comparaison multi-runs"), x = NULL, y = y_lab) +
        .theme_design() +
        theme(panel.grid.major.x = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 1, size = 9))
    }, res = 110)
  })
}
