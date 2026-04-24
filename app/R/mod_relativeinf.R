# =============================================================================
# mod_relativeinf.R — Onglet RELATIVEINF (+ multi-run)
# =============================================================================

mod_relativeinf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "plot-card",
      p(class = "section-title",
        "Relative information of design per ETA",
        tags$small(style = "color:#6b7280; font-weight:400; font-size:.8rem; margin-left:8px;",
                   "(TYPE 11 from .shk file)")
      ),
      plotOutput(ns("plot"), height = "380px"),
      plot_export_ui(ns, "ri_export", default_fname = "relativeinf_plot")
    )
  )
}

mod_relativeinf_server <- function(id, shk_data, tbl_no, param_labels, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {

    ri_plot <- reactive({
      runs <- all_runs()

      if (length(runs) <= 1) {
        shk <- shk_data()
        if (is.null(shk)) {
          return(ggplot() + labs(title = "Load a .shk file to display RELATIVEINF(%)") + theme_bw())
        }
        return(plot_relativeinf(shk, table_no = tbl_no(), param_labels = NULL))
      }

      combined <- purrr::imap(runs, function(r, rid) {
        if (is.null(r$shk_data)) return(NULL)
        ri <- get_relativeinf(r$shk_data, tbl_no())
        if (nrow(ri) == 0) return(NULL)
        ri |> mutate(run = rid)
      }) |> dplyr::bind_rows()

      if (nrow(combined) == 0) return(ggplot() + labs(title = "No RELATIVEINF data") + theme_bw())

      run_labels <- setNames(vapply(runs, function(r) r$name %||% "?", character(1L)),
                             names(runs))
      run_colors <- setNames(vapply(names(runs), run_color, character(1L)),
                             names(runs))
      primary_run_rid <- names(runs)[1]
      eta_order <- combined |>
        dplyr::filter(run == primary_run_rid) |>
        dplyr::arrange(relativeinf_pct) |>
        dplyr::pull(eta)
      combined <- combined |> dplyr::mutate(eta = factor(eta, levels = eta_order))

      ggplot(combined, aes(x = eta, y = relativeinf_pct, fill = run)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65,
                 color = "white", size = 0.3) +
        geom_hline(yintercept = c(20, 50), linetype = "dashed", color = "grey40", size = 0.45) +
        scale_fill_manual(values = run_colors, labels = run_labels,
                          name = NULL) +
        coord_flip() +
        labs(title = "RELATIVEINF (%) — Multi-run comparison", x = NULL, y = "RELATIVEINF (%)") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank())
    })

    output$plot <- renderPlot({ ri_plot() }, res = 110)
    plot_export_server(input, output, session, "ri_export", ri_plot)
  })
}
