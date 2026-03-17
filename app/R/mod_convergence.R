# =============================================================================
# mod_convergence.R — Onglet Convergence (+ multi-run overlay)
# =============================================================================

mod_convergence_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "plot-card",
      p(class = "section-title", "Evolution du critere d'optimalite (OFV) par iteration"),
      plotOutput(ns("plot"), height = "420px")
    )
  )
}

mod_convergence_server <- function(id, ext_data, log_conv, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      runs <- all_runs()

      if (length(runs) <= 1) {
        ext <- ext_data()
        if (is.null(ext)) return(NULL)
        return(plot_convergence(ext, log_iter = log_conv()))
      }

      # Multi-run: overlay convergence curves
      combined <- purrr::imap(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        r$ext_data |>
          filter(type == "iteration") |>
          select(table_no, ITERATION, OBJ) |>
          filter(!is.na(OBJ), !is.na(ITERATION)) |>
          mutate(run = r$name)
      }) |> purrr::list_rbind()

      if (nrow(combined) == 0) return(ggplot() + labs(title = "Pas de convergence") + .theme_design())

      p <- ggplot(combined, aes(x = ITERATION, y = OBJ, color = run)) +
        geom_line(linewidth = 0.75, alpha = 0.9) +
        scale_color_manual(values = .RUN_COLORS, name = NULL) +
        labs(title = "Convergence -- Comparaison multi-runs",
             x = "Iteration ($DESIGN)", y = "OFV (-log det FIM)",
             caption = "Source : .ext") +
        .theme_design()

      if (log_conv()) p <- p + scale_x_log10()
      p
    }, res = 110)
  })
}
