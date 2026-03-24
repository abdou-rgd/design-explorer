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

        # Robust design : beaucoup de sous-problèmes → histogramme OFV finaux
        n_blocs <- dplyr::n_distinct(ext$table_no)
        if (n_blocs > 5L) {
          finals <- ext |>
            dplyr::filter(type == "final") |>
            dplyr::select(table_no, OBJ) |>
            dplyr::filter(!is.na(OBJ))

          if (nrow(finals) == 0L) {
            return(ggplot() +
              labs(title = paste0("Design robuste (", n_blocs,
                                  " sous-problemes) — pas de donnees OFV final")) +
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
                       label = sprintf("mediane = %.3f", med_val),
                       color = "#dc2626", size = 3.5) +
              labs(
                title = paste0("Distribution du critere D-optimalite",
                               " (N = ", nrow(finals), " sous-problemes)"),
                subtitle = "Design robuste : OFV evalue sous chaque realisation du prior",
                x = "OFV  (-log det FIM)",
                y = "Nombre de sous-problemes",
                caption = paste0(
                  "Ligne tiretee = mediane | ",
                  "Dispersion = sensibilite du critere a l'incertitude du prior"
                )
              ) +
              .theme_design()
          )
        }

        return(plot_convergence(ext, log_iter = log_conv()))
      }

      # Multi-run: overlay convergence curves
      combined <- purrr::imap(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        r$ext_data |>
          filter(type == "iteration") |>
          select(table_no, ITERATION, OBJ) |>
          filter(!is.na(OBJ), !is.na(ITERATION)) |>
          mutate(run = idx)
      }) |> dplyr::bind_rows()

      if (nrow(combined) == 0) return(ggplot() + labs(title = "Pas de convergence") + .theme_design())

      run_labels <- setNames(vapply(runs, function(r) r$name, character(1L)),
                             names(runs))
      p <- ggplot(combined, aes(x = ITERATION, y = OBJ, color = run)) +
        geom_line(size = 0.75, alpha = 0.9) +
        scale_color_manual(values = .RUN_COLORS, labels = run_labels,
                           name = NULL) +
        labs(title = "Convergence -- Comparaison multi-runs",
             x = "Iteration ($DESIGN)", y = "OFV (-log det FIM)",
             caption = "Source : .ext") +
        .theme_design()

      if (log_conv()) p <- p + scale_x_log10()
      p
    }, res = 110)
  })
}
