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

      run_labels <- setNames(vapply(runs, function(r) r$name %||% "?", character(1L)),
                             names(runs))
      run_colors <- setNames(vapply(names(runs), run_color, character(1L)),
                             names(runs))

      # Detecter si au moins un run est un design robuste
      any_robust <- any(vapply(runs, function(r) {
        !is.null(r$ext_data) &&
        "table_no" %in% names(r$ext_data) &&
        dplyr::n_distinct(r$ext_data$table_no) > 1L
      }, logical(1L)))

      if (any_robust) {
        # Density pour les runs robustes, vline pour les non-robustes
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
          return(ggplot() + labs(title = "Pas de donnees OFV final") + .theme_design())
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

        caption_txt <- "Courbe = distribution OFV des sous-problemes robustes"
        if (nrow(nonrob_data) > 0L)
          caption_txt <- paste0(caption_txt,
                                " | Ligne verticale = OFV final du run non-robuste")

        return(
          p +
            scale_fill_manual(values  = run_colors, labels = run_labels, name = NULL) +
            scale_color_manual(values = run_colors, labels = run_labels, name = NULL) +
            labs(title   = "Distribution OFV final -- Comparaison multi-runs",
                 x       = "OFV (-log det FIM)",
                 y       = "Densite",
                 caption = caption_txt) +
            .theme_design()
        )
      }

      # Tous les runs sont non-robustes : overlay de courbes de convergence
      combined <- purrr::imap(runs, function(r, idx) {
        if (is.null(r$ext_data)) return(NULL)
        r$ext_data |>
          dplyr::filter(type == "iteration") |>
          dplyr::select(table_no, ITERATION, OBJ) |>
          dplyr::filter(!is.na(OBJ), !is.na(ITERATION)) |>
          dplyr::mutate(run = idx)
      }) |> dplyr::bind_rows()

      if (nrow(combined) == 0) return(ggplot() + labs(title = "Pas de convergence") + .theme_design())

      p <- ggplot(combined, aes(x = ITERATION, y = OBJ, color = run)) +
        geom_line(size = 0.75, alpha = 0.9) +
        scale_color_manual(values = run_colors, labels = run_labels,
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
