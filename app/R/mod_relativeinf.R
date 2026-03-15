# =============================================================================
# mod_relativeinf.R — Onglet RELATIVEINF (+ multi-run)
# =============================================================================

mod_relativeinf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "plot-card",
      p(class = "section-title",
        "Information relative du design par ETA",
        tags$small(style = "color:#6b7280; font-weight:400; font-size:.8rem; margin-left:8px;",
                   "(TYPE 11 du fichier .shk)")
      ),
      plotOutput(ns("plot"), height = "380px")
    )
  )
}

mod_relativeinf_server <- function(id, shk_data, tbl_no, param_labels, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      runs <- all_runs()

      # Single run
      if (length(runs) <= 1) {
        shk <- shk_data()
        if (is.null(shk)) {
          return(ggplot() + labs(title = "Chargez un fichier .shk pour afficher RELATIVEINF(%)") + theme_bw())
        }
        lbls <- param_labels()
        eta_labels <- NULL
        if (!is.null(lbls)) {
          eta_labels <- setNames(lbls, paste0("ETA", seq_along(lbls)))
        }
        return(plot_relativeinf(shk, table_no = tbl_no(), param_labels = eta_labels))
      }

      # Multi-run
      combined <- purrr::imap_dfr(runs, function(r, idx) {
        if (is.null(r$shk_data)) return(NULL)
        ri <- get_relativeinf(r$shk_data, tbl_no())
        if (nrow(ri) == 0) return(NULL)
        lbls <- param_labels()
        if (!is.null(lbls)) {
          eta_labels <- setNames(lbls, paste0("ETA", seq_along(lbls)))
          ri <- ri |> mutate(eta = if_else(eta %in% names(eta_labels), eta_labels[eta], eta))
        }
        ri |> mutate(run = r$name)
      })

      if (nrow(combined) == 0) return(ggplot() + labs(title = "Pas de RELATIVEINF") + theme_bw())

      ggplot(combined, aes(x = reorder(eta, relativeinf_pct), y = relativeinf_pct, fill = run)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65,
                 color = "white", linewidth = 0.3) +
        geom_hline(yintercept = c(20, 50), linetype = "dashed", color = "grey40", linewidth = 0.45) +
        scale_fill_manual(values = .RUN_COLORS, name = NULL) +
        coord_flip() +
        labs(title = "RELATIVEINF (%) -- Comparaison multi-runs", x = NULL, y = "RELATIVEINF (%)") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank())
    }, res = 110)
  })
}
