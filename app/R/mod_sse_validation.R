# =============================================================================
# mod_sse_validation.R — Onglet Validation SSE
#
# Compare les RSE predites par la FIM avec les RSE empiriques issues d'une SSE.
# Inputs : CSV PsN brut + .ctl (valeurs vraies) + .ext deja charge (FIM RSE)
# =============================================================================

mod_sse_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Info banner ---
    div(class = "alert alert-info", style = "border-radius:10px; margin-bottom:12px;",
      tags$strong("Validation FIM vs SSE"),
      tags$p(style = "margin:6px 0 0; font-size:0.9em;",
        "Comparez les RSE predites par la FIM (depuis le .ext charge) ",
        "avec les RSE empiriques calculees sur les estimations SSE. ",
        "Un point sur la diagonale = prediction parfaite."
      )
    ),

    # --- File uploads (STATIC, never inside renderUI) ---
    fluidRow(
      column(6,
        fileInput(ns("sse_file"), "Resultats SSE bruts (CSV PsN)",
                  accept = ".csv", width = "100%")
      ),
      column(6,
        fileInput(ns("ctl_file"), "Control stream simulation (.ctl/.mod/.con)",
                  accept = c(".ctl", ".mod", ".con"), width = "100%")
      )
    ),

    # --- Status banner ---
    uiOutput(ns("status_banner")),

    # --- Plot + Table ---
    fluidRow(
      column(12,
        div(class = "plot-card",
          p(class = "section-title", "Scatter FIM RSE vs SSE RSE"),
          plotOutput(ns("scatter"), height = "500px")
        )
      )
    ),
    br(),
    fluidRow(
      column(12,
        div(class = "param-table-wrap",
          div(style = "display:flex; justify-content:space-between; align-items:center;",
            p(class = "section-title", style = "margin:0;", "Table de comparaison"),
            downloadButton(ns("export_csv"), "Exporter CSV",
                           class = "btn-sm btn-default")
          ),
          DTOutput(ns("comp_table"))
        )
      )
    )
  )
}


mod_sse_validation_server <- function(id, ext_data,
                                      param_labels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # --- Parse SSE CSV ---
    sse_raw <- reactive({
      req(input$sse_file)
      tryCatch(
        read_sse_raw(input$sse_file$datapath),
        error = function(e) {
          showNotification(paste("Erreur lecture SSE :", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
    })

    # --- Parse true values from .ctl ---
    true_vals <- reactive({
      req(input$ctl_file)
      ctl_lines <- tryCatch(
        readLines(input$ctl_file$datapath, warn = FALSE),
        error = function(e) NULL
      )
      if (is.null(ctl_lines)) return(NULL)
      vals <- read_true_values(ctl_lines)
      if (length(vals) == 0L) {
        showNotification("Aucune valeur vraie extraite du .ctl",
                         type = "warning", duration = 6)
        return(NULL)
      }
      vals
    })

    # --- Compute SSE metrics ---
    sse_metrics <- reactive({
      req(sse_raw(), true_vals())
      compute_sse_metrics(sse_raw(), true_vals(), param_labels())
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

    # --- Status banner ---
    output$status_banner <- renderUI({
      raw <- sse_raw()
      if (is.null(raw)) return(NULL)

      n_total <- attr(raw, "n_total") %||% "?"
      n_success <- attr(raw, "n_success") %||% nrow(raw)

      tv <- true_vals()
      n_params <- if (!is.null(tv)) length(tv) else 0L

      div(class = "alert alert-success",
          style = "border-radius:8px; margin-bottom:10px; padding:8px 14px;",
        tags$strong(sprintf("SSE : %s/%s runs valides (minimization_successful = 1)",
                            n_success, n_total)),
        if (n_params > 0L) {
          tags$span(style = "margin-left:16px;",
            sprintf("| %d parametres (valeurs vraies du .ctl)", n_params))
        }
      )
    })

    # --- Scatter plot ---
    output$scatter <- renderPlot({
      comp <- comparison()
      if (is.null(comp) || nrow(comp) == 0L) {
        return(ggplot() +
          labs(title = "Chargez un CSV SSE et un .ctl pour voir le scatter plot") +
          .theme_design())
      }
      plot_fim_vs_sse(comp)
    }, res = 110)

    # --- Comparison table ---
    output$comp_table <- renderDT({
      comp <- comparison()
      req(comp)

      display <- comp |>
        dplyr::filter(status == "matched") |>
        dplyr::select(
          Parametre = param_label,
          Type = param_type,
          `RSE FIM (%)` = rse_fim,
          `RSE SSE (%)` = rse_sse,
          `RMSE SSE (%)` = rmse_sse,
          `Biais rel. (%)` = relative_bias,
          Ratio = ratio,
          `+/-20%` = pass_20pct
        ) |>
        dplyr::mutate(
          Ratio = round(Ratio, 2),
          `+/-20%` = ifelse(`+/-20%`, "OK", "Hors bande")
        )

      datatable(display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(
                  pageLength = 20, dom = "t",
                  scrollX = TRUE,
                  columnDefs = list(
                    list(className = "dt-center", targets = 6:7)
                  )
                )) |>
        formatStyle("+/-20%",
          backgroundColor = styleEqual(
            c("OK", "Hors bande"),
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
                        rse_fim, rse_sse = rse_sse, rmse_sse,
                        relative_bias, ratio, pass_20pct)
        tryCatch(
          write.csv(export, file, row.names = FALSE),
          error = function(e) warning("CSV export failed: ", conditionMessage(e))
        )
      }
    )
  })
}
