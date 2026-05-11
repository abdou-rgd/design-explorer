# =============================================================================
# mod_raw.R — Onglet Donnees brutes (+ multi-run dropdown)
# =============================================================================

mod_raw_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    page_header(
      "Raw Data",
      "Raw parsed .ext content for diagnostics and export.",
      eyebrow = "Diagnostic"
    ),
    page_section(
      "Raw data controls",
      control_panel(
        uiOutput(ns("run_selector")),
        downloadButton(ns("export_csv"), "Download CSV",
                       class = "btn btn-sm btn-default")
      )
    ),
    table_panel(
      "Full .ext file content",
      DTOutput(ns("raw_ext"))
    )
  )
}

mod_raw_server <- function(id, ext_data, all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$run_selector <- renderUI({
      runs <- all_runs()
      if (length(runs) <= 1) return(NULL)
      choices <- setNames(names(runs), sapply(runs, `[[`, "name"))
      selectInput(ns("selected_run"), "Run", choices = choices, selected = names(runs)[1])
    })

    selected_ext <- reactive({
      runs <- all_runs()
      if (length(runs) <= 1) return(ext_data())
      sel <- input$selected_run %||% names(runs)[1]
      runs[[sel]]$ext_data
    })

    output$raw_ext <- renderDT({
      ext <- selected_ext(); req(ext)
      datatable(
        ext |> mutate(across(where(is.double), ~ round(.x, 6))),
        rownames = FALSE, filter = "top",
        class = "stripe hover compact",
        options = list(pageLength = 20, scrollX = TRUE)
      )
    })

    output$export_csv <- downloadHandler(
      filename = function() paste0("design_", Sys.Date(), ".csv"),
      content  = function(file) {
        ext <- selected_ext()
        req(ext)
        tryCatch(
          write.csv(ext, file, row.names = FALSE),
          error = function(e) warning("CSV export failed: ", conditionMessage(e))
        )
      }
    )
  })
}
