# =============================================================================
# mod_raw.R — Onglet Donnees brutes (+ multi-run dropdown)
# =============================================================================

mod_raw_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "param-table-wrap",
      fluidRow(
        column(9, p(class = "section-title", "Contenu complet du fichier .ext")),
        column(3, uiOutput(ns("run_selector")))
      ),
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
  })
}
