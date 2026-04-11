# =============================================================================
# mod_ctl_stream.R — Affichage du control stream (.ctl / .mod / .con)
# =============================================================================

mod_ctl_stream_ui <- function(id) {
  ns <- NS(id)
  div(class = "surface-card", style = "padding: 20px;",
    p(class = "section-title", "Control Stream"),
    uiOutput(ns("ctl_placeholder")),
    verbatimTextOutput(ns("ctl_text"))
  )
}

mod_ctl_stream_server <- function(id, ctl_lines) {
  moduleServer(id, function(input, output, session) {

    output$ctl_placeholder <- renderUI({
      lines <- ctl_lines()
      if (is.null(lines) || length(lines) == 0L) {
        div(class = "alert alert-info", style = "border-radius: 10px;",
            "No .ctl / .mod / .con file loaded.")
      }
    })

    output$ctl_text <- renderText({
      lines <- ctl_lines()
      req(lines, length(lines) > 0L)
      paste(lines, collapse = "\n")
    })
  })
}
