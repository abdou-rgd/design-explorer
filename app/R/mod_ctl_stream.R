# =============================================================================
# mod_ctl_stream.R — Affichage du control stream (.ctl / .mod / .con)
# =============================================================================

mod_ctl_stream_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    page_header(
      "Control Stream",
      "Loaded .ctl/.mod/.con content used for labels, true values, GROUPSIZE, and robust-prior context.",
      eyebrow = "Diagnostic"
    ),
    table_panel(
      "Control stream",
      uiOutput(ns("ctl_placeholder")),
      verbatimTextOutput(ns("ctl_text"))
    )
  )
}

mod_ctl_stream_server <- function(id, ctl_lines) {
  moduleServer(id, function(input, output, session) {

    output$ctl_placeholder <- renderUI({
      lines <- ctl_lines()
      if (is.null(lines) || length(lines) == 0L) {
        empty_state("No control stream loaded", "Upload a .ctl, .mod, or .con file in Home.", "file-code")
      }
    })

    output$ctl_text <- renderText({
      lines <- ctl_lines()
      req(lines, length(lines) > 0L)
      paste(lines, collapse = "\n")
    })
  })
}
