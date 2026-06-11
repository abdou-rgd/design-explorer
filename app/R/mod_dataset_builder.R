# =============================================================================
# mod_dataset_builder.R -- NONMEM elementary dataset builder
# =============================================================================

dataset_builder_defaults <- function() {
  list(
    n_prototypes = 1,
    dose = 1800,
    dose_unit = "mg",
    dose_interval = 672,
    n_administrations = 1,
    dose_cmt = 1,
    observation_cmt = 1,
    rate = 0,
    sampling_times = "1, 24, 168, 671.9"
  )
}

mod_dataset_builder_ui <- function(id) {
  ns <- NS(id)
  defaults <- dataset_builder_defaults()

  page_shell(
    page_header(
      "Dataset Builder",
      "Create elementary-design CSV files for NONMEM $DESIGN evaluation.",
      eyebrow = "Design"
    ),
    page_section(
      "Settings",
      subtitle = "Elementary evaluation design only.",
      control_panel(
        fluidRow(
          column(
            3,
            numericInput(
              ns("n_prototypes"), "Prototypes",
              value = defaults$n_prototypes, min = 1, step = 1
            )
          ),
          column(
            3,
            numericInput(
              ns("dose"), "Dose",
              value = defaults$dose, min = 0, step = 100
            )
          ),
          column(
            3,
            textInput(
              ns("dose_unit"), "Dose unit",
              value = defaults$dose_unit
            )
          ),
          column(
            3,
            numericInput(
              ns("dose_interval"), "Dose interval",
              value = defaults$dose_interval, min = 0, step = 24
            )
          )
        ),
        fluidRow(
          column(
            3,
            numericInput(
              ns("n_administrations"), "Administrations",
              value = defaults$n_administrations, min = 1, step = 1
            )
          ),
          column(
            3,
            numericInput(
              ns("dose_cmt"), "Dose CMT",
              value = defaults$dose_cmt, min = 1, step = 1
            )
          ),
          column(
            3,
            numericInput(
              ns("observation_cmt"), "Observation CMT",
              value = defaults$observation_cmt, min = 1, step = 1
            )
          ),
          column(
            3,
            numericInput(
              ns("rate"), "Rate",
              value = defaults$rate, min = 0, step = 1
            )
          )
        ),
        textAreaInput(
          ns("sampling_times"), "Sampling schedule",
          value = defaults$sampling_times,
          rows = 3,
          width = "100%"
        ),
        actionButton(
          ns("generate_preview"), "Generate preview",
          class = "btn-primary"
        )
      )
    ),
    uiOutput(ns("validation_banner")),
    page_section(
      "Dataset summary",
      uiOutput(ns("dataset_summary"))
    ),
    page_section(
      "Preview",
      table_panel(
        "Generated rows",
        DTOutput(ns("dataset_preview")),
        actions = downloadButton(
          ns("download_csv"), "Download CSV",
          class = "btn-sm btn-default"
        ),
        subtitle = "NONMEM-ready comma CSV with elementary evaluation columns."
      )
    )
  )
}

mod_dataset_builder_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    defaults <- dataset_builder_defaults()
    dataset <- reactiveVal(NULL)
    build_error <- reactiveVal(NULL)
    generated_signature <- reactiveVal(NULL)

    input_signature <- reactive({
      list(
        n_prototypes = input$n_prototypes,
        dose = input$dose,
        dose_unit = input$dose_unit,
        dose_interval = input$dose_interval,
        n_administrations = input$n_administrations,
        dose_cmt = input$dose_cmt,
        observation_cmt = input$observation_cmt,
        rate = input$rate,
        sampling_times = input$sampling_times
      )
    })

    is_current <- reactive({
      !is.null(dataset()) &&
        !is.null(generated_signature()) &&
        identical(generated_signature(), input_signature())
    })

    current_dataset <- reactive({
      if (isTRUE(is_current())) {
        dataset()
      } else {
        NULL
      }
    })

    can_download <- reactive({
      dat <- current_dataset()
      if (is.null(dat)) {
        return(FALSE)
      }
      isTRUE(validate_nonmem_dataset(dat)$valid)
    })

    reset_inputs <- function() {
      updateNumericInput(session, "n_prototypes", value = defaults$n_prototypes)
      updateNumericInput(session, "dose", value = defaults$dose)
      updateTextInput(session, "dose_unit", value = defaults$dose_unit)
      updateNumericInput(session, "dose_interval", value = defaults$dose_interval)
      updateNumericInput(
        session, "n_administrations",
        value = defaults$n_administrations
      )
      updateNumericInput(session, "dose_cmt", value = defaults$dose_cmt)
      updateNumericInput(
        session, "observation_cmt",
        value = defaults$observation_cmt
      )
      updateNumericInput(session, "rate", value = defaults$rate)
      updateTextAreaInput(
        session, "sampling_times",
        value = defaults$sampling_times
      )
      dataset(NULL)
      build_error(NULL)
      generated_signature(NULL)
    }

    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        reset_inputs()
      }, ignoreInit = TRUE)
    }

    observeEvent(input$generate_preview, {
      built <- tryCatch({
        observation_times <- parse_sampling_times(input$sampling_times)
        build_nonmem_elementary_dataset(
          n_prototypes = input$n_prototypes,
          dose = input$dose,
          dose_interval = input$dose_interval,
          n_administrations = input$n_administrations,
          observation_times = observation_times,
          dose_cmt = input$dose_cmt,
          observation_cmt = input$observation_cmt,
          rate = input$rate
        )
      }, error = function(e) {
        build_error(conditionMessage(e))
        dataset(NULL)
        generated_signature(NULL)
        NULL
      })

      if (!is.null(built)) {
        dataset(built)
        generated_signature(input_signature())
        build_error(NULL)
      }
    }, ignoreInit = TRUE)

    validation <- reactive({
      dat <- current_dataset()
      if (is.null(dat)) {
        return(NULL)
      }
      validate_nonmem_dataset(dat)
    })

    output$validation_banner <- renderUI({
      err <- build_error()
      if (!is.null(err)) {
        return(status_panel(
          "Dataset could not be generated",
          tags$p(err),
          tone = "warning",
          icon_name = "exclamation-triangle"
        ))
      }

      val <- validation()
      if (is.null(val)) {
        if (!is.null(dataset()) && !isTRUE(is_current())) {
          return(status_panel(
            "Preview needs regeneration",
            tags$p("Inputs changed after the last preview. Generate again before previewing or downloading."),
            tone = "warning",
            icon_name = "exclamation-triangle"
          ))
        }
        return(status_panel(
          "No dataset generated",
          tags$p("Set the elementary design inputs and generate a preview."),
          tone = "info",
          icon_name = "info-circle"
        ))
      }

      if (isTRUE(val$valid)) {
        return(status_panel(
          "Dataset is valid",
          tags$p("The generated rows satisfy the NONMEM elementary CSV checks."),
          tone = "success",
          icon_name = "check-circle"
        ))
      }

      status_panel(
        "Dataset needs attention",
        tags$ul(lapply(val$errors, tags$li)),
        tone = "warning",
        icon_name = "exclamation-triangle"
      )
    })

    output$dataset_summary <- renderUI({
      dat <- current_dataset()
      if (is.null(dat)) {
        if (!is.null(dataset()) && !isTRUE(is_current())) {
          return(status_panel(
            "Summary needs regeneration",
            tags$p("Inputs changed after the last preview."),
            tone = "warning",
            icon_name = "exclamation-triangle"
          ))
        }
        return(status_panel(
          "Summary unavailable",
          tags$p("Generate a preview to inspect row counts."),
          tone = "neutral"
        ))
      }

      dose_rows <- sum(dat$EVID == 1)
      observation_rows <- sum(dat$EVID == 0)

      fact_strip(
        fact_item("Prototypes", length(unique(dat$ID))),
        fact_item("Dose rows", dose_rows),
        fact_item("Observation rows", observation_rows),
        fact_item("Total rows", nrow(dat))
      )
    })

    output$dataset_preview <- renderDT({
      dat <- current_dataset()
      req(dat)
      val <- validate_nonmem_dataset(dat)
      req(isTRUE(val$valid))

      datatable(
        dat,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(pageLength = 20, dom = "tip", scrollX = TRUE)
      )
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("nonmem_elementary_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(can_download())
        dat <- current_dataset()
        req(dat)
        write_nonmem_csv(dat, file)
      }
    )
  })
}
