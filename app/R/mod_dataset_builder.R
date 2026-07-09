# =============================================================================
# mod_dataset_builder.R -- NONMEM elementary dataset builder
# =============================================================================

dataset_builder_simple_example <- function() {
  paste(
    "DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,DOSE_CMT,OBS_CMT,RATE",
    "1,A,\"0:100:100:1;24:100:100:1\",\"1,2,23.9,25\",1,2,0",
    "2,B,\"0:200:200:1;24:200:200:1\",\"4,8,12,23.9\",1,2,0",
    sep = "\n"
  )
}

dataset_builder_psm_eval_example <- function() {
  paste(
    "DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,DOSE_CMT,OBS_CMT,RATE",
    "1,0,\"0:1800:1800:2;672:1200:1200:2;1344:1200:1200:2;2016:1200:1200:2;2688:1200:1200:2;3360:1200:1200:2;4032:1200:1200:2\",\"1,671.9,2015.9,3359.9,3361,3528,3696,4031.9,4033\",2,2,0",
    "2,1,\"0:1800:1800:1;336:1800:1800:1;672:1800:1800:1;1344:1800:1800:1;2016:1800:1800:1;2688:1800:1800:1;3360:1800:1800:1;4032:1800:1800:1\",\"335.9,671.9,2015.9,3359.9,3528,3696,3864,4031.9\",1,2,0",
    sep = "\n"
  )
}

dataset_builder_defaults <- function() {
  list(schedule_table = dataset_builder_simple_example())
}

mod_dataset_builder_ui <- function(id) {
  ns <- NS(id)
  defaults <- dataset_builder_defaults()

  page_shell(
    page_header(
      "Dataset Builder",
      "Create per-design elementary CSV files for NONMEM $DESIGN evaluation.",
      eyebrow = "Design"
    ),
    page_section(
      "Schedule table",
      subtitle = "Paste a CSV table. Dose events use time:amt, time:amt:rate, or time:amt:rate:cmt tokens separated by semicolons.",
      control_panel(
        tags$div(
          class = "dataset-builder-actions",
          actionButton(
            ns("load_simple_example"),
            "Simple example",
            class = "btn-default btn-sm"
          ),
          actionButton(
            ns("load_psm_eval_example"),
            "psm_eval example",
            class = "btn-default btn-sm"
          )
        ),
        textAreaInput(
          ns("schedule_table"),
          "Design schedule CSV",
          value = defaults$schedule_table,
          rows = 8,
          width = "100%"
        ),
        actionButton(
          ns("generate_preview"), "Generate preview",
          class = "btn-primary"
        )
      )
    ),
    uiOutput(ns("validation_banner")),
    uiOutput(ns("warning_banner")),
    page_section(
      "Dataset summary",
      uiOutput(ns("dataset_summary"))
    ),
    page_section(
      "Preview",
      table_panel(
        "Generated rows",
        DTOutput(ns("dataset_preview")),
        actions = tagList(
          downloadButton(
            ns("download_schedule"), "Download schedule",
            class = "btn-sm btn-default"
          ),
          downloadButton(
            ns("download_csv"), "Download CSV",
            class = "btn-sm btn-default"
          )
        ),
        subtitle = "NONMEM-ready comma CSV with per-design dosing and sampling."
      )
    )
  )
}

mod_dataset_builder_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    defaults <- dataset_builder_defaults()
    dataset <- reactiveVal(NULL)
    build_error <- reactiveVal(NULL)
    build_warnings <- reactiveVal(character())
    generated_signature <- reactiveVal(NULL)

    input_signature <- reactive({
      list(schedule_table = input$schedule_table)
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

    set_schedule_table <- function(value) {
      updateTextAreaInput(session, "schedule_table", value = value)
      dataset(NULL)
      build_error(NULL)
      build_warnings(character())
      generated_signature(NULL)
    }

    reset_inputs <- function() {
      set_schedule_table(defaults$schedule_table)
    }

    observeEvent(input$load_simple_example, {
      set_schedule_table(dataset_builder_simple_example())
    }, ignoreInit = TRUE)

    observeEvent(input$load_psm_eval_example, {
      set_schedule_table(dataset_builder_psm_eval_example())
    }, ignoreInit = TRUE)

    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        reset_inputs()
      }, ignoreInit = TRUE)
    }

    observeEvent(input$generate_preview, {
      validation <- validate_design_schedule_table(input$schedule_table)
      if (!isTRUE(validation$valid)) {
        build_error(paste(validation$errors, collapse = "\n"))
        build_warnings(validation$warnings)
        dataset(NULL)
        generated_signature(NULL)
        return(invisible(NULL))
      }

      built <- tryCatch({
        build_nonmem_dataset_from_schedule_table(input$schedule_table)
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
        build_warnings(validation$warnings)
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
          tags$pre(err),
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
          tags$p("Paste or load a schedule table and generate a preview."),
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

    output$warning_banner <- renderUI({
      warnings <- build_warnings()
      if (length(warnings) == 0L) {
        return(NULL)
      }
      status_panel(
        "Schedule warnings",
        tags$ul(lapply(warnings, tags$li)),
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
      time_range <- paste0(min(dat$TIME), " to ", max(dat$TIME))
      design_values <- if ("DESIGN" %in% names(dat)) dat$DESIGN else dat$ID

      fact_strip(
        fact_item("Designs", length(unique(design_values))),
        fact_item("IDs", length(unique(dat$ID))),
        fact_item("Dose rows", dose_rows),
        fact_item("Observation rows", observation_rows),
        fact_item("Total rows", nrow(dat)),
        fact_item("Time range", time_range)
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

    output$download_schedule <- downloadHandler(
      filename = function() {
        paste0("nonmem_elementary_schedule_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        text <- input$schedule_table
        if (is.null(text)) {
          text <- ""
        }
        writeLines(text, file, useBytes = TRUE)
      }
    )

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
