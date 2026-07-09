# =============================================================================
# mod_dataset_builder.R -- NONMEM elementary dataset builder
# =============================================================================

DATASET_BUILDER_MAX_DESIGNS <- 4L

dataset_builder_examples <- function() {
  list(
    simple = list(
      n_designs = 2,
      designs = list(
        list(
          design = "1",
          arm = "A",
          dose_events = "0:100:100:1;24:100:100:1",
          sampling_times = "1, 2, 23.9, 25",
          dose_cmt = 1,
          obs_cmt = 2,
          rate = 0
        ),
        list(
          design = "2",
          arm = "B",
          dose_events = "0:200:200:1;24:200:200:1",
          sampling_times = "4, 8, 12, 23.9",
          dose_cmt = 1,
          obs_cmt = 2,
          rate = 0
        )
      )
    ),
    psm_eval = list(
      n_designs = 2,
      designs = list(
        list(
          design = "1",
          arm = "0",
          dose_events = paste(
            "0:1800:1800:2",
            "672:1200:1200:2",
            "1344:1200:1200:2",
            "2016:1200:1200:2",
            "2688:1200:1200:2",
            "3360:1200:1200:2",
            "4032:1200:1200:2",
            sep = ";"
          ),
          sampling_times = paste(
            "1:1800",
            "671.9:1200",
            "2015.9:1200",
            "3359.9:1200",
            "3361:1200",
            "3528:1200",
            "3696:1200",
            "4031.9:1200",
            "4033:1200",
            sep = ", "
          ),
          dose_cmt = 2,
          obs_cmt = 2,
          rate = 0
        ),
        list(
          design = "2",
          arm = "1",
          dose_events = paste(
            "0:1800:1800:1",
            "336:1800:1800:1",
            "672:1800:1800:1",
            "1344:1800:1800:1",
            "2016:1800:1800:1",
            "2688:1800:1800:1",
            "3360:1800:1800:1",
            "4032:1800:1800:1",
            sep = ";"
          ),
          sampling_times = paste(
            "335.9",
            "671.9",
            "2015.9",
            "3359.9",
            "3528",
            "3696",
            "3864",
            "4031.9",
            sep = ", "
          ),
          dose_cmt = 1,
          obs_cmt = 2,
          rate = 0
        )
      )
    )
  )
}

dataset_builder_simple_example <- function() {
  compose_schedule_table_from_designs(dataset_builder_examples()$simple$designs)
}

dataset_builder_psm_eval_example <- function() {
  compose_schedule_table_from_designs(dataset_builder_examples()$psm_eval$designs)
}

dataset_builder_defaults <- function() {
  dataset_builder_examples()$simple
}

quote_schedule_field <- function(x) {
  x <- as.character(x)
  x <- gsub("\"", "\"\"", x, fixed = TRUE)
  paste0("\"", x, "\"")
}

compose_schedule_table_from_designs <- function(designs) {
  rows <- vapply(designs, function(design) {
    paste(
      design$design,
      design$arm,
      quote_schedule_field(design$dose_events),
      quote_schedule_field(design$sampling_times),
      design$dose_cmt,
      design$obs_cmt,
      design$rate,
      sep = ","
    )
  }, character(1))
  paste(
    c("DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,DOSE_CMT,OBS_CMT,RATE", rows),
    collapse = "\n"
  )
}

guided_design_from_input <- function(input, index) {
  list(
    design = input[[paste0("design_", index)]],
    arm = input[[paste0("arm_", index)]],
    dose_events = input[[paste0("dose_events_", index)]],
    sampling_times = input[[paste0("sampling_times_", index)]],
    dose_cmt = input[[paste0("dose_cmt_", index)]],
    obs_cmt = input[[paste0("obs_cmt_", index)]],
    rate = input[[paste0("rate_", index)]]
  )
}

compose_guided_schedule_table <- function(input, max_designs = DATASET_BUILDER_MAX_DESIGNS) {
  n_designs <- input$n_designs
  if (is.null(n_designs) || is.na(n_designs)) {
    n_designs <- 1L
  }
  n_designs <- max(1L, min(as.integer(n_designs), max_designs))
  designs <- lapply(seq_len(n_designs), function(i) {
    guided_design_from_input(input, i)
  })
  compose_schedule_table_from_designs(designs)
}

dataset_builder_design_ui <- function(ns, index, defaults) {
  design <- if (index <= length(defaults$designs)) defaults$designs[[index]] else NULL
  if (is.null(design)) {
    design <- list(
      design = as.character(index),
      arm = as.character(index - 1L),
      dose_events = "0:100",
      sampling_times = "1, 2",
      dose_cmt = 1,
      obs_cmt = 2,
      rate = 0
    )
  }

  conditionalPanel(
    condition = sprintf("input.n_designs >= %d", index),
    ns = ns,
    control_panel(
      label = paste("Elementary design", index),
      fluidRow(
        column(
          3,
          textInput(ns(paste0("design_", index)), "Design", value = design$design)
        ),
        column(
          3,
          textInput(ns(paste0("arm_", index)), "ARM", value = design$arm)
        ),
        column(
          2,
          numericInput(ns(paste0("dose_cmt_", index)), "Dose CMT", value = design$dose_cmt, min = 1, step = 1)
        ),
        column(
          2,
          numericInput(ns(paste0("obs_cmt_", index)), "Observation CMT", value = design$obs_cmt, min = 1, step = 1)
        ),
        column(
          2,
          numericInput(ns(paste0("rate_", index)), "Default rate", value = design$rate, min = 0, step = 1)
        )
      ),
      textAreaInput(
        ns(paste0("dose_events_", index)),
        "Dose schedule",
        value = design$dose_events,
        rows = 2,
        width = "100%"
      ),
      textAreaInput(
        ns(paste0("sampling_times_", index)),
        "Sampling schedule",
        value = design$sampling_times,
        rows = 2,
        width = "100%"
      )
    )
  )
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
      "Design setup",
      subtitle = "Enter the dose and sampling schedules for each elementary design.",
      control_panel(
        fluidRow(
          column(
            3,
            numericInput(
              ns("n_designs"), "Elementary designs",
              value = defaults$n_designs, min = 1, max = DATASET_BUILDER_MAX_DESIGNS, step = 1
            )
          ),
          column(
            9,
            tags$div(
              class = "dataset-builder-actions",
              actionButton(ns("load_simple_example"), "Simple example", class = "btn-default btn-sm"),
              actionButton(ns("load_psm_eval_example"), "psm_eval example", class = "btn-default btn-sm")
            )
          )
        )
      )
    ),
    page_section(
      "Schedules",
      subtitle = "Dose entries use time:amount, time:amount:rate, or time:amount:rate:cmt. Sampling entries can be times, or time:dose when an observation should carry a specific DOSE value.",
      lapply(seq_len(DATASET_BUILDER_MAX_DESIGNS), function(i) {
        dataset_builder_design_ui(ns, i, defaults)
      })
    ),
    page_section(
      NULL,
      control_panel(
        actionButton(ns("generate_preview"), "Generate preview", class = "btn-primary")
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
          downloadButton(ns("download_schedule"), "Download schedule spec", class = "btn-sm btn-default"),
          downloadButton(ns("download_csv"), "Download CSV", class = "btn-sm btn-default")
        ),
        subtitle = "NONMEM-ready comma CSV with per-design dosing and sampling."
      )
    )
  )
}

mod_dataset_builder_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactiveVal(NULL)
    build_error <- reactiveVal(NULL)
    build_warnings <- reactiveVal(character())
    generated_signature <- reactiveVal(NULL)

    input_signature <- reactive({
      compose_guided_schedule_table(input)
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

    apply_example <- function(example) {
      updateNumericInput(session, "n_designs", value = example$n_designs)
      for (i in seq_len(DATASET_BUILDER_MAX_DESIGNS)) {
        design <- example$designs[[i]]
        if (is.null(design)) {
          design <- list(
            design = as.character(i),
            arm = as.character(i - 1L),
            dose_events = "0:100",
            sampling_times = "1, 2",
            dose_cmt = 1,
            obs_cmt = 2,
            rate = 0
          )
        }
        updateTextInput(session, paste0("design_", i), value = design$design)
        updateTextInput(session, paste0("arm_", i), value = design$arm)
        updateTextAreaInput(session, paste0("dose_events_", i), value = design$dose_events)
        updateTextAreaInput(session, paste0("sampling_times_", i), value = design$sampling_times)
        updateNumericInput(session, paste0("dose_cmt_", i), value = design$dose_cmt)
        updateNumericInput(session, paste0("obs_cmt_", i), value = design$obs_cmt)
        updateNumericInput(session, paste0("rate_", i), value = design$rate)
      }
      dataset(NULL)
      build_error(NULL)
      build_warnings(character())
      generated_signature(NULL)
    }

    observeEvent(input$load_simple_example, {
      apply_example(dataset_builder_examples()$simple)
    }, ignoreInit = TRUE)

    observeEvent(input$load_psm_eval_example, {
      apply_example(dataset_builder_examples()$psm_eval)
    }, ignoreInit = TRUE)

    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        apply_example(dataset_builder_defaults())
      }, ignoreInit = TRUE)
    }

    observeEvent(input$generate_preview, {
      schedule_table <- compose_guided_schedule_table(input)
      validation <- validate_design_schedule_table(schedule_table)
      if (!isTRUE(validation$valid)) {
        build_error(paste(validation$errors, collapse = "\n"))
        build_warnings(validation$warnings)
        dataset(NULL)
        generated_signature(NULL)
        return(invisible(NULL))
      }

      built <- tryCatch({
        build_nonmem_dataset_from_schedule_table(schedule_table)
      }, error = function(e) {
        build_error(conditionMessage(e))
        dataset(NULL)
        generated_signature(NULL)
        NULL
      })

      if (!is.null(built)) {
        dataset(built)
        generated_signature(schedule_table)
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
          tags$p("Enter the design schedules and generate a preview."),
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
        writeLines(compose_guided_schedule_table(input), file, useBytes = TRUE)
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
