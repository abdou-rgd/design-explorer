# =============================================================================
# mod_covariates.R -- Covariate effect post-processing
# =============================================================================

mod_covariates_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    page_header(
      "Covariate Effects",
      "Classify covariate effects from loaded FIM-predicted estimates and standard errors.",
      eyebrow = "Decision"
    ),
    doc_callout(
      "covariates",
      "Use a mapping to identify covariate-effect THETAs. Optionally add the NONMEM dataset to compute Fayette-style P10/P90 contrasts. No model/FIM recalculation is performed.",
      "Open documentation"
    ),
    page_section(
      "Effect mapping",
      subtitle = "Provide one row per covariate-effect THETA. CSV columns: param, effect_label, optional covariate, relationship.",
      control_panel(
        fileInput(ns("mapping_csv"), "Mapping CSV",
                  accept = c(".csv", "text/csv"),
                  buttonLabel = "Browse"),
        textAreaInput(
          ns("mapping_text"),
          "Manual mapping",
          rows = 4,
          placeholder = "THETA3=CLCR on CL=log_ratio\nTHETA4=WEIGHT on V=log_ratio\nTHETA5=SEX on V=log_ratio"
        ),
        numericInput(ns("margin_low"), "Lower margin", value = 0.80, min = 0.01, step = 0.01),
        numericInput(ns("margin_high"), "Upper margin", value = 1.25, min = 0.01, step = 0.01),
        numericInput(ns("ci_level"), "CI level", value = 0.90, min = 0.50, max = 0.99, step = 0.01)
      )
    ),
    page_section(
      "Covariate dataset",
      subtitle = "Optional: upload the NONMEM dataset or a one-row-per-subject covariate file to compute P10/P90 and binary contrasts.",
      control_panel(
        fileInput(ns("covariate_csv"), "Covariate dataset",
                  accept = c(".csv", ".txt", "text/csv", "text/plain"),
                  buttonLabel = "Browse"),
        textInput(ns("covariate_cols"), "Covariate columns", value = "CLCR, WEIGHT, SEX"),
        textInput(ns("id_col"), "ID column", value = "ID"),
        textInput(ns("time_col"), "Time column", value = "TIME"),
        numericInput(ns("baseline_time"), "Baseline time", value = 0, step = 1)
      ),
      uiOutput(ns("mode_note")),
      table_panel(
        "Covariate summary",
        DTOutput(ns("covariate_summary")),
        actions = NULL
      )
    ),
    page_section(
      "Clinical relevance",
      subtitle = "Relevant = CI fully outside the margin; non-relevant = CI fully inside the margin.",
      fluidRow(
        column(8,
          plot_panel(
            "Forest plot",
            plotOutput(ns("forest"), height = "460px"),
            plot_export_ui(ns, "forest_export", default_fname = "covariate_forest")
          )
        ),
        column(4,
          uiOutput(ns("decision_cards"))
        )
      ),
      table_panel(
        "Covariate effects table",
        DTOutput(ns("effects_table")),
        actions = downloadButton(ns("export_csv"), "Export CSV",
                                 class = "btn-sm btn-default")
      )
    )
  )
}

mod_covariates_server <- function(id, ext_data, tbl_no,
                                  param_labels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    .parse_mapping_text <- function(txt) {
      if (is.null(txt) || !nzchar(trimws(txt))) return(tibble::tibble())
      lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]])
      lines <- lines[nzchar(lines)]
      rows <- lapply(lines, function(ln) {
        parts <- trimws(strsplit(ln, "=", fixed = TRUE)[[1]])
        if (length(parts) < 1L || !nzchar(parts[1])) return(NULL)
        tibble::tibble(
          param = parts[1],
          effect_label = if (length(parts) >= 2L && nzchar(parts[2])) parts[2] else parts[1],
          transform = if (length(parts) >= 3L && nzchar(parts[3])) parts[3] else "log_ratio",
          covariate = if (length(parts) >= 4L && nzchar(parts[4])) parts[4] else NA_character_,
          relationship = if (length(parts) >= 5L && nzchar(parts[5])) parts[5] else "Exp"
        )
      })
      dplyr::bind_rows(rows)
    }

    .split_cols <- function(txt) {
      if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
      trimws(unlist(strsplit(txt, "[,;\\n]+", perl = TRUE)))
    }

    .pretty_effect_label <- function(label) {
      cov <- .infer_covariate_from_label(label)
      beta_match <- regexec("(?i)^BETA_([^_]+)_([^_]+)$", label, perl = TRUE)
      beta_hit <- regmatches(label, beta_match)[[1]]
      if (length(beta_hit) >= 3L && !is.na(cov)) {
        return(paste0(cov, " on ", beta_hit[2]))
      }
      label
    }

    auto_mapping <- reactive({
      ext <- ext_data()
      if (is.null(ext)) return(tibble::tibble())
      rse <- get_rse(ext, table_no = tbl_no())
      lbls <- param_labels()
      if (is.null(lbls) || length(lbls) == 0L || nrow(rse) == 0L) return(tibble::tibble())
      labels <- lbls[rse$param]
      keep <- !is.na(labels) & grepl("\\bon\\b|covariate|cov|wt|weight|age|sex|egfr|crcl|bmi",
                                     tolower(labels))
      if (!any(keep)) return(tibble::tibble())
      effect_labels <- vapply(unname(labels[keep]), .pretty_effect_label, character(1))
      tibble::tibble(
        param = rse$param[keep],
        effect_label = effect_labels,
        transform = "log_ratio",
        covariate = vapply(effect_labels, .infer_covariate_from_label, character(1)),
        relationship = "Exp"
      )
    })

    csv_mapping <- reactive({
      file <- input$mapping_csv
      if (is.null(file)) return(tibble::tibble())
      dat <- tryCatch(
        readr::read_csv(file$datapath, show_col_types = FALSE, progress = FALSE),
        error = function(e) {
          showNotification(paste("Covariate mapping read error:", e$message), type = "error")
          NULL
        }
      )
      if (is.null(dat) || !("param" %in% names(dat))) return(tibble::tibble())
      if (!("effect_label" %in% names(dat))) dat$effect_label <- dat$param
      if (!("transform" %in% names(dat))) dat$transform <- "log_ratio"
      if (!("covariate" %in% names(dat))) dat$covariate <- NA_character_
      if (!("relationship" %in% names(dat))) dat$relationship <- "Exp"
      tibble::as_tibble(dat[, c("param", "effect_label", "transform", "covariate", "relationship")])
    })

    observeEvent(input$mapping_csv, {
      file <- input$mapping_csv
      if (is.null(file)) return()
      dat <- tryCatch(
        readr::read_csv(file$datapath, n_max = 5, show_col_types = FALSE, progress = FALSE),
        error = function(e) NULL
      )
      if (!is.null(dat) && !("param" %in% names(dat))) {
        showNotification(
          "This looks like a dataset, not a mapping CSV. Use the 'Covariate dataset' input for FDATA/NONMEM data.",
          type = "warning",
          duration = 8
        )
      }
    }, ignoreInit = TRUE)

    mapping <- reactive({
      manual <- .parse_mapping_text(input$mapping_text)
      csv <- csv_mapping()
      auto <- auto_mapping()
      out <- dplyr::bind_rows(csv, manual, auto)
      if (nrow(out) == 0L) return(out)
      out |>
        dplyr::filter(!is.na(.data$param), .data$param != "") |>
        dplyr::distinct(.data$param, .keep_all = TRUE)
    })

    covariate_data <- reactive({
      file <- input$covariate_csv
      if (is.null(file)) return(tibble::tibble())
      tryCatch(
        read_covariate_dataset_csv(file$datapath),
        error = function(e) {
          showNotification(paste("Covariate dataset read error:", e$message), type = "error")
          tibble::tibble()
        }
      )
    })

    covariate_summary <- reactive({
      dat <- covariate_data()
      if (is.null(dat) || nrow(dat) == 0L) return(tibble::tibble())
      cols <- .split_cols(input$covariate_cols)
      summarise_covariate_dataset(
        dat,
        covariates = cols,
        id_col = trimws(input$id_col %||% ""),
        time_col = trimws(input$time_col %||% ""),
        baseline_time = input$baseline_time %||% 0
      )
    })

    effects <- reactive({
      ext <- ext_data()
      if (is.null(ext)) return(tibble::tibble())
      rse <- get_rse(ext, table_no = tbl_no())
      margin <- c(input$margin_low %||% 0.8, input$margin_high %||% 1.25)
      cov_dat <- covariate_data()
      if (!is.null(cov_dat) && nrow(cov_dat) > 0L) {
        contrast_eff <- compute_covariate_contrast_effects(
          mapping(),
          rse,
          covariate_data = cov_dat,
          id_col = trimws(input$id_col %||% ""),
          time_col = trimws(input$time_col %||% ""),
          baseline_time = input$baseline_time %||% 0,
          margin = margin,
          ci = input$ci_level %||% 0.90
        )
        if (nrow(contrast_eff) > 0L) return(contrast_eff)
      }
      compute_covariate_effects_table(
        mapping(),
        rse,
        margin = margin,
        ci = input$ci_level %||% 0.90
      )
    })

    output$mode_note <- renderUI({
      sum_tbl <- covariate_summary()
      if (is.null(sum_tbl) || nrow(sum_tbl) == 0L) {
        return(status_panel(
          "Log-ratio mode",
          tags$p("Without a covariate dataset, ratios use the mapped THETA directly as a log-ratio."),
          tone = "info",
          icon_name = "sliders"
        ))
      }
      status_panel(
        "Contrast mode",
        tags$p("Using the uploaded covariate dataset to compute P90/P10 vs median for continuous covariates and high vs reference for binary covariates."),
        tone = "success",
        icon_name = "diagram-3"
      )
    })

    output$covariate_summary <- renderDT({
      sum_tbl <- covariate_summary()
      if (is.null(sum_tbl) || nrow(sum_tbl) == 0L) return(NULL)
      display <- sum_tbl |>
        dplyr::transmute(
          Covariate = .data$covariate,
          Type = .data$type,
          N = .data$n,
          Reference = round(.data$reference, 4),
          Low = round(.data$value_low, 4),
          High = round(.data$value_high, 4)
        )
      datatable(display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(pageLength = 8, dom = "tip", scrollX = TRUE))
    })

    forest_plot <- reactive({
      plot_covariate_forest(effects())
    })
    output$forest <- renderPlot({ forest_plot() }, res = 110)
    plot_export_server(input, output, session, "forest_export", forest_plot)

    output$decision_cards <- renderUI({
      eff <- effects()
      if (is.null(eff) || nrow(eff) == 0L) {
        return(status_panel(
          "Mapping required",
          tags$p("Add a CSV or manual mapping for covariate-effect THETAs."),
          tone = "warning",
          icon_name = "table"
        ))
      }
      counts <- table(factor(eff$decision,
                             levels = c("relevant", "inconclusive", "non-relevant")))
      fact_strip(
        fact_item("Relevant", counts[["relevant"]] %||% 0L, "CI outside margin", "danger"),
        fact_item("Inconclusive", counts[["inconclusive"]] %||% 0L, "CI crosses margin", "warning"),
        fact_item("Non-relevant", counts[["non-relevant"]] %||% 0L, "CI inside margin", "success")
      )
    })

    output$effects_table <- renderDT({
      eff <- effects()
      if (is.null(eff) || nrow(eff) == 0L) return(NULL)
      if (!("covariate" %in% names(eff))) eff$covariate <- NA_character_
      if (!("contrast_label" %in% names(eff))) eff$contrast_label <- NA_character_
      if (!("delta" %in% names(eff))) eff$delta <- NA_real_
      display <- eff |>
        dplyr::transmute(
          Parameter = .data$param,
          Effect = .data$effect_label,
          Transform = .data$transform,
          Covariate = .data$covariate %||% NA_character_,
          Contrast = .data$contrast_label %||% NA_character_,
          Delta = round(.data$delta %||% NA_real_, 4),
          Ratio = round(.data$ratio, 3),
          `CI low` = round(.data$ci_lower, 3),
          `CI high` = round(.data$ci_upper, 3),
          Decision = .data$decision
        )
      datatable(display, rownames = FALSE,
                class = "stripe hover compact",
                options = list(pageLength = 12, dom = "tip", scrollX = TRUE)) |>
        formatStyle(
          "Decision",
          backgroundColor = styleEqual(
            c("relevant", "inconclusive", "non-relevant"),
            c("#fee2e2", "#fef3c7", "#d1fae5")
          )
        )
    })

    output$export_csv <- downloadHandler(
      filename = function() {
        paste0("covariate_effects_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(effects(), file, row.names = FALSE)
      }
    )
  })
}
