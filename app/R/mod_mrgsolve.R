# =============================================================================
# mod_mrgsolve.R — Module mrgsolve : upload modele, config doses, simulation
# =============================================================================

mod_mrgsolve_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("mrgsolve_panel"))
}

.mrgsolve_outvar_names <- function(
  mod,
  kind,
  outvars_provider = mrgsolve::outvars
) {
  kind <- match.arg(kind, c("capture", "cmt"))
  if (is.null(mod)) {
    return(character())
  }

  outvars <- outvars_provider(mod)
  values <- outvars[[kind]]
  if (is.null(values)) character() else as.character(values)
}

mod_mrgsolve_server <- function(
  id,
  ext_data,
  tab_data,
  ctl_lines = reactive(NULL),
  theta_labels = reactive(NULL),
  mrg_status_provider = has_mrgsolve
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Check mrgsolve availability ------------------------------------------
    mrg_status <- mrg_status_provider()

    # -- Compiled model object (reactiveVal) ----------------------------------
    compiled_model <- reactiveVal(NULL)
    compiled_model_code <- reactiveVal(NULL)
    compiled_model_hash <- reactiveVal(NULL)
    compile_error <- reactiveVal(NULL)
    sim_result <- reactiveVal(NULL)
    sim_context <- reactiveVal(NULL)

    clear_sim_result <- function() {
      sim_context(NULL)
      sim_result(NULL)
    }

    # -- UI: conditional on mrgsolve availability -----------------------------
    output$mrgsolve_panel <- renderUI({
      if (!mrg_status$available) {
        return(status_panel(
          "PK simulation unavailable",
          tags$p(
            mrg_status$reason,
            " - Install mrgsolve and Rtools for smooth PK curves."
          ),
          tone = "warning",
          icon_name = "exclamation-triangle"
        ))
      }

      control_panel(
        label = "PK simulation",
        actionLink(
          ns("toggle_panel"),
          label = tagList(
            icon("chevron-down"),
            span("mrgsolve")
          )
        ),
        uiOutput(ns("panel_body"))
      )
    })

    # -- Toggle panel visibility -----------------------------------------------
    show_panel <- reactiveVal(FALSE)
    observeEvent(input$toggle_panel, {
      show_panel(!show_panel())
    })

    output$panel_body <- renderUI({
      if (!show_panel()) {
        return(NULL)
      }

      tags$div(
        style = "margin-top: 10px;",
        # File upload for .cpp / .mod
        fileInput(
          ns("mrg_file"),
          "mrgsolve model (.cpp / .mod)",
          accept = c(".cpp", ".mod", ".txt"),
          width = "100%",
          placeholder = "Drop an mrgsolve file"
        ),

        # OR: text area
        tags$details(
          style = "margin-top: 6px;",
          tags$summary(
            style = "cursor: pointer; font-size: 0.85em; color: #6b7280;",
            "Or paste mrgsolve code"
          ),
          textAreaInput(
            ns("mrg_code"),
            label = NULL,
            rows = 8,
            width = "100%",
            placeholder = "$PARAM CL = 1, V = 10, KA = 0.5\n$CMT DEPOT CENTRAL\n$ODE\ndxdt_DEPOT = -KA * DEPOT;\ndxdt_CENTRAL = KA * DEPOT - (CL/V) * CENTRAL;\n$CAPTURE CP = CENTRAL / V;"
          )
        ),

        # Compile button
        actionButton(
          ns("compile"),
          "Compile model",
          class = "btn-sm",
          style = "margin-top: 8px;"
        ),

        # Compile status
        uiOutput(ns("compile_status")),

        # Parameter mapping validation
        uiOutput(ns("param_mapping")),

        # Dose configuration (shown when AMT or RATE is missing from .tab)
        uiOutput(ns("dose_config")),

        # Simulate button
        uiOutput(ns("simulate_btn"))
      )
    })

    # -- Model code source (file OR text area) --------------------------------
    mrg_code_text <- reactive({
      # File upload takes priority
      f <- input$mrg_file
      if (!is.null(f)) {
        return(paste(readLines(f$datapath, warn = FALSE), collapse = "\n"))
      }
      # Fallback to text area
      code <- input$mrg_code
      if (!is.null(code) && nchar(trimws(code)) > 0L) {
        return(code)
      }
      NULL
    })

    model_hash_reactive <- reactive({
      compiled_model_hash()
    })

    model_code_reactive <- reactive({
      compiled_model_code()
    })

    param_names_reactive <- reactive({
      mod <- compiled_model()
      if (is.null(mod) || !requireNamespace("mrgsolve", quietly = TRUE)) {
        return(character())
      }
      tryCatch(names(mrgsolve::param(mod)), error = function(e) character())
    })

    capture_names_reactive <- reactive({
      mod <- compiled_model()
      if (is.null(mod) || !requireNamespace("mrgsolve", quietly = TRUE)) {
        return(character())
      }
      tryCatch(
        .mrgsolve_outvar_names(mod, "capture"),
        error = function(e) character()
      )
    })

    cmt_names_reactive <- reactive({
      mod <- compiled_model()
      if (is.null(mod) || !requireNamespace("mrgsolve", quietly = TRUE)) {
        return(character())
      }
      tryCatch(
        .mrgsolve_outvar_names(mod, "cmt"),
        error = function(e) character()
      )
    })

    # -- Compile model ---------------------------------------------------------
    observeEvent(input$compile, {
      code <- mrg_code_text()
      if (is.null(code)) {
        compile_error("No mrgsolve code provided")
        compiled_model(NULL)
        compiled_model_code(NULL)
        compiled_model_hash(NULL)
        clear_sim_result()
        return()
      }

      compile_error(NULL)
      result <- compile_mrgsolve_model(code)

      if (is.character(result)) {
        compile_error(result)
        compiled_model(NULL)
        compiled_model_code(NULL)
        compiled_model_hash(NULL)
        clear_sim_result()
      } else {
        result_hash <- mrgsolve_code_hash(code, n_chars = 32L)
        compiled_model_code(code)
        compiled_model_hash(result_hash)
        compiled_model(result)
        compile_error(NULL)
        clear_sim_result()
      }
    })

    # -- Compile status UI -----------------------------------------------------
    output$compile_status <- renderUI({
      err <- compile_error()
      mod <- compiled_model()

      if (!is.null(err)) {
        return(tags$div(
          style = "margin-top: 8px; padding: 8px; background: #fef2f2; border-radius: 4px; font-size: 0.85em; color: #991b1b;",
          tags$strong("Error: "),
          err
        ))
      }

      if (!is.null(mod)) {
        n_params <- length(names(mrgsolve::param(mod)))
        n_cmt <- length(.mrgsolve_outvar_names(mod, "cmt"))
        return(tags$div(
          style = "margin-top: 8px; padding: 8px; background: #f0fdf4; border-radius: 4px; font-size: 0.85em; color: #166534;",
          tags$strong("Model compiled"),
          sprintf(" — %d parameters, %d compartments", n_params, n_cmt)
        ))
      }

      NULL
    })

    # -- Parameter mapping validation ------------------------------------------
    output$param_mapping <- renderUI({
      mod <- compiled_model()
      if (is.null(mod)) {
        return(NULL)
      }

      labels <- theta_labels()
      mrg_params <- names(mrgsolve::param(mod))

      mapping <- validate_param_mapping(mrg_params, labels)

      if (length(mapping$unmatched_mrg) == 0L && length(mapping$matched) > 0L) {
        return(tags$div(
          style = "margin-top: 6px; font-size: 0.82em; color: #166534;",
          sprintf(
            "Mapping OK: %d/%d parameters mapped",
            length(mapping$matched),
            length(mrg_params)
          )
        ))
      }

      # Show mapping table if issues found
      tbl <- mapping$mapping_table
      tags$div(
        style = "margin-top: 6px;",
        tags$p(
          style = "font-size: 0.82em; color: #92400e;",
          "Parameter mapping (THETA labels → mrgsolve names):"
        ),
        tags$table(
          class = "table table-sm",
          style = "font-size: 0.8em;",
          tags$thead(tags$tr(
            tags$th("mrgsolve"),
            tags$th("THETA"),
            tags$th("Status")
          )),
          tags$tbody(
            lapply(seq_len(nrow(tbl)), function(i) {
              row_style <- if (tbl$status[i] == "OK") "" else "color: #dc2626;"
              tags$tr(
                style = row_style,
                tags$td(tbl$mrgsolve[i]),
                tags$td(tbl$theta[i] %||% "-"),
                tags$td(tbl$status[i])
              )
            })
          )
        )
      )
    })

    # -- Dosing info from .tab -------------------------------------------------
    dose_schedule <- reactive({
      tab <- tab_data()
      if (is.null(tab)) {
        return(NULL)
      }
      extract_dosing_from_tab(tab)
    })

    needs_dose_input <- reactive({
      ds <- dose_schedule()
      if (is.null(ds)) {
        return(TRUE)
      }
      any(is.na(ds$amt)) || any(is.na(ds$rate))
    })

    dose_config <- reactive({
      ds <- dose_schedule()
      if (!needs_dose_input() || is.null(ds)) {
        return(NULL)
      }
      arm_ids <- sort(unique(ds$id[!is.na(ds$id)]))
      stats::setNames(
        lapply(arm_ids, function(aid) {
          arm_rows <- !is.na(ds$id) & ds$id == aid
          list(
            amt = if (any(is.na(ds$amt[arm_rows]))) {
              input[[paste0("amt_", aid)]] %||% 100
            } else {
              NULL
            },
            rate = if (any(is.na(ds$rate[arm_rows]))) {
              input[[paste0("rate_", aid)]] %||% 0
            } else {
              NULL
            }
          )
        }),
        as.character(arm_ids)
      )
    })

    dose_events <- reactive({
      ds <- dose_schedule()
      if (is.null(ds)) {
        return(NULL)
      }
      events <- build_dosing_events(ds, dose_config())
      if (is.null(events) || nrow(events) == 0L) {
        return(NULL)
      }
      events <- events[!is.na(events$amt), , drop = FALSE]
      if (nrow(events) == 0L) {
        return(NULL)
      }
      events
    })

    simulation_context <- reactive({
      list(
        model_hash = compiled_model_hash(),
        ext_data = ext_data(),
        tab_data = tab_data(),
        theta_labels = theta_labels(),
        dose_events = dose_events()
      )
    })

    current_sim_result <- reactive({
      result <- sim_result()
      context <- sim_context()
      if (is.null(result) || is.null(context)) {
        return(NULL)
      }
      if (!identical(context, simulation_context())) {
        return(NULL)
      }

      result
    })

    observeEvent(
      simulation_context(),
      clear_sim_result(),
      ignoreInit = TRUE,
      priority = 100
    )

    # -- Dose config UI (when AMT or RATE is missing) --------------------------
    output$dose_config <- renderUI({
      mod <- compiled_model()
      if (is.null(mod)) {
        return(NULL)
      }
      if (!needs_dose_input()) {
        return(NULL)
      }

      ds <- dose_schedule()
      if (is.null(ds)) {
        return(tags$div(
          style = "margin-top: 8px; padding: 8px; background: #fffbeb; border-radius: 4px; font-size: 0.85em;",
          "No dose events detected in .tab. ",
          "Verify the file contains EVID=1 lines."
        ))
      }

      # Detect unique arms
      arm_ids <- sort(unique(ds$id[!is.na(ds$id)]))
      missing_fields <- c(
        if (any(is.na(ds$amt))) "AMT",
        if (any(is.na(ds$rate))) "RATE"
      )

      tags$div(
        style = "margin-top: 8px;",
        tags$p(
          style = "font-size: 0.85em; font-weight: 600;",
          paste0(
            "Dose configuration (missing ",
            paste(missing_fields, collapse = " / "),
            " in .tab):"
          )
        ),
        lapply(arm_ids, function(aid) {
          arm_rows <- !is.na(ds$id) & ds$id == aid
          n_doses <- sum(arm_rows)
          needs_amt <- any(is.na(ds$amt[arm_rows]))
          needs_rate <- any(is.na(ds$rate[arm_rows]))
          tags$div(
            style = "display: flex; gap: 8px; align-items: center; margin-bottom: 4px;",
            tags$span(
              style = "font-size: 0.82em; min-width: 60px;",
              paste0("Arm ", aid, " (", n_doses, " doses):")
            ),
            if (needs_amt) {
              numericInput(
                ns(paste0("amt_", aid)),
                label = "AMT",
                value = 100,
                min = 0,
                step = 1,
                width = "100px"
              )
            },
            if (needs_rate) {
              numericInput(
                ns(paste0("rate_", aid)),
                label = "Rate",
                value = 0,
                min = 0,
                step = 0.1,
                width = "80px"
              )
            }
          )
        })
      )
    })

    # -- Simulate button -------------------------------------------------------
    output$simulate_btn <- renderUI({
      mod <- compiled_model()
      if (is.null(mod)) {
        return(NULL)
      }

      # Check if doses are ready
      ds <- dose_schedule()
      if (is.null(ds)) {
        return(NULL)
      }

      if (any(is.na(ds$amt))) {
        # Check if user has provided AMT inputs for arms that need them.
        arm_ids <- sort(unique(ds$id[!is.na(ds$id)]))
        missing_amt_arms <- arm_ids[vapply(
          arm_ids,
          function(aid) {
            arm_rows <- !is.na(ds$id) & ds$id == aid
            any(is.na(ds$amt[arm_rows]))
          },
          logical(1L)
        )]
        all_set <- all(vapply(
          missing_amt_arms,
          function(aid) {
            val <- input[[paste0("amt_", aid)]]
            !is.null(val) && !is.na(val) && val > 0
          },
          logical(1L)
        ))
        if (!all_set) return(NULL)
      }

      tags$div(
        style = "margin-top: 10px;",
        actionButton(
          ns("simulate"),
          "Simulate PK profile",
          class = "btn-sm btn-primary",
          style = "width: 100%;"
        )
      )
    })

    # -- Run simulation --------------------------------------------------------
    observeEvent(input$simulate, {
      mod <- compiled_model()
      req(mod)

      ext <- ext_data()
      labels <- theta_labels()

      # Extract params from .ext
      mrg_params <- extract_params_for_mrgsolve(ext, labels)
      if (is.null(mrg_params)) {
        clear_sim_result()
        return()
      }

      events <- dose_events()
      if (is.null(events) || nrow(events) == 0L) {
        clear_sim_result()
        return()
      }

      # Simulate
      result <- simulate_pk_profile(
        mod = mod,
        params = mrg_params$params,
        omega = mrg_params$omega,
        sigma = mrg_params$sigma,
        events = events,
        delta = 0.5
      )

      sim_context(simulation_context())
      sim_result(result)
    })

    # -- Internal helper: detect obvious param-mapping failure ------------------
    validate_param_mapping_mismatch <- reactive({
      sim <- current_sim_result()
      if (is.null(sim) || nrow(sim) == 0L) {
        return(NA)
      }
      if (!"IPRED" %in% names(sim)) {
        return(NA)
      }
      ipred <- sim$IPRED
      all(is.na(ipred)) || all(!is.na(ipred) & ipred == 0)
    })

    # -- Tier reactive ---------------------------------------------------------
    tier_reactive <- reactive({
      if (!mrg_status$available) {
        return("unavailable")
      }
      if (!is.null(compile_error())) {
        return("compile_failed")
      }
      if (is.null(current_sim_result())) {
        return("no_sim")
      }
      if (isTRUE(validate_param_mapping_mismatch())) {
        return("param_mismatch")
      }
      "mrgsolve"
    })

    status_reactive <- reactive({
      if (!mrg_status$available) {
        return("unavailable")
      }
      if (!is.null(compile_error())) {
        return("compile_failed")
      }
      if (!is.null(compiled_model())) {
        return("compiled")
      }
      "not_compiled"
    })

    # -- Warning reason reactive -----------------------------------------------
    warning_reason_reactive <- reactive({
      switch(
        tier_reactive(),
        unavailable = paste(
          "Rtools/mrgsolve not installed -",
          mrg_status$reason %||%
            "install mrgsolve + Rtools for smooth PK curves"
        ),
        compile_failed = paste(
          "mrgsolve model failed to compile:",
          compile_error() %||% "unknown error"
        ),
        no_sim = NULL,
        param_mismatch = "Parameter mapping incomplete - using template fallback",
        NULL
      )
    })

    is_available_reactive <- reactive({
      identical(tier_reactive(), "mrgsolve")
    })

    # -- Return named list of reactives ----------------------------------------
    shared_state_contract <- list(
      model = reactive({
        compiled_model()
      }),
      compile_error = reactive({
        compile_error()
      }),
      model_code = model_code_reactive,
      model_hash = model_hash_reactive,
      param_names = param_names_reactive,
      capture_names = capture_names_reactive,
      cmt_names = cmt_names_reactive,
      is_compiled = reactive({
        !is.null(compiled_model())
      }),
      status = status_reactive,
      sim_data = current_sim_result,
      is_available = is_available_reactive,
      dose_events = dose_events,
      dose_times = reactive({
        ds <- dose_schedule()
        if (is.null(ds)) {
          return(NULL)
        }
        unique(ds$time)
      }),
      tier = tier_reactive,
      warning_reason = warning_reason_reactive
    )

    shared_state_contract
  })
}
