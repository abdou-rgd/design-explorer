# =============================================================================
# mod_mrgsolve.R — Module mrgsolve : upload modele, config doses, simulation
# =============================================================================

mod_mrgsolve_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("mrgsolve_panel"))
}

mod_mrgsolve_server <- function(id, ext_data, tab_data, ctl_lines = reactive(NULL),
                                theta_labels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Check mrgsolve availability ------------------------------------------
    mrg_status <- has_mrgsolve()

    # -- Compiled model object (reactiveVal) ----------------------------------
    compiled_model <- reactiveVal(NULL)
    compile_error  <- reactiveVal(NULL)
    sim_result     <- reactiveVal(NULL)

    # -- UI: conditional on mrgsolve availability -----------------------------
    output$mrgsolve_panel <- renderUI({
      if (!mrg_status$available) {
        return(tags$div(
          class = "surface-card",
          style = "padding: 12px; margin-bottom: 12px; background: #fffbeb; border-left: 3px solid #f59e0b;",
          tags$strong("Simulation PK non disponible"),
          tags$p(style = "margin: 4px 0 0 0; font-size: 0.85em; color: #92400e;",
            mrg_status$reason,
            " — Installez mrgsolve et Rtools pour obtenir des courbes PK lisses."
          )
        ))
      }

      tags$div(class = "surface-card",
        style = "padding: 12px; margin-bottom: 12px;",

        # Header with collapse toggle
        tags$div(
          style = "display: flex; align-items: center; gap: 8px; cursor: pointer;",
          onclick = paste0("Shiny.setInputValue('", ns("toggle_panel"), "', Math.random())"),
          tags$strong("Simulation PK (mrgsolve)"),
          icon("chevron-down", style = "font-size: 0.85em; color: #6b7280;")
        ),

        # Collapsible body (rendered server-side)
        uiOutput(ns("panel_body"))
      )
    })

    # -- Toggle panel visibility -----------------------------------------------
    show_panel <- reactiveVal(FALSE)
    observeEvent(input$toggle_panel, {
      show_panel(!show_panel())
    })

    output$panel_body <- renderUI({
      if (!show_panel()) return(NULL)

      tags$div(style = "margin-top: 10px;",
        # File upload for .cpp / .mod
        fileInput(ns("mrg_file"), "Modele mrgsolve (.cpp / .mod)",
          accept = c(".cpp", ".mod", ".txt"),
          width = "100%",
          placeholder = "Deposer un fichier mrgsolve"
        ),

        # OR: text area
        tags$details(style = "margin-top: 6px;",
          tags$summary(style = "cursor: pointer; font-size: 0.85em; color: #6b7280;",
            "Ou coller le code mrgsolve"),
          textAreaInput(ns("mrg_code"), label = NULL,
            rows = 8, width = "100%",
            placeholder = "$PARAM CL = 1, V = 10, KA = 0.5\n$CMT DEPOT CENTRAL\n$ODE\ndxdt_DEPOT = -KA * DEPOT;\ndxdt_CENTRAL = KA * DEPOT - (CL/V) * CENTRAL;\n$CAPTURE CP = CENTRAL / V;"
          )
        ),

        # Compile button
        actionButton(ns("compile"), "Compiler le modele",
          class = "btn-sm", style = "margin-top: 8px;"),

        # Compile status
        uiOutput(ns("compile_status")),

        # Parameter mapping validation
        uiOutput(ns("param_mapping")),

        # Dose configuration (shown only when AMT missing from .tab)
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

    # -- Compile model ---------------------------------------------------------
    observeEvent(input$compile, {
      code <- mrg_code_text()
      if (is.null(code)) {
        compile_error("Aucun code mrgsolve fourni")
        compiled_model(NULL)
        sim_result(NULL)
        return()
      }

      compile_error(NULL)
      result <- compile_mrgsolve_model(code)

      if (is.character(result)) {
        compile_error(result)
        compiled_model(NULL)
        sim_result(NULL)
      } else {
        compiled_model(result)
        compile_error(NULL)
        sim_result(NULL)  # Reset sim when model changes
      }
    })

    # -- Compile status UI -----------------------------------------------------
    output$compile_status <- renderUI({
      err <- compile_error()
      mod <- compiled_model()

      if (!is.null(err)) {
        return(tags$div(
          style = "margin-top: 8px; padding: 8px; background: #fef2f2; border-radius: 4px; font-size: 0.85em; color: #991b1b;",
          tags$strong("Erreur: "), err
        ))
      }

      if (!is.null(mod)) {
        n_params <- length(names(mrgsolve::param(mod)))
        n_cmt <- length(mrgsolve::cmt(mod))
        return(tags$div(
          style = "margin-top: 8px; padding: 8px; background: #f0fdf4; border-radius: 4px; font-size: 0.85em; color: #166534;",
          tags$strong("Modele compile"),
          sprintf(" — %d parametres, %d compartiments", n_params, n_cmt)
        ))
      }

      NULL
    })

    # -- Parameter mapping validation ------------------------------------------
    output$param_mapping <- renderUI({
      mod <- compiled_model()
      if (is.null(mod)) return(NULL)

      labels <- theta_labels()
      mrg_params <- names(mrgsolve::param(mod))

      mapping <- validate_param_mapping(mrg_params, labels)

      if (length(mapping$unmatched_mrg) == 0L && length(mapping$matched) > 0L) {
        return(tags$div(
          style = "margin-top: 6px; font-size: 0.82em; color: #166534;",
          sprintf("Mapping OK : %d/%d parametres mappes",
                  length(mapping$matched), length(mrg_params))
        ))
      }

      # Show mapping table if issues found
      tbl <- mapping$mapping_table
      tags$div(style = "margin-top: 6px;",
        tags$p(style = "font-size: 0.82em; color: #92400e;",
          "Mapping parametres (labels THETA -> noms mrgsolve):"),
        tags$table(class = "table table-sm", style = "font-size: 0.8em;",
          tags$thead(tags$tr(
            tags$th("mrgsolve"), tags$th("THETA"), tags$th("Status")
          )),
          tags$tbody(
            lapply(seq_len(nrow(tbl)), function(i) {
              row_style <- if (tbl$status[i] == "OK") "" else "color: #dc2626;"
              tags$tr(style = row_style,
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
      if (is.null(tab)) return(NULL)
      extract_dosing_from_tab(tab)
    })

    needs_dose_input <- reactive({
      ds <- dose_schedule()
      if (is.null(ds)) return(TRUE)
      any(is.na(ds$amt))
    })

    # -- Dose config UI (only when AMT missing) --------------------------------
    output$dose_config <- renderUI({
      mod <- compiled_model()
      if (is.null(mod)) return(NULL)
      if (!needs_dose_input()) return(NULL)

      ds <- dose_schedule()
      if (is.null(ds)) {
        return(tags$div(
          style = "margin-top: 8px; padding: 8px; background: #fffbeb; border-radius: 4px; font-size: 0.85em;",
          "Aucun evenement de dose detecte dans le .tab. ",
          "Verifiez que le fichier contient des lignes EVID=1."
        ))
      }

      # Detect unique arms
      arm_ids <- sort(unique(ds$id))

      tags$div(style = "margin-top: 8px;",
        tags$p(style = "font-size: 0.85em; font-weight: 600;",
          "Dose (AMT absent du .tab) :"),
        lapply(arm_ids, function(aid) {
          n_doses <- sum(ds$id == aid)
          tags$div(style = "display: flex; gap: 8px; align-items: center; margin-bottom: 4px;",
            tags$span(style = "font-size: 0.82em; min-width: 60px;",
              paste0("Arm ", aid, " (", n_doses, " doses):")),
            numericInput(ns(paste0("amt_", aid)), label = NULL,
              value = 100, min = 0, step = 1, width = "100px"),
            numericInput(ns(paste0("rate_", aid)), label = "Rate",
              value = 0, min = 0, step = 0.1, width = "80px")
          )
        })
      )
    })

    # -- Simulate button -------------------------------------------------------
    output$simulate_btn <- renderUI({
      mod <- compiled_model()
      if (is.null(mod)) return(NULL)

      # Check if doses are ready
      ds <- dose_schedule()
      if (is.null(ds)) return(NULL)

      if (needs_dose_input()) {
        # Check if user has provided AMT inputs
        arm_ids <- sort(unique(ds$id))
        all_set <- all(vapply(arm_ids, function(aid) {
          val <- input[[paste0("amt_", aid)]]
          !is.null(val) && !is.na(val) && val > 0
        }, logical(1L)))
        if (!all_set) return(NULL)
      }

      tags$div(style = "margin-top: 10px;",
        actionButton(ns("simulate"), "Simuler le profil PK",
          class = "btn-sm btn-primary",
          style = "width: 100%;")
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
        sim_result(NULL)
        return()
      }

      # Build dosing events
      ds <- dose_schedule()
      dose_config <- NULL

      if (needs_dose_input() && !is.null(ds)) {
        arm_ids <- sort(unique(ds$id))
        dose_config <- setNames(lapply(arm_ids, function(aid) {
          list(
            amt  = input[[paste0("amt_", aid)]] %||% 100,
            rate = input[[paste0("rate_", aid)]] %||% 0
          )
        }), as.character(arm_ids))
      }

      events <- build_dosing_events(ds, dose_config)
      if (is.null(events) || nrow(events) == 0L) {
        sim_result(NULL)
        return()
      }

      # Remove rows where AMT is still NA
      events <- events[!is.na(events$amt), , drop = FALSE]
      if (nrow(events) == 0L) {
        sim_result(NULL)
        return()
      }

      # Simulate
      result <- simulate_pk_profile(
        mod    = mod,
        params = mrg_params$params,
        omega  = mrg_params$omega,
        sigma  = mrg_params$sigma,
        events = events,
        delta  = 0.5
      )

      sim_result(result)
    })

    # -- Return named list of reactives ----------------------------------------
    list(
      sim_data     = reactive({ sim_result() }),
      is_available = reactive({
        !is.null(sim_result()) && nrow(sim_result()) > 0L
      }),
      dose_times   = reactive({
        ds <- dose_schedule()
        if (is.null(ds)) return(NULL)
        unique(ds$time)
      })
    )
  })
}
