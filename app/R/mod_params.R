# =============================================================================
# mod_params.R — Onglet Parametres (TABLE 3 style, multi-runs en colonnes)
# =============================================================================

mod_params_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("table3_ui"))
}

mod_params_server <- function(id, ext_data, shk_data, ext_lines, tbl_no,
                               param_labels,
                               all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Helpers — construire les sections de la TABLE 3 depuis all_runs
    # -------------------------------------------------------------------------

    rse_long <- reactive({
      runs <- all_runs()
      if (length(runs) == 0L) return(tibble(metric = character(), value = numeric(), run = character()))
      tbl  <- tbl_no()
      lbls <- param_labels()

      purrr::map_dfr(runs, function(r) {
        if (is.null(r$ext_data)) return(NULL)
        rse <- tryCatch(get_rse(r$ext_data, tbl), error = function(e) NULL)
        if (is.null(rse) || nrow(rse) == 0L) return(NULL)
        rse |>
          mutate(
            label  = ifelse(!is.null(lbls) & param %in% names(lbls), lbls[param], param),
            metric = paste0("%RSE(", label, ")"),
            value  = round(rse_pct, 2),
            run    = r$name
          ) |>
          select(metric, value, run)
      })
    })

    shk_long <- reactive({
      runs <- all_runs()
      if (length(runs) == 0L) return(tibble(metric = character(), value = numeric(), run = character()))
      tbl <- tbl_no()

      purrr::map_dfr(runs, function(r) {
        if (is.null(r$shk_data)) return(NULL)
        shk <- tryCatch(get_shrinkage(r$shk_data, tbl), error = function(e) NULL)
        if (is.null(shk) || nrow(shk) == 0L) return(NULL)
        shk |>
          mutate(
            metric = paste0("%SHK(", eta, ")"),
            value  = round(shrinkage_pct, 2),
            run    = r$name
          ) |>
          select(metric, value, run)
      })
    })

    times_long <- reactive({
      runs <- all_runs()
      if (length(runs) == 0L) return(tibble(metric = character(), value = numeric(), run = character()))

      purrr::map_dfr(runs, function(r) {
        if (is.null(r$tab_data)) return(NULL)
        tab <- r$tab_data
        if ("EVID" %in% names(tab)) tab <- filter(tab, EVID == 0)
        if (nrow(tab) == 0L) return(NULL)

        if ("TSTRAT" %in% names(tab)) {
          strats <- sort(unique(tab$TSTRAT))
          tibble(
            metric = paste0("Time TSTRAT=", strats),
            value  = round(vapply(strats, function(s) tab$TIME[tab$TSTRAT == s][1], numeric(1)), 4),
            run    = r$name
          )
        } else if ("TIME" %in% names(tab)) {
          tibble(metric = "Time", value = round(tab$TIME[1], 4), run = r$name)
        } else {
          NULL
        }
      })
    })

    run_names <- reactive({
      runs <- all_runs()
      unname(vapply(runs, function(r) r$name, character(1)))
    })

    # -------------------------------------------------------------------------
    # TABLE 3 — layout UI dynamique
    # -------------------------------------------------------------------------
    output$table3_ui <- renderUI({
      runs <- all_runs()
      if (length(runs) == 0L || all(sapply(runs, function(r) is.null(r$ext_data)))) {
        return(div(class = "alert alert-info", style = "border-radius:10px;",
                   "Chargez un fichier .ext pour commencer l'analyse."))
      }

      has_shk   <- nrow(shk_long()) > 0L
      has_times <- nrow(times_long()) > 0L

      tagList(
        div(class = "param-table-wrap",
            p(class = "section-title",
              HTML("Crit\u00e8re d'optimalit\u00e9 \u2014 <em>-log(det(FIM))</em>")),
            DTOutput(ns("dt_ofv"))
        ),
        br(),
        div(class = "param-table-wrap",
            p(class = "section-title", "%RSE par param\u00e8tre"),
            DTOutput(ns("dt_rse"))
        ),
        if (has_shk) tagList(
          br(),
          div(class = "param-table-wrap",
              p(class = "section-title", "Shrinkage EBV (%) par ETA"),
              DTOutput(ns("dt_shk"))
          )
        ),
        if (has_times) tagList(
          br(),
          div(class = "param-table-wrap",
              p(class = "section-title", "Temps d'echantillonnage optimaux (h)"),
              DTOutput(ns("dt_times"))
          )
        )
      )
    })

    # -------------------------------------------------------------------------
    # DT — Critere OFV
    # -------------------------------------------------------------------------
    output$dt_ofv <- renderDT({
      runs <- all_runs(); req(length(runs) > 0L)
      tbl  <- tbl_no()

      ofv_vals <- vapply(runs, function(r) {
        if (is.null(r$ext_data)) return(NA_real_)
        val <- tryCatch(get_ofv(r$ext_data, tbl), error = function(e) NA_real_)
        if (length(val) == 0L) return(NA_real_)
        round(val, 4)
      }, numeric(1))
      names(ofv_vals) <- vapply(runs, function(r) r$name, character(1))

      ext_for_crit <- ext_data()
      lines <- ext_lines() %||% if (!is.null(ext_for_crit)) attr(ext_for_crit, "ext_lines") else NULL
      crit  <- if (!is.null(lines)) detect_criterion(lines) else "D-OPTIMALITY"

      df <- as.data.frame(
        c(list(Metrique = paste0("\u2212log(det(FIM)) [", crit, "]")),
          as.list(ofv_vals)),
        check.names = FALSE
      )

      datatable(df, rownames = FALSE, class = "stripe compact",
                options = list(dom = "t", ordering = FALSE))
    })

    # -------------------------------------------------------------------------
    # DT — %RSE
    # -------------------------------------------------------------------------
    output$dt_rse <- renderDT({
      long <- rse_long()
      req(nrow(long) > 0L)

      rnms      <- run_names()
      safe_rnms <- make.names(rnms)

      wide <- long |>
        pivot_wider(names_from = run, values_from = value) |>
        rename(Parametre = metric) |>
        select(Parametre, any_of(rnms))

      dt <- datatable(
        wide, rownames = FALSE, class = "stripe hover compact",
        options = list(pageLength = 30, dom = "tip", ordering = FALSE)
      )

      # Utiliser les indices de colonnes (pas les noms) pour que formatStyle
      # reste stable meme si le nom de la run change apres chargement
      run_col_indices <- which(names(wide) %in% rnms)
      for (col_idx in run_col_indices) {
        dt <- dt |>
          formatStyle(col_idx,
            color      = styleInterval(c(20, 50), c("#16a34a", "#d97706", "#dc2626")),
            fontWeight = "bold"
          )
      }
      dt
    })

    # -------------------------------------------------------------------------
    # DT — Shrinkage EBV
    # -------------------------------------------------------------------------
    output$dt_shk <- renderDT({
      long <- shk_long()
      req(nrow(long) > 0L)

      rnms <- run_names()

      wide <- long |>
        pivot_wider(names_from = run, values_from = value) |>
        rename(ETA = metric) |>
        select(ETA, any_of(rnms))

      dt <- datatable(
        wide, rownames = FALSE, class = "stripe hover compact",
        options = list(dom = "t", ordering = FALSE)
      )

      run_col_indices <- which(names(wide) %in% rnms)
      for (col_idx in run_col_indices) {
        dt <- dt |>
          formatStyle(col_idx,
            color      = styleInterval(30, c("#16a34a", "#dc2626")),
            fontWeight = "bold"
          )
      }
      dt
    })

    # -------------------------------------------------------------------------
    # DT — Temps optimaux
    # -------------------------------------------------------------------------
    output$dt_times <- renderDT({
      long <- times_long()
      req(nrow(long) > 0L)

      rnms <- run_names()

      wide <- long |>
        pivot_wider(names_from = run, values_from = value) |>
        rename(Temps = metric) |>
        select(Temps, any_of(rnms))

      datatable(
        wide, rownames = FALSE, class = "stripe hover compact",
        options = list(dom = "t", ordering = FALSE)
      )
    })
  })
}
