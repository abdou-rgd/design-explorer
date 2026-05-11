# =============================================================================
# mod_params.R — Onglet Parametres (TABLE 3 style, multi-runs en colonnes)
# =============================================================================

mod_params_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("table3_ui"))
  )
}

mod_params_server <- function(id, ext_data, shk_data, ext_lines,
                               param_labels,
                               tbl_no,
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
        # For robust runs, use only subproblem 1
        if ("table_no" %in% names(tab) && n_distinct(tab$table_no) > 1L)
          tab <- dplyr::filter(tab, table_no == 1L)
        if ("EVID" %in% names(tab)) tab <- dplyr::filter(tab, EVID == 0)
        if (nrow(tab) == 0L) return(NULL)

        if ("TSTRAT" %in% names(tab)) {
          strats <- sort(unique(tab$TSTRAT))
          tibble(
            metric = paste0("Time TSTRAT=", strats),
            value  = round(vapply(strats, function(s) tab$TIME[tab$TSTRAT == s][1], numeric(1)), 4),
            run    = r$name
          )
        } else if ("TIME" %in% names(tab)) {
          tibble(
            metric = paste0("Time ", seq_len(nrow(tab))),
            value  = round(tab$TIME, 4),
            run    = r$name
          )
        } else {
          NULL
        }
      })
    })

    run_names <- reactive({
      runs <- all_runs()
      unname(vapply(runs, function(r) r$name %||% "?", character(1)))
    })

    # -------------------------------------------------------------------------
    # TABLE 3 — layout UI dynamique
    # -------------------------------------------------------------------------
    output$table3_ui <- renderUI({
      runs <- all_runs()
      if (length(runs) == 0L || all(sapply(runs, function(r) is.null(r$ext_data)))) {
        return(page_shell(
          page_header(
            "Parameters",
            "Unified Table 3 style comparison of criteria, precision, shrinkage, and optimal times.",
            eyebrow = "Results"
          ),
          empty_state("Load a .ext file", "Parameters are available after a primary NONMEM .ext file is loaded.", "table")
        ))
      }

      page_shell(
        page_header(
          "Parameters",
          "Unified Table 3 style comparison of criteria, precision, shrinkage, and optimal times.",
          eyebrow = "Results"
        ),
        status_panel(
          "Metric definitions",
          tags$p("OFV, D-efficiency, robust D-criterion, RSE, and shrinkage definitions are collected in Documentation."),
          doc_link("parameters", "Open parameter documentation"),
          tone = "info",
          icon_name = "book-open"
        ),
        table_panel(
          "Parameters - run comparison",
          DTOutput(ns("dt_all")),
          subtitle = "Rows are grouped by criterion, precision, shrinkage, and time outputs."
        )
      )
    })


    # -------------------------------------------------------------------------
    # Unified Parameters table
    # Single DT grouping OFV / RSE (THETA/OMEGA/SIGMA) / Shrinkage / Times.
    # Column 0 is the Group key (hidden, drives DT RowGroup section headers).
    # Row colours applied via rowCallback JS, per-group thresholds:
    #   %RSE*      : 20 / 50 / 100 (green / orange / red / dark red)
    #   Shrinkage  : 30            (green / red)
    #   Others     : no colouring
    # -------------------------------------------------------------------------
    params_table_df <- reactive({
      runs <- all_runs(); req(length(runs) > 0L)
      tbl  <- tbl_no()
      rnms <- run_names()
      fmt  <- function(x, d = 2) ifelse(is.na(x), NA_character_,
                                        formatC(x, format = "f", digits = d))

      sections <- list()

      # --- OFV + D-efficiency + Robust D-criterion --------------------------
      ofv_vals <- vapply(runs, function(r) {
        if (is.null(r$ext_data)) return(NA_real_)
        val <- tryCatch(get_ofv(r$ext_data, tbl), error = function(e) NA_real_)
        if (length(val) == 0L) return(NA_real_)
        round(val, 4)
      }, numeric(1))
      names(ofv_vals) <- rnms

      ext_for_crit <- ext_data()
      ext_ln <- ext_lines() %||% if (!is.null(ext_for_crit)) attr(ext_for_crit, "ext_lines") else NULL
      crit   <- if (!is.null(ext_ln)) detect_criterion(ext_ln) else "D-OPTIMALITY"

      ofv_label <- if (crit == "D-OPTIMALITY") {
        "OFV  [−log(det FIM)]"
      } else {
        paste0("OFV  [", crit, "]")
      }
      ofv_row_vals <- setNames(lapply(ofv_vals, function(v) fmt(v, 4)), rnms)
      ofv_rows <- list(c(list(Group = "Optimality criterion",
                              Parameter = ofv_label), ofv_row_vals))

      primary_ofv <- ofv_vals[[1]]
      has_cmp <- length(ofv_vals) >= 2L && !is.na(primary_ofv) && any(!is.na(ofv_vals[-1]))
      if (has_cmp) {
        # ΔOFV row: raw scale difference (OFV_ref - OFV_run).
        # For D-optimality: positive = run is more informative than ref.
        delta_disp <- vapply(seq_along(ofv_vals), function(i) {
          if (i == 1L) return("ref")
          if (is.na(ofv_vals[i])) return(NA_character_)
          sprintf("%+.4f", round(primary_ofv - ofv_vals[i], 4))
        }, character(1))
        names(delta_disp) <- rnms
        ofv_rows[[length(ofv_rows) + 1L]] <- c(
          list(Group = "Optimality criterion",
               Parameter = "ΔOFV vs ref"),
          as.list(delta_disp))

        # D-efficiency (D-optimality) or relative %ΔOFV (other criteria).
        n_params <- tryCatch(nrow(get_rse(runs[[1]]$ext_data, tbl)),
                             error = function(e) NA_integer_)
        if (crit == "D-OPTIMALITY") {
          eff_disp <- vapply(seq_along(ofv_vals), function(i) {
            if (i == 1L) return("ref")
            if (is.na(ofv_vals[i]) || is.na(n_params) || n_params == 0L) return(NA_character_)
            sprintf("%+.2f%%", round((exp((primary_ofv - ofv_vals[i]) / n_params) - 1) * 100, 2))
          }, character(1))
          eff_label <- "D-efficiency vs ref"
        } else {
          eff_disp <- vapply(seq_along(ofv_vals), function(i) {
            if (i == 1L) return("ref")
            if (is.na(ofv_vals[i]) || primary_ofv == 0) return(NA_character_)
            sprintf("%+.2f%%", round((primary_ofv - ofv_vals[i]) / abs(primary_ofv) * 100, 2))
          }, character(1))
          eff_label <- "Relative ΔOFV vs ref"
        }
        names(eff_disp) <- rnms
        ofv_rows[[length(ofv_rows) + 1L]] <- c(list(Group = "Optimality criterion",
                                                    Parameter = eff_label), as.list(eff_disp))
      }

      # Robust D-criterion (Monte-Carlo designs only)
      if (crit == "D-OPTIMALITY") {
        primary_ext <- runs[[1]]$ext_data
        if (!is.null(primary_ext)) {
          n_params_r <- tryCatch(nrow(get_rse(primary_ext, tbl)),
                                 error = function(e) NA_integer_)
          rdc <- tryCatch(get_robust_d_criterion(primary_ext, n_params_r),
                          error = function(e) NULL)
          if (!is.null(rdc)) {
            d_str <- sprintf("%.4f [%.4f – %.4f]", rdc$d_robust, rdc$d_p10, rdc$d_p90)
            rdc_vals <- setNames(
              lapply(seq_along(runs), function(i) if (i == 1L) d_str else NA_character_),
              rnms
            )
            ofv_rows[[length(ofv_rows) + 1L]] <- c(
              list(Group = "Optimality criterion",
                   Parameter = "Robust D-criterion  [P10-P90]"),
              rdc_vals
            )
          }
        }
      }
      # check.names=FALSE preserves run names with '/', '=', '(' etc. that
      # would otherwise be sanitised by make.names() and stop matching rnms.
      sections$ofv <- dplyr::bind_rows(lapply(
        ofv_rows,
        function(r) as.data.frame(r, check.names = FALSE, stringsAsFactors = FALSE)
      ))

      # --- RSE split by param type ------------------------------------------
      rse <- rse_long()
      if (nrow(rse) > 0L) {
        rse_wide <- rse |>
          pivot_wider(names_from = run, values_from = value) |>
          rename(Parameter = metric)
        for (col in rnms) {
          if (col %in% names(rse_wide)) {
            rse_wide[[col]] <- vapply(rse_wide[[col]], function(v) fmt(v, 2), character(1))
          }
        }
        rse_wide$Group <- dplyr::case_when(
          grepl("THETA", rse_wide$Parameter) ~ "%RSE - THETA",
          grepl("OMEGA", rse_wide$Parameter) ~ "%RSE - OMEGA",
          grepl("SIGMA", rse_wide$Parameter) ~ "%RSE - SIGMA",
          TRUE                                ~ "%RSE - other"
        )
        rse_wide <- rse_wide[, c("Group", "Parameter", rnms[rnms %in% names(rse_wide)])]
        rse_wide <- rse_wide[order(match(rse_wide$Group,
                                        c("%RSE - THETA", "%RSE - OMEGA",
                                          "%RSE - SIGMA", "%RSE - other"))), ]
        sections$rse <- rse_wide
      }

      # --- Shrinkage --------------------------------------------------------
      shk <- shk_long()
      if (nrow(shk) > 0L) {
        shk_wide <- shk |>
          pivot_wider(names_from = run, values_from = value) |>
          rename(Parameter = metric) |>
          mutate(Group = "EBV Shrinkage") |>
          select(Group, Parameter, any_of(rnms))
        for (col in rnms) {
          if (col %in% names(shk_wide)) {
            shk_wide[[col]] <- vapply(shk_wide[[col]], function(v) fmt(v, 2), character(1))
          }
        }
        sections$shk <- shk_wide
      }

      # --- Optimal sampling times -------------------------------------------
      tms <- times_long()
      if (nrow(tms) > 0L) {
        tms_wide <- tms |>
          pivot_wider(names_from = run, values_from = value) |>
          rename(Parameter = metric) |>
          mutate(Group = "Optimal sampling times (h)") |>
          select(Group, Parameter, any_of(rnms))
        for (col in rnms) {
          if (col %in% names(tms_wide)) {
            tms_wide[[col]] <- vapply(tms_wide[[col]], function(v) fmt(v, 3), character(1))
          }
        }
        sections$times <- tms_wide
      }

      out <- dplyr::bind_rows(sections)
      for (col in rnms) {
        if (col %in% names(out)) {
          out[[col]] <- ifelse(is.na(out[[col]]), "—", as.character(out[[col]]))
        } else {
          out[[col]] <- "—"
        }
      }
      out[, c("Group", "Parameter", rnms)]
    })

    output$dt_all <- renderDT({
      df <- params_table_df(); req(!is.null(df), nrow(df) > 0L)
      rnms <- run_names()

      n_runs <- length(rnms)
      # JS rowCallback applies per-group conditional colours to run cells.
      # It also paints light in-cell bars, scaled within each metric row, so
      # differences across runs remain visible in dense comparison tables.
      # data[0] = Group (hidden), data[1] = Parameter, data[2..n+1] = runs.
      # DOM td indices are offset -1 because the Group column is hidden:
      # td:eq(0) = Parameter, td:eq(1..n) = run cells.
      js_cb <- DT::JS(sprintf("
        function(row, data) {
          var group = String(data[0] || '');
          var nRuns = %d;
          var barColors = ['37,99,235', '22,163,74', '217,119,6', '124,58,237', '220,38,38', '8,145,178'];
          var nums = [];
          for (var i = 0; i < nRuns; i++) {
            var dataIdx = 2 + i;
            var raw     = String(data[dataIdx] || '');
            var num     = parseFloat(raw.replace(/[^0-9.\\-]/g, ''));
            nums.push(isNaN(num) ? null : Math.abs(num));
          }
          var maxAbs = Math.max.apply(null, nums.filter(function(x) { return x !== null; }));
          if (!isFinite(maxAbs) || maxAbs <= 0) maxAbs = null;
          for (var i = 0; i < nRuns; i++) {
            var dataIdx = 2 + i;
            var domIdx  = 1 + i;
            var raw     = String(data[dataIdx] || '');
            var num     = parseFloat(raw.replace(/[^0-9.\\-]/g, ''));
            var color   = null;
            var cell    = $('td:eq(' + domIdx + ')', row);
            if (group.indexOf('%%RSE') === 0) {
              if (!isNaN(num)) {
                if (num < 20)       color = '#16a34a';
                else if (num < 50)  color = '#d97706';
                else if (num < 100) color = '#dc2626';
                else                color = '#7f1d1d';
              }
            } else if (group === 'EBV Shrinkage') {
              if (!isNaN(num)) color = num < 30 ? '#16a34a' : '#dc2626';
            }
            if (maxAbs !== null && !isNaN(num)) {
              var pct = Math.max(4, Math.min(100, Math.abs(num) / maxAbs * 100));
              var rgb = barColors[i %% barColors.length];
              cell.css({
                'background': 'linear-gradient(90deg, rgba(' + rgb + ',0.18) 0%%, rgba(' + rgb + ',0.18) ' + pct + '%%, transparent ' + pct + '%%)',
                'backgroundClip': 'padding-box'
              });
            }
            if (color) {
              cell.css({color: color, 'fontWeight': 'bold'});
            }
          }
        }
      ", n_runs))

      datatable(
        df, rownames = FALSE, class = "stripe hover compact",
        extensions = "RowGroup",
        options = list(
          dom = "t", ordering = FALSE, pageLength = -1,
          rowGroup   = list(dataSrc = 0),
          columnDefs = list(list(visible = FALSE, targets = 0)),
          rowCallback = js_cb
        )
      )
    })


  })
}
