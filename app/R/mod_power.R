# =============================================================================
# mod_power.R — Onglet Power / NSN / Equivalence TOST (Decision)
#
# Calcul de puissance (Wald) et nombre de sujets necessaire (NSN) a partir
# des SE predites par la FIM. Formules portees de PopED (Retout et al. 2007).
# Test d'equivalence TOST : formules PFIM (user guide, eq. 4-7).
# =============================================================================

mod_power_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("content")))
}

mod_power_server <- function(id, ext_data, param_labels,
                             tbl_no = reactive(NULL),
                             suggested_groupsize = reactive(1L),
                             reset_trigger = reactive(0L),
                             all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Design-level reactives -----------------------------------------------
    groupsize_r <- reactive({
      val <- input$groupsize
      if (is.null(val) || is.na(val) || val < 1L) 1L else as.integer(val)
    })

    # Auto-fill groupsize from .ctl when upload/example suggests one
    observeEvent(suggested_groupsize(), {
      gs <- suggested_groupsize()
      if (!is.na(gs) && gs >= 1L) updateNumericInput(session, "groupsize", value = gs)
    }, ignoreInit = TRUE)

    # Reset handler
    observeEvent(reset_trigger(), {
      updateNumericInput(session, "groupsize", value = 1L)
    }, ignoreInit = TRUE)

    # -- Sync N total from parsed groupsize (auto-fill depuis .ctl) -----------
    observeEvent(groupsize_r(), {
      gs <- groupsize_r()
      if (!is.null(gs) && !is.na(gs) && gs >= 1L) {
        updateNumericInput(session, "n_total", value = gs)
      }
    })

    # -- Local N reactive (from module input) ----------------------------------
    n_total_r <- reactive({
      val <- input$n_total
      if (is.null(val) || is.na(val) || val < 1L) groupsize_r() else as.integer(val)
    })

    # -- RSE table reactive (from primary run) ---------------------------------
    rse_r <- reactive({
      ext <- ext_data(); req(ext)
      get_rse(ext, tbl_no())
    })

    # -- Power table reactive --------------------------------------------------
    power_tbl <- reactive({
      ext <- ext_data(); req(ext)
      gs <- n_total_r()
      h0 <- input$h0 %||% 0
      alpha <- input$alpha %||% 0.05
      two_sided <- input$two_sided %||% TRUE
      pt <- input$power_target %||% 0.80
      lbl <- param_labels()
      compute_power_table(ext, table_no = tbl_no(), groupsize = gs,
                          h0 = h0, alpha = alpha, two_sided = two_sided,
                          power_target = pt, param_labels = lbl)
    })

    # -- Equiv table reactive (TOST) ------------------------------------------
    equiv_tbl <- reactive({
      ext <- ext_data(); req(ext)
      gs <- n_total_r()
      if (is.null(gs) || is.na(gs) || gs < 1L) gs <- 1L
      h0    <- input$h0 %||% 0
      alpha <- input$alpha %||% 0.05
      pt    <- input$power_target %||% 0.80
      dL    <- input$delta_L %||% 0.2
      lbl   <- param_labels()
      compute_equiv_table(ext, table_no = tbl_no(), groupsize = gs,
                          delta_L = dL, h0 = h0, alpha = alpha,
                          power_target = pt, param_labels = lbl)
    })

    # -- Main UI ---------------------------------------------------------------
    output$content <- renderUI({
      ext <- ext_data()
      if (is.null(ext)) {
        return(div(class = "alert alert-info",
                   "Load a .ext file to compute power."))
      }

      current_num <- function(value, default) {
        if (is.null(value) || is.na(value)) default else value
      }
      groupsize_value <- isolate(groupsize_r())
      n_total_value <- isolate(n_total_r())
      h0_value <- isolate(current_num(input$h0, 0))
      alpha_value <- isolate(current_num(input$alpha, 0.05))
      power_target_value <- isolate(current_num(input$power_target, 0.80))
      two_sided_value <- isolate(input$two_sided %||% TRUE)
      delta_l_value <- isolate(current_num(input$delta_L, 0.2))

      tagList(
        # Settings bar
        settings_bar(
          numericInput(ns("groupsize"), "GROUPSIZE source",
                       value = groupsize_value, min = 1L, step = 1L, width = "120px"),
          numericInput(ns("n_total"), "N (what-if)",
                       value = n_total_value,
                       min = 1L, step = 1L, width = "120px"),
          numericInput(ns("h0"), "H0", value = h0_value,
                       step = 0.1, width = "90px"),
          numericInput(ns("alpha"), "Alpha", value = alpha_value,
                       min = 0.001, max = 0.20, step = 0.005, width = "80px"),
          numericInput(ns("power_target"), "Target power", value = power_target_value,
                       min = 0.50, max = 0.99, step = 0.05, width = "90px"),
          checkboxInput(ns("two_sided"), "Two-sided Wald", value = two_sided_value)
        ),
        tags$div(
          class = "settings-help",
          HTML("<b>GROUPSIZE source</b> is the design-scale N detected from the control stream. <b>N (what-if)</b> initialized from GROUPSIZE; change it to explore power/NSN scenarios without re-running $DESIGN. &nbsp;&middot;&nbsp; Wald statistic: W = (&theta; &minus; H<sub>0</sub>) / SE. &nbsp;&middot;&nbsp; Uncheck <b>Two-sided Wald</b> for the app directional policy: Directional H1: theta &gt; H0.")
        ),

        # Tabs (popkin-style underline)
        popkin_tabs(ns,
          tabPanel("Power (Wald)",
            br(),
            status_panel(
              "Wald power quick read",
              tags$p("Power is the probability of rejecting H0 when the effect exists. Two-sided Wald is the source-locked default. Directional H1: theta > H0 is the app-specific unchecked mode."),
              doc_link("power", "Open Power/TOST documentation"),
              tone = "info",
              icon_name = "chart-bar"
            ),
            table_panel(
              "Power per parameter",
              DTOutput(ns("power_table")),
              actions = downloadButton(ns("dl_power"), "CSV",
                                       class = "btn btn-default btn-sm")
            )
          ),
          tabPanel("Sample Size (NSN)",
            br(),
            status_panel(
              "NSN quick read",
              tags$p("N needed estimates the sample size required to reach target power under FIM scaling. N ratio <= 1 means the current N is sufficient."),
              doc_link("power", "Open Power/TOST documentation"),
              tone = "success",
              icon_name = "calculator"
            ),
            table_panel(
              "N needed to reach target power",
              DTOutput(ns("nsn_table")),
              actions = downloadButton(ns("dl_nsn"), "CSV",
                                       class = "btn btn-default btn-sm")
            ),
            br(),
            plot_panel(
              "Power curve Power(N)",
              fluidRow(
                column(4,
                  uiOutput(ns("param_selector"))
                ),
                column(8,
                  plotOutput(ns("power_curve"), height = "380px")
                )
              )
            )
          ),
          tabPanel("Equivalence (TOST)",
            br(),
            status_panel(
              "TOST quick read",
              tags$p("TOST checks whether a parameter is close enough to H0 to be considered equivalent. Parameters outside the margin cannot demonstrate equivalence, so power is 0."),
              doc_link("power", "Open Power/TOST documentation"),
              tone = "info",
              icon_name = "balance-scale"
            ),
            control_panel(
              label = "Equivalence margin",
              fluidRow(
                column(3,
                  numericInput(ns("delta_L"), "Delta (symmetric margin)",
                               value = delta_l_value, min = 0.001, step = 0.05, width = "100%")
                ),
                column(9,
                  tags$p(style = "font-size:.85rem; color:#475569; margin-top:28px;",
                    HTML(paste0(
                      "H<sub>0</sub>: &theta; &le; h<sub>0</sub> &minus; &delta; ",
                      "or &theta; &ge; h<sub>0</sub> + &delta; &nbsp;&bull;&nbsp; ",
                      "H<sub>1</sub>: &theta; &isin; ]h<sub>0</sub> &minus; &delta;, ",
                      "h<sub>0</sub> + &delta;[ &nbsp;&bull;&nbsp; ",
                      "Two one-sided tests, one-sided alpha (not divided)."
                    ))
                  )
                )
              )
            ),
            uiOutput(ns("tost_margin_warning")),
            table_panel(
              "Equivalence power per parameter",
              DTOutput(ns("equiv_table")),
              actions = downloadButton(ns("dl_equiv"), "CSV",
                                       class = "btn btn-default btn-sm")
            ),
            br(),
            table_panel(
              "N needed - equivalence",
              DTOutput(ns("equiv_nsn_table")),
              actions = downloadButton(ns("dl_equiv_nsn"), "CSV",
                                       class = "btn btn-default btn-sm")
            ),
            br(),
            plot_panel(
              "Power curve Power(N) - equivalence",
              fluidRow(
                column(4,
                  uiOutput(ns("equiv_param_selector"))
                ),
                column(8,
                  plotOutput(ns("equiv_power_curve"), height = "380px")
                )
              )
            )
          )
        )
      )
    })

    # -- Power table -----------------------------------------------------------
    output$power_table <- renderDT({
      tbl <- power_tbl(); req(tbl)

      display <- tbl |>
        transmute(
          Parameter  = label,
          Type       = param_type,
          Estimate   = round(estimate, 5),
          `RSE (%)`  = round(rse_pct, 2),
          SE         = round(se, 5),
          Power      = ifelse(is.na(power), NA_real_, round(power, 4))
        )

      dt <- datatable(display, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = -1, dom = "t",
                                     columnDefs = list(
                                       list(className = "dt-right", targets = 2:5)
                                     )))

      power_col <- which(names(display) == "Power")
      dt |>
        formatStyle(power_col,
          backgroundColor = styleInterval(
            c(0.50, 0.80),
            c("#fee2e2", "#fef3c7", "#dcfce7")
          ),
          fontWeight = "bold"
        )
    })

    # -- NSN table -------------------------------------------------------------
    output$nsn_table <- renderDT({
      tbl <- power_tbl(); req(tbl)
      gs <- n_total_r()

      display <- tbl |>
        transmute(
          Parameter      = label,
          Type           = param_type,
          Estimate       = round(estimate, 5),
          `RSE (%)`      = round(rse_pct, 2),
          `Target RSE (%)` = ifelse(is.na(rse_needed), NA_real_, round(rse_needed, 2)),
          `Current N`    = as.integer(gs),
          `N needed`     = ifelse(is.na(n_needed), NA_integer_, as.integer(n_needed)),
          `N ratio`      = ifelse(is.na(n_needed), NA_real_, round(n_needed / gs, 2))
        )

      dt <- datatable(display, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = -1, dom = "t",
                                     columnDefs = list(
                                       list(className = "dt-right", targets = 2:7)
                                     )))

      ratio_col <- which(names(display) == "N ratio")
      dt |>
        formatStyle(ratio_col,
          backgroundColor = styleInterval(
            c(1.0, 3.0),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          ),
          fontWeight = "bold"
        )
    })

    # -- Param selector for power curve ----------------------------------------
    output$param_selector <- renderUI({
      tbl <- power_tbl(); req(tbl)
      valid <- tbl |> filter(!is.na(power))
      if (nrow(valid) == 0L) return(tags$p("No parameter with computable power."))
      choices <- setNames(valid$param, valid$label)
      selectInput(ns("curve_param"), "Parameter:",
                  choices = choices, selected = choices[1], width = "100%")
    })

    # -- Power curve plot ------------------------------------------------------
    output$power_curve <- renderPlot({
      tbl <- power_tbl(); req(tbl)
      sel <- input$curve_param; req(sel)
      row <- tbl |> filter(param == sel)
      req(nrow(row) == 1L)
      gs <- n_total_r()

      plot_power_curve(
        theta_val    = row$estimate,
        rse_at_n     = row$rse_pct,
        n_current    = gs,
        h0           = input$h0 %||% 0,
        alpha        = input$alpha %||% 0.05,
        two_sided    = input$two_sided %||% TRUE,
        power_target = input$power_target %||% 0.80,
        param_name   = row$label
      )
    }, res = 110)

    # -- Downloads -------------------------------------------------------------
    output$dl_power <- downloadHandler(
      filename = function() {
        paste0("power_wald_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        tbl <- power_tbl()
        if (is.null(tbl)) return()
        out <- tbl |>
          transmute(param, label, estimate, rse_pct, se, power,
                    n_needed, rse_needed, param_type)
        readr::write_csv(out, file)
      }
    )

    output$dl_nsn <- downloadHandler(
      filename = function() {
        paste0("nsn_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        tbl <- power_tbl()
        if (is.null(tbl)) return()
        gs <- n_total_r()
        out <- tbl |>
          transmute(param, label, estimate, rse_pct,
                    n_current = as.integer(gs), n_needed, rse_needed,
                    ratio_n = ifelse(is.na(n_needed), NA_real_, n_needed / gs),
                    param_type)
        readr::write_csv(out, file)
      }
    )

    # -- TOST: margin warning --------------------------------------------------
    output$tost_margin_warning <- renderUI({
      tbl <- equiv_tbl(); req(tbl)
      n_outside <- sum(tbl$outside_margin, na.rm = TRUE)
      if (n_outside == 0L) return(NULL)
      dL <- input$delta_L %||% 0.2
      div(class = "alert alert-warning", style = "margin-bottom:12px;",
        sprintf("%d parameter(s) outside margin [-%g, +%g]: power = 0.",
                n_outside, dL, dL))
    })

    # -- TOST: power table -----------------------------------------------------
    output$equiv_table <- renderDT({
      tbl <- equiv_tbl(); req(tbl)

      display <- tbl |>
        transmute(
          Parameter  = label,
          Type       = param_type,
          Estimate   = round(estimate, 5),
          `RSE (%)`  = round(rse_pct, 2),
          SE         = round(se, 5),
          `TOST Power`     = ifelse(is.na(power), NA_real_, round(power, 4)),
          `Outside margin` = ifelse(outside_margin, "Yes", "")
        )

      dt <- datatable(display, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = -1, dom = "t",
                                     columnDefs = list(
                                       list(className = "dt-right", targets = 2:5)
                                     )))

      power_col <- which(names(display) == "TOST Power")
      margin_col <- which(names(display) == "Outside margin")
      dt |>
        formatStyle(power_col,
          backgroundColor = styleInterval(
            c(0.50, 0.80),
            c("#fee2e2", "#fef3c7", "#dcfce7")
          ),
          fontWeight = "bold"
        ) |>
        formatStyle(margin_col,
          backgroundColor = styleEqual("Yes", "#fee2e2"),
          fontWeight = styleEqual("Yes", "bold"),
          color = styleEqual("Yes", "#dc2626")
        )
    })

    # -- TOST: NSN table -------------------------------------------------------
    output$equiv_nsn_table <- renderDT({
      tbl <- equiv_tbl(); req(tbl)
      gs <- n_total_r()

      display <- tbl |>
        transmute(
          Parameter       = label,
          Type            = param_type,
          Estimate        = round(estimate, 5),
          `RSE (%)`       = round(rse_pct, 2),
          `Target RSE (%)` = ifelse(is.na(rse_needed), NA_real_, round(rse_needed, 2)),
          `Current N`     = as.integer(gs),
          `N needed`      = ifelse(is.na(n_needed), NA_integer_, as.integer(n_needed)),
          `N ratio`       = ifelse(is.na(n_needed), NA_real_, round(n_needed / gs, 2))
        )

      dt <- datatable(display, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = -1, dom = "t",
                                     columnDefs = list(
                                       list(className = "dt-right", targets = 2:7)
                                     )))

      ratio_col <- which(names(display) == "N ratio")
      dt |>
        formatStyle(ratio_col,
          backgroundColor = styleInterval(
            c(1.0, 3.0),
            c("#dcfce7", "#fef3c7", "#fee2e2")
          ),
          fontWeight = "bold"
        )
    })

    # -- TOST: param selector --------------------------------------------------
    output$equiv_param_selector <- renderUI({
      tbl <- equiv_tbl(); req(tbl)
      valid <- tbl |> filter(!is.na(power), power > 0)
      if (nrow(valid) == 0L) {
        return(tags$p("No parameter within the equivalence margin."))
      }
      choices <- setNames(valid$param, valid$label)
      selectInput(ns("equiv_curve_param"), "Parameter:",
                  choices = choices, selected = choices[1], width = "100%")
    })

    # -- TOST: power curve (inline) --------------------------------------------
    output$equiv_power_curve <- renderPlot({
      tbl <- equiv_tbl(); req(tbl)
      sel <- input$equiv_curve_param; req(sel)
      row <- tbl |> filter(param == sel)
      req(nrow(row) == 1L)
      gs <- n_total_r()
      dL <- input$delta_L %||% 0.2
      h0 <- input$h0 %||% 0
      al <- input$alpha %||% 0.05
      pt <- input$power_target %||% 0.80

      n_max <- max(10L, as.integer(gs * 5))
      n_range <- seq(1L, n_max, by = max(1L, n_max %/% 200L))

      powers <- vapply(n_range, function(n) {
        rse_n <- row$rse_pct * sqrt(gs / n)
        compute_power_tost(row$estimate, rse_n, dL, h0, al)
      }, numeric(1))

      df <- data.frame(N = n_range, Power = powers)

      nsn <- compute_nsn_tost(row$estimate, row$rse_pct, gs, dL, h0, al, pt)

      subtitle_txt <- sprintf(
        "Current RSE = %.1f%% | Current N = %d | N needed = %s | delta = %g",
        row$rse_pct, gs,
        if (is.na(nsn$n_needed)) "N/A" else as.character(nsn$n_needed),
        dL
      )

      p <- ggplot(df, aes(x = N, y = Power)) +
        geom_line(color = "#7c3aed", size = 1) +
        geom_hline(yintercept = pt, linetype = "dashed",
                   color = "#dc2626", size = 0.6) +
        geom_vline(xintercept = gs, linetype = "dotted",
                   color = "#6b7280", size = 0.6) +
        annotate("text", x = gs, y = 0.05,
                 label = paste0("N=", gs), hjust = -0.15,
                 size = 3.2, color = "#6b7280") +
        annotate("text", x = max(n_range) * 0.95, y = pt + 0.03,
                 label = sprintf("Target = %g%%", pt * 100),
                 hjust = 1, size = 3.2, color = "#dc2626") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                           labels = function(x) paste0(x * 100, "%")) +
        labs(x = "Number of subjects (N)", y = "TOST Power",
             title = paste0("TOST Power vs N — ", row$label),
             subtitle = subtitle_txt) +
        .theme_design()

      if (!is.na(nsn$n_needed) && nsn$n_needed <= n_max) {
        power_at_nsn <- compute_power_tost(
          row$estimate, row$rse_pct * sqrt(gs / nsn$n_needed), dL, h0, al
        )
        p <- p +
          geom_vline(xintercept = nsn$n_needed, linetype = "dotted",
                     color = "#16a34a", size = 0.6) +
          geom_point(data = data.frame(N = nsn$n_needed, Power = power_at_nsn),
                     aes(x = N, y = Power), color = "#16a34a", size = 3) +
          annotate("text", x = nsn$n_needed, y = 0.05,
                   label = paste0("N=", nsn$n_needed), hjust = -0.15,
                   size = 3.2, color = "#16a34a")
      }

      p
    }, res = 110)

    # -- TOST: downloads -------------------------------------------------------
    output$dl_equiv <- downloadHandler(
      filename = function() {
        paste0("equiv_tost_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        tbl <- equiv_tbl()
        if (is.null(tbl)) return()
        out <- tbl |>
          transmute(param, label, estimate, rse_pct, se, power,
                    outside_margin, param_type)
        readr::write_csv(out, file)
      }
    )

    output$dl_equiv_nsn <- downloadHandler(
      filename = function() {
        paste0("equiv_nsn_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        tbl <- equiv_tbl()
        if (is.null(tbl)) return()
        gs <- n_total_r()
        out <- tbl |>
          transmute(param, label, estimate, rse_pct,
                    n_current = as.integer(gs), n_needed, rse_needed,
                    ratio_n = ifelse(is.na(n_needed), NA_real_, n_needed / gs),
                    outside_margin, param_type)
        readr::write_csv(out, file)
      }
    )

    # -- Expose design-level reactives to parent/sibling modules --------------
    list(
      tbl_no    = tbl_no,
      groupsize = groupsize_r
    )
  })
}
