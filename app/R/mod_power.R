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

mod_power_server <- function(id, ext_data, tbl_no, param_labels,
                             groupsize = reactive(1L),
                             all_runs = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- RSE table reactive (from primary run) ---------------------------------
    rse_r <- reactive({
      ext <- ext_data(); req(ext)
      get_rse(ext, tbl_no())
    })

    # -- Power table reactive --------------------------------------------------
    power_tbl <- reactive({
      ext <- ext_data(); req(ext)
      gs <- groupsize()
      if (is.null(gs) || is.na(gs) || gs < 1L) gs <- 1L
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
      gs <- groupsize()
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
                   "Chargez un fichier .ext pour calculer la puissance."))
      }

      tagList(
        # Settings panel
        div(class = "surface-card", style = "margin-bottom: 16px;",
          p(class = "section-title", "Parametres du test"),
          fluidRow(
            column(3,
              numericInput(ns("h0"), "H0 (hypothese nulle)", value = 0,
                           step = 0.1, width = "100%")
            ),
            column(2,
              numericInput(ns("alpha"), "Alpha", value = 0.05,
                           min = 0.001, max = 0.20, step = 0.005, width = "100%")
            ),
            column(2,
              numericInput(ns("power_target"), "Puissance cible", value = 0.80,
                           min = 0.50, max = 0.99, step = 0.05, width = "100%")
            ),
            column(2,
              checkboxInput(ns("two_sided"), "Bilateral", value = TRUE)
            ),
            column(3,
              tags$p(style = "font-size:.75rem; color:#64748b; margin-top:28px;",
                sprintf("N total = %d sujets", groupsize() %||% 1L))
            )
          ),
          tags$p(style = "font-size:.85rem; color:#475569; margin:6px 0 0 0;",
            HTML(paste0(
              "<em>Test de Wald : W = (&theta;<sub>0</sub> &minus; ",
              "<span style='text-decoration:overline'>&theta;</span>) / SE</em>",
              " &nbsp;&bull;&nbsp; ",
              "<em>FIM scaling : RSE(N) = RSE(N<sub>0</sub>) &times; &radic;(N<sub>0</sub>/N)</em>"
            ))
          )
        ),

        # Tabs
        tabsetPanel(id = ns("power_tabs"), type = "tabs",
          tabPanel("Puissance (Wald)",
            br(),
            div(class = "surface-card", style = "margin-bottom:12px; padding:12px 16px;
                         border-left: 4px solid #2563eb;",
              HTML(paste0(
                "<p style='margin:0 0 6px 0; font-weight:600;'>Qu'est-ce que le test de Wald ?</p>",
                "<p style='font-size:.85rem; color:#475569; margin:0;'>",
                "Le test de Wald evalue si un parametre est <b>significativement different</b> ",
                "d'une valeur de reference (H<sub>0</sub>, souvent 0). ",
                "La statistique W = (&theta;<sub>0</sub> &minus; ",
                "<span style='text-decoration:overline'>&theta;</span>) / SE suit une loi normale.<br>",
                "<b>Puissance</b> = probabilite de rejeter H<sub>0</sub> quand l'effet existe reellement. ",
                "Une puissance &ge; 80%% signifie que le design detectera l'effet dans 80%% des cas.<br>",
                "<b>Usage</b> : \"Mon design a-t-il assez de sujets pour estimer ce parametre avec precision ?\"<br>",
                "<em>Ref. : Retout et al. 2007, Mentre &amp; Rousseau 2011.</em></p>"
              ))
            ),
            div(class = "surface-card",
              p(class = "section-title", "Puissance par parametre"),
              DTOutput(ns("power_table")),
              downloadButton(ns("dl_power"), "CSV", class = "btn btn-default btn-sm",
                             style = "margin-top:8px;")
            )
          ),
          tabPanel("Nombre de sujets (NSN)",
            br(),
            div(class = "surface-card", style = "margin-bottom:12px; padding:12px 16px;",
              HTML(paste0(
                "<p style='margin:0 0 6px 0; font-weight:600;'>Comment lire ce tableau ?</p>",
                "<p style='font-size:.85rem; color:#475569; margin:0;'>",
                "<b>RSE cible</b> = RSE maximum pour atteindre la puissance cible. ",
                "Calcul : SE<sub>cible</sub> = |&theta;<sub>0</sub> &minus; ",
                "<span style='text-decoration:overline'>&theta;</span>| / (z<sub>&alpha;</sub> + z<sub>&beta;</sub>), ",
                "puis RSE<sub>cible</sub> = SE<sub>cible</sub> / |",
                "<span style='text-decoration:overline'>&theta;</span>| &times; 100.<br>",
                "<b>N necessaire</b> = nombre de sujets pour atteindre le RSE cible, ",
                "par scaling lineaire de la FIM : N = N<sub>0</sub> &times; (RSE / RSE<sub>cible</sub>)&sup2;.<br>",
                "<b>Ratio N</b> = N necessaire / N actuel. ",
                "Ratio &le; 1 : puissance deja atteinte. ",
                "Ratio &gt; 1 : il faut plus de sujets.</p>"
              ))
            ),
            div(class = "surface-card",
              p(class = "section-title", "N necessaire pour atteindre la puissance cible"),
              DTOutput(ns("nsn_table")),
              downloadButton(ns("dl_nsn"), "CSV", class = "btn btn-default btn-sm",
                             style = "margin-top:8px;")
            ),
            br(),
            div(class = "surface-card",
              p(class = "section-title", "Courbe Power(N)"),
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
            div(class = "surface-card", style = "margin-bottom:12px; padding:12px 16px;
                         border-left: 4px solid #7c3aed;",
              HTML(paste0(
                "<p style='margin:0 0 6px 0; font-weight:600;'>Qu'est-ce que le test TOST ?</p>",
                "<p style='font-size:.85rem; color:#475569; margin:0;'>",
                "Le test TOST (Two One-Sided Tests) evalue si un parametre est ",
                "<b>equivalent</b> a une valeur de reference, c'est-a-dire compris dans une ",
                "marge d'equivalence [&minus;&delta;, +&delta;].<br>",
                "Contrairement au test de Wald (\"l'effet est-il different de zero ?\"), ",
                "le TOST repond a la question : \"l'effet est-il <b>suffisamment proche</b> de zero ",
                "pour etre considere comme negligeable ?\"<br>",
                "<b>Usage</b> : effet d'une covariable, bioequivalence, absence d'effet clinique. ",
                "Si le parametre est en dehors de la marge, la puissance est 0 ",
                "(l'equivalence ne peut pas etre demontree).<br>",
                "<em>Ref. : PFIM user guide (Retout et al.), eq. 4-7. Alpha une face (non divise).</em></p>"
              ))
            ),
            div(class = "surface-card", style = "margin-bottom: 16px;",
              p(class = "section-title", "Marge d'equivalence"),
              fluidRow(
                column(3,
                  numericInput(ns("delta_L"), "Delta (marge symetrique)",
                               value = 0.2, min = 0.001, step = 0.05, width = "100%")
                ),
                column(9,
                  tags$p(style = "font-size:.85rem; color:#475569; margin-top:28px;",
                    HTML(paste0(
                      "H<sub>0</sub> : &theta; &le; h<sub>0</sub> &minus; &delta; ",
                      "ou &theta; &ge; h<sub>0</sub> + &delta; &nbsp;&bull;&nbsp; ",
                      "H<sub>1</sub> : &theta; &isin; ]h<sub>0</sub> &minus; &delta;, ",
                      "h<sub>0</sub> + &delta;[ &nbsp;&bull;&nbsp; ",
                      "Deux tests unilateraux, alpha une face (non divise)."
                    ))
                  )
                )
              )
            ),
            uiOutput(ns("tost_margin_warning")),
            div(class = "surface-card",
              p(class = "section-title", "Puissance d'equivalence par parametre"),
              DTOutput(ns("equiv_table")),
              downloadButton(ns("dl_equiv"), "CSV", class = "btn btn-default btn-sm",
                             style = "margin-top:8px;")
            ),
            br(),
            div(class = "surface-card", style = "margin-bottom:12px; padding:12px 16px;",
              HTML(paste0(
                "<p style='margin:0 0 6px 0; font-weight:600;'>Comment lire ce tableau ?</p>",
                "<p style='font-size:.85rem; color:#475569; margin:0;'>",
                "<b>Hors marge</b> : si |&theta; &minus; h<sub>0</sub>| &ge; &delta;, ",
                "l'equivalence ne peut pas etre demontree (puissance = 0).<br>",
                "<b>N necessaire</b> : par FIM scaling, comme pour le test de Wald.<br>",
                "<b>Ratio N</b> &le; 1 : le design actuel suffit.</p>"
              ))
            ),
            div(class = "surface-card",
              p(class = "section-title", "N necessaire — equivalence"),
              DTOutput(ns("equiv_nsn_table")),
              downloadButton(ns("dl_equiv_nsn"), "CSV", class = "btn btn-default btn-sm",
                             style = "margin-top:8px;")
            ),
            br(),
            div(class = "surface-card",
              p(class = "section-title", "Courbe Power(N) — equivalence"),
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
          Parametre  = label,
          Type       = param_type,
          Estimate   = round(estimate, 5),
          `RSE (%)`  = round(rse_pct, 2),
          SE         = round(se, 5),
          Puissance  = ifelse(is.na(power), NA_real_, round(power, 4))
        )

      dt <- datatable(display, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = -1, dom = "t",
                                     columnDefs = list(
                                       list(className = "dt-right", targets = 2:5)
                                     )))

      power_col <- which(names(display) == "Puissance")
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
      gs <- groupsize() %||% 1L

      display <- tbl |>
        transmute(
          Parametre      = label,
          Type           = param_type,
          Estimate       = round(estimate, 5),
          `RSE (%)`      = round(rse_pct, 2),
          `RSE cible (%)` = ifelse(is.na(rse_needed), NA_real_, round(rse_needed, 2)),
          `N actuel`     = as.integer(gs),
          `N necessaire` = ifelse(is.na(n_needed), NA_integer_, as.integer(n_needed)),
          `Ratio N`      = ifelse(is.na(n_needed), NA_real_, round(n_needed / gs, 2))
        )

      dt <- datatable(display, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = -1, dom = "t",
                                     columnDefs = list(
                                       list(className = "dt-right", targets = 2:7)
                                     )))

      ratio_col <- which(names(display) == "Ratio N")
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
      if (nrow(valid) == 0L) return(tags$p("Aucun parametre avec power calculable."))
      choices <- setNames(valid$param, valid$label)
      selectInput(ns("curve_param"), "Parametre :",
                  choices = choices, selected = choices[1], width = "100%")
    })

    # -- Power curve plot ------------------------------------------------------
    output$power_curve <- renderPlot({
      tbl <- power_tbl(); req(tbl)
      sel <- input$curve_param; req(sel)
      row <- tbl |> filter(param == sel)
      req(nrow(row) == 1L)
      gs <- groupsize() %||% 1L

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
        gs <- groupsize() %||% 1L
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
        sprintf("%d parametre(s) hors marge [-%g, +%g] : puissance = 0.",
                n_outside, dL, dL))
    })

    # -- TOST: power table -----------------------------------------------------
    output$equiv_table <- renderDT({
      tbl <- equiv_tbl(); req(tbl)

      display <- tbl |>
        transmute(
          Parametre  = label,
          Type       = param_type,
          Estimate   = round(estimate, 5),
          `RSE (%)`  = round(rse_pct, 2),
          SE         = round(se, 5),
          `Puissance TOST` = ifelse(is.na(power), NA_real_, round(power, 4)),
          `Hors marge`     = ifelse(outside_margin, "Oui", "")
        )

      dt <- datatable(display, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = -1, dom = "t",
                                     columnDefs = list(
                                       list(className = "dt-right", targets = 2:5)
                                     )))

      power_col <- which(names(display) == "Puissance TOST")
      margin_col <- which(names(display) == "Hors marge")
      dt |>
        formatStyle(power_col,
          backgroundColor = styleInterval(
            c(0.50, 0.80),
            c("#fee2e2", "#fef3c7", "#dcfce7")
          ),
          fontWeight = "bold"
        ) |>
        formatStyle(margin_col,
          backgroundColor = styleEqual("Oui", "#fee2e2"),
          fontWeight = styleEqual("Oui", "bold"),
          color = styleEqual("Oui", "#dc2626")
        )
    })

    # -- TOST: NSN table -------------------------------------------------------
    output$equiv_nsn_table <- renderDT({
      tbl <- equiv_tbl(); req(tbl)
      gs <- groupsize() %||% 1L

      display <- tbl |>
        transmute(
          Parametre       = label,
          Type            = param_type,
          Estimate        = round(estimate, 5),
          `RSE (%)`       = round(rse_pct, 2),
          `RSE cible (%)` = ifelse(is.na(rse_needed), NA_real_, round(rse_needed, 2)),
          `N actuel`      = as.integer(gs),
          `N necessaire`  = ifelse(is.na(n_needed), NA_integer_, as.integer(n_needed)),
          `Ratio N`       = ifelse(is.na(n_needed), NA_real_, round(n_needed / gs, 2))
        )

      dt <- datatable(display, rownames = FALSE,
                      class = "stripe hover compact",
                      options = list(pageLength = -1, dom = "t",
                                     columnDefs = list(
                                       list(className = "dt-right", targets = 2:7)
                                     )))

      ratio_col <- which(names(display) == "Ratio N")
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
        return(tags$p("Aucun parametre dans la marge d'equivalence."))
      }
      choices <- setNames(valid$param, valid$label)
      selectInput(ns("equiv_curve_param"), "Parametre :",
                  choices = choices, selected = choices[1], width = "100%")
    })

    # -- TOST: power curve (inline) --------------------------------------------
    output$equiv_power_curve <- renderPlot({
      tbl <- equiv_tbl(); req(tbl)
      sel <- input$equiv_curve_param; req(sel)
      row <- tbl |> filter(param == sel)
      req(nrow(row) == 1L)
      gs <- groupsize() %||% 1L
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
        "RSE actuel = %.1f%% | N actuel = %d | N necessaire = %s | delta = %g",
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
                 label = sprintf("Cible = %g%%", pt * 100),
                 hjust = 1, size = 3.2, color = "#dc2626") +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                           labels = function(x) paste0(x * 100, "%")) +
        labs(x = "Nombre de sujets (N)", y = "Puissance TOST",
             title = paste0("Puissance TOST vs N — ", row$label),
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
        gs <- groupsize() %||% 1L
        out <- tbl |>
          transmute(param, label, estimate, rse_pct,
                    n_current = as.integer(gs), n_needed, rse_needed,
                    ratio_n = ifelse(is.na(n_needed), NA_real_, n_needed / gs),
                    outside_margin, param_type)
        readr::write_csv(out, file)
      }
    )

  })
}
