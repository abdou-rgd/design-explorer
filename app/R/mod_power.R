# =============================================================================
# mod_power.R — Onglet Power / NSN (Decision)
#
# Calcul de puissance (Wald) et nombre de sujets necessaire (NSN) a partir
# des SE predites par la FIM. Formules portees de PopED (Retout et al. 2007).
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

  })
}
