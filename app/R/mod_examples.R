# =============================================================================
# mod_examples.R -- Built-in warfarin examples (Bauer 2021)
# =============================================================================

.EXAMPLES <- list(
  example1 = list(
    title = "Exemple 1 : Evaluation d'un design",
    desc = "Modele warfarin 1-CMT, evaluation FIM bloc-diagonale (FIMDIAG=1). Pas d'optimisation.",
    dir = "examples/example1",
    prefix = "warfarin",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    guide = list(
      context = "Modele warfarin 1-compartiment (ADVAN2 TRANS2), absorption premier ordre, erreur combinee. 32 sujets, GROUPSIZE=32.",
      points = c(
        "RSE de CL et V sont faibles (< 20%) : le design est informatif pour ces parametres",
        "RSE de KA est plus eleve : l'absorption est plus difficile a estimer",
        "RELATIVEINF montre la reduction d'incertitude apportee par le design sur chaque ETA",
        "MAXEVAL=0 : c'est une evaluation, pas une optimisation"
      )
    )
  ),
  example2 = list(
    title = "Exemple 2 : Optimisation des temps",
    desc = "Optimisation des temps de prelevement par Nelder-Mead (DESEL=TIME, MAXEVAL=4000).",
    dir = "examples/example2",
    prefix = "warfarin2b",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    compare_with = "example1",
    guide = list(
      context = "Meme modele warfarin, mais avec optimisation des temps via NELDER. Les temps sont libres dans les fenetres definies par TMIN/TMAX.",
      points = c(
        "Comparez les RSE avant/apres optimisation (bouton 'Comparer avec l'evaluation')",
        "L'OFV (= -log(det(FIM))) diminue : le determinant de la FIM augmente",
        "Les temps optimaux dans l'onglet 'Temps optimaux' montrent ou prelever",
        "Consultez la convergence : le NELDER converge-t-il bien ?"
      )
    )
  ),
  example3 = list(
    title = "Exemple 3 : Robust design ($PRIOR)",
    desc = "Optimisation robuste via $SIM TRUE=PRIOR SUBPROB=1000. Distribution des temps et predictions.",
    dir = "examples/example3",
    prefix = "priortrue",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    has_summary = TRUE,
    summary_file = "examples/example3/summary.tab",
    guide = list(
      context = "Design robuste : $SIM TRUE=PRIOR genere 1000 jeux de parametres depuis le prior, puis optimise le design pour chacun. Le summary.tab contient les statistiques (Mean, STD, RSTD, IC 95%) des temps et predictions optimaux.",
      points = c(
        "Le summary.tab montre la distribution des temps optimaux sur les 1000 replications",
        "RSTD (%) mesure la variabilite relative des temps optimaux : plus c'est bas, plus le design est robuste",
        "Comparez les IC 2.5%-97.5% : des intervalles larges signalent des temps sensibles au prior",
        "$PRIOR NWPRI + PLEV=0.99 : echantillonne 99% de la distribution prior (evite les extremes)"
      )
    )
  ),
  example4 = list(
    title = "Exemple 4 : PK-PD multi-reponses",
    desc = "Modele warfarin PK-PD (concentration + effet), FIMTYPE=1 + VARCROSS=1.",
    dir = "examples/example4",
    prefix = "warfarin_pkpd_eval",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA\nTHETA4=EMAX\nTHETA5=EC50",
    guide = list(
      context = "Modele PK-PD multi-compartiment avec effet Emax. CMT==2 pour PK, CMT==3 pour PD. FIMTYPE=1 (bloc-diagonal) + VARCROSS=1.",
      points = c(
        "Plus de parametres a estimer (5 THETA + OMEGA + SIGMA PK et PD)",
        "FIMTYPE=1 + VARCROSS=1 equivaut a PFIM style bloc-diagonal",
        "Les RSE des parametres PD (EMAX, EC50) sont generalement plus grands",
        "Le design doit etre informatif pour les deux reponses simultanement"
      )
    )
  )
)

mod_examples_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("open_examples"), "Exemples", icon = icon("book-open"),
                 class = "btn-sm btn-outline-secondary w-100",
                 style = "margin-bottom: 8px;")
  )
}

mod_examples_server <- function(id, session_main = NULL, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Selected example data to return
    selected <- reactiveValues(
      file_paths = NULL, labels = NULL, guide = NULL,
      compare_paths = NULL, compare_name = NULL,
      summary_data = NULL
    )

    # Universal reset from app.R
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        selected$file_paths   <- NULL
        selected$labels       <- NULL
        selected$guide        <- NULL
        selected$compare_paths <- NULL
        selected$compare_name <- NULL
        selected$summary_data <- NULL
      })
    }

    observeEvent(input$open_examples, {
      showModal(modalDialog(
        title = "Exemples Bauer 2021 -- Warfarin",
        size = "l",
        easyClose = TRUE,
        fluidRow(
          lapply(names(.EXAMPLES), function(ex_id) {
            ex <- .EXAMPLES[[ex_id]]
            column(3,
              div(class = "upload-box", style = "cursor:pointer; min-height:200px;",
                tags$h6(ex$title),
                tags$p(style = "font-size:.85rem; color:#4b5563;", ex$desc),
                actionButton(ns(paste0("load_", ex_id)), "Charger",
                             class = "btn-sm btn-primary w-100")
              )
            )
          })
        )
      ))
    })

    # Load example handlers
    lapply(names(.EXAMPLES), function(ex_id) {
      observeEvent(input[[paste0("load_", ex_id)]], {
        ex <- .EXAMPLES[[ex_id]]
        base <- ex$dir

        paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL)
        for (ext_type in names(paths)) {
          f <- file.path(base, paste0(ex$prefix, ".", ext_type))
          if (file.exists(f)) paths[[ext_type]] <- f
        }

        selected$file_paths <- paths
        selected$labels <- ex$labels
        selected$guide <- ex$guide

        # Handle compare_with
        if (!is.null(ex$compare_with)) {
          comp_ex <- .EXAMPLES[[ex$compare_with]]
          comp_paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL)
          for (ext_type in names(comp_paths)) {
            f <- file.path(comp_ex$dir, paste0(comp_ex$prefix, ".", ext_type))
            if (file.exists(f)) comp_paths[[ext_type]] <- f
          }
          selected$compare_paths <- comp_paths
          selected$compare_name <- comp_ex$title
        } else {
          selected$compare_paths <- NULL
          selected$compare_name <- NULL
        }

        # Handle summary.tab (robust design)
        if (isTRUE(ex$has_summary) && !is.null(ex$summary_file)) {
          if (file.exists(ex$summary_file)) {
            selected$summary_data <- tryCatch(
              read_summary_tab(ex$summary_file),
              error = function(e) NULL
            )
          }
        } else {
          selected$summary_data <- NULL
        }

        removeModal()
        showNotification(paste("Exemple charge :", ex$title), type = "message")
      })
    })

    list(
      file_paths    = reactive(selected$file_paths),
      labels        = reactive(selected$labels),
      guide         = reactive(selected$guide),
      compare_paths = reactive(selected$compare_paths),
      compare_name  = reactive(selected$compare_name),
      summary_data  = reactive(selected$summary_data)
    )
  })
}
