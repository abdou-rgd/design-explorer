# =============================================================================
# mod_examples.R -- Built-in examples (Bauer 2021)
# =============================================================================

.EXAMPLES <- list(
  example1 = list(
    title = "Exemple 1 : Evaluation d'un design",
    desc = "Modele warfarin 1-CMT, evaluation FIM bloc-diagonale (FIMDIAG=1). Pas d'optimisation.",
    dir = "examples/example1",
    prefix = "warfarin",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    guide = list(
      context = "Modele warfarin 1-compartiment (ADVAN2 TRANS2), absorption premier ordre, erreur combinee. 32 sujets (GROUPSIZE=32), 3 temps de prelevement a 0.5, 2 et 8 h. Valeurs de reference Bauer 2021 Table 3 : OFV = -39.518.",
      points = c(
        "FIMDIAG=1 (bloc-diagonal) : hypothese d'independance entre sujets et entre parametres de variabilite. Plus rapide mais moins precis que la FIM complete (FIMDIAG=0)",
        "MAXEVAL=0 : evaluation pure, aucun temps n'est modifie. Sert de baseline pour comparer avec l'optimisation (Ex. 2)",
        "RSE(CL) = 36.9%, RSE(V) = 5.0% (Bauer Table 3) : V est bien estime avec ce design, CL beaucoup moins. Un seul temps precoce est insuffisant pour CL",
        "RELATIVEINF (onglet ddie) = contribution de chaque observation a la reduction de la variance de l'ETA. Une RELATIVEINF faible indique un ETA mal supporte par le design",
        "La FIM (onglet FIM) est une matrice 3x3 (CL, V, KA) : les elements hors-diagonale mesurent la correlation entre les incertitudes des parametres"
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
    desc = "Optimisation robuste via $SIM TRUE=PRIOR SUBPROB=1000. Distribution des temps sur 1000 jeux de parametres.",
    dir = "examples/example3",
    prefix = "priortrue",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    has_summary = TRUE,
    summary_file = "examples/example3/summary.tab",
    guide = list(
      context = "Meme modele warfarin, optimisation robuste : $SIM TRUE=PRIOR SUBPROB=1000 tire 1000 jeux de parametres (THETA) depuis leur distribution a priori, puis optimise le design pour chacun. L'OFV minimise est E[-log(det(FIM))] sur le prior = critere standard du robust design (Nyberg et al., Bauer 2021).",
      points = c(
        "Mecanisme : SUBPROB=1000 genere 1000 sous-problemes independants. Pour chaque subprob, les vrais THETAs sont tires du prior ($PRIOR NWPRI ou $OMEGA/$SIGMA) et la FIM est evaluee/optimisee avec ces valeurs",
        "Onglet 'Temps optimaux' : le tableau P10/mediane/P90 par strate montre la distribution des temps optimaux. Nos donnees : medianes 0.13 h, triplet groupe autour de 6.9 h [P2.5=1.5, P97.5=23], 158.1 h. Bauer rapporte 159.9 h avec un seed different",
        "Interpretation pratique (Bauer) : choisir les temps comme 0.13, 1.5, 7.0, 23.0, 160.0 h. OFV resultant = -51.374, proche de l'optimal (mean OFV = -51.95 dans nos donnees, -51.598 dans Bauer)",
        "Onglet 'Parametres' : la ligne 'D-critere robuste' affiche exp(-mean(OFV_i)/p) avec ses bornes P10/P90 — c'est la moyenne geometrique de det(FIM)^(1/p) sur les 1000 realisations du prior",
        "RSE et RELATIVEINF refletent le sous-probleme 1000 (dernier). Ils ne sont pas une moyenne — la distribution des RSE n'est pas directement accessible ici"
      )
    )
  ),
  example4 = list(
    title = "Exemple 4 : PK-PD multi-reponses (evaluation)",
    desc = "Modele warfarin PK-PD (concentration + effet), FIMTYPE=1 + VARCROSS=1. Evaluation du design.",
    dir = "examples/example4",
    prefix = "warfarin_pkpd_eval",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA\nTHETA4=EMAX\nTHETA5=EC50",
    compare_with = "example4_opt",
    guide = list(
      context = "Modele PK-PD multi-compartiment avec effet Emax. CMT==2 pour PK, CMT==3 pour PD. FIMTYPE=1 (bloc-diagonal) + VARCROSS=1. Evaluation du design initial.",
      points = c(
        "Plus de parametres a estimer (5 THETA + OMEGA + SIGMA PK et PD)",
        "FIMTYPE=1 + VARCROSS=1 equivaut a PFIM style bloc-diagonal",
        "Les RSE des parametres PD (EMAX, EC50) sont generalement plus grands",
        "Comparez avec l'optimisation (bouton 'Comparer') : les RSE diminuent apres optimisation"
      )
    )
  ),
  example4_opt = list(
    title = "Exemple 4 : PK-PD multi-reponses (optimisation)",
    desc = "Optimisation des temps PK-PD par Nelder-Mead. Comparez les RSE avant/apres.",
    dir = "examples/example4",
    prefix = "warfarin_pkpd_opt",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA\nTHETA4=EMAX\nTHETA5=EC50",
    guide = list(
      context = "Meme modele PK-PD, apres optimisation Nelder-Mead des temps de prelevement. MAXEVAL > 0.",
      points = c(
        "Les RSE sont reduits par rapport a l'evaluation initiale",
        "Les temps optimaux (onglet 'Temps optimaux') montrent les fenetres d'echantillonnage PK et PD",
        "Le critere D-OFV est minimise : la FIM est plus grande qu'avant optimisation",
        "Chargez l'Exemple 4 evaluation pour comparer directement"
      )
    )
  ),
  example5 = list(
    title = "Exemple 5 : DS-optimality",
    desc = "Critere DS-optimality (OFVTYPE=6) avec parametres non-interessants (UNINT). Modele warfarin etendu.",
    dir = "examples/example5",
    prefix = "optdesign2",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA\nTHETA4=F1",
    compare_with = "example1",
    guide = list(
      context = "DS-optimality : maximise la precision sur un sous-ensemble de parametres d'interet, en traitant les autres comme non-interessants (UNINT). Ici F1 (biodisponibilite) est le parametre cible.",
      points = c(
        "Le critere DS-OFV = -log(det(FIM_interet)) : seule la FIM des parametres d'interet compte",
        "UNINT designe les parametres non-interessants (nuisance parameters)",
        "Comparez avec l'Exemple 1 (D-optimality) : les temps optimaux different selon le critere",
        "Utile quand certains parametres sont deja bien estimes ou non pertinents pour la decision"
      )
    )
  ),
  example6 = list(
    title = "Exemple 6 : TMDD, STRAT/STRATF",
    desc = "Modele TMDD ODE (ADVAN13), optimisation avec stratification dose. 4 blocs $DESIGN chaines.",
    dir = "examples/example6",
    prefix = "tmdd2",
    labels = "THETA1=VC\nTHETA2=K10\nTHETA3=K12\nTHETA4=K21\nTHETA5=VM\nTHETA6=KMC\nTHETA7=K03\nTHETA8=K30",
    guide = list(
      context = "Modele TMDD (Target-Mediated Drug Disposition) a 3 compartiments avec ODE (ADVAN13). 8 parametres PK (VC, K10, K12, K21, VM, KMC, K03, K30), 2 niveaux de dose (300 et 10000), 5 temps par strate. GROUPSIZE=50, FIMDIAG=1, VARCROSS=1.",
      points = c(
        "4 blocs $DESIGN chaines avec NELDER : strategie de redemarrage pour eviter les minima locaux. Comparer les OFV des 4 tables dans l'onglet convergence",
        "STRAT/STRATF : stratification par dose. STRATF optimise la proportion de sujets par strate (~59%/41%)",
        "Multi-CMT : CMT=1 (drug) et CMT=3 (receptor) observes. 2 erreurs residuelles separees (EPS(1-2) PK, EPS(3-4) receptor)",
        "Modele ODE (ADVAN13) : necessaire pour la cinetique TMDD non-lineaire. TOL=12, ATOL=12 pour precision",
        "8 OMEGAs diagonaux + 4 SIGMAs (dont 2 FIXED a 0.001) -- matrice FIM 20x20"
      )
    )
  ),
  example7 = list(
    title = "Exemple 7 : Bayes FIM (OFVTYPE=8)",
    desc = "FIM bayesienne individuelle. Workflow D-opt (optimisation) puis Bayes (evaluation).",
    dir = "examples/example7",
    prefix = "tmdd2b",
    labels = "THETA1=VC\nTHETA2=K10\nTHETA3=K12\nTHETA4=K21\nTHETA5=VM\nTHETA6=KMC\nTHETA7=K03\nTHETA8=K30",
    compare_with = "example7_bayes",
    guide = list(
      context = "Meme modele TMDD qu'exemple 6. Workflow en 2 problemes : (1) optimisation D-opt classique (OFVTYPE=1, MAXEVAL=50000), (2) evaluation Bayesian FIM (OFVTYPE=8, MAXEVAL=0) sur le design optimise. Le fichier .bfm contient la matrice de variance conditionnelle individuelle.",
      points = c(
        "Workflow 2 etapes (Bauer) : optimiser avec D-opt (rapide, robuste) puis evaluer avec Bayes FIM (plus realiste)",
        "OFVTYPE=8 = FIM bayesienne : integre l'information a priori ($OMEGA) dans le critere",
        "Le .bfm contient la matrice ETC (conditional variance-covariance) -- mesure la precision individuelle, pas populationnelle (visualisation prevue en V5)",
        "Comparez avec l'optimisation Bayes pure (bouton Comparer) : memes parametres mais critere different",
        "Optimisation TIME+DOSE simultanee (DESEL=TIME et DESEL=AMT dans le meme bloc $DESIGN)"
      )
    )
  ),
  example7_bayes = list(
    title = "Exemple 7b : Optimisation Bayes pure",
    desc = "Optimisation directe OFVTYPE=8 (Bayes FIM). Comparez avec le workflow D-opt puis Bayes.",
    dir = "examples/example7",
    prefix = "optex6d17_8",
    labels = "THETA1=VC\nTHETA2=K10\nTHETA3=K12\nTHETA4=K21\nTHETA5=VM\nTHETA6=KMC\nTHETA7=K03\nTHETA8=K30",
    guide = list(
      context = "Meme modele TMDD, mais optimisation directe avec OFVTYPE=8 (Bayes FIM). Contrairement a l'Exemple 7 qui optimise d'abord en D-opt puis evalue en Bayes, ici l'optimisation utilise directement le critere bayesien.",
      points = c(
        "OFVTYPE=8 des le depart : le critere d'optimisation est la FIM bayesienne, pas la FIM populationnelle",
        "Plusieurs blocs $DESIGN chaines (6 tables) : exploration progressive du paysage d'optimisation",
        "Comparez les temps optimaux avec l'Exemple 7 : le critere bayesien peut favoriser des designs differents",
        "Les OFV D-opt et Bayes ne sont pas comparables directement (echelles differentes)",
        "Le fichier .bfm contient la progression de la variance conditionnelle au fil des iterations (visualisation prevue en V5)"
      )
    )
  )
)

mod_examples_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("open_examples"), "Exemples", icon = icon("book-open"),
                 class = "btn-sm btn-default w-100",
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
        title = "Exemples Bauer 2021",
        size = "l",
        easyClose = TRUE,
        fluidRow(
          lapply(names(.EXAMPLES), function(ex_id) {
            ex <- .EXAMPLES[[ex_id]]
            column(3,
              div(class = "upload-box", style = "cursor:pointer; min-height:180px;",
                tags$h6(ex$title, style = "font-size:.9rem;"),
                tags$p(style = "font-size:.8rem; color:#4b5563;", ex$desc),
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

        paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, bfm = NULL)
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
          comp_paths <- list(ext = NULL, shk = NULL, coi = NULL, clt = NULL, tab = NULL, bfm = NULL)
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
