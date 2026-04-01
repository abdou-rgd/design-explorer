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
  # Convention de nommage :
  # compare_with      = cle d'une autre entree .EXAMPLES (cross-exemple)
  # prev_step_prefix  = prefix fichier de l'etape precedente dans le meme dir (intra-exemple)
  example2 = list(
    title = "Exemple 2 : Optimisation des temps (3 etapes)",
    desc = "Chaine d'optimisations NELDER : 3 passes successives ou chaque etape demarre depuis le design optimal precedent.",
    dir = "examples/example2",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    steps = list(
      list(
        label = "Etape 1/3 : Premiere passe NELDER (warfarin2)",
        prefix = "warfarin2",
        prev_step_prefix = NULL,
        guide = list(
          context = "Premiere optimisation NELDER a partir du design initial (3 temps dans des fenetres TMIN/TMAX). GROUPSIZE=32, FIMDIAG=1, MAXEVAL=9999.",
          points = c(
            "Point de depart : memes temps que l'Exemple 1 (evaluation pure)",
            "NELDER explore les fenetres de temps pour minimiser -log(det(FIM))",
            "Consultez l'onglet Convergence : le NELDER trouve-t-il un minimum stable ?",
            "Les temps optimaux dans l'onglet 'Temps optimaux' montrent le premier design optimise"
          )
        )
      ),
      list(
        label = "Etape 2/3 : Deuxieme passe NELDER (warfarin2b)",
        prefix = "warfarin2b",
        prev_step_prefix = "warfarin2",
        guide = list(
          context = "Deuxieme passe NELDER : demarre depuis les temps optimaux de l'etape 1. Le dataset warfarin2b.csv contient le design optimise de l'etape precedente.",
          points = c(
            "Comparez les RSE avec l'etape precedente (auto-chargee en comparaison)",
            "L'OFV devrait etre inferieur ou egal a l'etape 1 (raffinement)",
            "Si l'OFV est identique, le NELDER a deja converge a l'etape 1",
            "Consultez la convergence pour verifier la stabilite"
          )
        )
      ),
      list(
        label = "Etape 3/3 : Troisieme passe NELDER (warfarin2c)",
        prefix = "warfarin2c",
        prev_step_prefix = "warfarin2b",
        guide = list(
          context = "Troisieme et derniere passe NELDER. Confirme que l'optimum est stable en redemarrant depuis l'etape 2.",
          points = c(
            "Comparez les RSE avec l'etape 2 : les valeurs devraient etre quasi-identiques si l'optimum est atteint",
            "L'OFV final est le meilleur critere D-optimal obtenu pour ce design",
            "Strategie anti-minima locaux (Bauer 2021) : enchainer RS -> STGR -> NELDER ou plusieurs passes NELDER",
            "Les temps optimaux finaux sont ceux a retenir pour le protocole"
          )
        )
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
    title = "Exemple 4 : PK-PD multi-reponses (4 etapes)",
    desc = "Modele warfarin PK-PD (concentration + effet). 4 etapes : evaluation FO, evaluation FOCEI, optimisation, raffinement.",
    dir = "examples/example4",
    labels = "THETA1=KA\nTHETA2=CL\nTHETA3=V\nTHETA4=RIN\nTHETA5=IC50\nTHETA6=KOUT",
    steps = list(
      list(
        label = "Etape 1/4 : Evaluation FO (eval)",
        prefix = "warfarin_pkpd_eval",
        prev_step_prefix = NULL,
        guide = list(
          context = "Modele PK-PD (ADVAN13 ODE) : CMT=2 PK, CMT=3 PD Emax. GROUPSIZE=52, FIMDIAG=1, VARCROSS=1. Evaluation du design initial (MAXEVAL=0).",
          points = c(
            "6 THETAs (KA, CL, V, RIN, IC50, KOUT) + 6 OMEGAs + 2 SIGMAs",
            "FIMDIAG=1 + VARCROSS=1 : FIM bloc-diagonale style PFIM",
            "Les RSE des parametres PD (RIN, IC50, KOUT) sont generalement plus grands que les PK",
            "Ce design initial sert de baseline pour les etapes suivantes"
          )
        )
      ),
      list(
        label = "Etape 2/4 : Evaluation FOCEI (eval2)",
        prefix = "warfarin_pkpd_eval2",
        prev_step_prefix = "warfarin_pkpd_eval",
        guide = list(
          context = "Meme modele, mais FIMTYPE=1 (au lieu de FIMDIAG=1) et GROUPSIZE=26. Evaluation FOCEI du design.",
          points = c(
            "FIMTYPE=1 vs FIMDIAG=1 : hypotheses differentes sur la structure de la FIM",
            "GROUPSIZE=26 (moitie de l'etape 1) : impact direct sur la precision (FIM proportionnelle a N)",
            "Comparez les RSE avec l'etape 1 : l'effet du GROUPSIZE et du FIMTYPE",
            "Pas de .tab pour cette etape (pas de $TABLE) — les temps optimaux ne sont pas disponibles"
          )
        )
      ),
      list(
        label = "Etape 3/4 : Optimisation (opt)",
        prefix = "warfarin_pkpd_opt",
        prev_step_prefix = "warfarin_pkpd_eval2",
        guide = list(
          context = "Optimisation des temps PK et PD via NELDER. GROUPSIZE=52, FIMTYPE=1, VARCROSS=1, APPROX=FO, MAXEVAL=9999.",
          points = c(
            "DESEL=TIME optimise les temps dans les fenetres TMIN/TMAX pour chaque CMT",
            "Les RSE devraient diminuer par rapport a l'evaluation (etape 1/2)",
            "Les temps optimaux PK et PD sont distincts (CMT=2 vs CMT=3)",
            "APPROX=FO : approximation first-order pour le calcul de la FIM"
          )
        )
      ),
      list(
        label = "Etape 4/4 : Optimisation affinee (opt2)",
        prefix = "warfarin_pkpd_opt2",
        prev_step_prefix = "warfarin_pkpd_opt",
        guide = list(
          context = "Deuxieme passe d'optimisation NELDER avec GROUPSIZE=26. Raffine le design de l'etape 3.",
          points = c(
            "GROUPSIZE=26 : moitie des sujets — impact sur les RSE attendus",
            "Comparez avec l'etape 3 : les temps optimaux changent-ils avec moins de sujets ?",
            "L'OFV devrait etre different (FIM proportionnelle a N)",
            "Les temps optimaux finaux sont dans l'onglet 'Temps optimaux'"
          )
        )
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
    table_no_range = c(1L, 4L),
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

# Pure helper — build file paths for a given dir/prefix
build_paths <- function(dir, prefix) {
  exts <- c("ext", "shk", "coi", "clt", "tab", "bfm", "cpu")
  paths <- setNames(vector("list", length(exts)), exts)
  for (et in exts) {
    f <- file.path(dir, paste0(prefix, ".", et))
    if (file.exists(f)) paths[[et]] <- f
  }
  paths
}

mod_examples_server <- function(id, session_main = NULL,
                                reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected <- reactiveValues(
      file_paths       = NULL,
      labels           = NULL,
      guide            = NULL,
      compare_paths    = NULL,
      compare_name     = NULL,
      summary_data     = NULL,
      ex_id            = NULL,
      step_idx         = 1L,
      n_steps          = 0L,
      banner_dismissed = FALSE
    )

    # -- load_step: closure over selected -----------------------------------
    load_step <- function(ex_id, step_idx) {
      ex <- .EXAMPLES[[ex_id]]
      has_steps <- !is.null(ex$steps)

      if (has_steps) {
        step   <- ex$steps[[step_idx]]
        prefix <- step$prefix
        guide  <- step$guide
      } else {
        prefix <- ex$prefix
        guide  <- ex$guide
      }

      selected$file_paths <- build_paths(ex$dir, prefix)
      selected$labels     <- ex$labels
      selected$guide      <- guide
      selected$step_idx   <- step_idx
      selected$n_steps    <- if (has_steps) length(ex$steps) else 0L
      selected$banner_dismissed <- FALSE

      # Compare: intra-example (prev_step_prefix) or cross-example
      if (has_steps && !is.null(step$prev_step_prefix)) {
        selected$compare_paths <- build_paths(ex$dir,
                                              step$prev_step_prefix)
        selected$compare_name  <- step$prev_step_prefix
      } else if (!has_steps && !is.null(ex$compare_with)) {
        comp_ex <- .EXAMPLES[[ex$compare_with]]
        selected$compare_paths <- build_paths(comp_ex$dir,
                                              comp_ex$prefix)
        selected$compare_name  <- comp_ex$title
      } else {
        selected$compare_paths <- NULL
        selected$compare_name  <- NULL
      }

      # Handle summary.tab (robust design — example3 only)
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

      # Track which example is active (set last to avoid
      # re-triggering observers mid-update)
      selected$ex_id <- ex_id
    }

    # -- Universal reset from app.R -----------------------------------------
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        selected$file_paths      <- NULL
        selected$labels          <- NULL
        selected$guide           <- NULL
        selected$compare_paths   <- NULL
        selected$compare_name    <- NULL
        selected$summary_data    <- NULL
        selected$ex_id           <- NULL
        selected$step_idx        <- 1L
        selected$n_steps         <- 0L
        selected$banner_dismissed <- FALSE
      })
    }

    # -- Modal: example picker ----------------------------------------------
    observeEvent(input$open_examples, {
      showModal(modalDialog(
        title = "Exemples Bauer 2021",
        size = "l",
        easyClose = TRUE,
        fluidRow(
          lapply(names(.EXAMPLES), function(eid) {
            ex <- .EXAMPLES[[eid]]
            step_badge <- if (!is.null(ex$steps)) {
              tags$span(
                style = "font-size:.72rem; color:#6b7280;",
                paste0(" (", length(ex$steps), " etapes)")
              )
            }
            column(3,
              div(
                class = "upload-box",
                style = "cursor:pointer; min-height:180px;",
                tags$h6(ex$title, step_badge,
                        style = "font-size:.9rem;"),
                tags$p(style = "font-size:.8rem; color:#4b5563;",
                       ex$desc),
                actionButton(ns(paste0("load_", eid)), "Charger",
                             class = "btn-sm btn-primary w-100")
              )
            )
          })
        )
      ))
    })

    # -- Load example handlers (always step 1) ------------------------------
    lapply(names(.EXAMPLES), function(ex_id) {
      observeEvent(input[[paste0("load_", ex_id)]], {
        load_step(ex_id, 1L)
        removeModal()
        showNotification(
          paste("Exemple charge :",
                .EXAMPLES[[ex_id]]$title),
          type = "message"
        )
      })
    })

    # -- Step navigation observers ------------------------------------------
    observeEvent(input$step_prev, {
      req(selected$step_idx > 1L)
      load_step(selected$ex_id, selected$step_idx - 1L)
    })

    observeEvent(input$step_next, {
      req(selected$step_idx < selected$n_steps)
      load_step(selected$ex_id, selected$step_idx + 1L)
    })

    # -- Dismiss banner observer --------------------------------------------
    observeEvent(input$dismiss_banner, {
      selected$banner_dismissed <- TRUE
    })

    # -- Return reactives ---------------------------------------------------
    list(
      file_paths       = reactive(selected$file_paths),
      labels           = reactive(selected$labels),
      guide            = reactive(selected$guide),
      compare_paths    = reactive(selected$compare_paths),
      compare_name     = reactive(selected$compare_name),
      summary_data     = reactive(selected$summary_data),
      step_idx         = reactive(selected$step_idx),
      n_steps          = reactive(selected$n_steps),
      banner_dismissed = reactive(selected$banner_dismissed),
      step_label       = reactive({
        ex <- if (!is.null(selected$ex_id)) {
          .EXAMPLES[[selected$ex_id]]
        }
        if (is.null(ex) || is.null(ex$steps)) return(NULL)
        ex$steps[[selected$step_idx]]$label
      }),
      table_no_range   = reactive({
        if (is.null(selected$ex_id)) return(NULL)
        .EXAMPLES[[selected$ex_id]]$table_no_range
      })
    )
  })
}
