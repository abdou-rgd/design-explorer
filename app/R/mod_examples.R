# =============================================================================
# mod_examples.R -- Built-in examples (Bauer 2021)
# =============================================================================

.EXAMPLES <- list(
  example1 = list(
    title = "Example 1: Design evaluation",
    desc = "Warfarin 1-CMT model, block-diagonal FIM evaluation (FIMDIAG=1). No optimization.",
    dir = "examples/example1",
    prefix = "warfarin",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    guide = list(
      context = "Warfarin 1-compartment model (ADVAN2 TRANS2), first-order absorption, combined error. 32 subjects (GROUPSIZE=32), 3 sampling times at 0.5, 2 and 8 h. Reference values Bauer 2021 Table 3: OFV = -39.518.",
      points = c(
        "FIMDIAG=1 (block-diagonal): independence assumption between subjects and variability parameters. Faster but less precise than the full FIM (FIMDIAG=0)",
        "MAXEVAL=0: pure evaluation, no times are modified. Serves as baseline for comparison with optimization (Ex. 2)",
        "RSE(CL) = 36.9%, RSE(V) = 5.0% (Bauer Table 3): V is well estimated with this design, CL much less so. A single early time is insufficient for CL",
        "RELATIVEINF (dedicated tab) = contribution of each observation to ETA variance reduction. Low RELATIVEINF indicates an ETA poorly supported by the design",
        "The FIM (FIM tab) is a 3x3 matrix (CL, V, KA): off-diagonal elements measure correlation between parameter uncertainties"
      )
    )
  ),
  # Convention de nommage :
  # compare_with      = cle d'une autre entree .EXAMPLES (cross-exemple)
  # prev_step_prefix  = prefix fichier de l'etape precedente dans le meme dir (intra-exemple)
  example2 = list(
    title = "Example 2: Time optimization (3 steps)",
    desc = "Chain of NELDER optimizations: 3 successive passes where each step starts from the previous optimal design.",
    dir = "examples/example2",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    steps = list(
      list(
        label = "Step 1/3: First NELDER pass (warfarin2)",
        prefix = "warfarin2",
        prev_step_prefix = NULL,
        guide = list(
          context = "First NELDER optimization from initial design (3 times within TMIN/TMAX windows). GROUPSIZE=32, FIMDIAG=1, MAXEVAL=9999.",
          points = c(
            "Starting point: same times as Example 1 (pure evaluation)",
            "NELDER explores time windows to minimize -log(det(FIM))",
            "Check the Convergence tab: does NELDER find a stable minimum?",
            "Optimal times in the 'Optimal Times' tab show the first optimized design"
          )
        )
      ),
      list(
        label = "Step 2/3: Second NELDER pass (warfarin2b)",
        prefix = "warfarin2b",
        prev_step_prefix = "warfarin2",
        guide = list(
          context = "Second NELDER pass: starts from step 1 optimal times. The warfarin2b.csv dataset contains the optimized design from the previous step.",
          points = c(
            "Compare RSEs with the previous step (auto-loaded as comparison)",
            "OFV should be less than or equal to step 1 (refinement)",
            "If OFV is identical, NELDER already converged at step 1",
            "Check convergence to verify stability"
          )
        )
      ),
      list(
        label = "Step 3/3: Third NELDER pass (warfarin2c)",
        prefix = "warfarin2c",
        prev_step_prefix = "warfarin2b",
        guide = list(
          context = "Third and final NELDER pass. Confirms that the optimum is stable by restarting from step 2.",
          points = c(
            "Compare RSEs with step 2: values should be nearly identical if the optimum is reached",
            "Final OFV is the best D-optimal criterion obtained for this design",
            "Anti-local-minima strategy (Bauer 2021): chain RS -> STGR -> NELDER or multiple NELDER passes",
            "Final optimal times are the ones to retain for the protocol"
          )
        )
      )
    )
  ),
  example3 = list(
    title = "Example 3: Robust design ($PRIOR)",
    desc = "Robust optimization via $SIM TRUE=PRIOR SUBPROB=1000. Time distribution over 1000 parameter sets.",
    dir = "examples/example3",
    prefix = "priortrue",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA",
    has_summary = TRUE,
    summary_file = "examples/example3/summary.tab",
    guide = list(
      context = "Same warfarin model, robust optimization: $SIM TRUE=PRIOR SUBPROB=1000 draws 1000 parameter sets (THETA) from their prior distribution, then optimizes the design for each. The minimized OFV is E[-log(det(FIM))] over the prior = standard robust design criterion (Nyberg et al., Bauer 2021).",
      points = c(
        "Mechanism: SUBPROB=1000 generates 1000 independent sub-problems. For each subprob, true THETAs are drawn from the prior ($PRIOR NWPRI or $OMEGA/$SIGMA) and the FIM is evaluated/optimized with those values",
        "'Optimal Times' tab: the P10/median/P90 table by stratum shows the optimal times distribution. Our data: medians 0.13 h, triplet grouped around 6.9 h [P2.5=1.5, P97.5=23], 158.1 h. Bauer reports 159.9 h with a different seed",
        "Practical interpretation (Bauer): choose times as 0.13, 1.5, 7.0, 23.0, 160.0 h. Resulting OFV = -51.374, close to optimal (mean OFV = -51.95 in our data, -51.598 in Bauer)",
        "'Parameters' tab: the 'Robust D-criterion' row shows exp(-mean(OFV_i)/p) with P10/P90 bounds — the geometric mean of det(FIM)^(1/p) over 1000 prior realizations",
        "RSE and RELATIVEINF reflect sub-problem 1000 (last). They are not averages — the RSE distribution is not directly accessible here"
      )
    )
  ),
  example4 = list(
    title = "Example 4: PK-PD multi-response (4 steps)",
    desc = "Warfarin PK-PD model (concentration + effect). 4 steps: FO evaluation, FOCEI evaluation, optimization, refinement.",
    dir = "examples/example4",
    labels = "THETA1=KA\nTHETA2=CL\nTHETA3=V\nTHETA4=RIN\nTHETA5=IC50\nTHETA6=KOUT",
    steps = list(
      list(
        label = "Step 1/4: FO evaluation (eval)",
        prefix = "warfarin_pkpd_eval",
        prev_step_prefix = NULL,
        guide = list(
          context = "PK-PD model (ADVAN13 ODE): CMT=2 PK, CMT=3 PD Emax. GROUPSIZE=52, FIMDIAG=1, VARCROSS=1. Initial design evaluation (MAXEVAL=0).",
          points = c(
            "6 THETAs (KA, CL, V, RIN, IC50, KOUT) + 6 OMEGAs + 2 SIGMAs",
            "FIMDIAG=1 + VARCROSS=1: block-diagonal FIM (PFIM style)",
            "RSEs for PD parameters (RIN, IC50, KOUT) are generally larger than PK",
            "This initial design serves as baseline for the following steps"
          )
        )
      ),
      list(
        label = "Step 2/4: FOCEI evaluation (eval2)",
        prefix = "warfarin_pkpd_eval2",
        prev_step_prefix = "warfarin_pkpd_eval",
        guide = list(
          context = "Same model, but FIMTYPE=1 (instead of FIMDIAG=1) and GROUPSIZE=26. FOCEI design evaluation.",
          points = c(
            "FIMTYPE=1 vs FIMDIAG=1: different assumptions on FIM structure",
            "GROUPSIZE=26 (half of step 1): direct impact on precision (FIM proportional to N)",
            "Compare RSEs with step 1: effect of GROUPSIZE and FIMTYPE",
            "No .tab for this step (no $TABLE) — optimal times not available"
          )
        )
      ),
      list(
        label = "Step 3/4: Optimization (opt)",
        prefix = "warfarin_pkpd_opt",
        prev_step_prefix = "warfarin_pkpd_eval2",
        guide = list(
          context = "PK and PD time optimization via NELDER. GROUPSIZE=52, FIMTYPE=1, VARCROSS=1, APPROX=FO, MAXEVAL=9999.",
          points = c(
            "DESEL=TIME optimizes times within TMIN/TMAX windows for each CMT",
            "RSEs should decrease compared to evaluation (step 1/2)",
            "PK and PD optimal times are distinct (CMT=2 vs CMT=3)",
            "APPROX=FO: first-order approximation for FIM computation"
          )
        )
      ),
      list(
        label = "Step 4/4: Refined optimization (opt2)",
        prefix = "warfarin_pkpd_opt2",
        prev_step_prefix = "warfarin_pkpd_opt",
        guide = list(
          context = "Second NELDER optimization pass with GROUPSIZE=26. Refines the design from step 3.",
          points = c(
            "GROUPSIZE=26: half the subjects — impact on expected RSEs",
            "Compare with step 3: do optimal times change with fewer subjects?",
            "OFV should be different (FIM proportional to N)",
            "Final optimal times are in the 'Optimal Times' tab"
          )
        )
      )
    )
  ),
  example5 = list(
    title = "Example 5: DS-optimality",
    desc = "DS-optimality criterion (OFVTYPE=6) with uninteresting parameters (UNINT). Extended warfarin model.",
    dir = "examples/example5",
    prefix = "optdesign2",
    labels = "THETA1=CL\nTHETA2=V\nTHETA3=KA\nTHETA4=F1",
    compare_with = "example1",
    guide = list(
      context = "DS-optimality: maximizes precision on a subset of parameters of interest, treating others as uninteresting (UNINT). Here F1 (bioavailability) is the target parameter.",
      points = c(
        "DS-OFV criterion = -log(det(FIM_interest)): only the FIM of parameters of interest counts",
        "UNINT designates uninteresting (nuisance) parameters",
        "Compare with Example 1 (D-optimality): optimal times differ depending on the criterion",
        "Useful when some parameters are already well estimated or irrelevant for the decision"
      )
    )
  ),
  example6 = list(
    title = "Example 6: TMDD, STRAT/STRATF",
    desc = "TMDD ODE model (ADVAN13), optimization with dose stratification. 4 chained $DESIGN blocks.",
    dir = "examples/example6",
    table_no_range = c(1L, 4L),
    prefix = "tmdd2",
    labels = "THETA1=VC\nTHETA2=K10\nTHETA3=K12\nTHETA4=K21\nTHETA5=VM\nTHETA6=KMC\nTHETA7=K03\nTHETA8=K30",
    guide = list(
      context = "TMDD (Target-Mediated Drug Disposition) model with 3 compartments and ODE (ADVAN13). 8 PK parameters (VC, K10, K12, K21, VM, KMC, K03, K30), 2 dose levels (300 and 10000), 5 times per stratum. GROUPSIZE=50, FIMDIAG=1, VARCROSS=1.",
      points = c(
        "4 chained $DESIGN blocks with NELDER: restart strategy to avoid local minima. Compare OFVs of 4 tables in the convergence tab",
        "STRAT/STRATF: dose stratification. STRATF optimizes the subject proportion per stratum (~59%/41%)",
        "Multi-CMT: CMT=1 (drug) and CMT=3 (receptor) observed. 2 separate residual errors (EPS(1-2) PK, EPS(3-4) receptor)",
        "ODE model (ADVAN13): required for non-linear TMDD kinetics. TOL=12, ATOL=12 for precision",
        "8 diagonal OMEGAs + 4 SIGMAs (2 FIXED at 0.001) — 20x20 FIM matrix"
      )
    )
  ),
  example7 = list(
    title = "Example 7: Bayes FIM (OFVTYPE=8)",
    desc = "Individual Bayesian FIM. D-opt workflow (optimization) then Bayes (evaluation).",
    dir = "examples/example7",
    prefix = "tmdd2b",
    labels = "THETA1=VC\nTHETA2=K10\nTHETA3=K12\nTHETA4=K21\nTHETA5=VM\nTHETA6=KMC\nTHETA7=K03\nTHETA8=K30",
    compare_with = "example7_bayes",
    guide = list(
      context = "Same TMDD model as example 6. 2-problem workflow: (1) classic D-opt optimization (OFVTYPE=1, MAXEVAL=50000), (2) Bayesian FIM evaluation (OFVTYPE=8, MAXEVAL=0) on the optimized design. The .bfm file contains the individual conditional variance matrix.",
      points = c(
        "2-step workflow (Bauer): optimize with D-opt (fast, robust) then evaluate with Bayes FIM (more realistic)",
        "OFVTYPE=8 = Bayesian FIM: incorporates prior information ($OMEGA) into the criterion",
        "The .bfm contains the ETC matrix (conditional variance-covariance) — measures individual precision, not population-level (visualization planned)",
        "Compare with pure Bayes optimization (Compare button): same parameters but different criterion",
        "Simultaneous TIME+DOSE optimization (DESEL=TIME and DESEL=AMT in the same $DESIGN block)"
      )
    )
  ),
  example7_bayes = list(
    title = "Example 7b: Pure Bayes optimization",
    desc = "Direct OFVTYPE=8 optimization (Bayes FIM). Compare with the D-opt then Bayes workflow.",
    dir = "examples/example7",
    prefix = "optex6d17_8",
    labels = "THETA1=VC\nTHETA2=K10\nTHETA3=K12\nTHETA4=K21\nTHETA5=VM\nTHETA6=KMC\nTHETA7=K03\nTHETA8=K30",
    guide = list(
      context = "Same TMDD model, but direct optimization with OFVTYPE=8 (Bayes FIM). Unlike Example 7 which first optimizes with D-opt then evaluates with Bayes, here the optimization directly uses the Bayesian criterion.",
      points = c(
        "OFVTYPE=8 from the start: the optimization criterion is the Bayesian FIM, not the population FIM",
        "Multiple chained $DESIGN blocks (6 tables): progressive exploration of the optimization landscape",
        "Compare optimal times with Example 7: the Bayesian criterion may favor different designs",
        "D-opt and Bayes OFVs are not directly comparable (different scales)",
        "The .bfm file contains the conditional variance progression across iterations (visualization planned)"
      )
    )
  )
)

mod_examples_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("open_examples"), "Examples", icon = icon("book-open"),
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

    selection <- reactiveVal(NULL)
    selection_revision <- reactiveVal(0L)

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

      # Compare: intra-example (prev_step_prefix) or cross-example
      if (has_steps && !is.null(step$prev_step_prefix)) {
        compare_paths <- build_paths(ex$dir, step$prev_step_prefix)
        compare_name <- step$prev_step_prefix
      } else if (!has_steps && !is.null(ex$compare_with)) {
        comp_ex <- .EXAMPLES[[ex$compare_with]]
        compare_paths <- build_paths(comp_ex$dir, comp_ex$prefix)
        compare_name <- comp_ex$title
      } else {
        compare_paths <- NULL
        compare_name <- NULL
      }

      # Handle summary.tab (robust design — example3 only)
      if (isTRUE(ex$has_summary) && !is.null(ex$summary_file)) {
        if (file.exists(ex$summary_file)) {
          summary_data <- tryCatch(
            read_summary_tab(ex$summary_file),
            error = function(e) NULL
          )
        } else {
          summary_data <- NULL
        }
      } else {
        summary_data <- NULL
      }

      revision <- isolate(selection_revision()) + 1L
      selection_revision(revision)
      selection(list(
        file_paths = build_paths(ex$dir, prefix),
        labels = ex$labels,
        guide = guide,
        compare_paths = compare_paths,
        compare_name = compare_name,
        summary_data = summary_data,
        ex_id = ex_id,
        step_idx = step_idx,
        n_steps = if (has_steps) length(ex$steps) else 0L,
        banner_dismissed = FALSE,
        table_no_range = ex$table_no_range,
        revision = revision
      ))
    }

    # -- Universal reset from app.R -----------------------------------------
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        selection(NULL)
      })
    }

    # -- Modal: example picker ----------------------------------------------
    observeEvent(input$open_examples, {
      showModal(modalDialog(
        title = "Bauer 2021 Examples",
        size = "l",
        easyClose = TRUE,
        fluidRow(
          lapply(names(.EXAMPLES), function(eid) {
            ex <- .EXAMPLES[[eid]]
            step_badge <- if (!is.null(ex$steps)) {
              tags$span(
                style = "font-size:.72rem; color:#6b7280;",
                paste0(" (", length(ex$steps), " steps)")
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
                actionButton(ns(paste0("load_", eid)), "Load",
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
          paste("Example loaded:",
                .EXAMPLES[[ex_id]]$title),
          type = "message"
        )
      })
    })

    # -- Return reactives ---------------------------------------------------
    selected_field <- function(name) {
      reactive({
        value <- selection()
        if (is.null(value)) NULL else value[[name]]
      })
    }

    list(
      selection = reactive(selection()),
      file_paths = selected_field("file_paths"),
      labels = selected_field("labels"),
      guide = selected_field("guide"),
      compare_paths = selected_field("compare_paths"),
      compare_name = selected_field("compare_name"),
      summary_data = selected_field("summary_data"),
      table_no_range = selected_field("table_no_range")
    )
  })
}
