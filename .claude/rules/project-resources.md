# Ressources disponibles dans le projet

| Fichier | Description |
|---------|-------------|
| `R/design_utils.R` | Utilitaires partages : `%||%`, `.EXT_ITER`, `.parse_table_blocks()`, `prepare_tab_obs()` |
| `R/design_io.R` | Lecteurs NONMEM : `read_ext()`, `read_shk()`, `read_coi()`, `read_cov()`, `read_cor()`, `read_clt()`, `read_tab()`, `read_cpu()`, `read_summary_tab()` |
| `R/design_metrics.R` | Extracteurs + FIM : `get_rse()`, `get_relativeinf()`, `get_d_criterion()`, `get_cor_matrix()`, `.param_type()`, `scale_fim()`, `vcov_from_fim()`, `compute_robust_summary()` |
| `R/design_summary.R` | Affichage console : `summary_design()` |
| `R/ctl_parsers.R` | Parseurs .ctl : `parse_theta_labels()`, `parse_design_summary()`, `parse_cmt_labels()`, `parse_groupsize()`, `read_prior_nwpri()`, `parse_advan_trans()` |
| `R/fim_metrics.R` | Power/NSN : `compute_power_wald()`, `compute_n_needed()`, `compute_power_tost()`, `compute_nsn_tost()`, `plot_power_curve()` |
| `R/tab_dispatch.R` | Pattern detection + smooth-curve engine selection : `detect_tab_pattern()`, `pick_smooth_curve_engine()`, `render_empty_state()`, `render_fallback_dot_plot()`, `render_pk_timeline()` |
| `R/pk_templates.R` | Closed-form PK templates : `pk_1cpt_iv()`, `pk_1cpt_oral()`, `pk_2cpt_iv()`, `pk_2cpt_oral()`, `pk_template_for_advan()` |
| `R/report_design.R` | Visualisations : `plot_relativeinf()`, `plot_rse()`, `plot_se()`, `plot_rse_waterfall()`, `plot_convergence()`, `plot_convergence_steps()`, `build_convergence_steps()`, `plot_fim_heatmap()`, `plot_optimal_times()`, `plot_model_prediction()`, `plot_pk_profile()`. Helpers internes : `.empty_plot()`, `.prep_points()`, `.select_representative_ids()` |
| `R/sse_diagnostics.R` | SSE intrinsic analysis : `read_sse_raw_all()`, `compute_run_health()`, `compute_param_distributions()`, `compute_empirical_correlations()`, `compute_param_diagnostics()`, `plot_param_distributions()`, `plot_ofv_distribution()`, `plot_empirical_cor_heatmap()` |
| `app/app.R` | Application Shiny post-processing $DESIGN (V5.4) — navbarPage, `shiny::runApp("app/")` depuis la racine. Merged reactives centralisees : `merged_ext/shk/coi/clt/tab/ctl/cpu`, `merged_ctl_lines`, `merged_true_vals`. SSE uploads centralisees via `sse_upload` |
| `app/R/` | 18 modules : upload, compare, examples, params, rse, relativeinf, fim, times, prior, convergence, ctl_stream, raw, helpers_ui, power, sse_upload, sse_validation, sse_analysis, sse_comparison. `helpers_ui.R` contient `extract_design_files()`, `ctl_status_banner()`, `sse_status_banner()`, `make_health_pills()` |
| `app/examples/` | Exemples Bauer 2021 intégrés (example1–7, fichiers `.ext`/`.shk`/`.coi`/`.clt`/`.tab`) |
| `app/www/styles.css` | Styles CSS V4 (CSS variables, sidebar layout, drawer, metric cards) |
| `app/install_deps.R` | Installe les packages Shiny manquants (shiny, bslib, DT) |
| `docs/other_softwares/PFIM/` | Code source PFIM 7.0 — référence SE/RSE/shrinkage (non tracké git) |
| `docs/other_softwares/PopED-master/` | Code source PopED — référence efficiency(), plot_efficiency_of_windows() (non tracké git) |
| `docs/nonmem/manuel_nonmem.txt` | Manuel NONMEM 7.5.1 complet (13 057 lignes) — lisible via Read avec offset |
| `docs/papers/bauer2021/bauer2021_text.txt` | Papier Bauer 2021 extrait en texte — lisible directement |
| `docs/nonmem/doc_nonmem_design.pdf` | Section I.72 du manuel (18p) — $DESIGN |
| `docs/bauer2021_examples/` | 7 exemples complets avec tous les fichiers NONMEM |
| `docs/courses/nlme.pdf` | Cours NLME Leroux (janv. 2026) |
| `docs/intern_work/redaction.docx` | Mémoire de stage en cours — question de recherche, méthodologie, planning |

> **Vision V5 :** l'app évolue vers un outil bout-en-bout : **générateur de control stream $DESIGN** (inputs : modèle, paramètres, design candidat → output : `.ctl` prêt à soumettre) + post-processing existant.
