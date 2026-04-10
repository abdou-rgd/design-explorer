# Changelog

Tenu à jour à chaque PR mergée. VSCode Claude lit cette section en début de session pour rattraper le contexte.

## V4.5.2 — 2026-04-10 (SSE Validation + Convergence + Upload fix)

### Validation SSE (`app/R/mod_sse_validation.R`, `R/sse_metrics.R`) — PR #41 (GH)
- [feat] **Module SSE Validation** : nouveau tab comparant RSE FIM vs RSE SSE (scatter, table, metriques)
- [feat] **CI 95%** sur les RSE SSE, **D-criterion** comparison, **panneau methodologie** explicatif
- [feat] **Auto-detection format** fichiers SSE (raw_results vs pre-computed)
- [feat] **Filtre parametres** sur le scatter plot (selectInput dynamique)
- [feat] **REE Boxplot** : distribution REE par parametre (5e/95e whiskers, diamant RB, CI 95%) — inspire Fayette 2026 Fig. 2
- [feat] **RSE Bar Chart** : barres groupees FIM vs SSE RSE avec lignes ref 20%/50% — inspire Fayette 2026 Fig. 3
- [feat] **Selecteur de plots** : checkboxes pour afficher/masquer chaque plot (Scatter, REE Boxplot, RSE Bar)
- [fix] **Hauteur scatter plot** augmentee + titre centre

### Convergence (`app/R/mod_convergence.R`)
- [feat] **Steps de convergence** : detection et affichage des etapes d'optimisation (RS, STGR, NELDER)

### PK Timeline (`app/R/mod_times.R`, `R/report_design.R`)
- [fix] **Hard cap 4 facettes max** pour les elementary designs (evite surcharge graphique)
- [fix] **Zoom convergence** trop large corrige

### Upload / RStudio compat — PR #41
- [fix] **`launch.browser = TRUE`** force par defaut : contourne bug fileInput dans le viewer RStudio integre (anciennes versions). Confirme sur machine collegue (meme R 4.2.0, RStudio different).

---

## V4.5.1 — 2026-04-08 (Elementary design display fix) — PR #40

### Plot & Table — Temps optimaux (`R/report_design.R`, `app/R/mod_times.R`)
- [fix] **`plot_model_prediction()`** : les runs d'optimisation avec GROUPSIZE > 1 generaient 180 facettes (une par elementary design ID). Detection automatique des bras via signature TSTRAT unique — un seul ID representatif par bras est affiche.
- [fix] **Table "Donnees temps optimaux"** : meme logique de filtrage appliquee au tableau DT et a l'export CSV. Passe de ~1500 lignes a ~17 (un bras SC + un bras IV).
- [feat] Colonnes `ID`, `TMIN`, `TMAX` ajoutees au tableau pour contextualiser les fenetres d'optimisation.

---

## V4.5.0 — 2026-04-08 (TOST Equivalence + UX N total) — PR #38

### Test d'equivalence TOST (`R/fim_metrics.R`, `app/R/mod_power.R`)
- [feat] **`compute_power_tost()`** : puissance du test TOST (Two One-Sided Tests) pour demontrer l'equivalence d'un parametre dans une marge [-delta, +delta]. Formules PFIM eq. 4-7, alpha une face.
- [feat] **`compute_nsn_tost()`** : NSN pour equivalence, par scaling FIM (ratio carre). PFIM eq. 6-7.
- [feat] **`compute_equiv_table()`** : wrapper appliquant TOST a tous les parametres d'un run.

### App Shiny
- [feat] **Tab "Equivalence (TOST)"** dans le module Power : table puissance TOST, table NSN equivalence, courbe Power(N) TOST (violet), 2 exports CSV.
- [feat] **Input delta** (marge symetrique, defaut 0.2) avec formules H0/H1 en HTML.
- [feat] **Detection hors-marge** : parametres avec |theta - h0| >= delta affiches en rouge "Oui", warning banner en haut.
- [feat] **Encadres explicatifs** : cards Wald (bordure bleue) et TOST (bordure violette) expliquant chaque test, son usage, et ses references.
- [feat] **N total editable** : `numericInput` directement dans le panneau Power (plus besoin de retourner au sidebar). Sync auto depuis le .ctl.
- [feat] **HelpText dataset** : explique la distinction dataset normal (1 ID = 1 sujet, GROUPSIZE=1) vs elementaire (peu d'IDs, N = IDs x GROUPSIZE).
- [fix] **Label GROUPSIZE clarifie** : sidebar affiche "Nombre total de sujets (N)" au lieu de "Effectif du groupe (GROUPSIZE)".
- [fix] **`%%` dans `paste0()`** : corrige l'affichage "%%" literal sur l'axe Y de la courbe TOST.

### Tests
- [test] 17 nouveaux tests TOST : power centered/off-center, outside margin, negative branch, edge cases (theta=0, rse=NA, delta<=0), monotonicity, NSN consistency, equiv_table avec example1. **175 total, 0 failures.**
- [verify] Formules cross-checkees : power a NSE = 0.800000 exactement. NSN scaling verifie pour cas frexa (2 IDs x GROUPSIZE=40 = 80 patients).
- [verify] PFIM eq. (1)-(2) matchent exactement. Eq. (3) NNI : PFIM manual utilise ratio lineaire (conservatif), notre code utilise ratio carre (correct, valide par PopED).
- [compat] R 4.2.0 clean (scan r42-compat-checker).

---

## V4.4.0 — 2026-04-07 (Power / NSN) — PR #37

### Nouveau module Power / NSN (`R/fim_metrics.R`, `app/R/mod_power.R`)
- [feat] **`compute_power_wald()`** : puissance du test de Wald (H0: theta=0) a partir du RSE predit par la FIM. Formule portee de PopED `evaluate_power.R` (Retout et al. 2007).
- [feat] **`compute_n_needed()`** : nombre de sujets necessaire (NSN) par scaling lineaire de la FIM. RSE(N) = RSE(N0) x sqrt(N0/N).
- [feat] **`plot_power_curve()`** : courbe Power(N) avec markers N actuel, N cible et ligne puissance cible.
- [feat] **`compute_power_table()`** : helper qui applique power + NSN a tous les parametres d'un run.
- [feat] **`parse_groupsize()`** (`R/parse_design_outputs.R`) : extrait GROUPSIZE= du bloc $DESIGN d'un control stream.

### App Shiny
- [feat] **Section "Decision"** dans la sidebar avec onglet **Power / NSN**.
- [feat] **Tab "Puissance (Wald)"** : tableau colore par parametre (vert >= 80%, orange 50-80%, rouge < 50%).
- [feat] **Tab "Nombre de sujets (NSN)"** : tableau N necessaire + RSE cible + Ratio N colore, courbe Power(N) par parametre, encadre explicatif des formules.
- [feat] **GROUPSIZE dans le drawer** : `numericInput` auto-rempli depuis le .ctl (upload ou exemples), reset a 1.
- [feat] **Downloads CSV** : export power table et NSN table.
- [fix] **Formules HTML** : entites HTML (&theta;, &radic;, &times;) au lieu d'escapes Unicode non interpretes.
- [bump] **V4.4.0 "Power to the People"**.

### Tests
- [test] 28 nouveaux tests (`tests/testthat/test-fim_metrics.R`) : power Wald (edge cases theta=0, rse=0, rse=NA, negatif, H0=theta), NSN (scaling, consistency power), parse_groupsize, plot_power_curve. **151 total, 0 failures.**
- [compat] R 4.2.0 clean (scan r42-compat-checker).

---

## V4.3.2 — 2026-04-01 (Multi-run audit + Refactor) — PRs #28–#36

### Refactor
- [refactor] **Split `parse_design_outputs.R`** (1268 lignes, 34 fonctions) en 5 fichiers < 425 lignes : `design_utils.R`, `design_io.R`, `design_metrics.R`, `design_summary.R`, `ctl_parsers.R` (PR #39)

### Multi-run fixes (PRs #31–#33)
- [fix] **`robust_summary()` multi-run** : itere sur `all_runs()` avec colonne `Run` en long-format (PR #31)
- [fix] **Convergence density multi-run** : detection robuste → `geom_density` par run robuste + `geom_vline` pour non-robustes (PR #32)
- [fix] **FIM cards + eigenvalues** synchronises avec le selecteur de run heatmap (PR #32)
- [fix] **RSE toggle Barplot/Waterfall** cache en multi-run (PR #32)
- [fix] **`vapply(r$name)` sans garde** : ajout `%||% "?"` dans 6 modules (PRs #31-#33)
- [fix] **Bootstrap 3 `btn-default`** : remplace `btn-outline-secondary` dans mod_raw et mod_examples (PR #33)

### Multi-run polish (PRs #29–#30)
- [feat] **`compute_robust_summary()`** : reimpl en R de `summary.f90` (Bauer) — percentiles par variable sur N subproblemes (PR #29)
- [feat] **n_sub dynamique** dans le bandeau Design Robuste (PR #30)
- [feat] **Dedup noms de runs** : suffixe " (2)" quand deux runs ont les memes args $DESIGN (PR #30)
- [feat] **Strates robustes** par position de ligne quand TSTRAT absent (PR #30)
- [feat] **Overlay multi-run** sur gantt robuste et plot Design Robuste (PR #30)

### Upload & Parsers (PR #28)
- [fix] **`.ctl/.mod/.con` acceptes** dans le `fileInput` des runs de comparaison
- [fix] **`parse_theta_labels()` reecrit** : gere multiples blocs `$THETA`, format Sanofi, index corrects

### Navigation (PRs #34–#36)
- [feat] **Architecture steps** : navigation multi-etapes ex2 (3 steps) et ex4 (4 steps), boutons Precedent/Suivant (PR #35)
- [feat] **6 slots comparaison** (1 primaire + 5), pagination RSE supprimee, RSE median FIM (PR #36)
- [fix] **`effective_tab_data()` fallback** pour multi-run sans .tab principal (PR #34)

---

## Pre-V6 — 2026-04-03 (Bug fixes + API prep + plot improvements)

### Parsers / API (`R/parse_design_outputs.R`)
- [fix] **`distinct(dat)` supprime dans `.parse_table_blocks()`** : supprimait silencieusement les lignes dupliquees legitimees dans `.tab`/`.ext`. Certains designs NONMEM ont des timepoints repetes (ex: example3).
- [fix] **Mapping OMEGA-ETA par index parse** : `row_number()` remplace par extraction de l'index depuis le nom du parametre (`OMEGA(3,3)` -> 3). Corrige le desalignement quand un OMEGA diagonal est fixe/absent.
- [fix] **Normalisation exposants D/d** : `as.numeric("1D-3")` retournait `NA` en R. Ajout `gsub("[dD]", "E", ...)` dans `read_prior_nwpri()`.
- [feat] **`read_cov()` et `read_cor()`** : parsers pour les fichiers `.cov` (variance-covariance) et `.cor` (correlation + SE sur diagonale) produits par NONMEM. Utilise un parser interne partage `.read_named_matrix()` (refactor de `read_coi()`).
- [feat] **`scale_fim()` et `vcov_from_fim()`** : fonctions pre-V6 pour scaling FIM par N et inversion canonique avec gestion FIM singuliere.
- [refactor] **`.param_type()` centralise** : deplace de `R/report_design.R` vers `R/parse_design_outputs.R` (partage entre plots et futurs metrics V6).

### Plots (`R/report_design.R`)
- [feat] **`plot_model_prediction()` — facet par ID** : multi-ID (ex: IV+SC elementary designs) affiche une facette par ID au lieu de tout superposer.
- [feat] **`plot_model_prediction()` — labels numeriques** : labels TSTRAT compacts ("1", "2"...) au lieu de "Strate 1", "Strate 2"...
- [feat] **`plot_model_prediction()` — axe secondaire PFIM-style** : temps de sampling exacts affiches sur l'axe x superieur (`sec.axis`).
- [refactor] **`plot_fim_heatmap()` utilise `get_cor_matrix()`** : suppression de la logique `solve() + cov2cor()` dupliquee.

### App Shiny (`app/R/mod_times.R`)
- [fix] **TSTRAT 1 manquant** : `obs[-1L, ]` apres filtre `EVID == 0` supprimait la premiere observation au lieu de la premiere dose. Retire aux 3 endroits concernes.
- [refactor] **Plot TSTRAT (gantt) retire en single-run** : redondant avec la courbe predite ; table elargie a 12 colonnes.
- [refactor] **Pagination DT retiree** : `dom = "t"` et `pageLength = nrow(obs_display)` pour tout afficher d'un coup.

### Docs / Tests
- [docs] Inventaire complet des 15 types de fichiers $DESIGN NONMEM dans `nonmem-design-reference.md`.
- [test] Mise a jour test `compute_robust_summary()` : 6 lignes/bloc attendues (distinct() retire).

---

## V4.3.1 — 2026-03-30 (Review fixes) — PRs #23 #24

- [fix] **RSE color tiers** (PR #23 — `R/report_design.R`, `app/R/mod_rse.R`) : ligne de référence 100% ajoutée en mode multi-run (`yintercept = c(20, 50, 100)`), accents restaurés dans les titres (`"RSE prédit"`), roxygen mis à jour (20%/50%/100%).
- [fix] **Export CSV temps optimaux** (PR #24 — `app/R/mod_times.R`) : reactive `robust_summary` partagée (fin de la duplication), CSV robuste aligné avec l'affichage (arrange + rename), CSV normal avec labels CMT + arrondi 4 décimales, `btn-default` (Bootstrap 3), `tryCatch` autour de `write_csv`, `na.rm = TRUE` dans `quantile`/`median`.

---

## V4.3.0 — 2026-03-30 (Exemples 6 & 7 + RSE + CSV) — PRs #20 #21 #22 #23 #24

- [feat] **Version affichée dans la sidebar** (PR #20 — `app.R`, `helpers_ui.R`) : numéro de version visible en bas de la sidebar.
- [fix] **Bug Run A DT + visionneuse Control Stream** (PR #21 — `app/R/mod_upload.R`, `app/R/mod_raw.R`) : crash DT en mode Run A corrigé, ajout d'un viewer `.ctl`/`.mod`/`.con`.
- [feat] **Exemples Bauer 6 et 7** (PR #22 — `app/R/mod_examples.R`, `app/examples/`) : exemple 6 (TMDD, STRAT/STRATF, ODE), exemple 7 (Bayes FIM, `tmdd2b.*`). 95 tests unitaires.
- [feat] **RSE colorés par 4 tiers** (PR #23 — `R/report_design.R`, `app/R/mod_rse.R`) : palette `< 20%` vert / `20-50%` orange / `50-100%` rouge / `> 100%` bordeaux. Ligne de référence à 100% ajoutée.
- [feat] **Export CSV temps optimaux** (PR #24 — `app/R/mod_times.R`) : bouton "Exporter CSV" dans l'onglet Temps, visible uniquement quand des données sont chargées. Cas robuste : résumé P10/Médiane/P90. Cas normal : colonnes affichées avec labels CMT.

---

## V4.2.0 — 2026-03-27 (Phase 2 Quick Wins) — PRs #14–#19

- [feat] **RSE 4 niveaux de sévérité** (PR #14 — `helpers_ui.R`, `styles.css`, `mod_params.R`) : ajout d'un 4ème tier RSE > 100% en bordeaux foncé (`#7f1d1d`). `RSE_THRESHOLDS = c(20, 50, 100)`, classe `.rse-very-poor`, `styleInterval(c(20,50,100), 4 couleurs)`.
- [feat] **Labels CMT explicites** (PR #14 — `parse_design_outputs.R`, `app.R`, `mod_times.R`) : nouvelle fonction `parse_cmt_labels()` qui parse `$MODEL COMP=(NOM)`. Champ `cmt_labels` dans la sidebar avec auto-fill depuis `.ctl` et reset. Colonne CMT affiche "Nom (CMT=N)" dans la table et le plot des temps optimaux.
- [feat] **Percentiles TSTRAT par point d'observation** (PR #15 — `mod_times.R`) : remplace le tableau résumé 1 ligne/TSTRAT par un tableau (Strate × Obs) montrant la distribution (P10/Médiane/P90) de chaque temps optimal individuel sur les N subproblèmes.
- [feat] **Efficiency ratio dans la table OFV** (PR #16 — `mod_params.R`) : ligne "D-efficiency vs ref" = `(exp(ΔOFV/p)-1)×100%` pour D-optimality, `ΔOFV%` pour les autres critères. Run primaire = "ref", visible uniquement en multi-run.
- [feat] **D-critère robuste** (PR #18 — `parse_design_outputs.R`, `mod_params.R`) : nouvelle fonction `get_robust_d_criterion()`. Formule : `exp(-mean(OFV_i)/p)` (moyenne géométrique de det(FIM)^(1/p) — standard Nyberg et al. / Bauer 2021). Bornes P10/P90 avec inversion OFV/D-crit. Ligne affichée dans la table OFV pour les designs robustes (multi-table, D-OPTIMALITY uniquement).
- [feat] **Guides pédagogiques enrichis ex1 et ex3** (PR #19 — `mod_examples.R`) : ex1 avec valeurs de référence Bauer Table 3 (OFV=-39.518, RSE(CL)=36.9%, RSE(V)=5.0%) et explication FIMDIAG=1 ; ex3 avec mécanisme `$SIM TRUE=PRIOR SUBPROB=1000`, temps médians 0.13/6.9/158.1 h (nos données), lien avec D-critère robuste.

## V4.1.0 — 2026-03-27 (Phase 1) — commit 00fb84a

- [feat] **Exemples 4 et 5 intégrés** (`app.R`, `.EXAMPLES`) : `example4_opt` (PK-PD, `compare_with = "example4"`) et `example5` (DS-optimality, `optdesign2.*`, `compare_with = "example1"`).
- [feat] **Suppression KPI bar** (`app.R`, `helpers_ui.R`, `styles.css`) : div UI, `renderUI`, CSS responsive — tous supprimés.
- [fix] **`sprintf` + CSS `%`** (`helpers_ui.R`) : `50%` → `50%%` dans `run_pill()`.
- [fix] **Nommage colonnes RSE/Shrinkage** (`mod_params.R`) : `unname(vapply(...))` → affiche les vrais noms de runs.
- [docs] Gotchas `sprintf+CSS%` et `vapply+any_of()` dans `CLAUDE.md`.

---

## 2026-03-26 — docs : state.md + commandes session

- [docs] Ajout dans `CLAUDE.md` : lecture prioritaire de `state.md` en début de session, commandes `/save-session` et `/resume-session`.

---

## 2026-03-25 (session 3) — QC round 2 : DT formatStyle + heatmap fallback

- [fix] **DT `formatStyle` crash "column not found"** (`mod_params.R`) : `dt_rse` et `dt_shk` utilisaient `formatStyle(col_name)` avec des noms sanitisés via `make.names()`. Quand le nom de run contient des espaces ou caractères spéciaux, le mapping nom→colonne DT était instable. Fix : remplacer par `formatStyle(col_idx)` avec `which(names(wide) %in% rnms)` — les indices de colonnes sont stables quelle que soit la casse ou le renommage du run.
- [fix] **FIM heatmap — fallback `"primary"` hardcodé** (`mod_fim.R`) : `selected = "primary"` dans le `selectInput` et `%||% "primary"` dans `renderPlot` cassaient si la première run avait un `rid` différent (ex: run chargée via `mod_compare`). Fix : utiliser `names(runs)[1]` comme fallback dynamique.
- [fix] **KPI bar — affichage simplifié** (`app.R`, `helpers_ui.R`) : le KPI bar affichait des métriques calculées uniquement depuis la primary run, donnant une impression d'exactitude trompeuse en multi-run. Remplacé par des pills de run colorés (pattern `run_pill()`) — correct pour 1 à N runs, toujours à jour. Suppression de `render_kpi_bar()`.
- [fix] **`mod_relativeinf.R` — couleurs statiques** : même fix dynamique `run_colors` que les autres modules (était manqué dans le round 1).
- [fix] **`ctl_lines_raw` non réinitialisé sur reset** (`mod_upload.R`) : l'observer de reset ne remettait pas `ctl_lines_raw(NULL)` → les labels de la session précédente persistaient après "Retirer la run".

## 2026-03-25 (session 2) — QC multi-run : 5 bugs corrigés

- [fix] **Couleurs multi-run cassées** (`helpers_ui.R`, `mod_rse.R`, `mod_convergence.R`, `mod_times.R`) : `scale_*_manual(values = .RUN_COLORS)` passait la palette statique — si les `rid` dépassent `"run_3"` (counter non réinitialisé en session), les couleurs tombaient en NA. Fix : construire un vecteur dynamique `run_colors <- setNames(vapply(names(runs), run_color, ...), names(runs))` dans chaque module (pattern identique à `mod_relativeinf.R`). Extension de `.RUN_COLORS` jusqu'à `"run_5"`.
- [fix] **Param labels ignorées en multi-run** (`mod_params.R`) : suppression du override `if (length(runs) > 1) param` — les labels sont maintenant appliqués de la même façon qu'en mono-run.
- [fix] **Auto-fill nom des runs de comparaison** (`mod_compare.R`) : l'observer d'upload détecte maintenant les fichiers `.ctl`/`.mod`/`.con` dans le tar.gz ou les uploads individuels. Appel à `parse_design_summary()` après parsing → `updateTextInput()` sur le nom de la run. Même comportement que la primary run.
- [fix] **Courbe prédite Temps optimaux — seulement primary** (`mod_times.R`) : en multi-run, `output$prediction` construit un dataset combiné (`imap` sur `all_runs()`) et overlaye les courbes IPRED par run avec couleurs distinctes. Fallback mono-run inchangé.
- [fix] **FIM heatmap — seulement primary** (`mod_fim.R`) : ajout d'un `selectInput` dynamique (`heatmap_run_selector`) visible uniquement en multi-run. `output$heatmap` utilise le `rid` sélectionné pour accéder à `r$coi_data %||% r$clt_data` du run voulu.

---

## 2026-03-25 — Bugfixes audit multi-run + robust design

- [fix] **`.parse_table_blocks()` table_no séquentiel** (`R/parse_design_outputs.R`) : NONMEM labelle tous les blocs `.tab` comme `TABLE NO. 1` en design robuste ($SIM TRUE=PRIOR). Le parser assigne maintenant des indices séquentiels 1:N quand tous les blocs ont le même numéro → `is_robust()` retourne TRUE → UI bascule sur boxplot + table résumé P10/Mediane/P90.
- [fix] **Bug run_counter non-monotone** (`mod_compare.R`) : suppression du décrement `run_counter(max(0L, run_counter() - 1L))` à la suppression d'un run. La limite est désormais vérifiée via `length(run_ids()) >= 3L`. Sans ce fix, supprimer puis re-ajouter un run générait un `rid` déjà dans `observed_ids` → aucun observer créé pour le nouveau run (upload/rename/remove silencieusement cassés).
- [fix] **`.RUN_COLORS` keyed par `rid`** (`helpers_ui.R` + `mod_rse`, `mod_convergence`, `mod_relativeinf`, `mod_times`) : les clés passent de `"Run A"/"Run B"/...` à `"primary"/"run_1"/...`. Dans les modules ggplot2, `mutate(run = r$name)` → `mutate(run = idx/rid)` via `imap`, avec `labels = run_labels` dans `scale_*_manual`. Renommer un run ne casse plus les couleurs ni la légende.
- [fix] **Debounce renommage run** (`mod_compare.R`) : l'observer de renommage est désormais débounce à 500ms (`debounce(reactive(...), 500)`), évitant une cascade de recalculs dans tous les modules à chaque frappe clavier.

---

## 2026-03-24 (session 2)
- [maintenance] Correction paths stales dans CLAUDE.md (docs/inspiration/ → docs/other_softwares/, chemins PDF corrigés)
- [maintenance] Simplification workflow section CLAUDE.md (passage 1 seul laptop perso Sanofi)
- [maintenance] Ajout 3 gotchas Shiny (observeEvent ignoreNULL, %||% priority, str_split_1)
- [infra] 2 hooks PostToolUse configurés (lintr auto sur .R, testthat auto sur parse_design_outputs)
- [infra] Agent r42-compat-checker créé (~/.claude/agents/)
- [infra] Skill shiny-check créé (~/.claude/skills/shiny-check/)
- [fix] tests/run_tests.R path detection (normalizePath(".") au lieu de sys.frame(0)$ofile)
- [fix] test-parse_design_outputs.R source path corrigé (scripts/ → R/)
- [maintenance] settings.json nettoyé (permissions stales supprimées)

## 2026-03-24 (session 1) — Bugfixes audit V4 + robust design + infra

- [fix] **Example 3 crash** (`app.R`) : `upload$summary_data()` n'existait pas dans le return de `mod_upload` → crash silencieux. Simplifié : `merged_summary` utilise `examples$summary_data()` uniquement.
- [fix] **`RSE_THRESHOLDS` / `RELINF_THRESHOLDS` non définis** (`helpers_ui.R`) : constantes référencées dans `rse_badge()`/`ri_badge()` mais jamais déclarées → crash silencieux. Ajout des définitions.
- [fix] **Support `.mod` / `.con`** (`mod_upload.R`, `app.R`) : convention Sanofi — accepte `.mod` et `.con` en plus de `.ctl` dans `fileInput`, détection tar.gz, et routing multi-fichiers.
- [fix] **str_split_1 incompatible R 4.2.0** : remplacé par `strsplit(x, sep)[[1]]` (stringr 1.4.0).
- [fix] **Retirer la run + upload après exemple** (`app.R`) : les métriques de la session précédente persistaient après reset — clearing complet des `reactiveVal` exemples.
- [feat] **Auto-fill param labels depuis `.ctl`** (`R/parse_design_outputs.R`, `app.R`) : `parse_theta_labels()` extrait les noms THETA depuis les commentaires `;[CL]` du bloc `$THETA`. `parse_design_summary()` génère un label court depuis les args `$DESIGN` (ex: `"FT=1/FO/VC=1 (optim)"`). Auto-remplissage du textarea labels + `primary_run_name` à l'upload.
- [feat] **Temps optimaux — support robust design** (`mod_times.R`, `report_design.R`) : détection `is_robust` via `n_distinct(tab$table_no) > 1` → boxplot distribution par TSTRAT + banner "N subproblèmes" (au lieu de 10 000 points illisibles). PK-PD (CMT > 1) → gantt coloré par CMT. `plot_optimal_times()` : suppression `geom_segment` depuis time=0, param `cmt_col`.
- [feat] **Réactivation exemple 3** (robust design, SUBPROB=1000) : `.EXAMPLES` mis à jour, fichiers présents dans `app/examples/`.
- [docs] Récupération plan Dataset Builder depuis PR #12 fermée → `docs/plans/dataset-builder-plan.md`.
- [infra] Setup PR template, conventions CHANGELOG, workflow.
- [infra] 2 hooks PostToolUse (lintr auto `.R`, testthat auto `parse_design_outputs`), agent `r42-compat-checker`, skill `shiny-check`.

## 2026-03-24
- [maintenance] Nettoyage codebase : suppression dossiers vides (`scripts/`, `dev/`), `app/test_load.R` supprimé, CLAUDE.md/README.md désencombrés

## 2026-03-19 — PRs #10, #11 + commits directs

- [fix] **PR #10** : `imap_dfr()` → `map_dfr()` dans `mod_relativeinf.R` et `mod_times.R` (paramètre index inutilisé).
- [fix] **PR #11** : `.gitignore` étendu — exclusion docs internes (`intern_work/`, `superpowers/`), documents non-redistribuables, pages web sauvegardées.
- [fix] Correction paths `.gitignore` pour fichiers non-redistribuables (commit direct).

## 2026-03-19 — PR #9 (af8f935)
- [refactor] Réorganisation structure : `scripts/` → `R/`, docs réorganisés dans `docs/nonmem/` et `docs/papers/bauer2021/`
- [fix] Corrections compatibilité R 4.2.0 (purrr, tidyr, stringr fallbacks)

## 2026-03-17 — PRs B1–B8, C2–C12
- [fix] reset_trigger param dans mod_upload_server
- [fix] make.names() pour formatStyle (dt_rse, dt_shk)
- [fix] Bouton universel "Retirer la run" + clearing visuel outputs
- [fix] Refactoring parsing fichiers avec logging
- [feat] 81 tests unitaires dans `tests/testthat/test-parse_design_outputs.R`

## 2026-03-15 (approx.)
- [feat] Shiny app V4 : 12 modules, navigation sidebar custom JS, KPI bar, drawer toggle
- [feat] Onglet Design Robuste (mod_prior.R) avec summary.tab
- [feat] Support multi-run (jusqu'à 3 runs en comparaison)
- [feat] Exemple 3 (robust design, SUBPROB=1000) intégré

## Historique — PRs GitHub
- [#19] feat: guides enrichis ex1/ex3 (valeurs Bauer, mecanisme $SIM TRUE=PRIOR)
- [#18] feat: D-critere robuste exp(-mean(OFV)/p) pour design Monte Carlo
- [#17] docs: CHANGELOG audit — comble trou 19-27 mars, PRs #5-#19
- [#16] feat: efficiency ratio D-opt + ΔOFV% multi-run
- [#15] feat: percentiles TSTRAT par point d'observation (design robuste)
- [#14] feat: RSE 4 niveaux + labels CMT depuis $MODEL
- [#13] docs: VERSIONS.md — versioning poetique + technique
- [#12] (ferme) plan Dataset Builder recupere dans docs/plans/
- [#11] fix: .gitignore — fichiers internes et non-redistribuables
- [#10] fix: imap_dfr → map_dfr (index inutilisé)
- [#9] refactor: réorganisation structure scripts→R/, docs, compat R 4.2.0
- [#8] fix: versions packages R 4.2.0 compat (purrr, dplyr, ggplot2)
- [#7] fix: reset_trigger param mod_upload_server
- [#6] fix: refactoring parsing fichiers + logging + compat R 4.2.0 (supersède #4)
- [#5] feat: terminologie Strate, visualisation PK/PD multi-compartiments (supersède #3)
- [#2] fix: codebase review — 6 corrections critiques
- [#1] docs: README.md initial ajouté
