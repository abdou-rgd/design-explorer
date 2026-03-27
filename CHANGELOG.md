# Changelog

Tenu à jour à chaque PR mergée. VSCode Claude lit cette section en début de session pour rattraper le contexte.

## [En cours]
<!-- Ajouter ici les entrées des PRs mergées non encore archivées -->

---

## 2026-03-27 (session 2) — Phase 2 A5 : percentiles TSTRAT design robuste

- [feat] **Percentiles TSTRAT par point d'observation** (`mod_times.R`) : remplace le tableau résumé P10/médiane/P90 par strate (1 ligne/TSTRAT) par un tableau (Strate, Obs) montrant la distribution de chaque temps optimal individuel across les N subproblèmes. Calcul via `obs_idx = row_number()` par `(table_no, TSTRAT)` pour aligner les points avant agrégation par `(TSTRAT, obs_idx)`.

## 2026-03-27 (session 1) — Phase 1 : Ex4/Ex5 + suppression KPI bar + bugfixes

- [feat] **Exemple 4 et 5 intégrés** (`app.R`, `.EXAMPLES`) : ajout des entrées `example4_opt` (PK-PD multi-réponses, `compare_with = "example4"`) et `example5` (DS-optimality, `optdesign2.*`, `compare_with = "example1"`). Fichiers copiés depuis `docs/bauer2021_examples/example5/`.
- [feat] **Suppression complète du KPI bar** (`app.R`, `helpers_ui.R`, `styles.css`) : div UI, `renderUI`, et tous les styles CSS (y compris responsive) supprimés. Remplacé par les pills de run (`run_pill()`) déjà en place.
- [fix] **`sprintf` + CSS `%`** (`helpers_ui.R`) : `50%` → `50%%` dans `run_pill()` — le `%` non échappé levait une erreur (`%;ba` si `;background` suit).
- [fix] **Nommage colonnes RSE/Shrinkage** (`mod_params.R`) : `unname(vapply(...))` pour extraire les noms de runs → les colonnes affichent les vrais noms ("Run A") au lieu des clés rid (`PRIMARY`/`RUN_1`).
- [docs] Ajout gotchas `sprintf`+CSS`%` et `vapply`+`any_of()` dans `CLAUDE.md`.

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

- [fix] **Couleurs multi-run cassées** (`helpers_ui.H`, `mod_rse.R`, `mod_convergence.R`, `mod_times.R`) : `scale_*_manual(values = .RUN_COLORS)` passait la palette statique — si les `rid` dépassent `"run_3"` (counter non réinitialisé en session), les couleurs tombaient en NA. Fix : construire un vecteur dynamique `run_colors <- setNames(vapply(names(runs), run_color, ...), names(runs))` dans chaque module (pattern identique à `mod_relativeinf.R`). Extension de `.RUN_COLORS` jusqu'à `"run_5"`.
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
- [#11] fix: .gitignore — fichiers internes et non-redistribuables
- [#10] fix: imap_dfr → map_dfr (index inutilisé)
- [#9] refactor: réorganisation structure scripts→R/, docs, compat R 4.2.0
- [#8] fix: versions packages R 4.2.0 compat (purrr, dplyr, ggplot2)
- [#7] fix: reset_trigger param mod_upload_server
- [#6] fix: refactoring parsing fichiers + logging + compat R 4.2.0 (supersède #4)
- [#5] feat: terminologie Strate, visualisation PK/PD multi-compartiments (supersède #3)
- [#2] fix: codebase review — 6 corrections critiques
- [#1] docs: README.md initial ajouté
