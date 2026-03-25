# Changelog

Tenu à jour à chaque PR mergée. VSCode Claude lit cette section en début de session pour rattraper le contexte.

## [En cours]
<!-- Ajouter ici les entrées des PRs mergées non encore archivées -->

---

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

## 2026-03-24
- [maintenance] Nettoyage codebase : suppression dossiers vides (`scripts/`, `dev/`), `app/test_load.R` supprimé, CLAUDE.md/README.md désencombrés

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
- [#8] fix: versions packages R 4.2.0 compat (purrr, dplyr, ggplot2)
- [#7] fix: reset_trigger param mod_upload_server
- [#2] fix: codebase review — 6 corrections critiques
- [#1] docs: README.md initial ajouté
