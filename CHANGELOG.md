# Changelog

Tenu à jour à chaque PR mergée. VSCode Claude lit cette section en début de session pour rattraper le contexte.

## [En cours]
<!-- Ajouter ici les entrées des PRs mergées non encore archivées -->

---

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
