# CLAUDE.md — Stage Sanofi / Optimal Design NONMEM

## Workflow

### Début de session — rattraper le contexte
0. **EN PRIORITÉ** : lire `~/.claude/projects/c--Users-abdou-Desktop-ClaudeProjets/memory/state.md` → état courant (tâche en cours, prochaine étape, décisions actives)
1. `git fetch --all && git log --oneline origin/main..HEAD` → nouvelles branches/commits
2. Lire `CHANGELOG.md` section `[En cours]` → PRs récentes
3. Vérifier les PRs ouvertes via MCP GitHub (`mcp__plugin_github_github__list_pull_requests`)

### Dossiers locaux
- `C:/Users/abdou/Desktop/ClaudeProjets/` — working repo (docs, CLAUDE.md, tout)
- `C:/Users/abdou/Desktop/ClaudeProjets-app/` — miroir livrable (GitHub Desktop synchro auto depuis le laptop Sanofi)

---

## Notes pour Claude

- **Lecture du manuel** : `docs/nonmem/manuel_nonmem.txt` (13 057 lignes) — utiliser `Read` avec `offset`/`limit`. Offsets des sections → `.claude/rules/nonmem-design-reference.md`
- **`gh` CLI absent du PATH** — utiliser les outils MCP GitHub ou `git` local pour les opérations GitHub.
- **Skills R** : repo `https://github.com/abdou-rgd/r-claude-skills.git` — cloner dans `/tmp/` et copier les dossiers voulus dans `~/.claude/skills/`
- **Rscript** : `"/c/Program Files/R/R-4.5.2/bin/Rscript" script.R` — toujours passer par un fichier `.R` (jamais `-e "..."` : segfault sous bash/WSL Windows). Rscript local = R 4.5.2 ; **cible serveur Sanofi = R 4.2.0** (voir tableau versions).

---

## Commandes rapides

```r
# Lancer l'app Shiny (depuis la racine ClaudeProjets/)
shiny::runApp("app/")
```

```bash
# Installer les dépendances Shiny
"/c/Program Files/R/R-4.5.2/bin/Rscript" app/install_deps.R
```

```bash
# Lancer les tests unitaires — depuis la racine ClaudeProjets/
"/c/Program Files/R/R-4.5.2/bin/Rscript" tests/run_tests.R
```

```bash
# Sauvegarder l'état de session avant de fermer
/save-session
```

```bash
# Reprendre où on s'était arrêté au démarrage
/resume-session
```

---

## Contexte du projet

Stage M2 Sciences des données de santé, Sanofi.
Deux missions principales :
1. **Recherche scientifique** sur l'optimisation d'essais cliniques en pharmacométrie
2. **Outils R post-process** pour faciliter l'utilisation de `$DESIGN` NONMEM aux pharmacométriciens

Focus central : fonctionnalité **`$DESIGN`** de NONMEM 7.5+ — évaluation et optimisation de design via la Fisher Information Matrix (FIM).

Contrainte : aucune donnée réelle Sanofi ne peut être partagée (confidentialité).

---

## Stack technique

- **Langage** : R
- **Logiciel source** : NONMEM 7.5+ (sorties texte brut)
- **Python** : disponible à `C:/Users/abdou/AppData/Local/Python/pythoncore-3.14-64/python.exe` (PyMuPDF installé pour lire les PDFs)

### Versions serveur cible (R 4.2.0 — Sanofi RStudio Server)

> **IMPORTANT** : toujours coder pour ces versions. Ne pas utiliser les fonctionnalités listées dans "À NE PAS utiliser".

| Package  | Version | À NE PAS utiliser (trop récent)                                                  |
|----------|---------|-----------------------------------------------------------------------------------|
| R        | 4.2.0   | pipe natif `|>` OK (4.1+), mais pas `_` placeholder (4.2+ partiel, 4.3 stable)  |
| shiny    | 1.7.1   | Font Awesome 6 (`icon("xmark")` → utiliser `icon("times")`)                     |
| bslib    | 0.3.1   | `card()`, `sidebar()`, `layout_sidebar()`, `page_sidebar()` (≥ 0.4)             |
| DT       | 0.23    | OK                                                                                |
| ggplot2  | 3.3.6   | `linewidth=` (≥ 3.4.0, utiliser `size=`), `geom_sf_label()` coord changes       |
| dplyr    | 1.0.9   | `.default` dans `case_when()`, `.by=`, `reframe()`, `pick()`, `.env` (≥ 1.1.0)  |
| tidyr    | 1.2.0   | `separate_wider_delim()` (≥ 1.3.0), OK sinon                                    |
| stringr  | 1.4.0   | `str_equal()`, `str_like()`, `str_width()`, `str_split_1()` rewrite (≥ 1.5.0)   |
| purrr    | 0.3.4   | `list_c()`, `list_rbind()`, `list_flatten()`, `map_vec()` (≥ 1.0.0)             |
| readr    | 2.1.2   | OK                                                                                |

---

## Préférences de code

- Privilégier le style `tidyverse` (pipe `|>` natif R 4.1+)
- Gérer les cas limites : fichiers manquants, formats NONMEM variants, multiple `TABLE NO.`

---

## Conventions de nommage

- Fonctions : `read_ext()`, `read_shk()`, `read_clt()`, `read_coi()`, `read_tab()`
- Fonctions d'extraction : `get_ofv()`, `get_se()`, `get_rse()`, `get_relativeinf()`, `get_final_params()`, `get_optimal_times()`
- Fonctions de visualisation : `plot_fim()`, `plot_relativeinf()`, `plot_convergence()`
- Fichiers de scripts : `parse_ext.R`, `parse_design_outputs.R`, `report_design.R`

---

## Notes importantes — $DESIGN

- `$DESIGN` automatiquement configure `$COV MATRIX=R UNCONDITIONAL`
- FIM donne une **borne inférieure** de l'incertitude réelle (SEs prédits ≤ SEs réels)
- Limitation : **données continues uniquement** — pas discret, ordinal, TTE
- Recommandation : utiliser $DESIGN pour présélectionner 1-3 designs, puis valider par CTS
- Chaîner plusieurs `$DESIGN` dans un `$PROB` (RS → STGR → NELDER) pour éviter les minima locaux
- MU-referencing des THETAs = gain de vitesse majeur avec FIMTYPE=1

---

## Deliverables

- **Pas d'emojis dans le code livrable** : jamais d'emojis dans les noms d'onglets, metric cards, labels, messages d'alerte, ou titres de plots — utiliser des indicateurs textuels/CSS à la place
