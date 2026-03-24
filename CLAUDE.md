# CLAUDE.md — Stage Sanofi / Optimal Design NONMEM

## Workflow

### Début de session — rattraper le contexte
1. `git fetch --all && git log --oneline origin/main..HEAD` → nouvelles branches/commits
2. Lire `CHANGELOG.md` section `[En cours]` → PRs récentes
3. Vérifier les PRs ouvertes via MCP GitHub (`mcp__plugin_github_github__list_pull_requests`)

### Dossiers locaux
- `C:/Users/abdou/Desktop/ClaudeProjets/` — working repo (docs, CLAUDE.md, tout)
- `C:/Users/abdou/Desktop/ClaudeProjets-app/` — miroir livrable (GitHub Desktop synchro auto depuis le laptop Sanofi)

---

## Notes pour Claude

- **Lecture du manuel** : `docs/nonmem/manuel_nonmem.txt` (13 057 lignes) — utiliser `Read` avec `offset`/`limit`. Table des sections ci-dessous.

### Offsets `docs/nonmem/manuel_nonmem.txt` (numéros de ligne exacts)

| Section | Contenu | Ligne début | Ligne fin approx. |
|---------|---------|-------------|-------------------|
| I.34 | Introduction aux méthodes EM et Monte Carlo | 4322 | 4326 |
| I.35 | ITS (Iterative Two Stage) | 4327 | 4335 |
| I.36 | IMP (Importance Sampling EM) | 4336 | 4448 |
| I.37 | IMPMAP (IMP + MAP) | 4449 | 4458 |
| I.38 | SAEM | 4459 | 4605 |
| I.39 | BAYES (MCMC) | 4606 | 4685 |
| I.40 | NUTS (No U-Turn Sampler, NM74) | 4686 | 4956 |
| I.41 | $PRIOR NWPRI — structure des priors | 4957 | 5177 |
| I.43 | Notes générales EM + options $EST | 5190 | 5275 |
| I.44 | MU-Referencing | 5276 | 5477 |
| I.56 | $COV — options supplémentaires | 5922 | 6112 |
| I.61 | Format du fichier .res (rapport) | 6672 | 6748 |
| I.62 | Format du fichier .ext (raw output) | 6765 | 6859 |
| I.63 | Fichiers supplémentaires (.phi, .ets…) | 6860 | 7043 |
| I.67 | $RCOV / $RCOVI (design séquentiel) | 7317 | 7428 |
| I.72 | $DESIGN — évaluation et optimisation | 7521 | 8048 |
| I.73 | Parallel Computing | 8049 | 9099 |
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
# Lancer les tests unitaires (81 tests) — depuis la racine ClaudeProjets/
"/c/Program Files/R/R-4.5.2/bin/Rscript" tests/run_tests.R
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

## Ressources disponibles dans le projet

| Fichier | Description |
|---------|-------------|
| `R/parse_design_outputs.R` | Parsers R : `read_ext()`, `read_shk()`, `read_coi()`, `read_clt()`, `read_tab()`, `read_prior_nwpri()`, `read_summary_tab()`, `get_rse()`, `get_relativeinf()`, `get_d_criterion()`, `get_cor_matrix()`, `summary_design()` |
| `R/report_design.R` | Visualisations : `plot_relativeinf()`, `plot_rse()`, `plot_se()`, `plot_rse_waterfall()`, `plot_convergence()`, `plot_fim_heatmap()`, `plot_optimal_times()` |
| `app/app.R` | Application Shiny post-processing $DESIGN (V4) — sidebar nav, drawer, KPI bar — `shiny::runApp("app/")` depuis la racine |

> **Vision V5 :** l'app évolue vers un outil bout-en-bout : **générateur de control stream $DESIGN** (inputs : modèle, paramètres, design candidat → output : `.ctl` prêt à soumettre) + post-processing existant. C'est le cœur de la "méthode rapide et robuste".
| `app/R/` | 12 modules : upload, compare, examples, params, rse, relativeinf, fim, times, prior, convergence, raw, helpers_ui |
| `app/examples/` | Exemples Bauer 2021 intégrés (example1–4, fichiers `.ext`/`.shk`/`.coi`/`.clt`/`.tab`) |
| `app/www/styles.css` | Styles CSS V4 (CSS variables, sidebar layout, KPI bar, drawer, metric cards) |
| `app/install_deps.R` | Installe les packages Shiny manquants (shiny, bslib, DT) |
| `docs/other_softwares/PFIM/` | Code source PFIM 7.0 — référence SE/RSE/shrinkage (non tracké git) |
| `docs/other_softwares/PopED-master/` | Code source PopED — référence efficiency(), plot_efficiency_of_windows() (non tracké git) |
| `docs/nonmem/manuel_nonmem.txt` | Manuel NONMEM 7.5.1 complet (13 057 lignes) — lisible via Read avec offset |
| `docs/papers/bauer2021/bauer2021_text.txt` | Papier Bauer 2021 extrait en texte — lisible directement |
| `docs/papers/bauer2021/bauer2021.pdf` | Papier Bauer 2021 (PDF original) |
| `docs/nonmem/doc_nonmem_design.pdf` | Section I.72 du manuel (18p) — $DESIGN |
| `docs/nonmem/manuel_nonmem.pdf` | Manuel NONMEM 7.5.1 complet (PDF) |
| `docs/bauer2021_examples/` | 7 exemples complets avec tous les fichiers NONMEM |
| `docs/bauer2021_examples/Design_Theory.pdf` | Fondements mathématiques FIM (4p) |
| `docs/bauer2021_examples/Table_s1.pdf` | Tableau récap OFVTYPE (1p) |
| `docs/courses/nlme.pdf` | Cours NLME Leroux (janv. 2026) |
| `docs/intern_work/redaction.docx` | Mémoire de stage en cours — question de recherche, méthodologie, planning |

---

## Structure des fichiers $DESIGN NONMEM

### Fichier `.ext`
- En-tête : `TABLE NO. 1: First Order (Evaluation): D-OPTIMALITY: ...`
- Colonnes : `ITERATION`, `THETA1..n`, `OMEGA(x,x)..`, `SIGMA(x,x)..`, `OBJ`
- Dans le contexte $DESIGN, `OBJ` = critère d'optimalité (ex: `-log(det(FIM))`)
- Lignes spéciales :

| Itération | Contenu |
|-----------|----------|
| `-1000000000` | Résultat final (paramètres + OBJ) |
| `-1000000001` | Erreurs standard (SE) |
| `-1000000002` | Valeurs propres matrice de corrélation |
| `-1000000003` | Condition number, min/max eigenvalues |
| `-1000000004` | OMEGA/SIGMA en format SD/corrélation |
| `-1000000005` | SE des éléments SD/corrélation |
| `-1000000006` | Indicateur paramètre fixé (0/1) |
| `-1000000007` | Code de terminaison |
| `-1000000008` | Dérivée partielle log-vraisemblance |

### Fichier `.clt`
- Forme triangulaire inférieure de la matrice variance-covariance
- Ordre TOSL (Thetas, Omegas, Sigmas, Lower-triangular)
- En contexte $DESIGN : contient la **FIM** (= `.coi` en triangulaire)

### Fichier `.coi`
- **Fisher Information Matrix complète** (matrice inverse de covariance)
- Format nommé avec headers (THETA1, THETA2, OMEGA(1,1), SIGMA(1,1)...)
- Utilisé par `$RCOVI` pour le design séquentiel

### Fichier `.res`
- Rapport complet du run
- Métriques clés pour $DESIGN :
  - `#OBJV` — valeur de l'objectif
  - `RELATIVEINF(%)` — information relative par paramètre (ETA shrinkage type 11)
  - `DESIGN TYPE: D-OPTIMALITY, -LOG(DET(FIM))`
  - `Elapsed opt. design time in seconds`
  - `EBVSHRINKSD(%)`, `EBVSHRINKVR(%)`

### Fichier `.tab`
- Tableau utilisateur défini via `$TABLE`
- Après optimisation : contient les temps/doses optimisés (TIME, TSTRAT, IPRED...)

### Fichier `.shk`
- Shrinkage en format colonnes
- Type 11 = `RELATIVEINF(%)`

### Fichier `.bfm`
- Uniquement avec `OFVTYPE=8` (Bayes FIM)
- Progression des variances conditionnelles individuelles pendant l'optimisation

### Fichier `.vpd` / `.vpt`
- Variance-covariance des paramètres individuels et population

---

## Options $DESIGN — référence complète

### Critère d'optimalité (`OFVTYPE=`)
| Valeur | Critère | Notes |
|--------|---------|-------|
| 0/1/3/4/5 | D-optimality : `-log(det(FIM))` | Défaut |
| 2 | A-optimality : `-1/tr(FIM⁻¹)` | |
| 6 | DS-optimality : `-log(det(FIM)) + log(det(FIM_unint))` | Utiliser `UNINT` |
| 7 | R-optimality (RSE) : `-1/tr(√(FIM⁻¹)/θ)` | |
| 8 | Bayesian FIM individuel : `-log(det(FIM_bayes))` | Fichier `.bfm` |
| 9/10 | A/R avec filtre UNINT | |

### Type de FIM (`FIMTYPE=` ou `FIMDIAG=`)
| Valeur | Description | Équivalent |
|--------|-------------|----------|
| 0 | FIM complète, diff. finies pour tout | POPED fim.calc.type=0 |
| 1 | Bloc-diagonal, analytique si MU-ref | POPED fim.calc.type=1 / PFIM diagonal |
| 2 | Bloc, sans hypothèse indépendance C(θ) | POPED fim.calc.type=2 |
| 3 | Complète + analytique pour Σ×Σ | POPED fim.calc.type=3 |

`FIMTYPE=1` + `VARCROSS=1` = POPED `fim.calc.type=4` = PFIM diagonal

### Algorithmes d'optimisation
| Keyword | Méthode |
|---------|----------|
| `NELDER` | Nelder-Mead (risque minima locaux) |
| `RS` | Random Search |
| `STGR` | Gradient stochastique |
| `FEDOROV` | Exchange algorithm (temps discrets) |
| `DISCRETE` | Optimise nombre + positions (via NELDER) |
| `DISCRETE_RS` / `DISCRETE_SG` | Idem via RS/STGR |

### Autres options importantes
- `GROUPSIZE=N` — nombre de sujets (multiplie la FIM)
- `APPROX=FO/FOI/FOCE/FOCEI/LAPLACE` — approximation NLME (FO par défaut)
- `DESEL=TIME/AMT` + `DESELSTRAT/DESELMIN/DESELMAX` — éléments à optimiser
- `STRAT/STRATF` — optimise la proportion de sujets par groupe
- `NMIN/NMAX` — bornes sur le nombre de points (DISCRETE uniquement)
- `VARCROSS=0/1` — modèle erreur résiduelle (1 = style PFIM)
- `EOPTD=1` — FIM espérée sur l'incertitude paramétrique (avec `$PRIOR`)
- `NOHABORT` — ne pas avorter sur problème Hessien
- `POSTHOC` — calcul post-hoc des ETAs
- `SIGL=10/12` — chiffres significatifs
- `CLOCKSEED=1` — seed horloger (réplications)
- `MAXEVAL=0` — évaluation; `MAXEVAL>0` — optimisation
- `MODE=0/2` — pour APPROX=FOCEI (MODE=1 déconseillé)
- `GRD=TS(n)` dans `$EST` — THETA sigma-like (résiduelle avec FIMDIAG=1)

### Design séquentiel : `$RCOVI`
```
$RCOVI FILE=previous_run.coi
$DESIGN GROUPSIZE=50 FIMTYPE=1 MAXEVAL=9999 ...
```
- Lit la FIM d'un run précédent et l'**additionne** à la FIM courante
- Principe : FIM_totale = FIM_étude_A + FIM_étude_B (observations indépendantes)
- Cas d'usage : Phase I→II, design adaptatif, info historique, multi-centres
- `$RCOV FILE=run.cov` — même chose depuis le fichier `.cov` (variance-covariance)

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

## Exemples de référence (Bauer 2021)

| Exemple | Dossier | Ce qu'il illustre |
|---------|---------|-------------------|
| 1 | `docs/bauer2021_examples/example1/warfarin.*` | Évaluation simple, FIM bloc-diag |
| 2 | `docs/bauer2021_examples/example2/warfarin2.*` | Optimisation temps, NELDER |
| 3 | `docs/bauer2021_examples/example3/priortrue.*` | Robust design via $SIM TRUE=PRIOR |
| 4 | `docs/bauer2021_examples/example4/warfarin_pkpd_*` | PK-PD multi-réponses |
| 5 | `docs/bauer2021_examples/example5/optdesign2.*` | DS-optimality, UNINT |
| 6 | `docs/bauer2021_examples/example6/tmdd2.*` | TMDD, STRAT/STRATF, ODE |
| 7 | `docs/bauer2021_examples/example7/tmdd2b.*` | Bayes FIM, optimisation dose+temps |

---

## Notes importantes

- `$DESIGN` automatiquement configure `$COV MATRIX=R UNCONDITIONAL`
- FIM donne une **borne inférieure** de l'incertitude réelle (SEs prédits ≤ SEs réels)
- Limitation : **données continues uniquement** — pas discret, ordinal, TTE
- Recommandation : utiliser $DESIGN pour présélectionner 1-3 designs, puis valider par CTS
- Chaîner plusieurs `$DESIGN` dans un `$PROB` (RS → STGR → NELDER) pour éviter les minima locaux
- MU-referencing des THETAs = gain de vitesse majeur avec FIMTYPE=1

### Gotchas Shiny (app V4)

- **Navigation V4** : pas de `tabsetPanel` — navigation via `conditionalPanel("input.active_tab == 'id'")` contrôlé par JS `navTo(tab, el)` qui appelle `Shiny.setInputValue('active_tab', tab)`. Débugger navigation = vérifier `input$active_tab` côté serveur.
- **Drawer open/close** : `session$sendCustomMessage("evalJS", js)` — le handler JS est enregistré dans `output$js_handler` (uiOutput) avec `outputOptions(suspendWhenHidden=FALSE)`.
- **`formatStyle` + `colnames=`** : le paramètre `colnames=` de `datatable()` ne mappe pas avec `formatStyle` — toujours renommer les colonnes dans le df avec `rename()` avant `datatable()`
- **`if_else` vs `ifelse`** : `dplyr::if_else()` exige des conditions vectorisées — pour un check scalaire `!is.null(x)`, utiliser `base::ifelse()` ou un `if/else` ordinaire
- **Dose row dans `.tab`** : la row 1 est toujours la dose initiale (TIME=0, AMT>0) — l'exclure avec `tab[-1, , drop = FALSE]` avant tout traitement des temps d'échantillonnage
- **`observeEvent ignoreNULL`** : par défaut `ignoreNULL=TRUE` — si NULL est un état significatif (ex: reset après "Retirer la run"), toujours passer `ignoreNULL = FALSE`, sinon l'observer ne fire pas sur NULL
- **`%||%` priority + upload** : `merged_ext <- reactive({ example_ext() %||% upload$ext_data() })` donne toujours priorité à `example_ext()` — pattern requis : observer sur `upload$file_paths()` avec `ignoreNULL=FALSE` qui clear tous les `example_*()` reactiveVals quand `!is.null(fps$ext)`
- **`str_split_1` absent de stringr 1.4.0** : n'existe qu'à partir de stringr 1.5.0 — utiliser `strsplit(x, sep)[[1]]` à la place

### Gotchas multi-runs (app V4)

- **vapply + tbl_no cross-run** : `get_ofv(ext, tbl_no)` retourne `numeric(0)` si le run n'a pas la table demandée (ex: run comparaison a 1 table, primary en a 1000) — toujours guard `if (length(val) == 0L) return(NA_real_)` dans tout `vapply` sur `all_runs`
- **Exemple 3 SUBPROB=1000** : `.ext`/`.shk` ont 1000 TABLE NO., `.tab` a 1000 blocs concaténés sans TSTRAT ; `tbl_no()` passe à max=1000 au chargement
- **Exemple 2 auto-charge exemple 1** : `compare_with = "example1"` dans `.EXAMPLES` — charger exemple 2 ajoute exemple 1 dans `all_runs` (effet de bord à connaître pour debug)
- **Labels params multi-run** : en multi-run, utiliser noms bruts THETA1/OMEGA(1,1) et non les labels utilisateur pour éviter l'ambiguité inter-modèles

### Deliverables (app Shiny, rapports)

- **Pas d'emojis dans le code livrable** : jamais d'emojis dans les noms d'onglets, metric cards, labels, messages d'alerte, ou titres de plots — utiliser des indicateurs textuels/CSS à la place
