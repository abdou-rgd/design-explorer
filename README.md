# NONMEM $DESIGN — Post-Processing & Optimal Design

**V5.0.0** · Stage M2 Sciences des donn\u00e9es de sant\u00e9 · Sanofi
Optimisation de design d'essais cliniques en pharmacométrie via la Fisher Information Matrix (FIM).

---

## Objectifs

1. **Recherche scientifique** — étude des méthodes d'optimal design (D/A/R-optimality, design séquentiel, robust design) appliquées aux modèles NLME en pharmacocinétique/pharmacodynamie.
2. **Outils R post-processing** — faciliter l'analyse et la visualisation des sorties NONMEM `$DESIGN` pour les pharmacométriciens.

---

## Structure du projet

```
.
├── R/
│   ├── design_utils.R           # Utilitaires : %||%, constantes, helpers internes
│   ├── design_io.R              # Lecteurs NONMEM : read_ext(), read_shk(), read_coi(), ...
│   ├── design_metrics.R         # Extracteurs FIM : get_rse(), get_d_criterion(), ...
│   ├── design_summary.R         # Affichage console : summary_design()
│   ├── ctl_parsers.R            # Parseurs .ctl : parse_theta_labels(), parse_groupsize(), ...
│   ├── fim_metrics.R            # Power/NSN Wald & TOST : compute_power_wald(), ...
│   └── report_design.R          # Visualisations ggplot2 : plot_rse(), plot_convergence(), ...
├── app/
│   ├── app.R                    # Application Shiny "DE$IGN EXPLORER" (V5)
│   ├── install_deps.R           # Installation des dépendances Shiny
│   ├── examples/                # Exemples intégrés (Bauer 2021, examples 1-7)
│   └── R/                       # 13 modules Shiny
│       ├── mod_upload.R         # Import de fichiers NONMEM (.ext, .shk, .coi, .tab, ...)
│       ├── mod_params.R         # Paramètres finaux, RSE, shrinkage, temps optimaux
│       ├── mod_rse.R            # RSE prédit (%) avec seuils 4 paliers
│       ├── mod_relativeinf.R    # Information relative par ETA
│       ├── mod_convergence.R    # Convergence OFV par optimiseur
│       ├── mod_fim.R            # FIM heatmap, D-critère, eigenvalues
│       ├── mod_compare.R        # Comparaison multi-runs side-by-side
│       ├── mod_times.R          # Temps optimisés (+ design robuste boxplots)
│       ├── mod_prior.R          # Visualisation $PRIOR NWPRI
│       ├── mod_power.R          # Power Wald, NSN, Equivalence TOST
│       ├── mod_raw.R            # Fichiers bruts
│       ├── mod_ctl_stream.R     # Visualisation control stream
│       └── mod_examples.R       # Exemples intégrés avec auto-compare
├── tests/
│   ├── run_tests.R              # Script lanceur de tests
│   └── testthat/                # 52 tests unitaires
└── docs/
    ├── nonmem/
    │   └── manuel_nonmem.txt     # Manuel NONMEM 7.5.1 complet (13 057 lignes)
    ├── papers/                   # Littérature de référence
    ├── other_softwares/          # Code source PFIM 7.0, PopED (référence)
    └── intern_work/              # Mémoire de stage en cours
```

---

## Application Shiny — `DE$IGN EXPLORER`

Interface web pour explorer interactivement les sorties NONMEM `$DESIGN`.

### Lancement

```r
# Installer les dépendances si nécessaire
source("app/install_deps.R")

# Lancer l'application
shiny::runApp("app/")
```

### Fonctionnalités

| Onglet | Description |
|--------|-------------|
| **Upload** | Import de fichiers `.ext`, `.shk`, `.tab`, `.coi`, `.clt`, `.cov`, `.cor` |
| **Paramètres** | Paramètres finaux, valeurs initiales, temps optimaux |
| **RSE** | Relative Standard Errors prédits (%) par paramètre avec seuils 4 paliers |
| **RelInf** | Information relative (%) par ETA — mesure l'informativité du design |
| **Convergence** | Évolution de l'OFV au cours des itérations par optimiseur |
| **FIM** | Fisher Information Matrix (heatmap), D-critère, eigenvalues |
| **Comparer** | Comparaison multi-runs side-by-side |
| **Temps optimaux** | Temps/doses optimisés + courbe prédite (gère elementary designs) |
| **Prior** | Visualisation `$PRIOR NWPRI` |
| **Power** | Power Wald, NSN, Equivalence TOST |
| **Control stream** | Visualisation du fichier `.ctl` |
| **Fichiers bruts** | Visualisation des fichiers NONMEM bruts |
| **Exemples** | Exemples intégrés (Bauer 2021, 1–7) avec auto-compare |

---

## Scripts R

### Lecture et métriques

```r
source("R/design_utils.R")
source("R/design_io.R")
source("R/design_metrics.R")
source("R/design_summary.R")

# Lire les fichiers
ext <- read_ext("run001.ext")
shk <- read_shk("run001.shk")
coi <- read_coi("run001.coi")
tab <- read_tab("run001.tab")

# Extraire les métriques
get_final_params(ext)    # Paramètres finaux (itération -1000000000)
get_se(ext)              # Erreurs standard prédites par la FIM
get_ofv(ext)             # Valeur du critère d'optimalité
get_rse(ext)             # RSE (%) : tibble param / estimate / se / rse_pct
get_relativeinf(shk)     # Information relative (%) par ETA (TYPE 11)
get_d_criterion(ext)     # D-critère : -log(det(FIM))

# Synthèse d'un run
summary_design(ext, shk)
```

### Parseurs control stream

```r
source("R/ctl_parsers.R")

parse_theta_labels("run001.ctl")   # Labels THETA depuis les commentaires
parse_groupsize("run001.ctl")      # GROUPSIZE par bras
parse_design_summary("run001.ctl") # Résumé du bloc $DESIGN
read_prior_nwpri("run001.ctl")     # Paramètres $PRIOR NWPRI
```

### Power / NSN

```r
source("R/fim_metrics.R")

compute_power_wald(rse, n)          # Puissance test de Wald
compute_n_needed(rse, power = 0.8)  # NSN pour puissance cible
compute_power_tost(rse, n, delta)   # Puissance équivalence TOST
```

### Visualisations ggplot2

```r
source("R/report_design.R")

plot_rse(ext)              # Barplot RSE(%) par paramètre, facetté par type
plot_relativeinf(shk)      # Barplot RELATIVEINF(%) par ETA avec code couleur
plot_convergence(ext)      # Courbe OFV vs itération par optimiseur
plot_fim_heatmap(coi)      # Heatmap FIM avec annotations
plot_optimal_times(tab)    # Temps optimisés sur timeline
plot_model_prediction(tab) # Courbe IPRED vs TIME par bras
```

---

## Fichiers NONMEM `$DESIGN` — référence rapide

| Fichier | Contenu clé |
|---------|------------|
| `.ext` | Paramètres + OFV par itération ; lignes spéciales : `-1e9` (final), `-1e9-1` (SE) |
| `.shk` | Shrinkage — TYPE 11 = `RELATIVEINF(%)` |
| `.coi` | Fisher Information Matrix complète (format nommé) |
| `.clt` | FIM en triangulaire inférieure (ordre TOSL) |
| `.tab` | Temps/doses optimisés définis via `$TABLE` |
| `.res` | Rapport complet : `#OBJV`, `RELATIVEINF`, temps de calcul |
| `.bfm` | Bayes FIM individuelle (uniquement `OFVTYPE=8`) |

---

## Critères d'optimalité (`OFVTYPE=`)

| Valeur | Critère | Formule |
|--------|---------|---------|
| 0–1 | **D-optimality** (défaut) | `-log(det(FIM))` |
| 2 | A-optimality | `-1/tr(FIM⁻¹)` |
| 6 | DS-optimality | D-opt + pénalité paramètres non-intéressants |
| 7 | R-optimality (RSE) | `-1/tr(√(FIM⁻¹)/θ)` |
| 8 | Bayesian FIM individuelle | `-log(det(FIM_bayes))` |

---

## Stack technique

- **R** 4.2+ avec tidyverse (`dplyr`, `tidyr`, `purrr`, `ggplot2`, `stringr`, `readr`)
- **Shiny** 1.7+ / `bslib` 0.3+ (Bootstrap 3) / `DT`
- **NONMEM** 7.5+ (sorties texte brut)
- **Références** : PFIM 7.0, PopED, Bauer 2021

---

## Exemples de référence (Bauer 2021)

| N° | Modèle | Illustre |
|----|--------|---------|
| 1 | Warfarine | Évaluation simple, FIM bloc-diagonale |
| 2 | Warfarine | Optimisation des temps de prélèvement (NELDER) |
| 3 | Warfarine | Robust design via `$SIM TRUE=PRIOR` |
| 4 | PK-PD | Multi-réponses, lien PK/PD |
| 5 | Warfarine | DS-optimality, paramètres non-intéressants |
| 6 | TMDD | Stratification (`STRAT/STRATF`), ODE |
| 7 | TMDD | Bayes FIM, optimisation dose + temps |

Fichiers disponibles dans `docs/papers/bauer2021/examples/`.

---

## Ressources

- `docs/nonmem/manuel_nonmem.txt` — Manuel NONMEM 7.5.1 complet
- `docs/papers/bauer2021/bauer2021_text.txt` — Bauer 2021 (texte extrait)
- `docs/intern_work/redaction.docx` — Mémoire de stage en cours
