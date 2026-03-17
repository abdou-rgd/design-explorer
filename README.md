# NONMEM $DESIGN — Post-Processing & Optimal Design

Stage M2 Sciences des données de santé · Sanofi
Optimisation de design d'essais cliniques en pharmacométrie via la Fisher Information Matrix (FIM).

---

## Objectifs

1. **Recherche scientifique** — étude des méthodes d'optimal design (D/A/R-optimality, design séquentiel, robust design) appliquées aux modèles NLME en pharmacocinétique/pharmacodynamie.
2. **Outils R post-processing** — faciliter l'analyse et la visualisation des sorties NONMEM `$DESIGN` pour les pharmacométriciens.

---

## Structure du projet

```
.
├── scripts/
│   ├── parse_design_outputs.R   # Parsers : read_ext(), read_shk(), get_rse(), ...
│   └── report_design.R          # Visualisations : plot_rse(), plot_relativeinf(), ...
├── app/
│   ├── app.R                    # Application Shiny "$DESIGN Explorer" (v3.0)
│   ├── install_deps.R           # Installation des dépendances Shiny
│   └── R/                       # Modules Shiny
│       ├── mod_upload.R         # Import de fichiers NONMEM
│       ├── mod_params.R         # Paramètres finaux
│       ├── mod_rse.R            # RSE prédit (%)
│       ├── mod_relativeinf.R    # Information relative par ETA
│       ├── mod_convergence.R    # Convergence OFV
│       ├── mod_fim.R            # Fisher Information Matrix
│       ├── mod_compare.R        # Comparaison de runs
│       ├── mod_times.R          # Temps/doses optimisés
│       ├── mod_raw.R            # Fichiers bruts
│       └── mod_examples.R       # Exemples intégrés
└── docs/
    ├── manuel_nonmem.txt        # Manuel NONMEM 7.5.1 complet (13 057 lignes)
    ├── bauer2021_text.txt        # Bauer 2021 — référence $DESIGN
    ├── reading_list.md           # Liste de lecture annotée
    ├── bauer2021_examples/       # 7 exemples complets (Bauer 2021)
    ├── inspiration/PFIM/         # Code source PFIM 7.0
    ├── inspiration/PopED-master/ # Code source PopED
    └── intern_work/              # Mémoire de stage en cours
```

---

## Application Shiny — `$DESIGN Explorer`

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
| **Upload** | Import de fichiers `.ext`, `.shk`, `.tab` |
| **Paramètres** | Paramètres finaux et valeurs initiales |
| **RSE** | Relative Standard Errors prédits (%) par paramètre |
| **RelInf** | Information relative (%) par ETA — mesure l'informativité du design |
| **Convergence** | Évolution de l'OFV au cours des itérations |
| **FIM** | Fisher Information Matrix (heatmap) |
| **Comparer** | Comparaison side-by-side de plusieurs runs |
| **Temps optimaux** | Temps/doses optimisés issus du fichier `.tab` |
| **Fichiers bruts** | Visualisation des fichiers NONMEM bruts |
| **Exemples** | Exemples intégrés (Bauer 2021) |

---

## Scripts R

### `scripts/parse_design_outputs.R`

Parsers pour les fichiers de sortie NONMEM `$DESIGN` :

```r
source("scripts/parse_design_outputs.R")

# Lire les fichiers
ext <- read_ext("run001.ext")
shk <- read_shk("run001.shk")

# Extraire les métriques
get_final_params(ext)   # Paramètres finaux (itération -1000000000)
get_se(ext)             # Erreurs standard prédites par la FIM
get_ofv(ext)            # Valeur du critère d'optimalité
get_rse(ext)            # RSE (%) : tibble param / estimate / se / rse_pct
get_relativeinf(shk)    # Information relative (%) par ETA (TYPE 11)

# Synthèse d'un run
summary_design(ext, shk)
```

### `scripts/report_design.R`

Visualisations ggplot2 :

```r
source("scripts/parse_design_outputs.R")
source("scripts/report_design.R")

plot_rse(ext)            # Barplot RSE(%) par paramètre, facetté par type
plot_relativeinf(shk)    # Barplot RELATIVEINF(%) par ETA avec code couleur
plot_convergence(ext)    # Courbe OFV vs itération
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

- **R** 4.1+ avec tidyverse (`dplyr`, `tidyr`, `purrr`, `ggplot2`, `stringr`, `readr`)
- **Shiny** + `bslib` (Bootstrap 5) + `DT`
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

Fichiers disponibles dans `docs/bauer2021_examples/`.

---

## Ressources

- `docs/manuel_nonmem.txt` — Manuel NONMEM 7.5.1 complet
- `docs/bauer2021_text.txt` — Bauer 2021 (texte extrait)
- `docs/reading_list.md` — Liste de lecture annotée
- `docs/intern_work/redaction.docx` — Mémoire de stage en cours
