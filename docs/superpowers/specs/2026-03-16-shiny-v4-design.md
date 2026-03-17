# $DESIGN Explorer — Shiny App V4 Design Spec

**Date:** 2026-03-16
**Statut:** Approuvé
**Approche:** Redesign intégré — fix code + nouvelle UI en une seule passe, module par module

---

## 1. Contexte

Application Shiny de post-processing des sorties NONMEM `$DESIGN`. Utilisateurs : pharmacométriciens (experts techniques). Objectif V4 : corriger les bugs actifs, améliorer la qualité du code, et refaire l'interface pour qu'elle soit distinctive et production-grade.

---

## 2. Direction visuelle

**Style :** Clean Dashboard — fond blanc/gris clair, cards colorées par catégorie, typographie moderne. Proche de Posit Connect / RStudio, familier pour les pharma-statisticiens, mais avec une identité propre.

### Palette

```css
--bg:             #f8fafc
--surface:        #ffffff
--border:         #e2e8f0
--text:           #0f172a
--text-muted:     #64748b
--sidebar-bg:     #1e293b
--sidebar-text:   #94a3b8
--sidebar-active: #2563eb

/* Runs (max 4) */
--run-a: #2563eb
--run-b: #dc2626
--run-c: #16a34a
--run-d: #d97706

/* RSE seuils */
--rse-good:     #15803d  bg: #f0fdf4
--rse-moderate: #92400e  bg: #fef3c7
--rse-poor:     #dc2626  bg: #fef2f2

/* RelInf seuils */
--ri-good:      #15803d  bg: #f0fdf4
--ri-moderate:  #92400e  bg: #fef3c7
--ri-poor:      #dc2626  bg: #fef2f2
```

### Typographie

| Usage | Police | Taille |
|---|---|---|
| Titres / labels / UI | DM Sans (Google Fonts) | 13–16px |
| Valeurs numériques / code NONMEM | JetBrains Mono | 12–13px |
| Corps | DM Sans | 14px, line-height 1.5 |

---

## 3. Structure de navigation

### Layout global

```
┌─────────────────────────────────────────────────────┐
│  Sidebar (240px)  │  Barre KPI                      │
│                   │  ─────────────────────────────  │
│  Logo + titre     │  Contenu de l'onglet actif       │
│  [Runs actifs]    │                                  │
│  ── Résultats ──  │                                  │
│  ── Design ──     │                                  │
│  ── Diagnostic ── │                                  │
└─────────────────────────────────────────────────────┘
```

### Sidebar verticale (240px, fond `--sidebar-bg`)

- Logo + titre "$DESIGN Explorer" en haut
- Bouton "Runs actifs" → ouvre le drawer upload/compare
- Navigation groupée en 3 sections :

```
── Résultats ──
  Parametres
  RSE / SE
  RELATIVEINF

── Design ──
  FIM & Criteres
  Temps optimaux
  Design Robuste

── Diagnostic ──
  Convergence
  Donnees brutes
```

- Item actif : fond `--run-a` (#2563eb), texte blanc
- Hover : fond légèrement plus clair, transition 150ms

### Barre KPI compacte (persistante)

Rendue une seule fois dans `app.R` (pas dans chaque module), affichée en haut du contenu principal.

```
[ ● Run A  warfarin.ext ]  |  D-crit: 14.3  |  RSE moy: 18%  |  2/6 >20%  |  [⚠ 2 params médiocres]
```

Calculs :
- `D-crit` = `exp(-OFV / n_params)` où OFV est la valeur à ITER = -1e9 (= `exp(-(-log(det(FIM))) / n)`, normalisé par nb paramètres estimés)
- `RSE moy` = `mean(rse_pct[!is.na(rse_pct)])` sur tous les paramètres estimés (fixed=0)
- `n_poor / n_total` = `sum(rse_pct > 20)` / nb params estimés
- Badge "médiocres" = nb params avec RSE > 20% (seuil affiché explicitement)
- Se met à jour avec le run primaire (slot `primary` de `all_runs`)
- Run coloré selon `--run-a` ; `--sidebar-active` (#2563eb) est intentionnellement identique à `--run-a` pour cohérence visuelle

### Drawer upload/runs (slide depuis la droite)

**UX flow :**
1. Clic "Runs actifs" → drawer s'ouvre (largeur 380px, z-index élevé)
2. Upload ou sélection exemple → run ajouté à `all_runs` immédiatement, sidebar se rafraîchit
3. Édition du nom → mise à jour en temps réel dans `all_runs` et sidebar
4. Fermeture : clic en dehors du drawer ou touche ESC
5. Drawer reste ouvert pendant l'upload (pas d'auto-close)

**Contenu :**

Panneau latéral déclenché par le bouton "Runs actifs" :

- Upload fichiers individuels (`.ext`, `.shk`, `.coi`, `.clt`, `.tab`) ou archive `.tar.gz`
- Exemples Bauer 2021 intégrés (selectInput) → auto-charge les 5 fichiers (`.ext`, `.shk`, `.coi`, `.clt`, `.tab`), pré-remplit le nom avec "Example N — modele"
- Liste des runs chargés (max 4), chacun avec :
  - Nom modifiable
  - Couleur assignée automatiquement
  - Bouton supprimer
- Options globales :
  - Labels paramètres (textAreaInput, format `THETA1=CL`)
  - Sélection TABLE NO.
  - Toggle RSE% / SE absolues
  - Toggle axe log (convergence)

> **Règle feedback** : aucun `fileInput` dans un `renderUI` qui dépend de l'état modifié par son propre handler.

---

## 4. Modules — détail

### Signatures des modules (inputs → outputs)

| Module | Inputs reactifs | Output |
|---|---|---|
| `mod_params` | `all_runs`, `tbl_no`, `param_labels` | — |
| `mod_rse` | `all_runs`, `tbl_no`, `param_labels`, `se_mode` | — |
| `mod_relativeinf` | `all_runs`, `tbl_no`, `param_labels` | — |
| `mod_fim` | `all_runs`, `tbl_no`, `param_labels` | — |
| `mod_times` | `all_runs` | — |
| `mod_prior` | `summary_data`, `ctl_data` | — | *(ces deux inputs sont actifs dans mod_prior — les dead params supprimés sont `ctl_data` dans mod_params et `summary_data` dans mod_times)* |
| `mod_convergence` | `all_runs`, `log_conv` | — |
| `mod_raw` | `all_runs` | — |

Tous les inputs sont des `reactive` passés par référence (pas de valeurs directes).

### 4.1 Résultats

#### mod_params.R — Parametres
- TABLE 3 style : tableau multi-colonnes, une colonne par run
- Avec 4 runs : tableau scrollable horizontalement (overflow-x: auto), pas de collapse
- Colonnes renommées directement dans le dataframe (pas via `colnames=` de DT)
- Cards métriques : n_good (RSE<20%), n_mod (RSE 20–50%), n_poor (RSE>50%)
- Tabs internes : Valeurs | RSE% | SE absolues | Shrinkage (TYPE 4 = EBVSHRINKSD)

#### mod_rse.R — RSE / SE
- Toggle RSE% / SE absolues (via options globales dans drawer)
- `plot_rse()` multi-runs superposés, couleur par run, run primaire (Run A) en trait plein plus épais
- Légende runs dynamique
- Tableau récap en dessous du graphique

#### mod_relativeinf.R — RELATIVEINF
- `plot_relativeinf()` barres horizontales
- Lignes verticales seuils 20% et 50%
- Multi-runs superposés par défaut (même axe), couleur par run

### 4.2 Design

#### mod_fim.R — FIM & Criteres
- Cards : D-criterion, condition number, eigenvalue min/max
- Comparaison Run A vs Run B dans les cards (si multi-runs)
- `plot_fim_heatmap()` si `.coi` disponible
- Heatmap corrélation supprimée (déjà fait en V4)

#### mod_times.R — Temps optimaux
- Tableau `.tab` avec temps optimisés, affichage propre
- Histogramme de distribution des temps
- Exclusion row dose : `time_df[-1, , drop = FALSE]` — row 1 est toujours la dose initiale (TIME=0, AMT>0) dans les sorties NONMEM $DESIGN ; exclue pour éviter l'artefact dans l'histogramme
- Paramètre `summary_data` supprimé (dead param)

#### mod_prior.R — Design Robuste
- Si `ctl_data` ou `summary_data` est NULL : affiche message "Prior data unavailable — run without $PRIOR or $SIM TRUE=PRIOR"
- Prior NWPRI parsé : THETA, OMEGA, SIGMA priors affichés
- Summary.tab : stats Mean/STD/RSTD/Low/High par TIME point
- `plot_robust_design()` si summary disponible
- Logique exclusion dose row simplifiée : `time_df[-1, , drop = FALSE]`
- Logique `%||%` / `!is.na()` simplifiée

### 4.3 Diagnostic

#### mod_convergence.R — Convergence
- `plot_convergence()` OFV vs itérations
- Toggle axe log
- Multi-runs superposés avec couleurs runs

#### mod_raw.R — Donnees brutes
- `.ext` brut en DT scrollable, toutes colonnes
- Export CSV : bouton "Télécharger .ext" — données du run primaire brutes, format CSV

---

## 5. Fixes de code

| Fichier | Problème | Fix |
|---|---|---|
| `mod_params.R` | `if_else(!is.null(lbls) & ...)` — scalaire dans if_else vectorisé | `ifelse` base R, ou `if (is.null(lbls)) param else coalesce(lbls[param], param)` |
| `mod_params.R` | `colnames=` dans DT ne mappe pas avec `formatStyle` | Renommer colonnes dans le df avant `datatable()` |
| `mod_params.R` | `n_mod` supprimé des metric cards | Restaurer : `n_mod <- sum(rse$rse_pct >= 20 & rse$rse_pct < 50)` |
| `mod_params.R` | `ctl_data` dead param | Supprimer de la signature + de l'appel dans `app.R` |
| `mod_times.R` | `summary_data` dead param | Supprimer signature + appel `app.R` |
| `mod_times.R` | Guard `if (!is.null(tab))` redondant | Supprimer |
| `mod_prior.R` | Exclusion dose row fragile (2 étapes redondantes) | `time_df[-1, , drop = FALSE]` |
| `mod_prior.R` | Logique `%||% NA` puis `!is.na()` confuse | `if (isTRUE(!is.na(prior$plev))) prior$plev else "N/A"` |
| `CLAUDE.md` | Section Miniverse ajoutée par l'environnement Claude | Retirer avant commit |
| `app.R` | Appels modules avec params supprimés | Aligner avec nouvelles signatures |

---

## 6. Composants UI réutilisables (helpers_ui.R)

Composants à mettre à jour ou créer :

| Composant | Description |
|---|---|
| `metric_card(label, value, sub, color, run)` | Card avec border-top colorée selon run |
| `kpi_bar(run_name, d_crit, rse_mean, n_poor)` | Barre KPI compacte persistante |
| `rse_badge(x)` | Pill colorée RSE (inchangé fonctionnellement) |
| `ri_badge(x)` | Pill colorée RelInf (inchangé fonctionnellement) |
| `param_type_badge(param)` | Badge THETA/OMEGA/SIGMA |
| `run_pill(name, color)` | Pill run avec point coloré |
| `.RUN_COLORS` | Palette 4 runs (inchangée) |
| `run_color(name)` | Lookup couleur par run |

---

## 7. Ordre d'implémentation

```
Étape 1 : styles.css + helpers_ui.R      — fondations visuelles
Étape 2 : app.R                          — sidebar + drawer structure
Étape 3 : mod_upload.R + mod_compare.R  — drawer unifié
Étape 4 : mod_params.R                  — TABLE 3 + bugs fixes
Étape 5 : mod_rse.R + mod_relativeinf.R — graphiques
Étape 6 : mod_fim.R                     — FIM metrics
Étape 7 : mod_times.R + mod_prior.R     — Design section
Étape 8 : mod_convergence.R + mod_raw.R — Diagnostic
```

---

## 8. Ce qui ne change pas

- Structure `all_runs` (liste de runs avec `name`, `ext_data`, `shk_data`, etc.)
- Parsers dans `scripts/parse_design_outputs.R` et `scripts/report_design.R`
- Logique multi-runs et comparaison
- Concept TABLE 3 (conservé et amélioré)
- Exemples Bauer 2021 intégrés
- Support `.tar.gz` (workflow Sanofi `nrm`)
- Max 4 runs simultanés
