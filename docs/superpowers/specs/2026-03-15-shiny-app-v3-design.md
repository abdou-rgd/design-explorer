# Shiny App V3 — Design Spec

**Date** : 2026-03-15
**Auteur** : Abdou + Claude
**Horizon** : 2 semaines (retour tutrice ~2026-03-29)
**Approche** : Incremental coherent — 4 updates sequentielles + fil rouge Fayette
**Dependances** : Update 1 independant. Update 2 independant. Update 3 depend de Update 2 (multi-runs pour comparaison exemples). Update 4 depend de Update 2 (overlay multi-runs).

---

## Contexte

L'app Shiny $DESIGN Explorer V2 a ete presentee a la tutrice (Karl Brendel, Sanofi). Retours positifs, avec des demandes d'amelioration et une vision long terme d'un outil "tout-en-un" pour les pharmacometriciens.

### Etat actuel (V2)

- 7 onglets : Parametres, RSE/SE, RELATIVEINF, FIM & Criteres, Temps optimaux, Convergence, Donnees brutes
- 9 modules Shiny dans `app/R/`
- Upload de fichiers NONMEM (.ext, .shk, .coi, .clt, .tab) ou .tar.gz
- Parsers dans `scripts/parse_design_outputs.R`, plots dans `scripts/report_design.R`
- Single-run uniquement

---

## Update 1 : Quick wins + Shrinkages + Priors

### 1a. RSE a 2 decimales

- Appliquer `round(x, 2)` dans tous les tableaux RSE/SE du module `mod_rse.R`
- Appliquer `scale_y_continuous(labels = function(x) sprintf("%.2f", x))` dans les plots
- Aussi appliquer aux SE absolues

### 1b. Affichage des shrinkages

Section integree dans `mod_params.R` (panneau conditionnel, pas de module separe). Le `.shk` est deja parse par `read_shk()`.

**Contenu :**
- Tableau : ETA, TYPE (4=EBV%, 8=EPSSHRINK%, 11=RELATIVEINF%), valeur
- Barplot : shrinkage EBV (TYPE 4) par ETA, seuil a 30% (zone rouge au-dela)
- Distinction claire avec RELATIVEINF (TYPE 11) deja dans son onglet dedie

### 1c. Lecture des priors

Parser le bloc `$PRIOR NWPRI` depuis un fichier `.ctl` uploade optionnellement.

**Extraction :**
- NTHETA / NETA / NEPS : nombre de parametres avec prior
- THETAP, THETAPV : moyennes et variances des priors THETA
- OMEGAPD, degres de liberte (DF) pour OMEGA prior
- S'inspirer de `summary.f90` (exemple 3 Bauer) pour le format

**Affichage :**
- Tableau dans l'onglet "Parametres" (panneau conditionnel, visible si .ctl uploade)
- Moyennes, percentiles des distributions a priori

**Upload :**
- Ajout d'un champ upload optionnel pour le `.ctl` dans `mod_upload.R`
- Si absent, tout fonctionne normalement

---

## Update 2 : Comparaison multi-runs

### Architecture

- Run "principal" inchange
- Bouton **"Ajouter un run de comparaison"** dans la sidebar, sous l'upload
- Maximum **3 runs de comparaison** (4 runs total)
- Chaque run a un nom auto-genere (Run A, B, C, D) renommable par l'utilisateur
- Code couleur distinct par run, coherent partout

### Nouveau module `mod_compare.R`

Gere :
- Liste des runs charges (`reactiveValues`)
- Bouton ajout / suppression (bouton "X" par run)
- Stockage des donnees parsees par run (ext, shk, coi, clt, tab, res)

### Impact sur les onglets

| Onglet | Comportement multi-runs |
|--------|------------------------|
| Parametres | Colonnes par run (estimate Run A / B / ...) |
| RSE / SE | Barplot groupe (barres cote a cote, couleur par run) + tableau comparatif |
| RELATIVEINF | Barplot groupe idem |
| FIM & Criteres | Tableau comparatif (D-critere, condition#, eigenvalues). Heatmap FIM : dropdown pour selectionner le run |
| Temps optimaux | Overlay ou tableau comparatif |
| Convergence | Courbes superposees avec legende par run |
| Donnees brutes | Dropdown pour selectionner le run |

### Regles UX

- Le run principal ne peut pas etre supprime
- Les labels parametres et TABLE NO. s'appliquent a tous les runs (meme modele suppose)
- Si parametres differents entre runs : alignement sur noms communs, NA pour les manquants

---

## Update 3 : Exemples warfarin integres

### Exemples selectionnes

| Exemple | Fichiers source | Titre pedagogique | Concept cle |
|---------|----------------|-------------------|-------------|
| 1 | `example1/warfarin.*` | Evaluation d'un design | Lire RSE, FIM, RELATIVEINF |
| 2 | `example2/warfarin2.*` | Optimisation des temps | DESEL=TIME, NELDER, comparer avant/apres |
| 4 | `example4/warfarin_pkpd_*` | PK-PD multi-reponses | FIMTYPE=1+VARCROSS=1, multi-CMT |

### UX

- Bouton **"Exemples"** dans la sidebar (au-dessus de l'upload)
- Au clic : modal avec 3 cartes (titre + description 2 lignes + bouton "Charger")

### Contenu charge par exemple

1. Fichiers de sortie (.ext, .shk, .coi, .clt, .tab) charges automatiquement
2. Labels parametres pre-remplis (THETA1=CL, etc.)
3. **Panneau guide** en haut de la page (bandeau dismissable) :
   - Contexte du modele et du design
   - `.ctl` affiche dans un bloc code avec coloration syntaxique
   - 3-4 points cles a observer
   - Exemple 2 : bouton "Comparer avec l'evaluation" -> charge ex.1 en run de comparaison (multi-runs)

### Stockage

Fichiers copies dans `app/examples/example1/`, `example2/`, `example4/`. Quelques Ko chacun.

### Vision future

- Integrer les exemples 3, 5, 6, 7 progressivement
- Integrer le papier Bauer 2021 en PDF consultable dans l'app (onglet "Documentation")
- L'objectif : que chaque PMX puisse apprendre $DESIGN de maniere autonome via l'app

---

## Update 4 : Plots enrichis PopED/PFIM

### 4a. Model prediction plot

- Inspire de PopED `plot_model_prediction()`
- Courbe IPRED vs TIME a partir du `.tab`
- Points de sampling marques (actuels et/ou optimises)
- En multi-runs : overlay pour voir les temps bouger
- Integre dans l'onglet "Temps optimaux"

### 4b. RSE waterfall / forest plot

- Inspire de PFIM `plotRSE()`
- Barres horizontales par parametre, triees du plus grand au plus petit RSE
- Zones colorees : vert (<20%), orange (20-50%), rouge (>50%)
- Plus lisible que le barplot vertical avec beaucoup de parametres
- Complete le plot actuel dans "RSE / SE" (toggle pour basculer entre barplot vertical et waterfall horizontal)

### 4c. Matrice de correlation

- Inspire de PFIM `plotMatCor()`
- Heatmap des correlations entre parametres
- Source : inverser la FIM (.coi) en R via `solve()` pour obtenir la matrice var-cov, puis `cov2cor()` pour la matrice de correlation. Nouvelle fonction `get_cor_matrix()` dans `parse_design_outputs.R`
- Valeurs numeriques dans les cellules
- Complete la heatmap FIM dans "FIM & Criteres"

### 4d. Sensitivity plot (reporte)

- Inspire de PopED `plot_efficiency_of_windows()`
- Evolution du D-critere si on decale un temps de +/- delta
- Necessite calcul FIM cote R (pas juste parsing)
- Reporte a une update ulterieure

---

## Fil rouge : Papier Fayette (JPKPD 2025)

### Ref

Fayette L., Brendel K., Mentre F. (2025). "Using Fisher Information Matrix to predict uncertainty in covariate effects and power to detect their relevance in NLMEM in pharmacometrics." JPKPD, 52(4):38.

### Phase A — Lecture critique (semaine 1)

- Resume structure pour le memoire
- Equations cles : power significance (eq. 12), power relevance (eq. 16), NSN (eq. 13, 17)
- Hypotheses : linearisation FO, conditions asymptotiques, 3 methodes covariables (data, distributions, copules)
- Limites : SE sur-predites quand N petit ou categories sous-representees

### Phase B — Exploration methodologique (semaine 2, si le temps le permet)

Priorite inferieure aux Updates 3-4. Peut etre reporte a la semaine suivante.

- Reimplementer en R standalone :
  - Power significance : `PS = 1 - Phi(q - beta/SE) + Phi(-q - beta/SE)`
  - NSN significance : `NSNS = N * (SE / SE_target)^2`
  - Power relevance (eq. 16)
  - NSN relevance (eq. 17, root finder)
- Tester sur cabozantinib (Table 5 du papier) pour validation
- Evaluer faisabilite module Shiny (inputs necessaires : betas, FIM, distribution covariables)

### Phase C — Module Shiny (future, si A+B concluants)

Reporte apres validation. Necessiterait que l'utilisateur fournisse :
- Valeurs des betas covariables
- FIM (deja dispo via .coi)
- Distribution des covariables (dataset ou parametres)

---

## Vision long terme (hors scope)

| Chantier | Description |
|----------|-------------|
| Editeur .ctl | Edition du control stream depuis l'app, generation de .ctl, lancement manuel par le PMX |
| Exemples 3, 5, 6, 7 | Integration progressive des autres exemples Bauer |
| Documentation integree | Papier Bauer en PDF dans l'app |
| Sensitivity plot | Plot efficiency of windows (calcul FIM cote R) |
| Module covariables/power | Integration Fayette Phase C |
| Guide FIMTYPE | Documentation contextuelle : quel FIMTYPE selon le modele d'erreur |
| Guide $DESIGN | Workflow complet d'utilisation de $DESIGN (FIMTYPE, algo, erreurs courantes) |

---

## Fichiers impactes

### Nouveaux fichiers

- `app/R/mod_compare.R` — module comparaison multi-runs
- `app/R/mod_examples.R` — module exemples integres
- `app/examples/example1/` — fichiers warfarin exemple 1
- `app/examples/example2/` — fichiers warfarin exemple 2
- `app/examples/example4/` — fichiers warfarin PK-PD exemple 4
- `scripts/fayette_power.R` — fonctions power/NSN (fil rouge)

### Fichiers modifies

- `app/app.R` — integration nouveaux modules, restructuration server pour multi-runs
- `app/R/mod_upload.R` — ajout upload .ctl optionnel, interface avec mod_compare
- `app/R/mod_rse.R` — round(2), barplot groupe multi-runs, waterfall plot
- `app/R/mod_relativeinf.R` — barplot groupe multi-runs
- `app/R/mod_params.R` — colonnes multi-runs, section shrinkage/priors
- `app/R/mod_fim.R` — tableau comparatif multi-runs, matrice correlation, dropdown heatmap
- `app/R/mod_times.R` — overlay multi-runs, model prediction plot
- `app/R/mod_convergence.R` — courbes superposees multi-runs
- `app/R/mod_raw.R` — dropdown selection run
- `scripts/parse_design_outputs.R` — parser $PRIOR NWPRI depuis .ctl, `get_cor_matrix()`
- `scripts/report_design.R` — nouveaux plots (waterfall RSE, correlation, model prediction)
- `app/R/helpers_ui.R` — helpers UI multi-runs (couleurs, badges, palettes)
- `app/www/styles.css` — styles multi-runs (couleurs, badges run)
