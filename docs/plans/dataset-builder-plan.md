# Plan : Module "Dataset Builder" pour l'app Shiny $DESIGN

## Contexte

Pour faire de l'Optimal Design avec NONMEM `$DESIGN`, il faut un dataset `.csv` specifique qui n'existe pas en l'etat — les modelisateurs ont des datasets volumineux avec beaucoup de colonnes inutiles pour l'OD. Actuellement, la creation de ce dataset est manuelle : ouvrir le protocole (SOA), le `.ctl`, et recreer le `.csv` ligne par ligne dans Excel en calculant les heures a partir des jours/semaines du calendrier.

**Objectif** : Ajouter un onglet "Dataset Builder" dans l'app Shiny existante — un wizard pas-a-pas qui parse le `.ctl`, permet de definir le calendrier d'echantillonnage (style SOA), et genere le `.csv` NONMEM.

**Scope V1** : 1 seul groupe de design, PK simple, output = `.csv` uniquement.

---

## Fichiers a modifier/creer

| Fichier | Action |
|---------|--------|
| `app/R/mod_dataset_builder.R` | **CREER** — nouveau module (UI + Server + fonctions pures) |
| `R/parse_design_outputs.R` | **MODIFIER** — ajouter `parse_ctl_input()` et `parse_ctl_design()` |
| `app/app.R` | **MODIFIER** — ajouter nav sidebar + conditionalPanel + server call |
| `app/www/styles.css` | **MODIFIER** — ajouter CSS wizard/visit-rows/badges |

---

## 1. Parsers `.ctl` (dans `R/parse_design_outputs.R`)

### `parse_ctl_input(file)` -> character vector
- Lire toutes les lignes, strip commentaires (`;...`)
- Trouver `$INPUT`, concatener lignes de continuation (jusqu'au prochain `$`)
- Extraire tokens, gerer `DROP`/`SKIP`/`label=DROP`
- Retourne ex: `c("ID", "TIME", "AMT", "RATE", "EVID", "MDV", "DV")`

### `parse_ctl_design(file)` -> named list
- Trouver `$DESIGN`, extraire `MAXEVAL=`, `GROUPSIZE=`, `DESEL=`...
- Retourne `list(maxeval=, groupsize=, has_optimization=)`
- Permet d'auto-detecter eval vs optim

---

## 2. Module `mod_dataset_builder.R`

### UI : Wizard 4 etapes

**Etape 1 — Configuration**
- `fileInput` pour upload `.ctl`
- Affichage colonnes `$INPUT` detectees (badges colores)
- `radioButtons` : Evaluation / Optimisation (auto-detecte depuis `$DESIGN MAXEVAL`)
- Inputs dose : AMT, RATE (0=bolus), CMT (si detecte dans `$INPUT`)
- Bouton "Suivant"

**Etape 2 — Calendrier d'echantillonnage (style SOA)**
- Table dynamique de visites avec par ligne :
  - Label (texte libre, ex: "Jour 1")
  - Unite : Jour / Semaine (selectInput)
  - Valeur numerique (ex: 1, 2, 4...)
  - Type : Dose / PK-intensif / PK-sparse
- Conversion auto : Jour N -> (N-1)*24h, Semaine N -> (N-1)*168h
- Pour les visites "PK-intensif" : textInput pour saisir les temps post-dose (ex: `0.5, 1, 2, 4, 8, 12`)
- Pour les visites "PK-sparse" : 1 echantillon = temps de la visite
- Bouton "+ Visite" pour ajouter des lignes
- Resume : "N echantillons, 0-Xh"
- Boutons "Retour" / "Suivant"

**Etape 3 — Ajustement (optim. seulement)**
- Affiche uniquement si mode = Optimisation
- DT editable avec colonnes : TIME (read-only), TSTRAT (auto-incremente), TMIN, TMAX
- L'utilisateur peut modifier TMIN/TMAX par temps
- DT 0.23 supporte `editable = TRUE` avec `observeEvent(input$*_cell_edit, ...)`

**Etape 4 — Apercu et sauvegarde**
- `DTOutput` preview du dataset complet
- `textInput` nom du fichier (defaut : `design_data.csv`)
- `textInput` repertoire de sauvegarde (pre-rempli si possible)
- `actionButton` "Sauvegarder .csv" (ecrit sur disque via `writeLines`)
- `downloadButton` en fallback
- Message de confirmation/erreur

### Fonctions pures (testables independamment)

| Fonction | Role |
|----------|------|
| `visits_to_hours(visits)` | Convertit les visites (Jour/Semaine + intra-day) en vecteur d'heures absolues |
| `build_design_dataset(columns, visits, amt, rate, cmt, mode, fine_tune)` | Assemble le data.frame complet |
| `write_nonmem_csv(df, path)` | Ecrit le fichier espace-separe avec header C-prefixe |

### Logique d'assemblage du dataset

1. **Ligne dose** : `ID=1, TIME=0, AMT=dose, RATE=rate, EVID=1, MDV=1, DV=0`
2. **Lignes observation** : pour chaque temps de sampling -> `EVID=0, MDV=0, AMT=0, DV=1.0`
3. Tri par TIME
4. Si optim : TSTRAT auto-incremente (0=dose, 1,2,3...=obs), TMIN/TMAX depuis l'etape 3
5. Header : seule la 1ere colonne recoit le prefixe `C` (convention NONMEM `ignore=C`)
6. Format : espace-separe (comme les `.csv` existants dans les exemples)

---

## 3. Integration dans `app/app.R`

### Sidebar (UI) — apres "Design Robuste", avant "Diagnostic"
```r
div(class = "nav-section-label", "Outils"),
tags$button(class = "nav-item", id = "nav-builder",
  onclick = "navTo('builder', this)", "Dataset Builder"),
```

### Content area
```r
conditionalPanel("input.active_tab == 'builder'",
  mod_dataset_builder_ui("builder"))
```

### Server
```r
mod_dataset_builder_server("builder")
```

Module standalone, pas besoin des merged reactives (ext_data, etc.).

---

## 4. CSS (`app/www/styles.css`)

- `.wizard-step` / `.wizard-step.active` / `.wizard-step.completed` — stepper vertical
- `.wizard-step-number` — pastille numerotee (24px, ronde)
- `.visit-row` — ligne flex avec gap pour le formulaire de visites
- `.col-badge` / `.col-badge.opt-col` — badges colonnes detectees
- `.save-success` / `.save-error` — messages de confirmation

---

## 5. Contraintes techniques (R 4.2.0 / serveur Sanofi)

- `|>` OK, pas de placeholder `_`
- purrr 0.3.4 : `do.call(rbind, lapply(...))` au lieu de `list_rbind()`
- dplyr 1.0.9 : pas de `.by=`, pas de `reframe()`
- shiny 1.7.1 : `icon("times")` pas `icon("xmark")`
- bslib 0.3.1 : pas de `card()`, `sidebar()` — utiliser `div(class = "surface-card", ...)`
- DT 0.23 : `editable = TRUE` OK, renommer colonnes dans le df avant `datatable()`
- fileInput : ne pas mettre dans un `renderUI` qui depend de son propre handler

---

## 6. Sequence d'implementation

1. `parse_ctl_input()` + `parse_ctl_design()` dans `R/parse_design_outputs.R`
2. Creer `app/R/mod_dataset_builder.R` (fonctions pures d'abord, puis UI/Server)
3. Ajouter CSS dans `app/www/styles.css`
4. Integrer dans `app/app.R` (sidebar + conditionalPanel + server)

---

## 7. Verification

1. `shiny::runApp("app/")`
2. Cliquer "Dataset Builder" dans la sidebar
3. Uploader `app/examples/example1/warfarin.ctl`
4. Verifier colonnes detectees : `ID TIME AMT RATE EVID MDV DV`
5. Mode Evaluation : AMT=70, RATE=0
6. Ajouter visites : Jour 1 (PK-intensif 1h/4h/8h), Semaine 1 (PK-sparse)
7. Verifier preview : format identique a `warfarin.csv`
8. Sauvegarder et verifier le fichier sur disque
