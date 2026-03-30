---
paths:
  - "app/**"
---

# Shiny App V4 — Gotchas

## Navigation et UI

- **Navigation V4** : pas de `tabsetPanel` — navigation via `conditionalPanel("input.active_tab == 'id'")` contrôlé par JS `navTo(tab, el)` qui appelle `Shiny.setInputValue('active_tab', tab)`. Débugger navigation = vérifier `input$active_tab` côté serveur.
- **Drawer open/close** : `session$sendCustomMessage("evalJS", js)` — le handler JS est enregistré dans `output$js_handler` (uiOutput) avec `outputOptions(suspendWhenHidden=FALSE)`.
- **`formatStyle` + `colnames=`** : le paramètre `colnames=` de `datatable()` ne mappe pas avec `formatStyle` — toujours renommer les colonnes dans le df avec `rename()` avant `datatable()`
- **`formatStyle` + noms de runs** : utiliser `formatStyle(col_idx)` avec `which(names(wide) %in% run_names)` — jamais `formatStyle(run_name)` avec les noms d'affichage ou noms sanitisés `make.names()`. Les noms contenant des espaces ou caractères spéciaux lèvent `"column 'X' not found in data"` en runtime.
- **`if_else` vs `ifelse`** : `dplyr::if_else()` exige des conditions vectorisées — pour un check scalaire `!is.null(x)`, utiliser `base::ifelse()` ou un `if/else` ordinaire
- **Dose row dans `.tab`** : la row 1 est toujours la dose initiale (TIME=0, AMT>0) — l'exclure avec `tab[-1, , drop = FALSE]` avant tout traitement des temps d'échantillonnage
- **`observeEvent ignoreNULL`** : par défaut `ignoreNULL=TRUE` — si NULL est un état significatif (ex: reset après "Retirer la run"), toujours passer `ignoreNULL = FALSE`, sinon l'observer ne fire pas sur NULL
- **`%||%` priority + upload** : `merged_ext <- reactive({ example_ext() %||% upload$ext_data() })` donne toujours priorité à `example_ext()` — pattern requis : observer sur `upload$file_paths()` avec `ignoreNULL=FALSE` qui clear tous les `example_*()` reactiveVals quand `!is.null(fps$ext)`
- **`str_split_1` absent de stringr 1.4.0** : n'existe qu'à partir de stringr 1.5.0 — utiliser `strsplit(x, sep)[[1]]` à la place
- **`sprintf` + CSS `%`** : tout format string sprintf contenant des pourcentages CSS (`50%`, `100%`) doit les doubler : `50%%`, `100%%`. Sinon R interprète `50%` comme début de spécificateur de format et lève une erreur. S'applique à toute chaîne CSS dans `sprintf()` — notamment `run_pill()` et les styles inline.
- **`vapply` sur liste nommée + `any_of()`** : `vapply()` sur une liste nommée retourne un vecteur nommé. Passer ce vecteur à `dplyr::any_of()` renomme les colonnes avec les clés au lieu des valeurs. Fix : `unname(vapply(...))` partout où on extrait des noms de runs pour la sélection de colonnes.

## Gotchas multi-runs

- **vapply + tbl_no cross-run** : `get_ofv(ext, tbl_no)` retourne `numeric(0)` si le run n'a pas la table demandée — toujours guard `if (length(val) == 0L) return(NA_real_)` dans tout `vapply` sur `all_runs`
- **Exemple 3 SUBPROB=1000** : `.ext`/`.shk` ont 1000 TABLE NO., `.tab` a 1000 blocs concaténés sans TSTRAT ; `tbl_no()` passe à max=1000 au chargement. Dans `mod_times.R`, `is_robust = n_distinct(tab$table_no) > 1` → boxplot distribution + `tab_single = filter(tab, table_no == 1)` pour la courbe predite.
- **Exemple 2 auto-charge exemple 1** : `compare_with = "example1"` dans `.EXAMPLES` — charger exemple 2 ajoute exemple 1 dans `all_runs` (effet de bord à connaître pour debug)
- **Labels params multi-run** : en multi-run, utiliser noms bruts THETA1/OMEGA(1,1) et non les labels utilisateur pour éviter l'ambiguité inter-modèles
