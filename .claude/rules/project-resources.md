# Ressources disponibles dans le projet

| Fichier | Description |
|---------|-------------|
| `R/parse_design_outputs.R` | Parsers R : `read_ext()`, `read_shk()`, `read_coi()`, `read_clt()`, `read_tab()`, `read_prior_nwpri()`, `read_summary_tab()`, `get_rse()`, `get_relativeinf()`, `get_d_criterion()`, `get_cor_matrix()`, `summary_design()` |
| `R/report_design.R` | Visualisations : `plot_relativeinf()`, `plot_rse()`, `plot_se()`, `plot_rse_waterfall()`, `plot_convergence()`, `plot_fim_heatmap()`, `plot_optimal_times()` |
| `app/app.R` | Application Shiny post-processing $DESIGN (V4) — sidebar nav, drawer — `shiny::runApp("app/")` depuis la racine |
| `app/R/` | 13 modules : upload, compare, examples, params, rse, relativeinf, fim, times, prior, convergence, raw, helpers_ui |
| `app/examples/` | Exemples Bauer 2021 intégrés (example1–7, fichiers `.ext`/`.shk`/`.coi`/`.clt`/`.tab`) |
| `app/www/styles.css` | Styles CSS V4 (CSS variables, sidebar layout, drawer, metric cards) |
| `app/install_deps.R` | Installe les packages Shiny manquants (shiny, bslib, DT) |
| `docs/other_softwares/PFIM/` | Code source PFIM 7.0 — référence SE/RSE/shrinkage (non tracké git) |
| `docs/other_softwares/PopED-master/` | Code source PopED — référence efficiency(), plot_efficiency_of_windows() (non tracké git) |
| `docs/nonmem/manuel_nonmem.txt` | Manuel NONMEM 7.5.1 complet (13 057 lignes) — lisible via Read avec offset |
| `docs/papers/bauer2021/bauer2021_text.txt` | Papier Bauer 2021 extrait en texte — lisible directement |
| `docs/nonmem/doc_nonmem_design.pdf` | Section I.72 du manuel (18p) — $DESIGN |
| `docs/bauer2021_examples/` | 7 exemples complets avec tous les fichiers NONMEM |
| `docs/courses/nlme.pdf` | Cours NLME Leroux (janv. 2026) |
| `docs/intern_work/redaction.docx` | Mémoire de stage en cours — question de recherche, méthodologie, planning |

> **Vision V5 :** l'app évolue vers un outil bout-en-bout : **générateur de control stream $DESIGN** (inputs : modèle, paramètres, design candidat → output : `.ctl` prêt à soumettre) + post-processing existant.
