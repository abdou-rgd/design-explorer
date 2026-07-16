# NONMEM DESIGN Explorer

Application Shiny et helpers R pour explorer les sorties NONMEM `$DESIGN`,
comparer des designs pharmacometriques, et analyser les diagnostics SSE/FIM.

## Lancer l'application

Le projet cible exactement R 4.2.0. Le lock `renv` restaure les versions de
paquets validees pour cet environnement; il ne faut pas installer les versions
CRAN courantes a la place.

```r
source("app/install_deps.R")
shiny::runApp("app/")
```

Apres la premiere restauration, les demarrages suivants peuvent se limiter a
`shiny::runApp("app/")`: `.Rprofile` active automatiquement l'environnement du
projet.

## Tests

```powershell
Rscript tests/run_tests.R
```

La suite couvre les parsers NONMEM, les metriques FIM, les diagnostics SSE,
l'integration mrgsolve, la securite des archives et les principaux contrats
applicatifs. La CI execute cette suite sous R 4.2.0.

## Structure

- `app/` - application Shiny, modules UI/server, assets et exemples integres.
- `R/` - helpers partages pour parsing, metriques, diagnostics, plots et
  logique commune.
- `tests/` - tests unitaires et smoke tests locaux.
- `CHANGELOG.md` - suivi des changements utiles a la reprise du contexte.

## Donnees et artefacts locaux

Les sorties lourdes ou personnelles ne sont pas versionnees: rapports locaux,
exports personnels, archives SSE, logs, dossiers `outputs/`, `docs/`,
`audits/` et configurations d'outils locaux. Les exemples necessaires a
l'application restent explicitement autorises sous `app/examples/`.
