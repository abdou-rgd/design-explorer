# NONMEM DESIGN Explorer

Application Shiny et helpers R pour explorer les sorties NONMEM `$DESIGN`,
comparer des designs pharmacometriques, et analyser les diagnostics SSE/FIM.

## Lancer l'application

```r
source("app/install_deps.R")
shiny::runApp("app/")
```

## Tests

```powershell
Rscript tests/run_tests.R
```

La suite couvre les parsers NONMEM, les metriques FIM, les diagnostics SSE,
l'integration mrgsolve et les principaux contrats applicatifs.

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
