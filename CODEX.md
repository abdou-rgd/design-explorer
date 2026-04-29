# CODEX.md - Design Explorer handoff

This file is Codex's working note for this repository. It complements
`CLAUDE.md`; when in doubt, preserve the project conventions already described
there.

## Project shape

- R/Shiny application: `DE$IGN EXPLORER`, a NONMEM `$DESIGN` post-processing
  tool for FIM metrics, RSE/SE, RELATIVEINF, optimal times, power/NSN, and
  FIM-vs-SSE validation.
- Pure R logic lives in `R/`; Shiny modules live in `app/R/`; examples and
  regression fixtures live under `app/examples/` and `docs/results/`.
- The target server is R 4.2.0. Avoid package features newer than the versions
  listed in `CLAUDE.md` (`dplyr::.by`, `reframe()`, `pick()`,
  `stringr::str_split_1()`, `ggplot2::linewidth`, recent `bslib` cards, etc.).

## Common commands

Run from the repository root:

```powershell
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' tests\run_tests.R
```

```r
shiny::runApp("app/", launch.browser = TRUE)
```

Local note: this Codex session may not have all R packages installed. If tests
fail at `library(testthat)` or `library(readr)`, install/check dependencies
before interpreting failures as project regressions.

## Current branch of interest

`feat/times-tab-redesign` is a major rework of the Optimal Times tab:

- `R/tab_dispatch.R`: classifies `.tab` shapes and dispatches rendering.
- `R/pk_templates.R`: closed-form ADVAN1-4 PK templates for smooth curves when
  `mrgsolve` is unavailable.
- `app/R/mod_times.R`: uses a three-tier curve engine:
  `mrgsolve` -> closed-form template -> IPRED dot fallback.
- Added tests cover dispatcher behavior, parser hardening, templates, and Bauer
  examples 2-7 integration.

Main review focus for this branch:

- Keep parsing/normalization separate from rendering decisions.
- Verify all `.tab` patterns promised by the dispatcher are actually detected
  or intentionally deferred.
- Check dose marker behavior for template-tier plots, not only mrgsolve-tier
  plots.
- Validate examples visually after tests pass, especially examples 2, 3, 4, 6,
  and 7.

## Sensitive areas

- `.tab` parsing and display: baseline rows, per-ID behavior, robust designs
  with many `TABLE NO.`, representative IDs, dose rows, `TSTRAT`, `CMT`, and
  `AMTSTRAT`.
- Shiny module wiring in `app/app.R`: uploads/examples/merged reactives are
  central and easy to break indirectly.
- DT formatting: prefer robust column-index references when display names can
  contain spaces, symbols, or renamed labels.
- SSE/PsN parsing: column naming is variable; changes should come with fixtures
  or regression tests.

## Working style

- Make surgical changes and keep unrelated formatting churn out of diffs.
- Add or extend tests whenever touching parsers, NONMEM format assumptions, or
  cross-module contracts.
- Prefer small pure functions in `R/` and keep Shiny modules as consumers of
  normalized data.
