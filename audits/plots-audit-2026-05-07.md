# Plots audit - 2026-05-07

Scope: static Shiny UI audit after the KPI / metric-card pass. The goal is to
identify plot outputs still using legacy containers or inconsistent panel
composition before starting the visual QA pass.

## Snapshot

- `plotOutput()` calls: 20
- `plot_panel()` calls: 11
- `analysis_workspace()` calls around active plot views: 12
- Legacy `plot-card` containers: 0
- Legacy `surface-card` containers: 0

Updated after the first plots pass: `mod_times.R`, `mod_prior.R`,
`mod_power.R`, and `mod_mrgsolve.R` have been migrated away from the legacy
containers identified below. The findings are retained as the audit trail for
what was changed.

## Already Aligned

- `app/R/mod_convergence.R`: convergence plot uses `plot_panel()`.
- `app/R/mod_fim.R`: FIM heatmap uses `plot_panel()`; FIM KPI summaries now use
  `fact_strip()`.
- `app/R/mod_relativeinf.R`: RELATIVEINF plot uses `plot_panel()`.
- `app/R/mod_rse.R`: RSE plot uses `plot_panel()`.
- `app/R/mod_sse_comparison.R`: active comparison plots use `plot_panel()`.
- `app/R/mod_sse_analysis.R` and `app/R/mod_sse_validation.R`: active plot
  switching uses `analysis_workspace()`, which is visually compatible with
  `plot_panel()`.

## Findings

### P1 - Power / NSN still uses legacy surface cards for plot and table panels

Status: addressed in the first plots pass.

Files:

- `app/R/mod_power.R:126`
- `app/R/mod_power.R:141`
- `app/R/mod_power.R:150`
- `app/R/mod_power.R:167`
- `app/R/mod_power.R:174`
- `app/R/mod_power.R:188`
- `app/R/mod_power.R:205`
- `app/R/mod_power.R:226`
- `app/R/mod_power.R:233`
- `app/R/mod_power.R:243`
- `app/R/mod_power.R:250`

Impact: this tab is the largest remaining cluster of old-style panel markup. It
mixes explanatory notes, tables, controls, and two plots with raw `surface-card`
containers and inline CSS, so it will remain visually distinct from the V7
analysis workspace unless migrated.

Recommended pass:

- Convert explanatory blocks to `science_note()` or `status_panel()`.
- Convert DT table sections to `table_panel()`.
- Convert `power_curve` and `equiv_power_curve` sections to `plot_panel()`.
- Keep the existing `settings_bar()` and `settings-help` behavior intact.

### P1 - Optimal Times plot containers still use `plot-card`

Status: addressed in the first plots pass.

Files:

- `app/R/mod_times.R:313`
- `app/R/mod_times.R:333`

Impact: the Optimal Times tab already has a V7 page shell, but the main
prediction plot still drops into legacy `plot-card` markup. This is a contained
migration and should be low risk if the existing `plot_export_ui()` calls are
preserved.

Recommended pass:

- Replace both `div(class = "plot-card", ...)` blocks with `plot_panel()`.
- Replace the times table `param-table-wrap` with `table_panel()` in the same
  UI branch for consistency.

### P2 - Robust Design distribution plot still uses `plot-card`

Status: addressed in the first plots pass.

File:

- `app/R/mod_prior.R:75`

Impact: the Robust Design tab otherwise uses the new page shell and `fact_strip`
for prior facts, but its distribution plot is still in the legacy plot card.

Recommended pass:

- Convert the robust distribution block to `plot_panel()`.
- Convert robust table block to `table_panel()` if the same pass touches the
  adjacent table.

### P2 - mrgsolve panel uses raw `surface-card`

Status: addressed in the first plots pass.

Files:

- `app/R/mod_mrgsolve.R:27`
- `app/R/mod_mrgsolve.R:37`

Impact: this module is embedded in Optimal Times and currently brings old
surface styling plus inline CSS into the V7 shell. It is not a plot output
itself, but it affects the plot workspace stack.

Recommended pass:

- Use `status_panel()` for the unavailable state.
- Use `analysis_workspace()` or `control_panel()` for the collapsible upload and
  compile controls.

## Suggested Order

1. Browser visual QA: load examples with RSE, FIM, Relative Info, Optimal Times,
   Power, SSE Validation, SSE Analysis, and SSE Comparison tabs; check plot
   framing, export-bar spacing, and mobile wrapping.
2. If visual QA flags spacing issues, tune shared helpers/CSS rather than
   reintroducing per-module panel markup.

## Follow-up Checks

- Static check: no `class = "plot-card"` in `app/R/*.R`.
- Static check: no `class = "surface-card"` in `app/R/*.R`.
- Runtime check: run `tests/run_tests.R`.
- Visual check: screenshot plot-heavy tabs at desktop and mobile widths after
  the next migration pass.
