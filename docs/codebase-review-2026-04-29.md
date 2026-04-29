# Codebase Review - 2026-04-29

## Summary

Full-project review covering R scientific helpers, Shiny orchestration, tests,
documentation, local fixtures, and scientific assumptions. Statuses below
reflect the targeted implementation pass that followed the audit.

## Findings

| Priority | Area | Finding | Evidence | Recommendation | Status |
|---|---|---|---|---|---|
| P1 | Scientific power | One-sided Wald power used the two-sided tail structure, so `H1: theta > h0` could report high power when `theta < h0`. | `R/fim_metrics.R::compute_power_wald()` | Use directional normal power and return impossible NSN when `theta <= h0` for one-sided tests. | fixed |
| P1 | mrgsolve bridge | THETA mapping initialized as character and could coerce numeric parameters before injection into mrgsolve. | `R/mrgsolve_bridge.R::extract_params_for_mrgsolve()` | Initialize mapped values as numeric and test mapped parameter types. | fixed |
| P1 | PK templates | `pk_2cpt_iv(rate)` accepted infusion input but plotted bolus equations. | `R/pk_templates.R::pk_2cpt_iv()` | Fail explicitly for infusion templates and route infusion designs to mrgsolve. | fixed |
| P1 | Shiny FIM/SSE | SSE validation always used the last `.ext` table instead of the selected `TABLE NO.`. | `app/R/mod_sse_validation.R`, `app/app.R` | Pass the global `tbl_no` reactive into SSE validation and fall back to last table only if invalid. | fixed |
| P2 | Optimal-times UI | Robust-design plot ignored the Days/Hours toggle. | `app/R/mod_times.R` | Convert robust plot x values and labels through a display-time variable. | fixed |
| P2 | Upload | UI accepted `.gz`, but extraction only handled `.tar.gz`/`.tgz`; archive extraction errors were not wrapped. | `app/R/mod_upload.R`, `app/R/mod_compare.R`, `app/R/helpers_ui.R` | Remove `.gz` from accepted formats and surface archive extraction failures clearly. | fixed |
| P2 | PsN/NONMEM parsing | Inline `$OMEGA BLOCK()` and `$SIGMA BLOCK()` values were skipped by `read_true_values()`. | `R/sse_metrics.R::read_true_values()` | Parse inline lower-triangular values plus continuation lines. | fixed |
| P2 | SSE comparison script | `tests/test_sse_comparison.R` called missing `compute_empirical_correlations()` and was outside the main runner. | `tests/test_sse_comparison.R`, `tests/run_tests.R` | Add the helper and run local smoke checks from `tests/run_tests.R` when fixtures exist. | fixed |
| P2 | Empirical D wording | SSE `det(VarCov)^(1/p)` was named like FIM D even though lower is better. | `R/sse_metrics.R`, `app/R/mod_sse_validation.R` | Document and label it as empirical generalized variance; retain legacy field names for compatibility. | fixed |
| P2 | FIM inversion | Direct `solve()` lacked explicit conditioning and positive-definite diagnostics. | `R/design_metrics.R` | Use `rcond()` guard and Cholesky inversion for FIM-derived covariance/correlation. | fixed |
| P2 | Reproducibility | Tests depended on ignored `docs/results` fixtures; versions were misaligned; no dependency manifest existed. | `tests/run_tests.R`, `README.md`, `VERSIONS.md`, `app/app.R` | Skip local smoke tests when fixtures are absent, align V5.8, add `DESCRIPTION`, document fixture policy. | fixed |
| P3 | Shiny module coverage | Reactive flows are mostly uncovered by `testServer()`/browser checks. | `app/R/*.R` | Add module-level reactive tests for upload/reset/comparison/export in a future pass. | deferred |
| P3 | Public docs | Internal notes still contain context not meant for redistribution. | `CLAUDE.md`, local docs | Decide whether internal handoff docs remain in repo or move to private notes. | deferred |

## Verification Scope

Acceptance commands:

```powershell
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' tests\run_tests.R
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' tests\test_sse_comparison.R
```

Manual Shiny QA remains recommended for examples Bauer 1-7, uploads, multi-run
comparison, SSE validation/analysis/comparison, and small-screen navigation.
