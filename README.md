# NONMEM $DESIGN -- Post-Processing & Optimal Design

**V5.8** -- M2 Health Data Science Internship Project
Optimization of clinical trial designs in pharmacometrics using the Fisher Information Matrix (FIM).

---

## Objectives

1. **Scientific research** -- Study of optimal design methods (D/A/R-optimality, sequential design, robust design) applied to NLME models in pharmacokinetics/pharmacodynamics.
2. **R post-processing tools** -- Facilitate the analysis and visualization of NONMEM `$DESIGN` outputs for pharmacometricians.

---

## Project Structure

```
.
|-- R/
|   |-- design_utils.R           # Utilities: %||%, constants, internal helpers
|   |-- design_io.R              # NONMEM readers: read_ext(), read_shk(), read_coi(), ...
|   |-- design_metrics.R         # FIM extractors: get_rse(), get_d_criterion(), ...
|   |-- design_summary.R         # Console display: summary_design()
|   |-- ctl_parsers.R            # .ctl parsers: parse_theta_labels(), parse_groupsize(), ...
|   |-- fim_metrics.R            # Power/NSN Wald & TOST: compute_power_wald(), ...
|   |-- report_design.R          # ggplot2 visualizations: plot_rse(), plot_convergence(), ...
|   `-- sse_diagnostics.R        # SSE intrinsic analysis: compute_param_diagnostics(), ...
|-- app/
|   |-- app.R                    # Shiny application "DE$IGN EXPLORER" (V5.8)
|   |-- install_deps.R           # Install Shiny dependencies
|   |-- examples/                # Built-in examples (Bauer 2021, examples 1-7)
|   `-- R/                       # 18 Shiny modules
|       |-- mod_home.R           # Home: upload + run summary + compare
|       |-- mod_upload.R         # NONMEM file import (.ext, .shk, .coi, .tab, ...)
|       |-- mod_params.R         # Final parameters, RSE, shrinkage
|       |-- mod_rse.R            # Predicted RSE (%) with 4-tier thresholds
|       |-- mod_relativeinf.R    # Relative information per ETA
|       |-- mod_convergence.R    # OFV convergence by optimizer
|       |-- mod_fim.R            # FIM heatmap, D-criterion, eigenvalues
|       |-- mod_times.R          # Optimized sampling times (+ robust design boxplots)
|       |-- mod_prior.R          # $PRIOR NWPRI visualization
|       |-- mod_power.R          # Wald power, NSN, TOST equivalence
|       |-- mod_sse_upload.R     # SSE file upload
|       |-- mod_sse_validation.R # SSE vs FIM validation (scatter, REE, RSE bars)
|       |-- mod_sse_analysis.R   # SSE intrinsic analysis (distributions, correlations)
|       |-- mod_sse_comparison.R # Two-SSE A/B comparison
|       |-- mod_raw.R            # Raw files viewer
|       |-- mod_ctl_stream.R     # Control stream viewer
|       |-- mod_examples.R       # Built-in examples with auto-compare
|       `-- helpers_ui.R         # UI helpers: badges, pills, detect_method()
|-- tests/
|   |-- run_tests.R              # Test runner script
|   `-- testthat/                # unit tests + local SSE smoke checks
`-- docs/
    `-- papers/                  # Reference literature
```

---

## Shiny Application -- `DE$IGN EXPLORER`

Interactive web interface for exploring NONMEM `$DESIGN` outputs.

### Getting Started

```r
# Install dependencies if needed
source("app/install_deps.R")

# Launch the application
shiny::runApp("app/")
```

### Features

| Tab / Menu | Description |
|------------|-------------|
| **Home** | File upload + run summary (OFV, params, CPU, method) + multi-run comparison |
| **Parameters** | Final parameters, RSE, shrinkage |
| **Design** (menu) | Predicted RSE (4 tiers), Relative Information (%), Optimal Times, Robust Design |
| **Analysis** (menu) | FIM heatmap + eigenvalues, OFV convergence, SSE Validation (FIM vs SSE scatter) |
| **Decision** (menu) | Wald power, NSN, TOST equivalence |
| **Raw Data** | Raw files viewer, Control stream viewer |
| **Examples** | Built-in examples (Bauer 2021, 1-7) with auto-compare |

---

## R Scripts

### Reading and Metrics

```r
source("R/design_utils.R")
source("R/design_io.R")
source("R/design_metrics.R")
source("R/design_summary.R")

# Read files
ext <- read_ext("run001.ext")
shk <- read_shk("run001.shk")
coi <- read_coi("run001.coi")
tab <- read_tab("run001.tab")

# Extract metrics
get_final_params(ext)    # Final parameters (iteration -1000000000)
get_se(ext)              # Standard errors predicted by FIM
get_ofv(ext)             # Optimality criterion value
get_rse(ext)             # RSE (%): tibble with param / estimate / se / rse_pct
get_relativeinf(shk)     # Relative information (%) per ETA (TYPE 11)
get_d_criterion(ext)     # D-criterion: -log(det(FIM))

# Run summary
summary_design(ext, shk)
```

### Control Stream Parsers

```r
source("R/ctl_parsers.R")

parse_theta_labels("run001.ctl")   # THETA labels from comments
parse_groupsize("run001.ctl")      # GROUPSIZE per arm
parse_design_summary("run001.ctl") # $DESIGN block summary
read_prior_nwpri("run001.ctl")     # $PRIOR NWPRI parameters
```

### Power / Sample Size

```r
source("R/fim_metrics.R")

compute_power_wald(rse, n)          # Wald test power
compute_n_needed(rse, power = 0.8)  # Sample size for target power
compute_power_tost(rse, n, delta)   # TOST equivalence power
```

### ggplot2 Visualizations

```r
source("R/report_design.R")

plot_rse(ext)              # RSE (%) barplot by parameter, faceted by type
plot_relativeinf(shk)      # RELATIVEINF (%) barplot per ETA with color coding
plot_convergence(ext)      # OFV vs iteration curve by optimizer
plot_fim_heatmap(coi)      # FIM heatmap with annotations
plot_optimal_times(tab)    # Optimized times on timeline
plot_model_prediction(tab) # IPRED vs TIME curve by arm
```

---

## NONMEM `$DESIGN` Output Files -- Quick Reference

| File | Key Content |
|------|-------------|
| `.ext` | Parameters + OFV per iteration; special lines: `-1e9` (final), `-1e9-1` (SE) |
| `.shk` | Shrinkage -- TYPE 11 = `RELATIVEINF(%)` |
| `.coi` | Full Fisher Information Matrix (named format) |
| `.clt` | FIM in lower-triangular form (TOSL order) |
| `.tab` | Optimized times/doses defined via `$TABLE` |
| `.res` | Full report: `#OBJV`, `RELATIVEINF`, computation time |
| `.bfm` | Individual Bayesian FIM (only with `OFVTYPE=8`) |

---

## Optimality Criteria (`OFVTYPE=`)

| Value | Criterion | Formula |
|-------|-----------|---------|
| 0-1 | **D-optimality** (default) | `-log(det(FIM))` |
| 2 | A-optimality | `-1/tr(FIM^-1)` |
| 6 | DS-optimality | D-opt + penalty for nuisance parameters |
| 7 | R-optimality (RSE) | `-1/tr(sqrt(FIM^-1)/theta)` |
| 8 | Individual Bayesian FIM | `-log(det(FIM_bayes))` |

---

## Tech Stack

- **R** 4.2+ with tidyverse (`dplyr`, `tidyr`, `purrr`, `ggplot2`, `stringr`, `readr`)
- **Shiny** 1.7+ / `bslib` 0.3+ (Bootstrap 3) / `DT`
- **NONMEM** 7.5+ (plain text outputs)

---

## Built-in Examples (Bauer 2021)

| # | Model | Illustrates |
|---|-------|-------------|
| 1 | Warfarin | Simple evaluation, block-diagonal FIM |
| 2 | Warfarin | Sampling time optimization (NELDER) |
| 3 | Warfarin | Robust design via `$SIM TRUE=PRIOR` |
| 4 | PK-PD | Multi-response, PK/PD link |
| 5 | Warfarin | DS-optimality, nuisance parameters |
| 6 | TMDD | Stratification (`STRAT/STRATF`), ODE |
| 7 | TMDD | Bayesian FIM, dose + time optimization |

Example files are available in `app/examples/`.

### Fixture Policy

`app/examples/` intentionally tracks Bauer 2021 NONMEM example outputs so the
app and parser tests work without private data. Large local research material
and generated outputs under `docs/results/`, `docs/papers/`, and one-off
`scripts/` stay ignored; smoke tests that need those local fixtures skip
explicitly when the files are absent.

---

## References

- Bauer RJ (2021). NONMEM Tutorial Part III: Optimal Design. *CPT Pharmacometrics Syst Pharmacol*.
- Fayette L, Brendel K, Mentre F (2026). Evaluation of the Fisher Information Matrix-Based Optimal Design in Comparison with Stochastic Simulation and Estimation for Nonlinear Mixed-Effect Models. *Pharm Res*.

---

## License

This project is licensed under the MIT License -- see the [LICENSE](LICENSE) file for details.
