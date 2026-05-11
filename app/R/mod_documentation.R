# =============================================================================
# mod_documentation.R -- In-app scientific documentation
# =============================================================================

mod_documentation_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    page_header(
      "Documentation",
      "Operational reference for NONMEM DESIGN Explorer inputs, outputs, design diagnostics, and SSE workflows.",
      eyebrow = "Reference"
    ),
    tags$div(class = "documentation-layout",
      tags$nav(class = "documentation-nav",
        tags$a(href = "#doc-overview", "Overview"),
        tags$a(href = "#doc-inputs", "Input files"),
        tags$a(href = "#doc-parameters", "Parameters / RSE / Shrinkage"),
        tags$a(href = "#doc-fim", "FIM & Criteria"),
        tags$a(href = "#doc-times", "Optimal Times"),
        tags$a(href = "#doc-sse-validation", "SSE Validation"),
        tags$a(href = "#doc-sse-analysis", "SSE Analysis"),
        tags$a(href = "#doc-sse-comparison", "SSE Comparison"),
        tags$a(href = "#doc-power", "Power / NSN / TOST"),
        tags$a(href = "#doc-robust", "Robust Design"),
        tags$a(href = "#doc-references", "References")
      ),
      tags$div(class = "documentation-content",
        documentation_section(
          "overview", "Overview",
          tags$p(
            "NONMEM DESIGN Explorer is a post-processing dashboard for NONMEM ",
            tags$code("$DESIGN"), " outputs. The operational tabs focus on run state, ",
            "tables, plots, and exports; this page keeps the longer methodology notes ",
            "and formulas in one place."
          ),
          tags$p(
            "Recommended workflow: load NONMEM design outputs in Home, inspect ",
            "parameters and FIM criteria, review optimal times, then upload PsN SSE ",
            tags$code("raw_results_*.csv"), " files to validate empirical precision."
          )
        ),

        documentation_section(
          "inputs", "Input Files",
          tags$dl(
            tags$dt(tags$code(".ext")),
            tags$dd("Required for optimality criteria, RSE/SE, convergence, and most design summaries."),
            tags$dt(tags$code(".shk")),
            tags$dd("Optional shrinkage and RELATIVEINF source."),
            tags$dt(tags$code(".coi"), " / ", tags$code(".clt")),
            tags$dd("Optional FIM/correlation matrix source for eigenvalues and heatmaps."),
            tags$dt(tags$code(".tab")),
            tags$dd("Optional optimal-time table used by the Optimal Times and Robust Design views."),
            tags$dt(tags$code(".ctl"), " / ", tags$code(".mod"), " / ", tags$code(".con")),
            tags$dd("Optional control stream used for true values, GROUPSIZE, compartment labels, and robust-prior metadata."),
            tags$dt(tags$code("raw_results_*.csv")),
            tags$dd("PsN SSE raw results. Upload in SSE Upload; Design A is required and Design B enables comparison.")
          )
        ),

        documentation_section(
          "parameters", "Parameters / RSE / Shrinkage",
          tags$p(
            tags$strong("OFV"), " is ", tags$code("-log(det(FIM))"),
            " for D-optimality designs, or the optimality criterion value returned ",
            "by NONMEM for other criteria. For D-optimality, smaller OFV means a ",
            "more informative design."
          ),
          tags$p(
            tags$strong("Delta OFV vs ref"), " = OFVref - OFVrun on the raw ",
            "optimality scale. Positive values indicate the comparison run is more ",
            "informative than the reference in the D-optimality case."
          ),
          tags$p(HTML(paste0(
            "<strong>D-efficiency vs ref</strong> = ",
            "(det(FIM<sub>run</sub>) / det(FIM<sub>ref</sub>))<sup>1/p</sup> - 1, ",
            "expressed in percent. The exponent normalizes the ratio per estimable parameter."
          ))),
          tags$p(
            "Shrinkage and RELATIVEINF are read from the ",
            tags$code(".shk"), " file when present. Parameters with very high RSE ",
            "may be hidden by default in selected SSE views to keep plots readable."
          )
        ),

        documentation_section(
          "fim", "FIM & Criteria",
          tags$p(HTML(paste0(
            "The D-criterion displayed from NONMEM <code>$DESIGN</code> is ",
            "&phi;<sub>D</sub><sup>FIM</sup> = det(FIM)<sup>1/p</sup>. ",
            "It is a theoretical asymptotic prediction of design informativeness."
          ))),
          tags$p(
            "The FIM tab also reports determinant, condition number, eigenvalue range, ",
            "and the correlation heatmap when a matrix source is available. Robust designs ",
            "summarize the distribution of D-criterion values across prior realizations."
          ),
          tags$p(
            tags$strong("Robust D-criterion [P10-P90]"), " is the geometric mean of ",
            "det(FIM)^(1/p) over Monte Carlo prior realizations. Narrow percentile ",
            "spread suggests the design is stable across prior uncertainty."
          )
        ),

        documentation_section(
          "times", "Optimal Times",
          tags$p(
            "The Optimal Times tab reads observation times from the loaded ",
            tags$code(".tab"), " file. For robust designs, times are summarized over ",
            "all prior realizations by stratum with P10, median, and P90 intervals."
          ),
          tags$p(
            "When a supported model path is available, the tab overlays sampling points ",
            "on a prediction curve. The curve can come from mrgsolve, a closed-form ADVAN ",
            "template, or a fallback dot plot when no smooth engine is available."
          )
        ),

        documentation_section(
          "sse-validation", "SSE Validation",
          tags$p(
            "For each model parameter over K successful SSE runs, this app filters on ",
            tags$code("minimization_successful = 1"), " before computing empirical metrics."
          ),
          tags$table(class = "documentation-table",
            tags$thead(tags$tr(
              tags$th("Metric"), tags$th("Formula"), tags$th("Interpretation")
            )),
            tags$tbody(
              tags$tr(tags$td("REE"),
                tags$td(HTML("REE<sub>k</sub> = (&hat;x<sub>k</sub> - x*) / x* x 100")),
                tags$td("Relative Estimation Error for run k")),
              tags$tr(tags$td("RB (%)"),
                tags$td(HTML("RB = (1/K) sum REE<sub>k</sub>")),
                tags$td("Relative bias / accuracy")),
              tags$tr(tags$td("95% CI of RB"),
                tags$td(HTML("RB +/- 1.96 x sd(REE) / sqrt(K)")),
                tags$td("If the interval excludes 0, bias is significant")),
              tags$tr(tags$td("RRMSE (%)"),
                tags$td(HTML("sqrt((1/K) sum REE<sub>k</sub><sup>2</sup>)")),
                tags$td("Precision plus bias")),
              tags$tr(tags$td("Empirical RSE (%)"),
                tags$td(HTML("100 x sd(&hat;x) / |x*|")),
                tags$td("Empirical precision compared with FIM-predicted RSE")),
              tags$tr(tags$td("Empirical generalized variance"),
                tags$td(HTML("&phi;<sub>D</sub> = det(VarCov)<sup>1/p</sup>")),
                tags$td("Global uncertainty summary; lower is better"))
            )
          ),
          tags$p(HTML(paste0(
            "The empirical generalized variance summarizes global precision across ",
            "SSE replicates. Under asymptotic normality, VarCov approximates FIM",
            "<sup>-1</sup>, so the empirical and theoretical criteria are reciprocal ",
            "in expectation."
          ))),
          tags$p(
            "The raw variance-covariance determinant is scale-dependent. The Validation ",
            "tab also reports the determinant of the empirical correlation matrix to ",
            "separate unit-scale artifacts from structural near-collinearity."
          )
        ),

        documentation_section(
          "sse-analysis", "SSE Analysis",
          tags$p(
            "SSE Analysis examines the simulation-estimation run set itself: run health, ",
            "parameter distributions, OFV distribution, shrinkage views, and per-parameter ",
            "diagnostic tables."
          ),
          tags$p(
            "Shrinkage views require non-empty ", tags$code("shrinkage_eta*(%)"),
            " columns in the PsN raw results. PsN writes these only when NONMEM computes ",
            "post-hoc ETAs and PsN is not run with ", tags$code("-no_shrinkage"), "."
          )
        ),

        documentation_section(
          "sse-comparison", "SSE Comparison",
          tags$p(
            "SSE Comparison treats Design A and Design B as first-class analysis inputs. ",
            "Use it to compare an original and optimized design by empirical RSE, ",
            "distribution overlays, and the exported RSE comparison table."
          ),
          tags$p(
            "Both designs use the same true values extracted from the loaded control stream. ",
            "If true values are missing, upload the corresponding .ctl/.mod/.con file in Home."
          )
        ),

        documentation_section(
          "power", "Power / NSN / TOST",
          tags$p(HTML(paste0(
            "The Wald test evaluates whether a parameter is significantly different ",
            "from a reference value H<sub>0</sub>. The statistic ",
            "W = (&theta; - H<sub>0</sub>) / SE follows a normal approximation."
          ))),
          tags$p(
            "Power is the probability of rejecting H0 when the effect truly exists. ",
            "Power >= 80% is commonly used as a decision threshold for whether the ",
            "design is expected to detect the parameter effect."
          ),
          tags$p(
            "Two-sided Wald is the source-locked default used for PopED-compatible ",
            "checks. The app-specific directional mode uses H1: theta > H0 for ",
            "screening positive effects. PopED twoSided = FALSE only changes ",
            "the alpha split in its symmetric Wald equation, so it ",
            "should not be read as the same directional policy."
          ),
          tags$p(HTML(paste0(
            "Sample size calculations use FIM scaling: N = N<sub>0</sub> x ",
            "(RSE / RSE<sub>target</sub>)<sup>2</sup>."
          ))),
          tags$p(
            "N ratio is N needed divided by current N. Values <= 1 indicate the ",
            "current design is sufficient under the chosen target power and FIM scaling."
          ),
          tags$p(
            "TOST evaluates whether a parameter is equivalent to a reference value within ",
            "a symmetric margin [-delta, +delta]. It answers whether the effect is close ",
            "enough to zero to be considered negligible."
          ),
          tags$p(HTML(paste0(
            "TOST uses two one-sided tests. In this app, alpha is the one-sided alpha ",
            "used for each test, not alpha divided by two. Parameters outside the ",
            "equivalence margin cannot demonstrate equivalence, so their TOST power is 0."
          ))),
          tags$p(
            "Use these outputs as design-screening diagnostics. They inherit the usual ",
            "FIM assumptions: asymptotic normality, local linearization around the design, ",
            "and ideal scaling of information with N."
          )
        ),

        documentation_section(
          "robust", "Robust Design",
          tags$p(
            "Robust design uses ", tags$code("$PRIOR NWPRI"), " and ",
            tags$code("$SIM TRUE=PRIOR"), " to evaluate or optimize sampling schedules ",
            "over many parameter sets drawn from the prior distribution."
          ),
          tags$p(
            "The app reports prior metadata when available and summarizes sampling-time ",
            "ranges across prior replications. Practical interpretation: choose times ",
            "within stable ranges, then re-evaluate the proposed design to check efficiency loss."
          )
        ),

        documentation_section(
          "references", "References",
          tags$ul(
            tags$li("Atkinson AC, Donev AN. Optimum Experimental Designs. 1992."),
            tags$li("Mentre F et al. Population pharmacokinetic and pharmacodynamic experiment design. 1997."),
            tags$li("Retout S et al. PFIM and power calculation references for FIM-based design."),
            tags$li("Mentre F, Rousseau A. Theoretical and practical aspects of population pharmacokinetic design. 2011."),
            tags$li("Aoki Y, Nordgren R, Hooker AC. Preconditioning of the variance-covariance matrix. AAPS J. 2016;18(2):505-515."),
            tags$li("Bauer RJ. NONMEM tutorial and design examples. 2021."),
            tags$li("Djokoto et al. Stochastic simulation-estimation approach in dose optimization. Research Square preprint. 2024."),
            tags$li("Fayette L, Brendel K, Mentre F. Advances and Further Comparison of Software Tools for FIM-Based Design Evaluation in Pharmacometrics. Pharm Res. 2026."),
            tags$li("Pantaleo et al. SSE threshold recommendations. 2026."),
            tags$li("PsN SSE User Guide v5.7.0.")
          )
        )
      )
    )
  )
}
