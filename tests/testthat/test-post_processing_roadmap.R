test_that("covariate effects compute log-ratio CI and relevance class", {
  eff <- compute_covariate_effect(
    effect = log(1.35),
    se = 0.03,
    transform = "log_ratio",
    margin = c(0.8, 1.25),
    ci = 0.90
  )

  expect_equal(eff$ratio, 1.35, tolerance = 1e-8)
  expect_true(eff$ci_lower > 1.0)
  expect_true(eff$ci_upper > 1.25)
  expect_equal(eff$decision, "relevant")
})

test_that("covariate effects classify non-relevant and inconclusive intervals", {
  expect_equal(classify_covariate_effect(0.92, 1.08), "non-relevant")
  expect_equal(classify_covariate_effect(0.74, 1.10), "inconclusive")
  expect_equal(classify_covariate_effect(0.70, 0.79), "relevant")
})

test_that("covariate effects table has stable column names after joining mapping and RSE", {
  mapping <- tibble::tibble(
    param = "THETA3",
    effect_label = "WT on CL",
    transform = "log_ratio"
  )
  rse <- tibble::tibble(
    param = "THETA3",
    estimate = log(1.4),
    se = 0.05,
    rse_pct = 15
  )

  out <- compute_covariate_effects_table(mapping, rse)

  expect_true(all(c("estimate", "se", "transform", "ci_lower", "ci_upper") %in% names(out)))
  expect_false(any(grepl("\\.\\.\\.", names(out))))
  expect_equal(out$transform, "log_ratio")
})

test_that("covariate dataset summaries collapse observation-level rows by subject", {
  dat <- tibble::tibble(
    ID = c(1, 1, 2, 2, 3, 3, 4, 4),
    TIME = c(0, 1, 0, 1, 0, 1, 0, 1),
    WT = c(50, 50, 60, 60, 70, 70, 80, 80),
    SEX = c(0, 0, 1, 1, 0, 0, 1, 1),
    DV = seq_len(8)
  )

  out <- summarise_covariate_dataset(
    dat,
    covariates = c("WT", "SEX"),
    id_col = "ID",
    time_col = "TIME",
    baseline_time = 0
  )

  expect_equal(out$n[out$covariate == "WT"], 4L)
  expect_equal(out$reference[out$covariate == "WT"], 65)
  expect_equal(out$type[out$covariate == "SEX"], "binary")
  expect_equal(out$value_high[out$covariate == "SEX"], 1)
})

test_that("covariate CSV reader handles NONMEM preamble line", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "",
    "INPUT FILE: FDATA",
    "ID,TIME,CLCR,WEIGHT,SEX,DV",
    "1,0,-0.2,-0.3,0,.",
    "2,0,0.4,0.5,1,."
  ), path)

  out <- read_covariate_dataset_csv(path)

  expect_equal(names(out), c("ID", "TIME", "CLCR", "WEIGHT", "SEX", "DV"))
  expect_equal(nrow(out), 2L)
})

test_that("covariate summaries match requested columns case-insensitively", {
  dat <- tibble::tibble(
    CID = c(1, 2, 3),
    Weight = c(-0.2, 0, 0.3),
    SEX = c(0, 1, 0)
  )

  out <- summarise_covariate_dataset(dat, covariates = c("WEIGHT", "SEX"), id_col = "ID")

  expect_setequal(out$covariate, c("Weight", "SEX"))
})

test_that("covariate contrasts build P90/P10 and binary high-vs-reference rows", {
  summary <- tibble::tibble(
    covariate = c("WT", "SEX"),
    type = c("continuous", "binary"),
    n = c(4L, 4L),
    reference = c(65, 0),
    value_low = c(53, 0),
    value_high = c(77, 1),
    low_label = c("P10", "0"),
    high_label = c("P90", "1")
  )
  mapping <- tibble::tibble(
    param = c("THETA3", "THETA4"),
    effect_label = c("WT on CL", "SEX on V"),
    covariate = c("WT", "SEX"),
    relationship = "Exp"
  )

  out <- build_covariate_contrasts(mapping, summary)

  expect_equal(nrow(out), 3L)
  expect_setequal(out$contrast_label[out$covariate == "WT"], c("P90 vs median", "P10 vs median"))
  expect_equal(out$contrast_label[out$covariate == "SEX"], "1 vs 0")
})

test_that("covariate contrast effects use exp(beta * covariate delta)", {
  mapping <- tibble::tibble(
    param = "THETA3",
    effect_label = "WT on CL",
    covariate = "WT",
    relationship = "Exp"
  )
  rse <- tibble::tibble(
    param = "THETA3",
    estimate = 1,
    se = 0.1,
    rse_pct = 10
  )
  contrasts <- tibble::tibble(
    param = "THETA3",
    effect_label = "WT on CL",
    covariate = "WT",
    relationship = "Exp",
    reference = 0,
    value = 0.2,
    contrast_label = "P90 vs median"
  )

  out <- compute_covariate_contrast_effects(
    mapping,
    rse,
    contrast_tbl = contrasts,
    margin = c(0.8, 1.25),
    ci = 0.90
  )

  expect_equal(out$ratio, exp(0.2), tolerance = 1e-8)
  expect_true(out$ci_lower > 1)
  expect_equal(out$decision, "inconclusive")
})

test_that("parameter families use NONMEM names and covariate mapping", {
  cov_map <- tibble::tibble(param = "THETA3", effect_label = "WT on CL")

  expect_equal(classify_parameter_family("THETA1"), "base")
  expect_equal(classify_parameter_family("THETA3", covariate_map = cov_map), "covariate")
  expect_equal(classify_parameter_family("THETA4", label = "SEX on V"), "covariate")
  expect_equal(classify_parameter_family("OMEGA(1,1)"), "iiv")
  expect_equal(classify_parameter_family("SIGMA(1,1)"), "residual")
  expect_equal(classify_parameter_family("KAPPA1"), "other")
})

test_that("precision summaries group FIM/SSE comparison by parameter family", {
  comp <- tibble::tibble(
    param = c("THETA1", "THETA2", "OMEGA(1,1)", "SIGMA(1,1)"),
    param_label = c("CL", "WT on CL", "IIV CL", "PROP"),
    rse_fim = c(20, 40, 60, 10),
    rse_sse = c(22, 50, 80, 12),
    ratio = c(0.91, 0.80, 0.75, 0.83),
    pass_20pct = c(TRUE, TRUE, FALSE, TRUE),
    status = rep("matched", 4)
  )
  cov_map <- tibble::tibble(param = "THETA2", effect_label = "WT on CL")

  out <- summarise_precision_by_family(comp, covariate_map = cov_map)

  expect_equal(out$n_params[out$parameter_family == "covariate"], 1L)
  expect_equal(out$median_rse_fim[out$parameter_family == "base"], 20)
  expect_equal(out$within_20_pct[out$parameter_family == "iiv"], 0)
})

test_that("design provenance extracts method flags and scores risk", {
  ctl <- c(
    "$PROBLEM demo",
    "$DESIGN APPROX=FO FIMDIAG=1 FIMTYPE=1 OFVTYPE=8 VARCROSS=1 MAXEVAL=0",
    "$PRIOR NWPRI"
  )

  prov <- parse_design_provenance(ctl)
  expect_equal(prov$approx, "FO")
  expect_equal(prov$fimdiag, 1L)
  expect_equal(prov$ofvtype, 8L)
  expect_true(prov$has_prior)
  expect_equal(prov$run_mode, "evaluation")

  risk <- score_fim_approximation_risk(prov, has_sse = FALSE)
  expect_equal(risk$level, "needs_validation")
  expect_true(grepl("Bayesian", risk$message))
})

test_that("identifiability directions report weakest eigenvector contributors", {
  mat <- matrix(
    c(1, 0.95, 0,
      0.95, 1, 0,
      0, 0, 1),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("CL", "V", "KA"), c("CL", "V", "KA"))
  )

  out <- compute_identifiability_directions(mat, top_n = 2)

  expect_equal(out$direction[1], "weakest")
  expect_equal(out$eigenvalue[1], 0.05, tolerance = 1e-8)
  expect_setequal(out$param[1:2], c("CL", "V"))
})

test_that("pediatric readiness classifies scenario metrics", {
  metrics <- tibble::tibble(
    scenario = c("dense", "sparse", "weak"),
    rse_sse = c(22, 28, 45),
    relative_bias = c(8, 26, 35),
    rmse_sse = c(18, 30, 55),
    failed_pct = c(2, 8, 25),
    n_patients = c(40, 20, 8),
    samples_per_patient = c(4, 2, 1)
  )

  out <- summarise_pediatric_scenarios(metrics)

  expect_equal(out$readiness, c("ready", "caution", "weak"))
  expect_equal(classify_pediatric_readiness(list(rse_sse = 20, relative_bias = 5, failed_pct = 0)), "ready")
})
