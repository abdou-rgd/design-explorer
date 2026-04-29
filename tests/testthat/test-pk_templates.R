# tests/testthat/test-pk_templates.R
source(file.path(PROJECT_ROOT, "R", "pk_templates.R"))

test_that("pk_1cpt_iv matches mono-exponential closed form", {
  t <- c(0, 1, 4, 8, 24)
  out <- pk_1cpt_iv(times = t, dose = 100, rate = NULL, CL = 5, V = 50)
  expected <- (100 / 50) * exp(-(5 / 50) * t)
  expect_equal(out, expected, tolerance = 1e-8)
})

test_that("pk_1cpt_oral matches Bateman closed form", {
  t <- c(0.5, 1, 4, 8, 24)
  dose <- 100; CL <- 5; V <- 50; KA <- 1.5; F <- 1
  ke <- CL / V
  expected <- (F * dose * KA) / (V * (KA - ke)) * (exp(-ke * t) - exp(-KA * t))
  out <- pk_1cpt_oral(times = t, dose = dose, CL = CL, V = V, KA = KA, F = F)
  expect_equal(out, expected, tolerance = 1e-8)
})

test_that("pk_2cpt_iv returns positive concentrations and monotonic decay after early peak", {
  t <- c(0.1, 1, 4, 12, 48)
  out <- pk_2cpt_iv(times = t, dose = 100, rate = NULL,
                    CL = 5, V2 = 50, Q = 10, V3 = 100)
  expect_true(all(out > 0))
  expect_true(all(diff(out) < 0))  # already past peak at t=0.1 for IV bolus
})

test_that("multi-dose superposition equals sum of single-dose shifted curves", {
  t <- seq(0, 48, by = 0.5)
  CL <- 5; V <- 50
  single <- function(t) pk_1cpt_iv(times = t, dose = 100, rate = NULL, CL = CL, V = V)
  # Correct superposition: second dose contributes only for t >= 24
  manual <- single(t) + ifelse(t >= 24, single(t - 24), 0)
  auto   <- pk_1cpt_iv(times = t, dose = 100, rate = NULL, CL = CL, V = V,
                       dose_times = c(0, 24))
  expect_equal(auto, manual, tolerance = 1e-8)
})
