# tests/testthat/test-mrgsolve_bridge.R

source(file.path(PROJECT_ROOT, "R", "design_utils.R"))
source(file.path(PROJECT_ROOT, "R", "design_metrics.R"))
source(file.path(PROJECT_ROOT, "R", "mrgsolve_bridge.R"))

test_that("extract_params_for_mrgsolve() keeps mapped THETAs numeric", {
  ext <- tibble::tibble(
    table_no = 1L,
    type = "final",
    ITERATION = -1000000000,
    THETA1 = 5,
    THETA2 = 50,
    `OMEGA(1,1)` = 0.1,
    `SIGMA(1,1)` = 0.01,
    OBJ = 123
  )
  labels <- c(THETA1 = "CL", THETA2 = "V")

  params <- extract_params_for_mrgsolve(ext, theta_labels = labels)

  expect_type(params$params$CL, "double")
  expect_type(params$params$V, "double")
  expect_equal(params$params$CL, 5)
  expect_equal(params$params$V, 50)
})

