# tests/testthat/test-mrgsolve_bridge.R

library(testthat)

if (!exists("PROJECT_ROOT", inherits = TRUE)) {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  PROJECT_ROOT <- if (file.exists(file.path(cwd, "DESCRIPTION"))) {
    cwd
  } else {
    normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
  }
  Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
}

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

test_that("mrgsolve_code_hash() is deterministic without extra packages", {
  code <- "$PARAM CL = 1\n$CMT CENT\n$ODE dxdt_CENT = -CL * CENT;"

  hash_a <- mrgsolve_code_hash(code)
  hash_b <- mrgsolve_code_hash(code)
  hash_c <- mrgsolve_code_hash(paste0(code, "\n"))

  expect_type(hash_a, "character")
  expect_match(hash_a, "^[0-9a-f]{8}$")
  expect_identical(hash_a, hash_b)
  expect_false(identical(hash_a, hash_c))
})

test_that("simulate_pk_profile() coerces mrgsims output without attaching mrgsolve", {
  skip_if_not_installed("mrgsolve")

  code <- "
$PARAM CL = 1, V = 10
$CMT CENT
$ODE
dxdt_CENT = -(CL / V) * CENT;
$TABLE
double CP = CENT / V;
$CAPTURE CP
"
  mod <- compile_mrgsolve_model(code, model_name = "bridge_sim_test")
  skip_if(is.character(mod), mod)

  events <- data.frame(
    ID = 1L,
    time = 0,
    amt = 100,
    cmt = 1L,
    evid = 1L,
    rate = 0,
    stringsAsFactors = FALSE
  )

  sim <- simulate_pk_profile(
    mod = mod,
    params = c(CL = 1, V = 10),
    events = events,
    end_time = 12,
    delta = 1
  )

  expect_s3_class(sim, "tbl_df")
  expect_gt(nrow(sim), 1L)
  expect_true(any(sim$IPRED > 0, na.rm = TRUE))
})
