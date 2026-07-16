# tests/testthat/test-mrgsolve_bridge.R

library(testthat)

if (
  !exists("PROJECT_ROOT", inherits = TRUE) ||
    !file.exists(file.path(PROJECT_ROOT, "DESCRIPTION"))
) {
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
  full_hash <- mrgsolve_code_hash(code, n_chars = 32L)

  expect_type(hash_a, "character")
  expect_match(hash_a, "^[0-9a-f]{8}$")
  expect_match(full_hash, "^[0-9a-f]{32}$")
  expect_identical(hash_a, substr(full_hash, 1L, 8L))
  expect_identical(hash_a, hash_b)
  expect_false(identical(hash_a, hash_c))
})

test_that("manual dose configuration fills an unavailable RATE by arm", {
  schedule <- extract_dosing_from_tab(tibble::tibble(
    ID = c(1L, 2L),
    TIME = c(0, 0),
    EVID = c(1L, 1L)
  ))

  expect_true(all(is.na(schedule$amt)))
  expect_true(all(is.na(schedule$rate)))

  events <- build_dosing_events(
    schedule,
    dose_config = list(
      `1` = list(amt = 100, rate = 0),
      `2` = list(amt = 200, rate = 25)
    )
  )

  expect_equal(events$amt, c(100, 200))
  expect_equal(events$rate, c(0, 25))
})

test_that("valid table RATE values take priority over manual configuration", {
  schedule <- extract_dosing_from_tab(tibble::tibble(
    ID = c(1L, 2L, 3L),
    TIME = c(0, 0, 0),
    EVID = c(1L, 1L, 1L),
    RATE = c(0, 20, NA_real_)
  ))

  events <- build_dosing_events(
    schedule,
    dose_config = list(
      `1` = list(amt = 100, rate = 10),
      `2` = list(amt = 200, rate = 30),
      `3` = list(amt = 300, rate = 40)
    )
  )

  expect_equal(events$amt, c(100, 200, 300))
  expect_equal(events$rate, c(0, 20, 40))
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
