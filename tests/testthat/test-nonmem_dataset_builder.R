project_root <- Sys.getenv("DESIGN_EXPLORER_ROOT", unset = NA_character_)
if (is.na(project_root) || !nzchar(project_root)) {
  find_project_root <- function(start) {
    if (is.null(start) || is.na(start) || !nzchar(start)) {
      return(NA_character_)
    }
    d <- if (dir.exists(start)) start else dirname(start)
    for (i in seq_len(6)) {
      if (file.exists(file.path(d, "DESCRIPTION"))) {
        return(d)
      }
      parent <- dirname(d)
      if (identical(parent, d)) {
        break
      }
      d <- parent
    }
    NA_character_
  }

  this_file <- tryCatch(
    normalizePath(sys.frame(0)$ofile),
    error = function(e) NULL
  )
  project_root <- find_project_root(this_file)
  if (is.na(project_root) || !nzchar(project_root)) {
    project_root <- find_project_root(getwd())
  }
  if (is.na(project_root) || !nzchar(project_root)) {
    project_root <- getwd()
  }
}
PROJECT_ROOT <- project_root

source(file.path(PROJECT_ROOT, "R", "nonmem_dataset_builder.R"))

test_that("parse_schedule_times accepts common protocol schedule separators", {
  expect_equal(
    parse_schedule_times("1, 24; 168\n671.9 672", "sampling time"),
    c(1, 24, 168, 671.9, 672)
  )
  expect_equal(parse_schedule_times("672, 1, 1, 24", "sampling time"), c(1, 24, 672))
  expect_equal(parse_schedule_times("", "sampling time"), numeric())
  expect_error(
    parse_schedule_times("0, abc, 24", "dose time"),
    "Invalid dose time"
  )
})

test_that("build_nonmem_elementary_dataset creates strict evaluation columns", {
  dat <- build_nonmem_elementary_dataset(
    n_elementary_designs = 2,
    dose = 1800,
    dose_times = c(0, 336, 672),
    observation_times = c(1, 24, 671.9),
    dose_cmt = 2,
    observation_cmt = 2,
    rate = 0,
    optional_column = "GROUP"
  )

  expect_equal(
    names(dat),
    c("ID", "TIME", "DOSE", "AMT", "RATE", "DV", "MDV", "EVID", "CMT", "GROUP")
  )
  expect_equal(unique(dat$ID), c(1, 2))
  expect_true(all(is.na(dat$GROUP)))

  dose_rows <- dat[dat$EVID == 1, , drop = FALSE]
  expect_equal(nrow(dose_rows), 6)
  expect_equal(unique(dose_rows$TIME), c(0, 336, 672))
  expect_true(all(dose_rows$MDV == 1))
  expect_true(all(dose_rows$DV == 0))
  expect_true(all(dose_rows$AMT == 1800))
  expect_true(all(dose_rows$DOSE == 1800))
  expect_true(all(dose_rows$CMT == 2))

  obs_rows <- dat[dat$EVID == 0, , drop = FALSE]
  expect_equal(nrow(obs_rows), 6)
  expect_true(all(obs_rows$MDV == 0))
  expect_true(all(obs_rows$DV == 1))
  expect_true(all(obs_rows$AMT == 0))
  expect_true(all(obs_rows$RATE == 0))
  expect_true(all(obs_rows$CMT == 2))
})

test_that("build_nonmem_elementary_dataset rejects fractional row counts and negative times", {
  expect_error(
    build_nonmem_elementary_dataset(
      n_elementary_designs = 1.9,
      dose = 100,
      dose_times = 0,
      observation_times = c(1)
    ),
    "n_elementary_designs"
  )
  expect_error(
    build_nonmem_elementary_dataset(
      dose = 100,
      dose_times = c(-1, 24),
      observation_times = c(1)
    ),
    "dose_times"
  )
  expect_error(
    build_nonmem_elementary_dataset(
      dose = 100,
      dose_times = 0,
      observation_times = c(-1, 1)
    ),
    "observation_times"
  )
})

test_that("rows sort by ID, TIME, then dose before observation at the same time", {
  dat <- build_nonmem_elementary_dataset(
    n_elementary_designs = 1,
    dose = 100,
    dose_times = c(0, 24),
    observation_times = c(0, 24),
    dose_cmt = 1,
    observation_cmt = 2
  )

  expect_equal(dat$TIME, c(0, 0, 24, 24))
  expect_equal(dat$EVID, c(1, 0, 1, 0))
})

test_that("validate_nonmem_dataset reports schema and row problems", {
  valid <- build_nonmem_elementary_dataset(
    dose = 100,
    dose_times = 0,
    observation_times = c(1, 2),
    dose_cmt = 1,
    observation_cmt = 2
  )
  validation <- validate_nonmem_dataset(valid)
  expect_true(validation$valid)
  expect_equal(validation$errors, character())

  lower <- valid
  names(lower)[1] <- "id"
  lower_validation <- validate_nonmem_dataset(lower)
  expect_false(lower_validation$valid)
  expect_true(any(grepl("uppercase", lower_validation$errors)))

  bad_dose <- valid
  bad_dose$AMT[which(bad_dose$EVID == 1)[1]] <- 0
  bad_dose_validation <- validate_nonmem_dataset(bad_dose)
  expect_false(bad_dose_validation$valid)
  expect_true(any(grepl("Dose rows", bad_dose_validation$errors)))

  bad_obs <- valid
  bad_obs$MDV[which(bad_obs$EVID == 0)[1]] <- 1
  bad_obs_validation <- validate_nonmem_dataset(bad_obs)
  expect_false(bad_obs_validation$valid)
  expect_true(any(grepl("Observation rows", bad_obs_validation$errors)))

  bad_evid <- valid
  bad_evid$EVID[which(bad_evid$EVID == 0)[1]] <- 2
  bad_evid_validation <- validate_nonmem_dataset(bad_evid)
  expect_false(bad_evid_validation$valid)
  expect_true(any(grepl("EVID", bad_evid_validation$errors)))

  bad_time <- valid
  bad_time$TIME[1] <- NA_real_
  bad_time_validation <- validate_nonmem_dataset(bad_time)
  expect_false(bad_time_validation$valid)
  expect_true(any(grepl("TIME", bad_time_validation$errors)))

  bad_cmt <- valid
  bad_cmt$CMT[which(bad_cmt$EVID == 0)[1]] <- 0
  bad_cmt_validation <- validate_nonmem_dataset(bad_cmt)
  expect_false(bad_cmt_validation$valid)
  expect_true(any(grepl("CMT", bad_cmt_validation$errors)))
})

test_that("build_nonmem_elementary_dataset rejects invalid compartments", {
  expect_error(
    build_nonmem_elementary_dataset(
      dose = 100,
      dose_times = 0,
      observation_times = c(1),
      dose_cmt = 0,
      observation_cmt = 1
    ),
    "dose_cmt"
  )
  expect_error(
    build_nonmem_elementary_dataset(
      dose = 100,
      dose_times = 0,
      observation_times = c(1),
      dose_cmt = 1,
      observation_cmt = 1.5
    ),
    "observation_cmt"
  )
})

test_that("write_nonmem_csv writes comma CSV with dot missing values", {
  dat <- build_nonmem_elementary_dataset(
    dose = 100,
    dose_times = 0,
    observation_times = c(1),
    dose_cmt = 1,
    observation_cmt = 2
  )
  dat$RATE[1] <- NA_real_

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write_nonmem_csv(dat, tmp)
  raw <- readLines(tmp, warn = FALSE)

  expect_equal(raw[1], "ID,TIME,DOSE,AMT,RATE,DV,MDV,EVID,CMT")
  expect_true(any(grepl(",\\.,", raw, fixed = FALSE)))

  roundtrip <- readr::read_csv(
    tmp,
    na = ".",
    show_col_types = FALSE,
    progress = FALSE
  )
  expect_equal(names(roundtrip), names(dat))
  expect_true(is.na(roundtrip$RATE[1]))
})

test_that("dataset builder Shiny module contract is present", {
  module_path <- file.path(PROJECT_ROOT, "app", "R", "mod_dataset_builder.R")

  expect_true(file.exists(module_path))
  if (!file.exists(module_path)) {
    return(invisible())
  }

  module_source <- readLines(module_path, warn = FALSE)
  module_text <- paste(module_source, collapse = "\n")

  expect_true(grepl("mod_dataset_builder_ui <- function(id)", module_text, fixed = TRUE))
  expect_true(grepl("mod_dataset_builder_server <- function(id, reset_trigger = NULL)", module_text, fixed = TRUE))
  expect_true(grepl("downloadHandler", module_text, fixed = TRUE))
  expect_true(grepl("write_nonmem_csv", module_text, fixed = TRUE))
  expect_true(grepl("DTOutput(ns(\"dataset_preview\"))", module_text, fixed = TRUE))
  expect_true(grepl("Elementary designs", module_text, fixed = TRUE))
  expect_false(grepl("Prototypes", module_text, fixed = TRUE))
  expect_false(grepl("Dose interval", module_text, fixed = TRUE))
})

test_that("dataset builder is wired into the Shiny app", {
  app_path <- file.path(PROJECT_ROOT, "app", "app.R")

  expect_true(file.exists(app_path))
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl(
    "tabPanel(\"Dataset Builder\", value = \"dataset_builder\"",
    app_text,
    fixed = TRUE
  ))
  expect_true(grepl(
    "mod_dataset_builder_ui(\"dataset_builder\")",
    app_text,
    fixed = TRUE
  ))
  expect_true(grepl(
    "mod_dataset_builder_server(\"dataset_builder\", reset_trigger = reset_trigger)",
    app_text,
    fixed = TRUE
  ))
})

test_that("dataset builder Shiny module tracks generation state", {
  module_path <- file.path(PROJECT_ROOT, "app", "R", "mod_dataset_builder.R")
  module_source <- readLines(module_path, warn = FALSE)
  module_text <- paste(module_source, collapse = "\n")

  expect_false(grepl("circle-info", module_text, fixed = TRUE))

  suppressWarnings(library(shiny))
  suppressWarnings(library(DT))
  source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"))
  source(module_path)

  shiny::testServer(mod_dataset_builder_server, {
    expect_null(dataset())
    expect_null(current_dataset())
    expect_false(can_download())
    expect_false(is_current())

    session$setInputs(
      n_elementary_designs = 2,
      dose = 100,
      dose_unit = "mg",
      dose_times = "0, 12, 24",
      dose_cmt = 1,
      observation_cmt = 2,
      rate = 0,
      sampling_times = "1, 2",
      optional_column = "Cohort"
    )
    session$setInputs(generate_preview = 1)

    dat <- current_dataset()
    expect_equal(names(dat), c(NONMEM_ELEMENTARY_COLUMNS, "COHORT"))
    expect_equal(nrow(dat), 10)
    expect_true(all(is.na(dat$COHORT)))
    expect_true(is_current())
    expect_true(can_download())
    expect_null(build_error())

    session$setInputs(dose = 200)
    expect_false(is_current())
    expect_null(current_dataset())
    expect_false(can_download())

    session$setInputs(dose = 100, sampling_times = "1, abc", generate_preview = 2)
    expect_null(dataset())
    expect_null(generated_signature())
    expect_null(current_dataset())
    expect_false(can_download())
    expect_match(build_error(), "Invalid sampling time")
  })
})
