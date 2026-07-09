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

test_that("parse_dose_events accepts per-event amount, rate, and compartment overrides", {
  events <- parse_dose_events(
    "0:1800:1800:2; 672:1200:1200:2; 1344:1200",
    default_rate = 0,
    default_cmt = 3
  )

  expect_equal(names(events), c("TIME", "AMT", "RATE", "CMT"))
  expect_equal(events$TIME, c(0, 672, 1344))
  expect_equal(events$AMT, c(1800, 1200, 1200))
  expect_equal(events$RATE, c(1800, 1200, 0))
  expect_equal(events$CMT, c(2L, 2L, 3L))
  expect_error(parse_dose_events("0:1800;bad", default_rate = 0, default_cmt = 1), "Invalid dose event")
})

test_that("parse_sampling_events accepts optional observation dose annotations", {
  events <- parse_sampling_events("1:1800, 671.9:1200, 2015.9")

  expect_equal(names(events), c("TIME", "DOSE"))
  expect_equal(events$TIME, c(1, 671.9, 2015.9))
  expect_equal(events$DOSE, c(1800, 1200, NA_real_))
  expect_error(parse_sampling_events("1:100:bad"), "Invalid sampling event")
})

test_that("schedule table builder reproduces psm_eval-style per-design schedules", {
  schedule <- paste(
    "DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,DOSE_CMT,OBS_CMT,RATE",
    "1,0,\"0:1800:1800:2;672:1200:1200:2;1344:1200:1200:2;2016:1200:1200:2;2688:1200:1200:2;3360:1200:1200:2;4032:1200:1200:2\",\"1:1800,671.9:1200,2015.9:1200,3359.9:1200,3361:1200,3528:1200,3696:1200,4031.9:1200,4033:1200\",2,2,0",
    "2,1,\"0:1800:1800:1;336:1800:1800:1;672:1800:1800:1;1344:1800:1800:1;2016:1800:1800:1;2688:1800:1800:1;3360:1800:1800:1;4032:1800:1800:1\",\"335.9,671.9,2015.9,3359.9,3528,3696,3864,4031.9\",1,2,0",
    sep = "\n"
  )

  dat <- build_nonmem_dataset_from_schedule_table(schedule)

  expect_equal(names(dat), c(NONMEM_ELEMENTARY_COLUMNS, "DESIGN", "ARM"))
  expect_equal(nrow(dat), 32)
  expect_equal(sum(dat$ID == 1 & dat$EVID == 1), 7)
  expect_equal(sum(dat$ID == 1 & dat$EVID == 0), 9)
  expect_equal(sum(dat$ID == 2 & dat$EVID == 1), 8)
  expect_equal(sum(dat$ID == 2 & dat$EVID == 0), 8)

  id1_doses <- dat[dat$ID == 1 & dat$EVID == 1, ]
  expect_equal(id1_doses$TIME, c(0, 672, 1344, 2016, 2688, 3360, 4032))
  expect_equal(id1_doses$AMT, c(1800, rep(1200, 6)))
  expect_true(all(id1_doses$CMT == 2))

  id2_doses <- dat[dat$ID == 2 & dat$EVID == 1, ]
  expect_equal(id2_doses$TIME, c(0, 336, 672, 1344, 2016, 2688, 3360, 4032))
  expect_true(all(id2_doses$AMT == 1800))
  expect_true(all(id2_doses$CMT == 1))

  id1_obs <- dat[dat$ID == 1 & dat$EVID == 0, ]
  expect_equal(id1_obs$TIME, c(1, 671.9, 2015.9, 3359.9, 3361, 3528, 3696, 4031.9, 4033))
  expect_equal(id1_obs$DOSE, c(1800, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200))
  expect_true(all(id1_obs$ARM == "0"))
  expect_true(all(id2_doses$ARM == "1"))

  fixture_path <- file.path(PROJECT_ROOT, "docs", "results", "psm_eval", "psm_eval.csv")
  if (file.exists(fixture_path)) {
    fixture <- read.csv(fixture_path, stringsAsFactors = FALSE)
    expect_equal(as.data.frame(dat[names(fixture)]), fixture)
  }
})

test_that("schedule table builder preserves arbitrary metadata after core columns", {
  schedule <- paste(
    "DESIGN,COHORT,ARM,DOSE_EVENTS,SAMPLING_TIMES,OBS_CMT,SCENARIO",
    "1,adult,A,\"0:100\",\"1\",2,reference",
    sep = "\n"
  )

  dat <- build_nonmem_dataset_from_schedule_table(schedule)

  expect_equal(
    names(dat),
    c(NONMEM_ELEMENTARY_COLUMNS, "DESIGN", "COHORT", "ARM", "SCENARIO")
  )
  expect_true(all(dat$COHORT == "adult"))
  expect_true(all(dat$SCENARIO == "reference"))
})

test_that("schedule table validation separates errors from warnings", {
  invalid <- paste(
    "DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,DOSE_CMT,OBS_CMT",
    "1,A,\"0:0\",\"0,1\",1,2",
    sep = "\n"
  )
  invalid_validation <- validate_design_schedule_table(invalid)
  expect_false(invalid_validation$valid)
  expect_true(any(grepl("positive", invalid_validation$errors)))

  warning_schedule <- paste(
    "DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,DOSE_CMT,OBS_CMT",
    "1,A,\"0:100\",\"0,-1\",1,2",
    sep = "\n"
  )
  warning_validation <- validate_design_schedule_table(warning_schedule)
  expect_false(warning_validation$valid)
  expect_true(any(grepl("non-negative", warning_validation$errors)))

  near_dose <- paste(
    "DESIGN,ARM,DOSE_EVENTS,SAMPLING_TIMES,DOSE_CMT,OBS_CMT",
    "1,A,\"0:100;24:100\",\"0,1,24\",1,2",
    sep = "\n"
  )
  near_dose_validation <- validate_design_schedule_table(near_dose)
  expect_true(near_dose_validation$valid)
  expect_true(any(grepl("same time as a dose", near_dose_validation$warnings)))
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

  reordered <- valid[c("TIME", "ID", setdiff(names(valid), c("TIME", "ID")))]
  reordered_validation <- validate_nonmem_dataset(reordered)
  expect_false(reordered_validation$valid)
  expect_true(any(grepl("Core columns", reordered_validation$errors)))

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
  expect_true(grepl("n_designs", module_text, fixed = TRUE))
  expect_true(grepl("dose_events_", module_text, fixed = TRUE))
  expect_true(grepl("sampling_times_", module_text, fixed = TRUE))
  expect_true(grepl("load_psm_eval_example", module_text, fixed = TRUE))
  expect_false(grepl("textAreaInput(\n          ns(\"schedule_table\")", module_text, fixed = TRUE))
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
      n_designs = 2,
      design_1 = "1",
      arm_1 = "A",
      dose_events_1 = "0:100:100:1;24:100:100:1",
      sampling_times_1 = "1,2",
      dose_cmt_1 = 1,
      obs_cmt_1 = 2,
      rate_1 = 0,
      design_2 = "2",
      arm_2 = "B",
      dose_events_2 = "0:200:200:1",
      sampling_times_2 = "4,8,12",
      dose_cmt_2 = 1,
      obs_cmt_2 = 2,
      rate_2 = 0
    )
    session$setInputs(generate_preview = 1)

    dat <- current_dataset()
    expect_equal(names(dat), c(NONMEM_ELEMENTARY_COLUMNS, "DESIGN", "ARM"))
    expect_equal(nrow(dat), 8)
    expect_equal(unique(dat$ARM), c("A", "B"))
    expect_true(is_current())
    expect_true(can_download())
    expect_null(build_error())

    session$setInputs(sampling_times_2 = "4,8,12,16")
    expect_false(is_current())
    expect_null(current_dataset())
    expect_false(can_download())

    session$setInputs(
      dose_events_1 = "bad",
      generate_preview = 2
    )
    expect_null(dataset())
    expect_null(generated_signature())
    expect_null(current_dataset())
    expect_false(can_download())
    expect_match(build_error(), "Invalid dose event")

    expect_match(dataset_builder_psm_eval_example(), "4032:1800:1800:1")
  })
})
