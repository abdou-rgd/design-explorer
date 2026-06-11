# NONMEM Elementary Dataset Builder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a Shiny `Dataset Builder` tab that generates strict NONMEM-compatible CSV files for elementary `$DESIGN` evaluation datasets.

**Architecture:** Put all dataset generation, parsing, validation, and CSV writing in a pure R helper file so it can be tested without Shiny. Add a thin Shiny module that collects elementary design inputs, previews the generated tibble, shows validation feedback, and downloads the CSV. Wire the module under the existing `Design` navbar menu without touching upload/SSE ownership.

**Tech Stack:** R 4.2-compatible code, Shiny 1.7.1, bslib 0.3.1-compatible UI helpers, DT, tibble, dplyr, readr, testthat.

---

## File Structure

- Create `R/nonmem_dataset_builder.R`
  - Owns pure helpers:
    - `parse_sampling_times()`
    - `build_nonmem_elementary_dataset()`
    - `validate_nonmem_dataset()`
    - `write_nonmem_csv()`
  - Has no Shiny dependency.

- Modify `R/source_core.R`
  - Add `nonmem_dataset_builder.R` to the explicit source list.

- Create `app/R/mod_dataset_builder.R`
  - Owns Shiny UI/server only.
  - Calls pure helpers from `R/nonmem_dataset_builder.R`.
  - Uses existing UI helpers from `app/R/helpers_ui.R`.

- Modify `app/app.R`
  - Add `Dataset Builder` under the `Design` menu.
  - Call `mod_dataset_builder_server("dataset_builder", reset_trigger = reset_trigger)`.

- Create `tests/testthat/test-nonmem_dataset_builder.R`
  - Tests pure helper behavior and CSV round-trip.

- Modify `tests/run_tests.R`
  - Add the new test file to the standard runner.

---

### Task 1: Pure Helper Tests And Minimal Implementation

**Files:**
- Create: `tests/testthat/test-nonmem_dataset_builder.R`
- Create: `R/nonmem_dataset_builder.R`
- Modify: `R/source_core.R`
- Modify: `tests/run_tests.R`

- [ ] **Step 1: Write the failing helper tests**

Create `tests/testthat/test-nonmem_dataset_builder.R` with:

```r
project_root <- Sys.getenv("DESIGN_EXPLORER_ROOT", unset = NA_character_)
if (is.na(project_root) || !nzchar(project_root)) {
  this_file <- tryCatch(normalizePath(sys.frame(0)$ofile),
                        error = function(e) NULL)
  if (!is.null(this_file)) {
    d <- dirname(this_file)
    for (i in seq_len(6)) {
      if (file.exists(file.path(d, "CLAUDE.md"))) { project_root <- d; break }
      d <- dirname(d)
    }
  }
  if (is.na(project_root) || !nzchar(project_root)) project_root <- getwd()
}
PROJECT_ROOT <- project_root

source(file.path(PROJECT_ROOT, "R", "nonmem_dataset_builder.R"))

test_that("parse_sampling_times accepts common protocol schedule separators", {
  expect_equal(parse_sampling_times("1, 24; 168\n671.9 672"), c(1, 24, 168, 671.9, 672))
  expect_equal(parse_sampling_times("672, 1, 1, 24"), c(1, 24, 672))
  expect_error(parse_sampling_times("1, abc, 24"), "Invalid sampling time")
})

test_that("build_nonmem_elementary_dataset creates strict evaluation columns", {
  dat <- build_nonmem_elementary_dataset(
    n_prototypes = 2,
    dose = 1800,
    dose_interval = 672,
    n_administrations = 2,
    observation_times = c(1, 24, 671.9),
    dose_cmt = 2,
    observation_cmt = 2,
    rate = 0
  )

  expect_equal(names(dat), c("ID", "TIME", "DOSE", "AMT", "RATE", "DV", "MDV", "EVID", "CMT", "ARM"))
  expect_equal(unique(dat$ID), c(1, 2))
  expect_true(all(dat$ARM == dat$ID))

  dose_rows <- dat[dat$EVID == 1, , drop = FALSE]
  expect_equal(nrow(dose_rows), 4)
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

test_that("rows sort by ID, TIME, then dose before observation at the same time", {
  dat <- build_nonmem_elementary_dataset(
    n_prototypes = 1,
    dose = 100,
    dose_interval = 24,
    n_administrations = 2,
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
    dose_interval = 24,
    n_administrations = 1,
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
  bad_dose$AMT[bad_dose$EVID == 1][1] <- 0
  bad_dose_validation <- validate_nonmem_dataset(bad_dose)
  expect_false(bad_dose_validation$valid)
  expect_true(any(grepl("Dose rows", bad_dose_validation$errors)))

  bad_obs <- valid
  bad_obs$MDV[bad_obs$EVID == 0][1] <- 1
  bad_obs_validation <- validate_nonmem_dataset(bad_obs)
  expect_false(bad_obs_validation$valid)
  expect_true(any(grepl("Observation rows", bad_obs_validation$errors)))
})

test_that("write_nonmem_csv writes comma CSV with dot missing values", {
  dat <- build_nonmem_elementary_dataset(
    dose = 100,
    dose_interval = 24,
    n_administrations = 1,
    observation_times = c(1),
    dose_cmt = 1,
    observation_cmt = 2
  )
  dat$RATE[1] <- NA_real_

  tmp <- tempfile(fileext = ".csv")
  write_nonmem_csv(dat, tmp)
  raw <- readLines(tmp, warn = FALSE)

  expect_equal(raw[1], "ID,TIME,DOSE,AMT,RATE,DV,MDV,EVID,CMT,ARM")
  expect_true(any(grepl(",\\.,", raw, fixed = FALSE)))

  roundtrip <- readr::read_csv(tmp, na = ".", show_col_types = FALSE, progress = FALSE)
  expect_equal(names(roundtrip), names(dat))
  expect_true(is.na(roundtrip$RATE[1]))
})
```

- [ ] **Step 2: Run the new test to verify it fails**

Run:

```powershell
$script = @'
library(testthat)
PROJECT_ROOT <- normalizePath(".")
Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
testthat::test_file(file.path(PROJECT_ROOT, "tests", "testthat", "test-nonmem_dataset_builder.R"))
'@
Set-Content -Path tmp_run_nonmem_dataset_builder_tests.R -Value $script
Rscript tmp_run_nonmem_dataset_builder_tests.R
Remove-Item tmp_run_nonmem_dataset_builder_tests.R
```

Expected: FAIL or ERROR because `R/nonmem_dataset_builder.R` does not exist yet.

- [ ] **Step 3: Create the minimal pure implementation**

Create `R/nonmem_dataset_builder.R` with:

```r
# =============================================================================
# nonmem_dataset_builder.R -- strict NONMEM elementary-design CSV helpers
# =============================================================================

NONMEM_ELEMENTARY_COLUMNS <- c(
  "ID", "TIME", "DOSE", "AMT", "RATE", "DV", "MDV", "EVID", "CMT", "ARM"
)

parse_sampling_times <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(numeric())
  tokens <- unlist(strsplit(text, "[,;[:space:]]+", perl = TRUE))
  tokens <- tokens[nzchar(tokens)]
  out <- suppressWarnings(as.numeric(tokens))
  bad <- is.na(out) & !is.na(tokens)
  if (any(bad)) {
    stop("Invalid sampling time: ", tokens[which(bad)[1L]], call. = FALSE)
  }
  sort(unique(out))
}

build_nonmem_elementary_dataset <- function(n_prototypes = 1L,
                                            dose,
                                            dose_interval,
                                            n_administrations,
                                            observation_times,
                                            dose_cmt = 1L,
                                            observation_cmt = 1L,
                                            rate = 0,
                                            arm_values = NULL) {
  n_prototypes <- as.integer(n_prototypes)
  n_administrations <- as.integer(n_administrations)
  if (is.na(n_prototypes) || n_prototypes < 1L) {
    stop("n_prototypes must be at least 1.", call. = FALSE)
  }
  if (is.na(n_administrations) || n_administrations < 1L) {
    stop("n_administrations must be at least 1.", call. = FALSE)
  }
  if (!is.numeric(dose) || length(dose) != 1L || !is.finite(dose) || dose <= 0) {
    stop("dose must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(dose_interval) || length(dose_interval) != 1L ||
      !is.finite(dose_interval) || dose_interval < 0) {
    stop("dose_interval must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(observation_times) || length(observation_times) == 0L ||
      any(!is.finite(observation_times))) {
    stop("observation_times must contain at least one finite time.", call. = FALSE)
  }
  if (is.null(arm_values)) arm_values <- seq_len(n_prototypes)
  if (length(arm_values) != n_prototypes) {
    stop("arm_values length must equal n_prototypes.", call. = FALSE)
  }

  dose_times <- (seq_len(n_administrations) - 1L) * dose_interval
  rows <- vector("list", n_prototypes)

  for (id in seq_len(n_prototypes)) {
    dose_rows <- tibble::tibble(
      ID = id,
      TIME = dose_times,
      DOSE = dose,
      AMT = dose,
      RATE = rate,
      DV = 0,
      MDV = 1,
      EVID = 1,
      CMT = as.integer(dose_cmt),
      ARM = arm_values[id]
    )
    obs_rows <- tibble::tibble(
      ID = id,
      TIME = sort(unique(observation_times)),
      DOSE = dose,
      AMT = 0,
      RATE = 0,
      DV = 1,
      MDV = 0,
      EVID = 0,
      CMT = as.integer(observation_cmt),
      ARM = arm_values[id]
    )
    rows[[id]] <- dplyr::bind_rows(dose_rows, obs_rows)
  }

  dplyr::bind_rows(rows) |>
    dplyr::arrange(.data$ID, .data$TIME, dplyr::desc(.data$EVID)) |>
    dplyr::select(dplyr::all_of(NONMEM_ELEMENTARY_COLUMNS))
}

validate_nonmem_dataset <- function(dat) {
  errors <- character()
  if (is.null(dat) || !is.data.frame(dat)) {
    return(list(valid = FALSE, errors = "Dataset must be a data frame."))
  }

  missing_cols <- setdiff(NONMEM_ELEMENTARY_COLUMNS, names(dat))
  if (length(missing_cols) > 0L) {
    errors <- c(errors, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  if (!identical(names(dat), toupper(names(dat)))) {
    errors <- c(errors, "Column names must be uppercase.")
  }

  if (length(missing_cols) == 0L) {
    dose_rows <- dat[dat$EVID == 1, , drop = FALSE]
    obs_rows <- dat[dat$EVID == 0, , drop = FALSE]

    bad_dose <- nrow(dose_rows) == 0L ||
      any(dose_rows$MDV != 1 | dose_rows$DV != 0 | dose_rows$AMT <= 0 |
            dose_rows$DOSE <= 0, na.rm = TRUE)
    if (bad_dose) {
      errors <- c(errors, "Dose rows must have EVID=1, MDV=1, DV=0, AMT>0, and DOSE>0.")
    }

    bad_obs <- nrow(obs_rows) == 0L ||
      any(obs_rows$MDV != 0 | obs_rows$DV != 1 | obs_rows$AMT != 0 |
            obs_rows$RATE != 0, na.rm = TRUE)
    if (bad_obs) {
      errors <- c(errors, "Observation rows must have EVID=0, MDV=0, DV=1, AMT=0, and RATE=0.")
    }
  }

  list(valid = length(errors) == 0L, errors = errors)
}

write_nonmem_csv <- function(dat, file) {
  readr::write_csv(dat, file, na = ".")
}
```

- [ ] **Step 4: Add the helper to the shared source list**

Modify `R/source_core.R` so `core_files` includes `nonmem_dataset_builder.R` after `design_utils.R`:

```r
  core_files <- c(
    "design_utils.R",
    "nonmem_dataset_builder.R",
    "design_io.R",
```

- [ ] **Step 5: Add the new test file to the test runner**

Modify `tests/run_tests.R` by adding this line after the `source_core(PROJECT_ROOT)` block and before existing parser tests:

```r
run_testthat_file(file.path("tests", "testthat", "test-nonmem_dataset_builder.R"))
```

The beginning of the run list should look like:

```r
run_testthat_file(file.path("tests", "testthat", "test-nonmem_dataset_builder.R"))
run_testthat_file(file.path("tests", "testthat", "test-parse_design_outputs.R"))
```

- [ ] **Step 6: Run the helper test to verify it passes**

Run:

```powershell
$script = @'
library(testthat)
PROJECT_ROOT <- normalizePath(".")
Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
testthat::test_file(file.path(PROJECT_ROOT, "tests", "testthat", "test-nonmem_dataset_builder.R"))
'@
Set-Content -Path tmp_run_nonmem_dataset_builder_tests.R -Value $script
Rscript tmp_run_nonmem_dataset_builder_tests.R
Remove-Item tmp_run_nonmem_dataset_builder_tests.R
```

Expected: PASS with all tests in `test-nonmem_dataset_builder.R` green.

- [ ] **Step 7: Commit Task 1**

Run:

```powershell
git add R/nonmem_dataset_builder.R R/source_core.R tests/run_tests.R tests/testthat/test-nonmem_dataset_builder.R
git commit -m "Add NONMEM elementary dataset helpers"
```

---

### Task 2: Shiny Dataset Builder Module

**Files:**
- Create: `app/R/mod_dataset_builder.R`

- [ ] **Step 1: Write a lightweight module contract test**

Add this test to the end of `tests/testthat/test-nonmem_dataset_builder.R`:

```r
test_that("dataset builder Shiny module exposes UI and server entry points", {
  module_path <- file.path(PROJECT_ROOT, "app", "R", "mod_dataset_builder.R")
  expect_true(file.exists(module_path))
  txt <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
  expect_match(txt, "mod_dataset_builder_ui <- function\\(id\\)")
  expect_match(txt, "mod_dataset_builder_server <- function\\(id, reset_trigger = NULL\\)")
  expect_match(txt, "downloadHandler")
  expect_match(txt, "write_nonmem_csv")
  expect_match(txt, "DTOutput\\(ns\\(\"dataset_preview\"\\)\\)")
})
```

- [ ] **Step 2: Run the module contract test to verify it fails**

Run:

```powershell
$script = @'
library(testthat)
PROJECT_ROOT <- normalizePath(".")
Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
testthat::test_file(file.path(PROJECT_ROOT, "tests", "testthat", "test-nonmem_dataset_builder.R"))
'@
Set-Content -Path tmp_run_nonmem_dataset_builder_tests.R -Value $script
Rscript tmp_run_nonmem_dataset_builder_tests.R
Remove-Item tmp_run_nonmem_dataset_builder_tests.R
```

Expected: FAIL because `app/R/mod_dataset_builder.R` does not exist yet.

- [ ] **Step 3: Create the Shiny module**

Create `app/R/mod_dataset_builder.R` with:

```r
# =============================================================================
# mod_dataset_builder.R -- Elementary NONMEM dataset builder
# =============================================================================

mod_dataset_builder_ui <- function(id) {
  ns <- NS(id)
  page_shell(
    page_header(
      "Dataset Builder",
      "Create elementary-design CSV files for NONMEM $DESIGN evaluation.",
      eyebrow = "Design"
    ),
    page_section(
      "Elementary design settings",
      subtitle = "Each ID is a design prototype, not a real subject.",
      control_panel(
        numericInput(ns("n_prototypes"), "Number of prototypes", value = 1, min = 1, step = 1),
        numericInput(ns("dose"), "Dose", value = 1800, min = 0, step = 1),
        textInput(ns("dose_unit"), "Dose unit", value = "mg"),
        numericInput(ns("dose_interval"), "Dosing interval (hours)", value = 672, min = 0, step = 1),
        numericInput(ns("n_administrations"), "Number of administrations", value = 1, min = 1, step = 1),
        numericInput(ns("dose_cmt"), "Dose CMT", value = 1, min = 1, step = 1),
        numericInput(ns("observation_cmt"), "Observation CMT", value = 1, min = 1, step = 1),
        numericInput(ns("rate"), "Rate", value = 0, min = 0, step = 1)
      )
    ),
    page_section(
      "Sampling schedule",
      subtitle = "Paste fixed sampling times in hours. Commas, semicolons, spaces, and new lines are accepted.",
      control_panel(
        textAreaInput(
          ns("observation_times"),
          "Observation times",
          value = "1, 24, 168, 671.9",
          rows = 4,
          width = "100%"
        ),
        actionButton(ns("generate"), "Generate preview", icon = icon("table"))
      )
    ),
    uiOutput(ns("validation_banner")),
    uiOutput(ns("dataset_summary")),
    table_panel(
      "Generated NONMEM CSV preview",
      DTOutput(ns("dataset_preview")),
      actions = downloadButton(ns("download_csv"), "Download CSV", class = "btn-sm btn-default")
    )
  )
}

mod_dataset_builder_server <- function(id, reset_trigger = NULL) {
  moduleServer(id, function(input, output, session) {

    dataset <- reactive({
      input$generate
      isolate({
        obs_times <- parse_sampling_times(input$observation_times)
        build_nonmem_elementary_dataset(
          n_prototypes = input$n_prototypes,
          dose = input$dose,
          dose_interval = input$dose_interval,
          n_administrations = input$n_administrations,
          observation_times = obs_times,
          dose_cmt = input$dose_cmt,
          observation_cmt = input$observation_cmt,
          rate = input$rate
        )
      })
    })

    validation <- reactive({
      tryCatch(
        validate_nonmem_dataset(dataset()),
        error = function(e) list(valid = FALSE, errors = conditionMessage(e))
      )
    })

    output$validation_banner <- renderUI({
      v <- validation()
      if (isTRUE(v$valid)) {
        status_panel(
          "CSV structure ready",
          tags$p("Columns and dose/observation row conventions match the elementary evaluation contract."),
          tone = "success",
          icon_name = "check-circle"
        )
      } else {
        status_panel(
          "CSV structure needs attention",
          tags$ul(lapply(v$errors, tags$li)),
          tone = "warning",
          icon_name = "exclamation-triangle"
        )
      }
    })

    output$dataset_summary <- renderUI({
      dat <- dataset()
      n_dose <- sum(dat$EVID == 1)
      n_obs <- sum(dat$EVID == 0)
      status_panel(
        "Dataset summary",
        tags$p(sprintf(
          "%d prototype(s), %d dose row(s), %d observation row(s), %d total row(s).",
          dplyr::n_distinct(dat$ID), n_dose, n_obs, nrow(dat)
        )),
        tone = "neutral",
        icon_name = "database"
      )
    })

    output$dataset_preview <- DT::renderDT({
      dat <- dataset()
      DT::datatable(
        dat,
        rownames = FALSE,
        filter = "top",
        class = "stripe hover compact",
        options = list(pageLength = 20, scrollX = TRUE)
      )
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("nonmem_elementary_design_", Sys.Date(), ".csv"),
      content = function(file) {
        write_nonmem_csv(dataset(), file)
      }
    )

    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        updateNumericInput(session, "n_prototypes", value = 1)
        updateNumericInput(session, "dose", value = 1800)
        updateTextInput(session, "dose_unit", value = "mg")
        updateNumericInput(session, "dose_interval", value = 672)
        updateNumericInput(session, "n_administrations", value = 1)
        updateNumericInput(session, "dose_cmt", value = 1)
        updateNumericInput(session, "observation_cmt", value = 1)
        updateNumericInput(session, "rate", value = 0)
        updateTextAreaInput(session, "observation_times", value = "1, 24, 168, 671.9")
      }, ignoreInit = TRUE)
    }
  })
}
```

- [ ] **Step 4: Run the module contract test to verify it passes**

Run:

```powershell
$script = @'
library(testthat)
PROJECT_ROOT <- normalizePath(".")
Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
testthat::test_file(file.path(PROJECT_ROOT, "tests", "testthat", "test-nonmem_dataset_builder.R"))
'@
Set-Content -Path tmp_run_nonmem_dataset_builder_tests.R -Value $script
Rscript tmp_run_nonmem_dataset_builder_tests.R
Remove-Item tmp_run_nonmem_dataset_builder_tests.R
```

Expected: PASS.

- [ ] **Step 5: Commit Task 2**

Run:

```powershell
git add app/R/mod_dataset_builder.R tests/testthat/test-nonmem_dataset_builder.R
git commit -m "Add dataset builder Shiny module"
```

---

### Task 3: App Wiring

**Files:**
- Modify: `app/app.R`
- Modify: `tests/testthat/test-nonmem_dataset_builder.R`

- [ ] **Step 1: Write the app wiring test**

Add this test to `tests/testthat/test-nonmem_dataset_builder.R`:

```r
test_that("dataset builder is wired into the Design navbar menu", {
  app_path <- file.path(PROJECT_ROOT, "app", "app.R")
  app_txt <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_match(app_txt, 'tabPanel\\("Dataset Builder",\\s+value = "dataset_builder"')
  expect_match(app_txt, 'mod_dataset_builder_ui\\("dataset_builder"\\)')
  expect_match(app_txt, 'mod_dataset_builder_server\\("dataset_builder",\\s+reset_trigger = reset_trigger\\)')
})
```

- [ ] **Step 2: Run the app wiring test to verify it fails**

Run:

```powershell
$script = @'
library(testthat)
PROJECT_ROOT <- normalizePath(".")
Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
testthat::test_file(file.path(PROJECT_ROOT, "tests", "testthat", "test-nonmem_dataset_builder.R"))
'@
Set-Content -Path tmp_run_nonmem_dataset_builder_tests.R -Value $script
Rscript tmp_run_nonmem_dataset_builder_tests.R
Remove-Item tmp_run_nonmem_dataset_builder_tests.R
```

Expected: FAIL because `app/app.R` has not been wired yet.

- [ ] **Step 3: Add the tab under the Design menu**

Modify `app/app.R` in the `navbarMenu("Design", ...)` block from:

```r
  navbarMenu("Design",
    tabPanel("FIM Diagnostics", value = "fim",   mod_fim_ui("fim")),
    tabPanel("Sampling Times",  value = "times", mod_times_ui("times")),
    tabPanel("Robust Design",   value = "prior", mod_prior_ui("prior"))
  ),
```

to:

```r
  navbarMenu("Design",
    tabPanel("FIM Diagnostics", value = "fim",   mod_fim_ui("fim")),
    tabPanel("Sampling Times",  value = "times", mod_times_ui("times")),
    tabPanel("Robust Design",   value = "prior", mod_prior_ui("prior")),
    tabPanel("Dataset Builder", value = "dataset_builder", mod_dataset_builder_ui("dataset_builder"))
  ),
```

- [ ] **Step 4: Add the server call**

Modify `app/app.R` near the other design module server calls. After:

```r
  mod_prior_server("prior",
    summary_data = merged_summary, ctl_data = merged_ctl,
    all_runs = all_runs)
```

add:

```r
  mod_dataset_builder_server("dataset_builder",
    reset_trigger = reset_trigger)
```

- [ ] **Step 5: Run the app wiring test to verify it passes**

Run:

```powershell
$script = @'
library(testthat)
PROJECT_ROOT <- normalizePath(".")
Sys.setenv(DESIGN_EXPLORER_ROOT = PROJECT_ROOT)
testthat::test_file(file.path(PROJECT_ROOT, "tests", "testthat", "test-nonmem_dataset_builder.R"))
'@
Set-Content -Path tmp_run_nonmem_dataset_builder_tests.R -Value $script
Rscript tmp_run_nonmem_dataset_builder_tests.R
Remove-Item tmp_run_nonmem_dataset_builder_tests.R
```

Expected: PASS.

- [ ] **Step 6: Commit Task 3**

Run:

```powershell
git add app/app.R tests/testthat/test-nonmem_dataset_builder.R
git commit -m "Wire dataset builder tab into app"
```

---

### Task 4: Full Verification And Visual Smoke Test

**Files:**
- No planned source edits. Fix only if verification exposes defects.

- [ ] **Step 1: Run the full test suite**

Run:

```powershell
Rscript tests/run_tests.R
```

Expected: all testthat files pass. Local SSE smoke tests may skip if ignored `docs/results` fixtures are absent; skips are acceptable only when the runner explicitly reports missing local fixtures.

- [ ] **Step 2: Source the Shiny app to catch startup errors**

Run:

```powershell
$script = @'
setwd("app")
source("app.R")
'@
Set-Content -Path tmp_source_app.R -Value $script
Rscript tmp_source_app.R
Remove-Item tmp_source_app.R
```

Expected: no parse/source error. The command may keep a Shiny app object alive briefly; interrupt if it starts serving interactively after source succeeds.

- [ ] **Step 3: Launch the app locally for browser verification**

Run:

```powershell
$script = @'
shiny::runApp("app", host = "127.0.0.1", port = 3838, launch.browser = FALSE)
'@
Set-Content -Path tmp_run_app.R -Value $script
Rscript tmp_run_app.R
```

Expected: app listens on `http://127.0.0.1:3838`.

- [ ] **Step 4: Open the app and inspect the new tab**

Use the Browser plugin or a manual browser to open:

```text
http://127.0.0.1:3838
```

Verify:

- `Design > Dataset Builder` is visible.
- Default preview appears after clicking `Generate preview`.
- Preview columns are exactly `ID,TIME,DOSE,AMT,RATE,DV,MDV,EVID,CMT,ARM`.
- Validation banner reports ready state.
- Download button produces a `.csv` file.

- [ ] **Step 5: Stop the Shiny app**

Stop the app process started in Step 3.
Then remove the temporary runner:

```powershell
Remove-Item tmp_run_app.R
```

- [ ] **Step 6: Commit any verification fixes**

If Step 1-4 required fixes, commit them:

```powershell
git add R/nonmem_dataset_builder.R R/source_core.R app/R/mod_dataset_builder.R app/app.R tests/run_tests.R tests/testthat/test-nonmem_dataset_builder.R
git commit -m "Fix dataset builder verification issues"
```

If no fixes were needed, do not create an empty commit.

---

## Self-Review Checklist

- Spec coverage:
  - Elementary design only: Task 1 helper API and Task 2 UI use `n_prototypes`, not real subject expansion.
  - Evaluation only: no `TSTRAT/TMIN/TMAX` or `DSTRAT/DMIN/DMAX` in columns or UI.
  - Strict CSV: Task 1 tests and `write_nonmem_csv()` use `readr::write_csv(na = ".")`.
  - Standalone tab: Task 2 and Task 3 do not depend on loaded NONMEM outputs.
  - Validation feedback: Task 1 `validate_nonmem_dataset()` and Task 2 `validation_banner`.
  - Tests: Task 1-3 add helper, module contract, and app wiring tests.

- Placeholder scan:
  - No placeholder markers or unspecified validation steps remain in this plan.

- Type consistency:
  - Function names are consistent: `parse_sampling_times()`, `build_nonmem_elementary_dataset()`, `validate_nonmem_dataset()`, `write_nonmem_csv()`.
  - Module names are consistent: `mod_dataset_builder_ui()` and `mod_dataset_builder_server()`.
  - Output id is consistently `dataset_preview`.
