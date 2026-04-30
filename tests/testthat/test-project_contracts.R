# tests/testthat/test-project_contracts.R

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

test_that("SSE validation server exposes and app passes selected tbl_no", {
  mod_file <- file.path(PROJECT_ROOT, "app", "R", "mod_sse_validation.R")
  app_file <- file.path(PROJECT_ROOT, "app", "app.R")
  power_file <- file.path(PROJECT_ROOT, "app", "R", "mod_power.R")
  mod_txt <- paste(readLines(mod_file, warn = FALSE), collapse = "\n")
  app_txt <- paste(readLines(app_file, warn = FALSE), collapse = "\n")
  power_txt <- paste(readLines(power_file, warn = FALSE), collapse = "\n")

  expect_match(mod_txt, "tbl_no\\s*=\\s*reactive\\(NULL\\)")
  expect_match(mod_txt, "selected_tbl <- tbl_no\\(\\)")
  expect_match(app_txt, "selectInput\\(\"global_table_no\", NULL")
  expect_match(app_txt, "selected_table_no <- reactiveVal\\(NULL\\)")
  expect_match(app_txt, "tbl_no\\s*=\\s*tbl_no")
  expect_match(power_txt, "tbl_no = reactive\\(NULL\\)")
  expect_false(grepl('selectInput\\(ns\\("table_no"\\)', power_txt))
})

test_that("upload UI only accepts archive formats that extract_design_files parses", {
  upload_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_upload.R"),
                                warn = FALSE), collapse = "\n")
  compare_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_compare.R"),
                                 warn = FALSE), collapse = "\n")

  expect_false(grepl('"\\.gz"', upload_txt, fixed = TRUE))
  expect_false(grepl('"\\.gz"', compare_txt, fixed = TRUE))
  expect_true(grepl(".tar.gz", upload_txt, fixed = TRUE))
  expect_true(grepl(".tgz", compare_txt, fixed = TRUE))
})

test_that("robust times plot has explicit days conversion contract", {
  times_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_times.R"),
                               warn = FALSE), collapse = "\n")

  expect_match(times_txt, "time_divisor <- if \\(identical\\(time_unit, \"days\"\\)\\) 24 else 1")
  expect_match(times_txt, "TIME_DISPLAY = TIME / time_divisor")
  expect_match(times_txt, "Time \\(days\\)")
})

test_that("architecture helpers centralize source order and shared app parsing", {
  source_core_txt <- paste(readLines(file.path(PROJECT_ROOT, "R", "source_core.R"),
                                     warn = FALSE), collapse = "\n")
  app_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "app.R"),
                             warn = FALSE), collapse = "\n")
  helpers_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"),
                                 warn = FALSE), collapse = "\n")

  expect_match(source_core_txt, "source_core <- function")
  expect_match(app_txt, "source_core\\(\"\\.\\.\"\\)")
  expect_match(helpers_txt, "parse_mapping_text <- function")
  expect_match(helpers_txt, "resolve_true_values <- function")
  expect_match(helpers_txt, "new_design_run <- function")
})

test_that("primary NONMEM ownership stays in upload, examples, and compare", {
  app_r_dir <- file.path(PROJECT_ROOT, "app", "R")
  app_files <- list.files(app_r_dir, pattern = "\\.R$", full.names = TRUE)
  txt <- stats::setNames(
    lapply(app_files, function(path) paste(readLines(path, warn = FALSE), collapse = "\n")),
    basename(app_files)
  )

  parser_pat <- paste0(
    "\\b(read_ext|read_shk|read_coi|read_clt|read_tab|read_cpu|",
    "read_prior_nwpri)\\s*\\("
  )
  parser_files <- names(txt)[vapply(txt, grepl, logical(1), pattern = parser_pat)]
  expect_setequal(parser_files, c("mod_compare.R", "mod_upload.R"))

  file_input_files <- names(txt)[vapply(txt, grepl, logical(1), pattern = "\\bfileInput\\s*\\(")]
  expect_setequal(file_input_files, c(
    "mod_compare.R", "mod_mrgsolve.R", "mod_sse_upload.R", "mod_upload.R"
  ))
})

test_that("SSE raw_results upload is centralized and injected into consumers", {
  app_file <- file.path(PROJECT_ROOT, "app", "app.R")
  app_txt <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  sse_files <- c(
    upload = file.path(PROJECT_ROOT, "app", "R", "mod_sse_upload.R"),
    validation = file.path(PROJECT_ROOT, "app", "R", "mod_sse_validation.R"),
    analysis = file.path(PROJECT_ROOT, "app", "R", "mod_sse_analysis.R"),
    comparison = file.path(PROJECT_ROOT, "app", "R", "mod_sse_comparison.R")
  )
  txt <- lapply(sse_files, function(path) paste(readLines(path, warn = FALSE), collapse = "\n"))

  expect_match(txt$upload, "\\bfileInput\\s*\\(ns\\(\"sse_a\"\\)")
  expect_match(txt$upload, "\\bread_sse_raw_all\\s*\\(")
  for (consumer in c("validation", "analysis", "comparison")) {
    expect_false(grepl("\\bfileInput\\s*\\(", txt[[consumer]]))
    expect_false(grepl("\\bread_sse_raw_all\\s*\\(", txt[[consumer]]))
  }

  expect_match(app_txt, "sse_a_data\\s*=\\s*sse_upload\\$sse_a_data")
  expect_match(app_txt, "sse_b_data\\s*=\\s*sse_upload\\$sse_b_data")
  expect_match(app_txt, "sse_a_shared\\s*=\\s*sse_upload\\$sse_a_data")
  expect_match(app_txt, "sse_orig\\s*=\\s*sse_upload\\$sse_a_data")
})

test_that("SSE consumers use injected control-stream derived true values", {
  sse_files <- c(
    file.path(PROJECT_ROOT, "app", "R", "mod_sse_validation.R"),
    file.path(PROJECT_ROOT, "app", "R", "mod_sse_comparison.R")
  )
  for (path in sse_files) {
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(txt, "resolve_true_values\\(shared_true_vals, shared_ctl_lines\\)")
    expect_false(grepl("\\bread_true_values\\s*\\(", txt))
  }

  analysis_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_sse_analysis.R"),
                                  warn = FALSE), collapse = "\n")
  expect_false(grepl("\\bread_true_values\\s*\\(", analysis_txt))
})

test_that("global TABLE NO selection is app-owned and injected", {
  app_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "app.R"),
                             warn = FALSE), collapse = "\n")
  power_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_power.R"),
                               warn = FALSE), collapse = "\n")
  validation_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_sse_validation.R"),
                                    warn = FALSE), collapse = "\n")

  expect_match(app_txt, "selectInput\\(\"global_table_no\", NULL")
  expect_match(app_txt, "selected_table_no <- reactiveVal\\(NULL\\)")
  expect_match(app_txt, "tbl_no <- reactive\\(selected_table_no\\(\\)\\)")
  expect_match(power_txt, "tbl_no = reactive\\(NULL\\)")
  expect_match(validation_txt, "tbl_no = reactive\\(NULL\\)")
  expect_false(grepl('selectInput\\(ns\\("table_no"\\)', power_txt))
  expect_false(grepl('selectInput\\(ns\\("table_no"\\)', validation_txt))
})

test_that("merged primary reactives prefer uploads over examples", {
  app_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "app.R"),
                             warn = FALSE), collapse = "\n")

  expect_match(app_txt, 'merged_ext\\s*<-\\s*reactive\\(\\{ if \\(upload_has\\("ext"\\)\\) upload\\$ext_data\\(\\) else example_ext\\(\\) \\}\\)')
  expect_match(app_txt, 'merged_shk\\s*<-\\s*reactive\\(\\{ if \\(upload_has\\("shk"\\)\\) upload\\$shk_data\\(\\) else example_shk\\(\\) \\}\\)')
  expect_match(app_txt, 'merged_ctl_lines\\s*<-\\s*reactive\\(\\{ if \\(upload_has\\("ctl"\\)\\) upload\\$ctl_lines\\(\\) else example_ctl_lines\\(\\) \\}\\)')
  expect_match(app_txt, 'merged_summary\\s*<-\\s*reactive\\(\\{ if \\(upload_has\\("tab"\\)\\) upload_summary\\(\\) else examples\\$summary_data\\(\\) \\}\\)')
})

test_that("universal reset reaches state owners", {
  app_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "app.R"),
                             warn = FALSE), collapse = "\n")
  compare_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_compare.R"),
                                 warn = FALSE), collapse = "\n")

  expect_match(app_txt, 'mod_upload_server\\("upload",\\s+reset_trigger = reset_trigger\\)')
  expect_match(app_txt, 'mod_compare_server\\("compare", reset_trigger = reset_trigger\\)')
  expect_match(app_txt, 'mod_examples_server\\("examples", reset_trigger = reset_trigger\\)')
  expect_match(app_txt, 'mod_sse_upload_server\\("sse_upload", reset_trigger = reset_trigger\\)')
  expect_match(app_txt, 'reset_trigger\\s*=\\s*reset_trigger')
  expect_match(compare_txt, "mod_compare_server <- function\\(id, reset_trigger = NULL\\)")
  expect_match(compare_txt, "observeEvent\\(reset_trigger\\(\\)")
  expect_match(compare_txt, "run_ids\\(character\\(\\)\\)")
})
