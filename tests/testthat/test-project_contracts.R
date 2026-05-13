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

test_that("SSE archive uploads support realistic PsN keep_tables zip sizes", {
  app_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "app.R"),
                             warn = FALSE), collapse = "\n")
  upload_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_sse_upload.R"),
                                warn = FALSE), collapse = "\n")

  expect_match(app_txt, "shiny\\.maxRequestSize")
  expect_match(app_txt, "100\\s*\\*\\s*1024\\s*\\*\\s*1024")
  expect_match(upload_txt, "100 MB")
  expect_match(upload_txt, "withProgress")
  expect_match(upload_txt, "upload-status-line--processing")
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

test_that("legacy metric card calls are absent from app modules", {
  app_r_dir <- file.path(PROJECT_ROOT, "app", "R")
  app_files <- list.files(app_r_dir, pattern = "\\.R$", full.names = TRUE)
  app_files <- app_files[basename(app_files) != "helpers_ui.R"]
  calls <- vapply(app_files, function(path) {
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    matches <- gregexpr("\\bmetric_card_v5\\s*\\(", txt, perl = TRUE)[[1]]
    if (length(matches) == 1L && matches[1] == -1L) 0L else length(matches)
  }, integer(1))
  calls <- calls[calls > 0L]

  expect_length(calls, 0L)
})

test_that("plot workspaces do not use legacy plot-card containers", {
  app_r_dir <- file.path(PROJECT_ROOT, "app", "R")
  app_files <- list.files(app_r_dir, pattern = "\\.R$", full.names = TRUE)
  offenders <- vapply(app_files, function(path) {
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    grepl('class\\s*=\\s*"plot-card"', txt)
  }, logical(1))

  expect_length(app_files[offenders], 0L)
})

test_that("power and mrgsolve panels use V7 workspace helpers", {
  checked_files <- file.path(PROJECT_ROOT, "app", "R",
                             c("mod_power.R", "mod_mrgsolve.R"))
  offenders <- vapply(checked_files, function(path) {
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    grepl('class\\s*=\\s*"surface-card"', txt)
  }, logical(1))

  expect_length(checked_files[offenders], 0L)
})

test_that("power settings keep GROUPSIZE and N what-if in one control bar", {
  power_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_power.R"),
                               warn = FALSE), collapse = "\n")

  expect_equal(length(gregexpr("\\bsettings_bar\\s*\\(", power_txt, perl = TRUE)[[1]]), 1L)
  expect_match(power_txt, "GROUPSIZE source")
  expect_match(power_txt, "N \\(what-if\\).*initialized from GROUPSIZE")
  expect_false(grepl("GROUPSIZE controls Power / NSN scaling", power_txt, fixed = TRUE))
})

test_that("power settings preserve GROUPSIZE across content rerenders", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  suppressWarnings(library(shiny))
  suppressWarnings(library(DT))

  source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"), local = TRUE)
  source(file.path(PROJECT_ROOT, "app", "R", "mod_power.R"), local = TRUE)

  ext_r <- shiny::reactiveVal(data.frame(
    table_no = 1L,
    type = "final",
    OBJ = 1,
    stringsAsFactors = FALSE
  ))

  shiny::testServer(
    mod_power_server,
    args = list(
      ext_data = ext_r,
      param_labels = shiny::reactive(NULL),
      tbl_no = shiny::reactive(1L),
      suggested_groupsize = shiny::reactive(1L),
      reset_trigger = shiny::reactiveVal(0L),
      all_runs = shiny::reactive(list())
    ),
    {
      invisible(output$content)
      session$setInputs(groupsize = 7L)
      session$flushReact()
      ext_r(data.frame(table_no = 1L, type = "final", OBJ = 2,
                       stringsAsFactors = FALSE))
      session$flushReact()

      html <- paste(as.character(output$content), collapse = "")

      expect_match(html, 'id="[^"]+-groupsize"[^>]+value="7"')
      expect_match(html, 'id="[^"]+-n_total"[^>]+value="7"')
    }
  )
})

test_that("power tab keeps long method education in documentation", {
  power_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_power.R"),
                               warn = FALSE), collapse = "\n")
  doc_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_documentation.R"),
                             warn = FALSE), collapse = "\n")

  expect_false(grepl("Ref.:", power_txt, fixed = TRUE))
  expect_false(grepl("What is the TOST test?", power_txt, fixed = TRUE))
  expect_false(grepl("How to read this table?", power_txt, fixed = TRUE))
  expect_match(power_txt, "Open Power/TOST documentation")
  expect_match(power_txt, "Directional H1: theta > H0")

  expect_match(doc_txt, "Wald test")
  expect_match(doc_txt, "N = N<sub>0</sub>")
  expect_match(doc_txt, "TOST")
  expect_match(doc_txt, "one-sided alpha")
  expect_match(doc_txt, "app-specific directional mode")
  expect_match(doc_txt, "PopED twoSided = FALSE")
})

test_that("SSE parameter filters stay compact and action-oriented", {
  helpers_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"),
                                 warn = FALSE), collapse = "\n")
  validation_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_sse_validation.R"),
                                    warn = FALSE), collapse = "\n")
  analysis_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "R", "mod_sse_analysis.R"),
                                  warn = FALSE), collapse = "\n")
  css_txt <- paste(readLines(file.path(PROJECT_ROOT, "app", "www", "styles.css"),
                             warn = FALSE), collapse = "\n")

  expect_match(helpers_txt, "compact_param_filter_ui <- function")
  expect_match(helpers_txt, "param-filter-summary")
  expect_match(helpers_txt, "Quick select")
  expect_match(validation_txt, "compact_param_filter_ui\\(")
  expect_match(validation_txt, "selected of")
  expect_match(analysis_txt, "compact_param_filter_ui\\(")
  expect_match(analysis_txt, "diagnostic_param_group")
  expect_match(analysis_txt, "updateCheckboxGroupInput\\(session, \"selected_params\"")
  expect_match(css_txt, "param-filter")
})
