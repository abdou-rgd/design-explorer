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
  mod_txt <- paste(readLines(mod_file, warn = FALSE), collapse = "\n")
  app_txt <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(mod_txt, "tbl_no\\s*=\\s*reactive\\(NULL\\)")
  expect_match(mod_txt, "selected_tbl <- tbl_no\\(\\)")
  expect_match(app_txt, "tbl_no\\s*=\\s*tbl_no")
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
