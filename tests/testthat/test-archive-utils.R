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

source(file.path(PROJECT_ROOT, "R", "archive_utils.R"))
source(file.path(
  PROJECT_ROOT,
  "tests",
  "testthat",
  "helper-archive-fixtures.R"
))

archive_error_message <- function(expr) {
  error <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  if (is.null(error)) "" else conditionMessage(error)
}

test_that("archive member paths are normalized and bounded", {
  members <- .validate_archive_members(c(
    "./nested\\model.ext",
    "results/"
  ))

  expect_equal(members$normalized, c("nested/model.ext", "results"))
  expect_equal(members$is_directory, c(FALSE, TRUE))
  expect_match(
    archive_error_message(.validate_archive_members("/tmp/model.ext")),
    "absolute member path"
  )
  expect_match(
    archive_error_message(.validate_archive_members("C:\\tmp\\model.ext")),
    "drive-qualified member path"
  )
  expect_match(
    archive_error_message(.validate_archive_members("safe/../model.ext")),
    "escapes the extraction root"
  )
  expect_match(
    archive_error_message(.validate_archive_members(c(
      "nested/model.ext",
      "NESTED\\MODEL.EXT"
    ))),
    "duplicate member paths"
  )
  expect_match(
    archive_error_message(.validate_archive_members("nested/model.ext.")),
    "ending in a dot or space"
  )
  expect_match(
    archive_error_message(.validate_archive_members("nested/CON.ext")),
    "reserved Windows device name"
  )
  expect_match(
    archive_error_message(.validate_archive_members(
      c("one.ext", "two.ext"),
      max_entries = 1L
    )),
    "too many members"
  )
})

test_that("safe ZIP extraction selects allowed patab members before writing", {
  zip_path <- withr::local_tempfile(fileext = ".zip")
  write_stored_zip(
    list(
      "nested/pk.tab-1" = "patab payload\n",
      "ignored.txt" = "ignored payload\n"
    ),
    zip_path
  )

  output_dir <- withr::local_tempdir(pattern = "archive-zip-output-")
  extracted <- safe_extract_zip(
    zip_path,
    exdir = output_dir,
    pattern = .PATAB_ARCHIVE_PATTERN
  )

  expect_equal(names(extracted), "nested/pk.tab-1")
  expect_equal(basename(extracted), "pk.tab-1")
  expect_equal(readLines(extracted, warn = FALSE), "patab payload")
  expect_length(
    list.files(output_dir, recursive = TRUE, full.names = TRUE),
    1L
  )

  limited_output <- file.path(output_dir, "limited")
  message <- archive_error_message(safe_extract_zip(
    zip_path,
    exdir = limited_output,
    pattern = .PATAB_ARCHIVE_PATTERN,
    max_uncompressed_bytes = 1
  ))
  expect_match(message, "above the 1-byte limit")
  expect_equal(dir.exists(limited_output), FALSE)
})

test_that("safe TAR extraction streams only regular design files", {
  source_dir <- withr::local_tempdir(pattern = "archive-tar-source-")
  dir.create(file.path(source_dir, "nested"))
  writeLines("design payload", file.path(source_dir, "nested", "run.ext"))
  writeLines("ignored payload", file.path(source_dir, "ignored.txt"))
  tar_path <- withr::local_tempfile(fileext = ".tar.gz")
  withr::local_dir(source_dir)
  utils::tar(
    tarfile = tar_path,
    files = c("nested/run.ext", "ignored.txt"),
    compression = "gzip",
    tar = "internal"
  )

  output_dir <- withr::local_tempdir(pattern = "archive-tar-output-")
  extracted <- safe_extract_tar(
    tar_path,
    exdir = output_dir,
    pattern = .DESIGN_ARCHIVE_PATTERN
  )

  expect_equal(names(extracted), "nested/run.ext")
  expect_equal(basename(extracted), "run.ext")
  expect_equal(readLines(extracted, warn = FALSE), "design payload")
  expect_length(
    list.files(output_dir, recursive = TRUE, full.names = TRUE),
    1L
  )

  limited_output <- file.path(output_dir, "limited")
  message <- archive_error_message(safe_extract_tar(
    tar_path,
    exdir = limited_output,
    pattern = .DESIGN_ARCHIVE_PATTERN,
    max_uncompressed_bytes = 1
  ))
  expect_match(message, "expands above the 1-byte limit")
  expect_equal(dir.exists(limited_output), FALSE)
})

test_that("PAX metadata parsing is linear and bounded", {
  repeated_records <- charToRaw(paste(rep("5 a=\n", 5000L), collapse = ""))
  parsed <- .tar_parse_pax(repeated_records, max_records = 5000L)

  expect_identical(parsed$a, "")
  expect_identical(attr(parsed, "record_count", exact = TRUE), 5000L)

  excessive_records <- charToRaw(paste(rep("5 a=\n", 1001L), collapse = ""))
  expect_error(
    .tar_parse_pax(excessive_records, max_records = 1000L),
    "too many records"
  )
})

test_that("global PAX headers are rejected instead of silently reinterpreted", {
  tar_path <- withr::local_tempfile(fileext = ".tar.gz")
  write_global_pax_tar(tar_path)
  output_dir <- paste0(tar_path, "-output")

  expect_error(
    safe_extract_tar(
      tar_path,
      exdir = output_dir,
      pattern = .DESIGN_ARCHIVE_PATTERN
    ),
    "global pax headers are not supported"
  )
  expect_false(dir.exists(output_dir))
})
