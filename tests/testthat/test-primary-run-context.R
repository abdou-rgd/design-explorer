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

source(file.path(PROJECT_ROOT, "app", "R", "app_state.R"))

test_that("activating a primary source replaces every field atomically", {
  example <- new_primary_run_context(
    source = "example",
    source_id = "example5@1",
    ext_data = data.frame(marker = "example-ext"),
    tab_data = data.frame(marker = "example-tab"),
    ctl_lines = "example-ctl",
    summary_data = list(marker = "example-summary"),
    comparison = list(ext_data = data.frame(marker = "comparison")),
    table_no_range = c(1L, 4L),
    param_labels = c(THETA1 = "CL"),
    cmt_labels = c(`1` = "CENT"),
    groupsize = 32L
  )
  upload <- new_primary_run_context(
    source = "upload",
    source_id = "uploaded.ext",
    ext_data = data.frame(marker = "upload-ext")
  )

  active <- select_primary_run_context(
    "example",
    upload_context = upload,
    example_context = example
  )
  expect_identical(active, example)

  active <- select_primary_run_context(
    "upload",
    upload_context = upload,
    example_context = example
  )
  expect_identical(active, upload)
  expect_identical(active$ext_data$marker, "upload-ext")
  expect_null(active$tab_data)
  expect_null(active$ctl_lines)
  expect_null(active$summary_data)
  expect_null(active$comparison)
  expect_null(active$table_no_range)
  expect_null(active$param_labels)
  expect_null(active$cmt_labels)
  expect_identical(active$groupsize, 1L)
})

test_that("an example without comparison clears the previous comparison", {
  with_comparison <- new_primary_run_context(
    source = "example",
    source_id = "example5@1",
    ext_data = data.frame(marker = "five"),
    comparison = list(ext_data = data.frame(marker = "one"))
  )
  without_comparison <- new_primary_run_context(
    source = "example",
    source_id = "example1@2",
    ext_data = data.frame(marker = "one"),
    comparison = NULL
  )

  active <- with_comparison
  expect_false(is.null(active$comparison))
  active <- without_comparison
  expect_null(active$comparison)
  expect_identical(active$source_id, "example1@2")
})

test_that("empty and invalid primary contexts are explicit", {
  empty <- empty_primary_run_context()

  expect_s3_class(empty, "primary_run_context")
  expect_identical(empty$source, "none")
  expect_null(empty$ext_data)
  expect_null(empty$comparison)
  expect_identical(empty$groupsize, 1L)
  expect_identical(select_primary_run_context("none"), empty)

  upload <- new_primary_run_context(source = "upload")
  expect_error(
    select_primary_run_context("example", example_context = upload),
    "source does not match"
  )
})

test_that("a rejected source candidate preserves the committed context", {
  committed <- new_primary_run_context(
    source = "example",
    source_id = "example6@1",
    ext_data = data.frame(marker = "committed"),
    table_no_range = c(1L, 4L)
  )

  expect_identical(activate_primary_run_context(committed, NULL), committed)
  expect_error(
    activate_primary_run_context(committed, empty_primary_run_context()),
    "explicitly to reset"
  )
})

test_that("built-in example selection is atomic and revisioned", {
  skip_if_not_installed("shiny")
  suppressWarnings(suppressPackageStartupMessages(library(shiny)))
  source(file.path(PROJECT_ROOT, "app", "R", "mod_examples.R"))

  shiny::testServer(mod_examples_server, {
    session$setInputs(load_example5 = 1L)
    session$flushReact()
    example_five <- selection()

    expect_identical(example_five$ex_id, "example5")
    expect_false(is.null(example_five$compare_paths))

    session$setInputs(load_example1 = 1L)
    session$flushReact()
    example_one <- selection()

    expect_identical(example_one$ex_id, "example1")
    expect_null(example_one$compare_paths)
    expect_null(example_one$compare_name)
    expect_gt(example_one$revision, example_five$revision)

    session$setInputs(load_example1 = 2L)
    session$flushReact()
    reloaded <- selection()
    expect_gt(reloaded$revision, example_one$revision)
  })
})

test_that("uploads commit atomically and clear stale fields on success", {
  skip_if_not_installed("shiny")
  suppressWarnings(suppressPackageStartupMessages(library(shiny)))

  source(file.path(PROJECT_ROOT, "R", "source_core.R"))
  suppressWarnings(source_core(PROJECT_ROOT, local = globalenv()))
  source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"))
  source(file.path(PROJECT_ROOT, "app", "R", "mod_upload.R"))

  fixture_dir <- tempfile("upload-state-")
  dir.create(fixture_dir)
  on.exit(unlink(fixture_dir, recursive = TRUE), add = TRUE)

  ext_path <- file.path(
    PROJECT_ROOT,
    "app",
    "examples",
    "example1",
    "warfarin.ext"
  )
  invalid_ext_path <- file.path(fixture_dir, "invalid.ext")
  ctl_path <- file.path(fixture_dir, "second.ctl")
  writeLines("not a NONMEM ext file", invalid_ext_path)
  writeLines("$PROBLEM SECOND", ctl_path)
  expected_ext_lines <- readr::read_lines(ext_path, progress = FALSE)

  file_input <- function(name, path) {
    data.frame(
      name = name,
      size = file.info(path)$size,
      type = "text/plain",
      datapath = path,
      stringsAsFactors = FALSE
    )
  }

  shiny::testServer(mod_upload_server, {
    session$setInputs(upload = file_input("first.ext", ext_path))
    session$flushReact()
    expect_identical(ext_lines_raw(), expected_ext_lines)
    expect_false(is.null(ext_data()))

    committed_state <- upload_state()
    session$setInputs(upload = file_input("invalid.ext", invalid_ext_path))
    session$flushReact()

    expect_identical(upload_state(), committed_state)
    expect_identical(file_paths(), committed_state$file_paths)
    expect_identical(ext_data(), committed_state$ext_data)
    expect_identical(ext_lines_raw(), committed_state$ext_lines)

    session$setInputs(upload = file_input("second.ctl", ctl_path))
    session$flushReact()
    expect_null(file_paths()$ext)
    expect_null(ext_lines_raw())
    expect_identical(ctl_lines_raw(), "$PROBLEM SECOND")
  })
})
