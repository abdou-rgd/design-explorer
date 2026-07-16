# tests/testthat/test-sse_design_pk_association.R

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

source(file.path(PROJECT_ROOT, "R", "source_core.R"))
suppressWarnings(source_core(PROJECT_ROOT, local = globalenv()))
source(file.path(PROJECT_ROOT, "app", "R", "helpers_ui.R"))
source(file.path(PROJECT_ROOT, "app", "R", "mod_sse_upload.R"))
source(file.path(PROJECT_ROOT, "app", "R", "mod_sse_analysis.R"))

make_design_patab <- function(design) {
  tibble::tibble(
    table = "pk_individuals.tab",
    sample = 1L,
    kind = "estimation",
    source_file = paste0("pk_individuals.tab-1-", design),
    ID = 101L,
    CL = if (identical(design, "a")) 1 else 2,
    design_marker = design
  )
}

make_design_sse <- function(design = "a") {
  tibble::tibble(
    sample = 1L,
    minimization_successful = 1L,
    converged = TRUE,
    covariance_step_successful = 1L,
    estimate_near_boundary = 0L,
    rounding_errors = 0L,
    THETA1 = 1,
    design_marker = design
  )
}

test_that("SSE analysis uses only the selected design's PK archive", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  suppressWarnings(suppressPackageStartupMessages(library(shiny)))
  suppressWarnings(suppressPackageStartupMessages(library(DT)))

  patab_a <- make_design_patab("a")
  patab_b <- make_design_patab("b")
  patab_a_state <- shiny::reactiveVal(patab_a)
  patab_b_state <- shiny::reactiveVal(patab_b)
  raw_a_state <- shiny::reactiveVal(make_design_sse("a"))
  raw_b_state <- shiny::reactiveVal(make_design_sse("b"))

  shiny::testServer(
    mod_sse_analysis_server,
    args = list(
      design_a_shared = shiny::reactive(list(
        id = "a",
        name = "Design A",
        raw_results = raw_a_state(),
        individual_pk = patab_a_state()
      )),
      design_b_shared = shiny::reactive(list(
        id = "b",
        name = "Design B",
        raw_results = raw_b_state(),
        individual_pk = patab_b_state()
      )),
      true_vals = shiny::reactive(stats::setNames(numeric(), character())),
      param_labels = shiny::reactive(NULL)
    ),
    {
      session$setInputs(which_design = "a")
      expect_identical(selected_design_bundle()$id, "a")
      expect_identical(selected_design_bundle()$raw_results$design_marker, "a")
      expect_identical(selected_individual_pk(), patab_a)
      expect_identical(pk_exposure_patab(), patab_a)

      session$setInputs(which_design = "b")
      expect_identical(selected_design_bundle()$id, "b")
      expect_identical(selected_design_bundle()$raw_results$design_marker, "b")
      expect_identical(selected_individual_pk(), patab_b)
      expect_identical(pk_exposure_patab(), patab_b)

      raw_b_state(NULL)
      session$flushReact()
      expect_identical(selected_design_id(), "a")
      expect_identical(selected_design_bundle()$id, "a")
      expect_identical(selected_individual_pk(), patab_a)
      expect_identical(pk_exposure_patab(), patab_a)

      raw_b_state(make_design_sse("b"))
      session$setInputs(which_design = "b")
      session$flushReact()

      patab_b_state(NULL)
      session$flushReact()
      expect_null(selected_individual_pk())
      expect_null(pk_exposure_patab())

      session$setInputs(which_design = "a")
      expect_identical(selected_individual_pk(), patab_a)
    }
  )
})

test_that("SSE upload stores and invalidates each design archive independently", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  suppressWarnings(suppressPackageStartupMessages(library(shiny)))
  suppressWarnings(suppressPackageStartupMessages(library(DT)))

  archive_a <- withr::local_tempfile(lines = "archive a")
  archive_b <- withr::local_tempfile(lines = "archive b")
  invalid_archive <- withr::local_tempfile(lines = "invalid archive")
  raw_a <- withr::local_tempfile(lines = "sample,converged")
  invalid_raw <- withr::local_tempfile(lines = "not,a,valid,sse")
  patab_a <- make_design_patab("a")
  patab_b <- make_design_patab("b")

  rlang::local_bindings(
    read_sse_patab_outputs = function(path) {
      if (identical(path, invalid_archive)) {
        stop("invalid archive")
      }
      if (identical(path, archive_a)) patab_a else patab_b
    },
    read_sse_raw_all = function(path) {
      if (identical(path, invalid_raw)) {
        stop("invalid raw results")
      }
      make_design_sse()
    },
    .env = globalenv()
  )

  file_input <- function(path, name) {
    data.frame(
      name = name,
      size = file.info(path)$size,
      type = "application/zip",
      datapath = path,
      stringsAsFactors = FALSE
    )
  }

  shiny::testServer(mod_sse_upload_server, {
    session$setInputs(
      patab_zip_a = file_input(archive_a, "design-a.zip"),
      patab_zip_b = file_input(archive_b, "design-b.zip")
    )
    session$flushReact()

    expect_equal(patab_state_a(), "ready", info = patab_error_a())
    expect_equal(patab_state_b(), "ready", info = patab_error_b())
    expect_identical(session$returned$individual_pk_a_data(), patab_a)
    expect_identical(session$returned$individual_pk_b_data(), patab_b)
    expect_null(session$returned$individual_pk_data)
    expect_identical(session$returned$design_a()$individual_pk, patab_a)
    expect_identical(session$returned$design_b()$individual_pk, patab_b)

    session$setInputs(sse_a = file_input(raw_a, "raw-results-a.csv"))
    session$flushReact()

    expect_null(session$returned$individual_pk_a_data())
    expect_identical(session$returned$individual_pk_b_data(), patab_b)
    expect_null(session$returned$design_a()$individual_pk)
    expect_identical(session$returned$design_b()$individual_pk, patab_b)

    session$setInputs(
      patab_zip_a = file_input(archive_a, "design-a-reloaded.zip")
    )
    session$flushReact()
    before_invalid_raw <- session$returned$design_a()

    session$setInputs(
      sse_a = file_input(invalid_raw, "invalid-raw-results-a.csv")
    )
    session$flushReact()
    expect_identical(session$returned$design_a(), before_invalid_raw)

    before_invalid_archive <- session$returned$design_a()$individual_pk
    session$setInputs(
      patab_zip_a = file_input(invalid_archive, "invalid-design-a.zip")
    )
    session$flushReact()
    expect_identical(
      session$returned$design_a()$individual_pk,
      before_invalid_archive
    )
  })
})
