library(testthat)

if (
  !exists("PROJECT_ROOT", inherits = TRUE) ||
    !file.exists(file.path(PROJECT_ROOT, "DESCRIPTION"))
) {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  PROJECT_ROOT <- if (file.exists(file.path(cwd, "DESCRIPTION"))) {
    cwd
  } else {
    normalizePath(file.path(cwd, "..", ".."), winslash = "/", mustWork = TRUE)
  }
}

project_r_sources <- function() {
  paths <- c(
    list.files(
      file.path(PROJECT_ROOT, "R"),
      pattern = "\\.R$",
      full.names = TRUE
    ),
    list.files(
      file.path(PROJECT_ROOT, "app", "R"),
      pattern = "\\.R$",
      full.names = TRUE
    )
  )
  paste(unlist(lapply(paths, readLines, warn = FALSE)), collapse = "\n")
}

test_that("source remains compatible with the locked 2022 package APIs", {
  source_text <- project_r_sources()

  expect_false(grepl("linewidth\\s*=", source_text, perl = TRUE))
  expect_false(grepl("mrgsolve::cmt\\s*\\(", source_text, perl = TRUE))

  font_awesome_6_names <- c(
    "circle-info",
    "triangle-exclamation",
    "diagram-3"
  )
  for (icon_name in font_awesome_6_names) {
    expect_false(grepl(icon_name, source_text, fixed = TRUE))
  }

  extract_icons <- function(pattern) {
    positions <- gregexpr(pattern, source_text, perl = TRUE)[[1]]
    if (identical(positions, -1L)) {
      return(character())
    }
    matches <- regmatches(source_text, list(positions))[[1]]
    sub(pattern, "\\1", matches, perl = TRUE)
  }
  literal_icons <- unique(c(
    extract_icons("icon\\(\\s*\"([^\"]+)\""),
    extract_icons("icon_name\\s*=\\s*\"([^\"]+)\"")
  ))
  valid_icons <- fontawesome::fa_metadata()$icon_names
  expect_setequal(setdiff(literal_icons, valid_icons), character())
})

test_that("renv locks the complete R 4.2.0 dependency cohort", {
  lock <- jsonlite::read_json(
    file.path(PROJECT_ROOT, "renv.lock"),
    simplifyVector = FALSE
  )
  expected <- c(
    shiny = "1.7.1",
    bslib = "0.3.1",
    DT = "0.23",
    ggplot2 = "3.3.6",
    dplyr = "1.0.9",
    tidyr = "1.2.0",
    stringr = "1.4.0",
    purrr = "0.3.4",
    readr = "2.1.2",
    tibble = "3.1.7",
    scales = "1.2.0",
    rlang = "1.0.2",
    testthat = "3.1.4",
    withr = "2.5.0",
    mrgsolve = "1.0.3",
    renv = "1.1.5"
  )
  locked <- vapply(
    lock$Packages,
    function(record) record$Version,
    character(1)
  )

  expect_identical(lock$R$Version, "4.2.0")
  expect_identical(
    lock$R$Repositories[[1]]$URL,
    "https://packagemanager.posit.co/cran/2022-05-12"
  )
  expect_length(locked, 91L)
  expect_identical(unname(locked[names(expected)]), unname(expected))
})
