# =============================================================================
# nonmem_dataset_builder.R -- strict NONMEM elementary-design CSV helpers
# =============================================================================

NONMEM_ELEMENTARY_COLUMNS <- c(
  "ID", "TIME", "DOSE", "AMT", "RATE", "DV", "MDV", "EVID", "CMT"
)

parse_schedule_times <- function(text, label = "time") {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(numeric())
  }

  tokens <- unlist(strsplit(text, "[,;[:space:]]+", perl = TRUE))
  tokens <- tokens[nzchar(tokens)]
  times <- suppressWarnings(as.numeric(tokens))
  invalid <- is.na(times) | !is.finite(times)

  if (any(invalid)) {
    stop(
      "Invalid ", label, ": ",
      tokens[which(invalid)[1L]],
      call. = FALSE
    )
  }

  sort(unique(times))
}

parse_sampling_times <- function(text) {
  parse_schedule_times(text, "sampling time")
}

parse_dose_events <- function(text, default_rate = 0, default_cmt = 1L) {
  if (is.null(text) || !nzchar(trimws(text))) {
    stop("DOSE_EVENTS must contain at least one dose event.", call. = FALSE)
  }
  if (!is.numeric(default_rate) || length(default_rate) != 1L ||
      !is.finite(default_rate) || default_rate < 0) {
    stop("RATE must be a non-negative number.", call. = FALSE)
  }
  default_cmt <- validate_compartment(default_cmt, "DOSE_CMT")

  tokens <- unlist(strsplit(text, "[;\r\n]+", perl = TRUE))
  tokens <- trimws(tokens[nzchar(trimws(tokens))])
  rows <- lapply(tokens, function(token) {
    parts <- trimws(strsplit(token, ":", fixed = TRUE)[[1]])
    if (!length(parts) %in% c(2L, 3L, 4L) || any(!nzchar(parts))) {
      stop("Invalid dose event: ", token, call. = FALSE)
    }
    values <- suppressWarnings(as.numeric(parts))
    if (any(is.na(values)) || any(!is.finite(values))) {
      stop("Invalid dose event: ", token, call. = FALSE)
    }
    time <- values[[1L]]
    amt <- values[[2L]]
    rate <- if (length(values) >= 3L) values[[3L]] else default_rate
    cmt <- if (length(values) >= 4L) values[[4L]] else default_cmt
    if (time < 0) {
      stop("Dose event times must be non-negative.", call. = FALSE)
    }
    if (amt <= 0) {
      stop("Dose event amounts must be positive.", call. = FALSE)
    }
    if (rate < 0) {
      stop("Dose event rates must be non-negative.", call. = FALSE)
    }
    cmt <- validate_compartment(cmt, "dose event CMT")
    tibble::tibble(TIME = time, AMT = amt, RATE = rate, CMT = cmt)
  })

  out <- dplyr::bind_rows(rows)
  dplyr::arrange(out, .data$TIME)
}

parse_sampling_events <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(tibble::tibble(TIME = numeric(), DOSE = numeric()))
  }

  tokens <- unlist(strsplit(text, "[,;[:space:]]+", perl = TRUE))
  tokens <- trimws(tokens[nzchar(trimws(tokens))])
  rows <- lapply(tokens, function(token) {
    parts <- trimws(strsplit(token, ":", fixed = TRUE)[[1]])
    if (!length(parts) %in% c(1L, 2L) || any(!nzchar(parts))) {
      stop("Invalid sampling event: ", token, call. = FALSE)
    }
    values <- suppressWarnings(as.numeric(parts))
    if (any(is.na(values)) || any(!is.finite(values))) {
      stop("Invalid sampling event: ", token, call. = FALSE)
    }
    time <- values[[1L]]
    dose <- if (length(values) == 2L) values[[2L]] else NA_real_
    if (time < 0) {
      stop("SAMPLING_TIMES must contain only non-negative times.", call. = FALSE)
    }
    if (!is.na(dose) && dose <= 0) {
      stop("Sampling event dose annotations must be positive.", call. = FALSE)
    }
    tibble::tibble(TIME = time, DOSE = dose)
  })

  out <- dplyr::bind_rows(rows)
  dplyr::arrange(out, .data$TIME)
}

read_schedule_table <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    stop("Schedule table must not be empty.", call. = FALSE)
  }
  con <- textConnection(text)
  on.exit(close(con), add = TRUE)
  dat <- tryCatch(
    utils::read.csv(
      con,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = c("", ".")
    ),
    error = function(e) {
      stop("Schedule table could not be parsed: ", conditionMessage(e), call. = FALSE)
    }
  )
  names(dat) <- toupper(trimws(names(dat)))
  dat
}

required_schedule_columns <- function() {
  c("DESIGN", "ARM", "DOSE_EVENTS", "SAMPLING_TIMES", "OBS_CMT")
}

schedule_control_columns <- function() {
  c("ID", "N_SUBJECTS", "DOSE_EVENTS", "SAMPLING_TIMES", "DOSE_CMT", "OBS_CMT", "RATE")
}

scalar_or_default <- function(row, name, default) {
  if (!name %in% names(row) || is.na(row[[name]]) || !nzchar(trimws(as.character(row[[name]])))) {
    return(default)
  }
  row[[name]]
}

parse_design_schedule_table <- function(text) {
  dat <- read_schedule_table(text)
  missing_cols <- setdiff(required_schedule_columns(), names(dat))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required schedule columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(dat) == 0L) {
    stop("Schedule table must contain at least one design row.", call. = FALSE)
  }

  specs <- vector("list", nrow(dat))
  for (i in seq_len(nrow(dat))) {
    row <- dat[i, , drop = FALSE]
    dose_cmt <- suppressWarnings(as.numeric(scalar_or_default(row, "DOSE_CMT", 1)))
    obs_cmt <- suppressWarnings(as.numeric(row[["OBS_CMT"]]))
    rate <- suppressWarnings(as.numeric(scalar_or_default(row, "RATE", 0)))
    n_subjects <- suppressWarnings(as.numeric(scalar_or_default(row, "N_SUBJECTS", 1)))
    id <- if ("ID" %in% names(row) && !is.na(row[["ID"]])) {
      suppressWarnings(as.numeric(row[["ID"]]))
    } else {
      NA_real_
    }

    dose_events <- parse_dose_events(
      as.character(row[["DOSE_EVENTS"]]),
      default_rate = rate,
      default_cmt = dose_cmt
    )
    sampling_events <- parse_sampling_events(as.character(row[["SAMPLING_TIMES"]]))
    if (nrow(sampling_events) == 0L) {
      stop("SAMPLING_TIMES must contain at least one sampling time.", call. = FALSE)
    }
    obs_cmt <- validate_compartment(obs_cmt, "OBS_CMT")
    n_subjects <- validate_positive_integer(n_subjects, "N_SUBJECTS")
    if (!is.na(id)) {
      id <- validate_positive_integer(id, "ID")
    }

    metadata <- list()
    metadata_cols <- setdiff(names(row), schedule_control_columns())
    for (col in metadata_cols) {
      value <- row[[col]]
      if (!is.na(value) && nzchar(trimws(as.character(value)))) {
        metadata[[col]] <- value
      }
    }

    specs[[i]] <- list(
      id = id,
      n_subjects = n_subjects,
      dose_events = dose_events,
      sampling_events = sampling_events,
      obs_cmt = obs_cmt,
      metadata = metadata
    )
  }
  specs
}

current_dose_at_time <- function(time, dose_events) {
  prior <- which(dose_events$TIME <= time)
  if (length(prior) == 0L) {
    return(dose_events$AMT[[1L]])
  }
  dose_events$AMT[[prior[[length(prior)]]]]
}

observation_dose_at_time <- function(time, dose_events, dose_override = NA_real_) {
  if (!is.na(dose_override)) {
    return(dose_override)
  }
  current_dose_at_time(time, dose_events)
}

build_nonmem_dataset_from_schedule_table <- function(text, id_start = 1L) {
  specs <- parse_design_schedule_table(text)
  id_start <- validate_positive_integer(id_start, "id_start")
  next_id <- id_start
  all_rows <- list()

  for (spec in specs) {
    ids <- if (!is.na(spec$id)) {
      seq.int(spec$id, length.out = spec$n_subjects)
    } else {
      seq.int(next_id, length.out = spec$n_subjects)
    }
    next_id <- max(c(next_id, ids)) + 1L

    for (id in ids) {
      dose_rows <- tibble::tibble(
        ID = id,
        TIME = spec$dose_events$TIME,
        DOSE = spec$dose_events$AMT,
        AMT = spec$dose_events$AMT,
        RATE = spec$dose_events$RATE,
        DV = 0,
        MDV = 1,
        EVID = 1,
        CMT = spec$dose_events$CMT
      )
      observation_rows <- tibble::tibble(
        ID = id,
        TIME = spec$sampling_events$TIME,
        DOSE = vapply(
          seq_len(nrow(spec$sampling_events)),
          function(i) {
            observation_dose_at_time(
              spec$sampling_events$TIME[[i]],
              spec$dose_events,
              spec$sampling_events$DOSE[[i]]
            )
          },
          numeric(1)
        ),
        AMT = 0,
        RATE = 0,
        DV = 1,
        MDV = 0,
        EVID = 0,
        CMT = spec$obs_cmt
      )
      rows <- dplyr::bind_rows(dose_rows, observation_rows)
      for (name in names(spec$metadata)) {
        rows[[name]] <- spec$metadata[[name]]
      }
      all_rows[[length(all_rows) + 1L]] <- rows
    }
  }

  out <- dplyr::bind_rows(all_rows)
  out <- dplyr::arrange(out, .data$ID, .data$TIME, dplyr::desc(.data$EVID))
  metadata_cols <- setdiff(names(out), NONMEM_ELEMENTARY_COLUMNS)
  dplyr::select(out, dplyr::all_of(c(NONMEM_ELEMENTARY_COLUMNS, metadata_cols)))
}

validate_design_schedule_table <- function(text) {
  errors <- character()
  warnings <- character()
  specs <- tryCatch(
    parse_design_schedule_table(text),
    error = function(e) {
      errors <<- c(errors, conditionMessage(e))
      NULL
    }
  )
  if (is.null(specs)) {
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    sampling_times <- spec$sampling_events$TIME
    same_time <- intersect(spec$dose_events$TIME, sampling_times)
    if (length(same_time) > 0L) {
      warnings <- c(
        warnings,
        paste0("Design ", i, " has sampling at the same time as a dose: ",
               paste(same_time, collapse = ", "))
      )
    }
    if (any(sampling_times < min(spec$dose_events$TIME))) {
      warnings <- c(warnings, paste0("Design ", i, " has sampling before the first dose."))
    }
    if (!any(sampling_times > min(spec$dose_events$TIME))) {
      warnings <- c(warnings, paste0("Design ", i, " has no observation after the first dose."))
    }
  }

  dat <- tryCatch(
    build_nonmem_dataset_from_schedule_table(text),
    error = function(e) {
      errors <<- c(errors, conditionMessage(e))
      NULL
    }
  )
  if (!is.null(dat)) {
    key <- paste(dat$ID, dat$TIME, dat$EVID, dat$CMT, sep = "\r")
    if (any(duplicated(key))) {
      warnings <- c(warnings, "Generated dataset contains duplicate ID/TIME/EVID/CMT rows.")
    }
    validation <- validate_nonmem_dataset(dat)
    errors <- c(errors, validation$errors)
  }

  list(valid = length(errors) == 0L, errors = unique(errors), warnings = unique(warnings))
}

is_integerish <- function(x) {
  is.numeric(x) && all(is.finite(x)) && all(x == floor(x))
}

validate_compartment <- function(x, name) {
  if (length(x) != 1L || !is_integerish(x) || x < 1) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  as.integer(x)
}

validate_positive_integer <- function(x, name) {
  if (length(x) != 1L || !is_integerish(x) || x < 1) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  as.integer(x)
}

normalize_optional_column <- function(optional_column) {
  if (is.null(optional_column) || !nzchar(trimws(optional_column))) {
    return(NULL)
  }
  name <- toupper(trimws(optional_column))
  if (!grepl("^[A-Z][A-Z0-9_]*$", name)) {
    stop(
      "optional_column must start with a letter and contain only letters, numbers, or underscores.",
      call. = FALSE
    )
  }
  if (name %in% NONMEM_ELEMENTARY_COLUMNS) {
    stop("optional_column must not duplicate a required column.", call. = FALSE)
  }
  name
}

is_blank_column <- function(x) {
  all(is.na(x) | trimws(as.character(x)) == "")
}

build_nonmem_elementary_dataset <- function(n_elementary_designs = 1L,
                                            dose,
                                            dose_times,
                                            observation_times,
                                            dose_cmt = 1L,
                                            observation_cmt = 1L,
                                            rate = 0,
                                            optional_column = NULL) {
  n_elementary_designs <- validate_positive_integer(
    n_elementary_designs,
    "n_elementary_designs"
  )
  if (!is.numeric(dose) || length(dose) != 1L || !is.finite(dose) || dose <= 0) {
    stop("dose must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(dose_times) || length(dose_times) == 0L ||
      any(!is.finite(dose_times))) {
    stop("dose_times must contain at least one finite time.", call. = FALSE)
  }
  if (any(dose_times < 0)) {
    stop("dose_times must contain only non-negative times.", call. = FALSE)
  }
  if (!is.numeric(observation_times) || length(observation_times) == 0L ||
      any(!is.finite(observation_times))) {
    stop(
      "observation_times must contain at least one finite time.",
      call. = FALSE
    )
  }
  if (any(observation_times < 0)) {
    stop("observation_times must contain only non-negative times.", call. = FALSE)
  }
  if (!is.numeric(rate) || length(rate) != 1L || !is.finite(rate) || rate < 0) {
    stop("rate must be a non-negative number.", call. = FALSE)
  }
  dose_cmt <- validate_compartment(dose_cmt, "dose_cmt")
  observation_cmt <- validate_compartment(observation_cmt, "observation_cmt")
  optional_column <- normalize_optional_column(optional_column)

  dose_times <- sort(unique(dose_times))
  observation_times <- sort(unique(observation_times))
  rows <- vector("list", n_elementary_designs)

  for (id in seq_len(n_elementary_designs)) {
    dose_rows <- tibble::tibble(
      ID = id,
      TIME = dose_times,
      DOSE = dose,
      AMT = dose,
      RATE = rate,
      DV = 0,
      MDV = 1,
      EVID = 1,
      CMT = dose_cmt
    )
    observation_rows <- tibble::tibble(
      ID = id,
      TIME = observation_times,
      DOSE = dose,
      AMT = 0,
      RATE = 0,
      DV = 1,
      MDV = 0,
      EVID = 0,
      CMT = observation_cmt
    )
    rows[[id]] <- dplyr::bind_rows(dose_rows, observation_rows)
  }

  out <- dplyr::bind_rows(rows)
  out <- dplyr::arrange(out, .data$ID, .data$TIME, dplyr::desc(.data$EVID))
  out <- dplyr::select(out, dplyr::all_of(NONMEM_ELEMENTARY_COLUMNS))
  if (!is.null(optional_column)) {
    out[[optional_column]] <- NA_character_
  }
  out
}

validate_nonmem_dataset <- function(dat) {
  errors <- character()

  if (is.null(dat) || !is.data.frame(dat)) {
    return(list(
      valid = FALSE,
      errors = "Dataset must be a data frame."
    ))
  }

  missing_cols <- setdiff(NONMEM_ELEMENTARY_COLUMNS, names(dat))
  extra_cols <- setdiff(names(dat), NONMEM_ELEMENTARY_COLUMNS)

  if (length(missing_cols) > 0L) {
    errors <- c(
      errors,
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
  }
  if (length(missing_cols) == 0L &&
      !identical(names(dat)[seq_along(NONMEM_ELEMENTARY_COLUMNS)],
                 NONMEM_ELEMENTARY_COLUMNS)) {
    errors <- c(
      errors,
      paste(
        "Core columns must appear first in this order:",
        paste(NONMEM_ELEMENTARY_COLUMNS, collapse = ", ")
      )
    )
  }
  if (!identical(names(dat), toupper(names(dat)))) {
    errors <- c(errors, "Column names must be uppercase.")
  }

  if (all(NONMEM_ELEMENTARY_COLUMNS %in% names(dat))) {
    numeric_cols <- NONMEM_ELEMENTARY_COLUMNS
    numeric_ok <- vapply(numeric_cols, function(col) {
      values <- dat[[col]]
      ok <- is.numeric(values) && all(is.finite(values))
      if (!ok) {
        errors <<- c(errors, paste("Column", col, "must be numeric and finite."))
      }
      ok
    }, logical(1))

    if (isTRUE(numeric_ok[["ID"]]) && any(dat$ID <= 0)) {
      errors <- c(errors, "Column ID must contain positive values.")
    }
    if (isTRUE(numeric_ok[["TIME"]]) && any(dat$TIME < 0)) {
      errors <- c(errors, "Column TIME must contain non-negative values.")
    }
    if (isTRUE(numeric_ok[["DOSE"]]) && any(dat$DOSE <= 0)) {
      errors <- c(errors, "Column DOSE must contain positive values.")
    }
    if (isTRUE(numeric_ok[["AMT"]]) && any(dat$AMT < 0)) {
      errors <- c(errors, "Column AMT must contain non-negative values.")
    }
    if (isTRUE(numeric_ok[["RATE"]]) && any(dat$RATE < 0)) {
      errors <- c(errors, "Column RATE must contain non-negative values.")
    }
    if (isTRUE(numeric_ok[["MDV"]]) && any(!dat$MDV %in% c(0, 1))) {
      errors <- c(errors, "Column MDV must contain only 0 or 1.")
    }
    if (isTRUE(numeric_ok[["EVID"]]) && any(!dat$EVID %in% c(0, 1))) {
      errors <- c(errors, "Column EVID must contain only 0 or 1.")
    }
    if (isTRUE(numeric_ok[["CMT"]]) &&
        any(dat$CMT < 1 | dat$CMT != floor(dat$CMT))) {
      errors <- c(errors, "Column CMT must contain positive integers.")
    }

    dose_rows <- dat[dat$EVID == 1, , drop = FALSE]
    observation_rows <- dat[dat$EVID == 0, , drop = FALSE]

    bad_dose <- nrow(dose_rows) == 0L ||
      any(
        is.na(dose_rows$MDV) | is.na(dose_rows$DV) |
          is.na(dose_rows$AMT) | is.na(dose_rows$DOSE) |
          dose_rows$MDV != 1 | dose_rows$DV != 0 |
          dose_rows$AMT <= 0 | dose_rows$DOSE <= 0
      )
    if (bad_dose) {
      errors <- c(
        errors,
        "Dose rows must have EVID=1, MDV=1, DV=0, AMT>0, and DOSE>0."
      )
    }

    bad_observation <- nrow(observation_rows) == 0L ||
      any(
        is.na(observation_rows$MDV) | is.na(observation_rows$DV) |
          is.na(observation_rows$AMT) | is.na(observation_rows$RATE) |
          observation_rows$MDV != 0 | observation_rows$DV != 1 |
          observation_rows$AMT != 0 | observation_rows$RATE != 0
      )
    if (bad_observation) {
      errors <- c(
        errors,
        "Observation rows must have EVID=0, MDV=0, DV=1, AMT=0, and RATE=0."
      )
    }
  }

  list(valid = length(errors) == 0L, errors = errors)
}

write_nonmem_csv <- function(dat, file) {
  readr::write_csv(dat, file, na = ".")
}
