# =============================================================================
# nonmem_dataset_builder.R -- strict NONMEM elementary-design CSV helpers
# =============================================================================

NONMEM_ELEMENTARY_COLUMNS <- c(
  "ID", "TIME", "DOSE", "AMT", "RATE", "DV", "MDV", "EVID", "CMT", "ARM"
)

parse_sampling_times <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(numeric())
  }

  tokens <- unlist(strsplit(text, "[,;[:space:]]+", perl = TRUE))
  tokens <- tokens[nzchar(tokens)]
  times <- suppressWarnings(as.numeric(tokens))
  invalid <- is.na(times) | !is.finite(times)

  if (any(invalid)) {
    stop(
      "Invalid sampling time: ",
      tokens[which(invalid)[1L]],
      call. = FALSE
    )
  }

  sort(unique(times))
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
    stop(
      "observation_times must contain at least one finite time.",
      call. = FALSE
    )
  }
  if (!is.numeric(rate) || length(rate) != 1L || !is.finite(rate) || rate < 0) {
    stop("rate must be a non-negative number.", call. = FALSE)
  }
  dose_cmt <- validate_compartment(dose_cmt, "dose_cmt")
  observation_cmt <- validate_compartment(observation_cmt, "observation_cmt")

  if (is.null(arm_values)) {
    arm_values <- seq_len(n_prototypes)
  }
  if (length(arm_values) != n_prototypes) {
    stop("arm_values length must equal n_prototypes.", call. = FALSE)
  }

  dose_times <- (seq_len(n_administrations) - 1L) * dose_interval
  observation_times <- sort(unique(observation_times))
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
      CMT = dose_cmt,
      ARM = arm_values[id]
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
      CMT = observation_cmt,
      ARM = arm_values[id]
    )
    rows[[id]] <- dplyr::bind_rows(dose_rows, observation_rows)
  }

  out <- dplyr::bind_rows(rows)
  out <- dplyr::arrange(out, .data$ID, .data$TIME, dplyr::desc(.data$EVID))
  dplyr::select(out, dplyr::all_of(NONMEM_ELEMENTARY_COLUMNS))
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
  if (length(extra_cols) > 0L) {
    errors <- c(
      errors,
      paste("Unexpected columns:", paste(extra_cols, collapse = ", "))
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
