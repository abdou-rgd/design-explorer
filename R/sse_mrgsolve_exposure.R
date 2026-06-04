# sse_mrgsolve_exposure.R -- mrgsolve-backed SSE exposure preflight helpers
# =============================================================================

.sse_mrgsolve_meta_columns <- c(
  "table", "sample", "alternative", "kind", "source_file",
  "ID", "ARM", "TIME", "EVID", "MDV", "CMT", "AMT", "RATE",
  "DV", "IPRED", "PRED", "CWRES", "WRES"
)

.sse_mrgsolve_or <- function(x, y) {
  if (is.null(x)) y else x
}

sse_mrgsolve_candidate_columns <- function(individual_pk_data) {
  if (is.null(individual_pk_data) || !is.data.frame(individual_pk_data)) {
    return(character())
  }

  cols <- names(individual_pk_data)
  keep <- !toupper(cols) %in% toupper(.sse_mrgsolve_meta_columns)
  keep <- keep & vapply(individual_pk_data, is.numeric, logical(1L))
  cols[keep]
}

.sse_mrgsolve_key <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", .sse_mrgsolve_or(x, "")))
}

.sse_mrgsolve_pick_auto_column <- function(model_param, candidate_columns) {
  if (length(candidate_columns) == 0L) return(NA_character_)

  exact <- which(candidate_columns == model_param)
  if (length(exact) > 0L) return(candidate_columns[exact[1L]])

  insensitive <- which(toupper(candidate_columns) == toupper(model_param))
  if (length(insensitive) > 0L) return(candidate_columns[insensitive[1L]])

  keyed <- which(.sse_mrgsolve_key(candidate_columns) == .sse_mrgsolve_key(model_param))
  if (length(keyed) > 0L) return(candidate_columns[keyed[1L]])

  NA_character_
}

build_sse_mrgsolve_parameter_mapping <- function(
  individual_pk_data,
  model_param_names,
  selected_mapping = NULL
) {
  model_param_names <- as.character(.sse_mrgsolve_or(model_param_names, character()))
  candidate_columns <- sse_mrgsolve_candidate_columns(individual_pk_data)

  selected_mapping <- .sse_mrgsolve_or(
    selected_mapping,
    stats::setNames(character(), character())
  )
  selected_mapping <- selected_mapping[nzchar(names(selected_mapping))]

  if (length(model_param_names) == 0L) {
    return(tibble::tibble(
      model_param = character(),
      patab_column = character(),
      status = character(),
      source = character()
    ))
  }

  rows <- lapply(model_param_names, function(param) {
    manual <- if (param %in% names(selected_mapping)) selected_mapping[[param]] else NULL
    if (!is.null(manual) && length(manual) == 1L && nzchar(manual)) {
      col <- if (manual %in% candidate_columns) manual else NA_character_
      src <- if (!is.na(col)) "manual" else "invalid"
    } else {
      col <- .sse_mrgsolve_pick_auto_column(param, candidate_columns)
      src <- if (!is.na(col)) "auto" else "none"
    }

    data.frame(
      model_param = param,
      patab_column = col,
      status = if (!is.na(col)) "mapped" else "unmapped",
      source = src,
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(do.call(rbind, rows))
}

.sse_mrgsolve_default_output <- function(capture_names) {
  capture_names <- as.character(.sse_mrgsolve_or(capture_names, character()))
  if (length(capture_names) == 0L) return(NULL)

  preferred <- c("CP", "IPRED", "CONC", "PRED", "DV")
  idx <- match(toupper(preferred), toupper(capture_names), nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx) > 0L) return(capture_names[idx[1L]])
  capture_names[1L]
}

build_sse_mrgsolve_exposure_preflight <- function(
  individual_pk_data,
  model_param_names,
  capture_names,
  concentration_output = NULL,
  selected_mapping = NULL,
  required_params = NULL
) {
  model_param_names <- as.character(.sse_mrgsolve_or(model_param_names, character()))
  capture_names <- as.character(.sse_mrgsolve_or(capture_names, character()))
  required_params <- as.character(.sse_mrgsolve_or(required_params, character()))

  output <- .sse_mrgsolve_or(
    concentration_output,
    .sse_mrgsolve_default_output(capture_names)
  )
  output_ok <- !is.null(output) &&
    length(output) == 1L &&
    nzchar(output) &&
    output %in% capture_names

  mapping <- build_sse_mrgsolve_parameter_mapping(
    individual_pk_data = individual_pk_data,
    model_param_names = model_param_names,
    selected_mapping = selected_mapping
  )
  required_mapping <- mapping[mapping$model_param %in% required_params, , drop = FALSE]
  missing_required <- required_mapping$model_param[required_mapping$status != "mapped"]

  n_samples <- if (!is.null(individual_pk_data) && "sample" %in% names(individual_pk_data)) {
    length(unique(stats::na.omit(individual_pk_data$sample)))
  } else {
    0L
  }
  n_ids <- if (!is.null(individual_pk_data) && "ID" %in% names(individual_pk_data)) {
    length(unique(stats::na.omit(individual_pk_data$ID)))
  } else {
    0L
  }
  n_rows <- if (is.null(individual_pk_data)) 0L else nrow(individual_pk_data)

  status <- "ready"
  if (n_rows == 0L) {
    status <- "needs_individual_pk"
  } else if (!output_ok) {
    status <- "needs_output"
  } else if (length(required_params) > 0L && length(missing_required) > 0L) {
    status <- "needs_mapping"
  }

  list(
    status = status,
    concentration_output = if (is.null(output)) NA_character_ else output,
    output_choices = capture_names,
    mapping = mapping,
    required_params = required_params,
    missing_required_params = missing_required,
    n_mapped_required = sum(required_mapping$status == "mapped"),
    n_samples = as.integer(n_samples),
    n_ids = as.integer(n_ids),
    n_rows = as.integer(n_rows)
  )
}

.sse_mrgsolve_unique_rows <- function(dat, cols) {
  if (nrow(dat) == 0L) return(dat)
  cols <- cols[cols %in% names(dat)]
  if (length(cols) == 0L) return(dat[!duplicated(dat), , drop = FALSE])
  dat[!duplicated(dat[, cols, drop = FALSE]), , drop = FALSE]
}

.sse_mrgsolve_pick_events <- function(dosing_events, individual_row) {
  ev <- dosing_events
  subject_id <- as.integer(individual_row$ID[[1]])

  if ("ARM" %in% names(individual_row) && "ID" %in% names(ev)) {
    arm <- suppressWarnings(as.numeric(individual_row$ARM[[1]]))
    arm_events <- ev[!is.na(ev$ID) & ev$ID == arm, , drop = FALSE]
    if (nrow(arm_events) > 0L) ev <- arm_events
  } else if ("ID" %in% names(ev)) {
    id_events <- ev[!is.na(ev$ID) & ev$ID == subject_id, , drop = FALSE]
    if (nrow(id_events) > 0L) ev <- id_events
  }

  ev$ID <- subject_id
  ev
}

build_sse_mrgsolve_sanity_inputs <- function(
  individual_pk_data,
  preflight,
  dosing_events,
  kind = "estimation",
  sample = NULL,
  ids = NULL,
  max_ids = 3L
) {
  if (is.null(preflight) || !identical(preflight$status, "ready")) {
    return(list(status = "not_ready", message = "PK exposure preflight is not ready."))
  }
  if (is.null(individual_pk_data) || !is.data.frame(individual_pk_data) ||
      nrow(individual_pk_data) == 0L) {
    return(list(status = "needs_individual_pk", message = "No individual PK table is available."))
  }
  if (is.null(dosing_events) || !is.data.frame(dosing_events) || nrow(dosing_events) == 0L) {
    return(list(status = "needs_dosing", message = "No dosing events are available."))
  }

  dat <- individual_pk_data
  if ("kind" %in% names(dat) && !is.null(kind) && nzchar(kind)) {
    dat <- dat[dat$kind == kind, , drop = FALSE]
  }
  if ("sample" %in% names(dat)) {
    if (is.null(sample)) {
      sample <- sort(unique(stats::na.omit(dat$sample)))[1L]
    }
    dat <- dat[dat$sample == sample, , drop = FALSE]
  }
  if (!is.null(ids) && length(ids) > 0L && "ID" %in% names(dat)) {
    dat <- dat[dat$ID %in% ids, , drop = FALSE]
  }
  if (nrow(dat) == 0L) {
    return(list(status = "needs_individual_pk", message = "No matching individual PK rows are available."))
  }

  mapping <- preflight$mapping
  mapping <- mapping[mapping$model_param %in% preflight$required_params, , drop = FALSE]
  mapping <- mapping[mapping$status == "mapped", , drop = FALSE]
  if (nrow(mapping) == 0L) {
    return(list(status = "needs_mapping", message = "No mapped individual model inputs are available."))
  }

  dat <- .sse_mrgsolve_unique_rows(
    dat,
    unique(c("sample", "kind", "ID", mapping$patab_column, "ARM"))
  )
  dat <- dat[!is.na(dat$ID), , drop = FALSE]
  if (nrow(dat) == 0L) {
    return(list(status = "needs_individual_pk", message = "No valid individual IDs are available."))
  }
  limit <- suppressWarnings(as.integer(max_ids))
  if (length(limit) == 0L || is.na(limit) || !is.finite(limit)) {
    limit <- nrow(dat)
  }
  dat <- dat[seq_len(min(nrow(dat), max(1L, limit))), , drop = FALSE]

  idata <- data.frame(ID = as.integer(dat$ID), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(mapping))) {
    param <- mapping$model_param[[i]]
    col <- mapping$patab_column[[i]]
    idata[[param]] <- dat[[col]]
  }

  event_rows <- lapply(seq_len(nrow(dat)), function(i) {
    .sse_mrgsolve_pick_events(dosing_events, dat[i, , drop = FALSE])
  })
  events <- do.call(rbind, event_rows)
  events <- events[!is.na(events$amt), , drop = FALSE]
  if (nrow(events) == 0L) {
    return(list(status = "needs_dosing", message = "Dosing events have no AMT values."))
  }

  list(
    status = "ready",
    message = NULL,
    sample = sample,
    kind = kind,
    idata = idata,
    events = events
  )
}

run_sse_mrgsolve_sanity_check <- function(
  mod,
  individual_pk_data,
  preflight,
  dosing_events,
  kind = "estimation",
  sample = NULL,
  ids = NULL,
  max_ids = 3L,
  end_time = NULL,
  delta = 0.5
) {
  if (is.null(mod) || is.character(mod)) {
    return(list(status = "model_unavailable", message = "No compiled mrgsolve model is available."))
  }
  if (!requireNamespace("mrgsolve", quietly = TRUE)) {
    return(list(status = "mrgsolve_unavailable", message = "mrgsolve is not installed."))
  }

  inputs <- build_sse_mrgsolve_sanity_inputs(
    individual_pk_data = individual_pk_data,
    preflight = preflight,
    dosing_events = dosing_events,
    kind = kind,
    sample = sample,
    ids = ids,
    max_ids = max_ids
  )
  if (!identical(inputs$status, "ready")) return(inputs)

  if (is.null(end_time)) {
    end_time <- max(inputs$events$time, na.rm = TRUE) + 24
  }
  output <- preflight$concentration_output

  tryCatch({
    sim <- mrgsolve::mrgsim(
      mod,
      data = inputs$events,
      idata = inputs$idata,
      end = end_time,
      delta = delta,
      carry_out = "evid,cmt"
    )
    out <- if (exists("mrgsolve_sim_to_data_frame", mode = "function")) {
      mrgsolve_sim_to_data_frame(sim)
    } else if (inherits(sim, "mrgsims") && methods::is(sim, "mrgsims")) {
      as.data.frame(sim@data)
    } else {
      as.data.frame(sim)
    }
    if ("evid" %in% names(out)) {
      out <- out[out$evid == 0, , drop = FALSE]
    }
    if (nrow(out) == 0L) {
      return(list(status = "empty_output", message = "mrgsolve returned no observation rows."))
    }
    if (!output %in% names(out)) {
      return(list(
        status = "missing_output",
        message = paste0("Output column not found: ", output),
        available_outputs = names(out)
      ))
    }

    vals <- suppressWarnings(as.numeric(out[[output]]))
    finite <- vals[is.finite(vals)]
    if (length(finite) == 0L) {
      return(list(status = "nonfinite_output", message = "Output contains no finite concentration values."))
    }
    value_range <- range(finite, na.rm = TRUE)
    if (isTRUE(all.equal(value_range[[1]], value_range[[2]]))) {
      return(list(
        status = "constant_output",
        message = "Output concentration is constant in the sanity simulation.",
        output = output,
        n_ids = length(unique(inputs$idata$ID)),
        n_rows = nrow(out),
        value_range = value_range,
        preview = tibble::as_tibble(utils::head(out, 20L))
      ))
    }

    list(
      status = "ok",
      message = NULL,
      output = output,
      sample = inputs$sample,
      kind = inputs$kind,
      n_ids = length(unique(inputs$idata$ID)),
      n_rows = nrow(out),
      time_range = range(out$time, na.rm = TRUE),
      value_range = value_range,
      preview = tibble::as_tibble(utils::head(out[, unique(c("ID", "time", output)), drop = FALSE], 20L))
    )
  }, error = function(e) {
    list(status = "simulation_error", message = conditionMessage(e))
  })
}

.sse_mrgsolve_trapz <- function(x, y) {
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 2L) return(NA_real_)
  sum(diff(x) * (head(y, -1L) + tail(y, -1L)) / 2)
}

.sse_mrgsolve_nearest_value <- function(time, value, target) {
  if (length(target) == 0L || all(is.na(target))) return(NA_real_)
  keep <- is.finite(time) & is.finite(value)
  time <- time[keep]
  value <- value[keep]
  if (length(time) == 0L) return(NA_real_)
  target <- target[is.finite(target)]
  if (length(target) == 0L) return(NA_real_)
  idx <- vapply(target, function(t) which.min(abs(time - t)), integer(1L))
  stats::median(value[idx], na.rm = TRUE)
}

compute_sse_mrgsolve_exposure_metrics <- function(
  profile,
  concentration_output,
  trough_times = numeric(0L)
) {
  if (is.null(profile) || !is.data.frame(profile) || nrow(profile) == 0L) {
    return(tibble::tibble(
      sample = integer(),
      kind = character(),
      ID = integer(),
      metric = character(),
      value = numeric()
    ))
  }
  if (!concentration_output %in% names(profile)) {
    stop("Concentration output not found: ", concentration_output, call. = FALSE)
  }

  split_cols <- intersect(c("sample", "kind", "ID"), names(profile))
  groups <- split(profile, profile[split_cols], drop = TRUE)
  rows <- lapply(groups, function(dat) {
    dat <- dat[order(dat$time), , drop = FALSE]
    conc <- suppressWarnings(as.numeric(dat[[concentration_output]]))
    finite <- is.finite(dat$time) & is.finite(conc)
    dat <- dat[finite, , drop = FALSE]
    conc <- conc[finite]
    if (nrow(dat) == 0L) return(NULL)

    cmax_idx <- which.max(conc)
    meta <- lapply(split_cols, function(col) dat[[col]][[1]])
    names(meta) <- split_cols
    data.frame(
      meta,
      metric = c("AUC", "Cmax", "Tmax", "Ctrough"),
      value = c(
        .sse_mrgsolve_trapz(dat$time, conc),
        conc[[cmax_idx]],
        dat$time[[cmax_idx]],
        .sse_mrgsolve_nearest_value(dat$time, conc, trough_times)
      ),
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(dplyr::bind_rows(rows))
}

.sse_mrgsolve_simulate_profile <- function(
  mod,
  individual_pk_data,
  preflight,
  dosing_events,
  kind,
  sample,
  ids = NULL,
  max_ids = NULL,
  end_time = NULL,
  delta = 0.5
) {
  inputs <- build_sse_mrgsolve_sanity_inputs(
    individual_pk_data = individual_pk_data,
    preflight = preflight,
    dosing_events = dosing_events,
    kind = kind,
    sample = sample,
    ids = ids,
    max_ids = .sse_mrgsolve_or(max_ids, Inf)
  )
  if (!identical(inputs$status, "ready")) return(NULL)

  if (is.null(end_time)) {
    end_time <- max(inputs$events$time, na.rm = TRUE) + 24
  }

  sim <- mrgsolve::mrgsim(
    mod,
    data = inputs$events,
    idata = inputs$idata,
    end = end_time,
    delta = delta,
    carry_out = "evid,cmt"
  )
  out <- if (exists("mrgsolve_sim_to_data_frame", mode = "function")) {
    mrgsolve_sim_to_data_frame(sim)
  } else if (inherits(sim, "mrgsims") && methods::is(sim, "mrgsims")) {
    as.data.frame(sim@data)
  } else {
    as.data.frame(sim)
  }
  if ("evid" %in% names(out)) out <- out[out$evid == 0, , drop = FALSE]
  out$sample <- sample
  out$kind <- kind
  tibble::as_tibble(out)
}

compute_sse_mrgsolve_exposure_recovery <- function(
  mod,
  individual_pk_data,
  preflight,
  dosing_events,
  samples = NULL,
  ids = NULL,
  max_samples = NULL,
  max_ids = NULL,
  end_time = NULL,
  delta = 0.5,
  trough_times = numeric(0L)
) {
  if (is.null(mod) || is.character(mod)) {
    stop("A compiled mrgsolve model is required.", call. = FALSE)
  }
  if (!requireNamespace("mrgsolve", quietly = TRUE)) {
    stop("mrgsolve is not installed.", call. = FALSE)
  }
  if (is.null(preflight) || !identical(preflight$status, "ready")) {
    stop("PK exposure preflight must be ready before computing exposures.", call. = FALSE)
  }
  if (is.null(samples)) {
    samples <- sort(unique(stats::na.omit(individual_pk_data$sample)))
  }
  if (!is.null(max_samples)) {
    samples <- head(samples, max(1L, as.integer(max_samples)))
  }

  metric_rows <- lapply(samples, function(sample) {
    sim_profile <- .sse_mrgsolve_simulate_profile(
      mod, individual_pk_data, preflight, dosing_events,
      kind = "simulation", sample = sample, ids = ids,
      max_ids = max_ids, end_time = end_time, delta = delta
    )
    est_profile <- .sse_mrgsolve_simulate_profile(
      mod, individual_pk_data, preflight, dosing_events,
      kind = "estimation", sample = sample, ids = ids,
      max_ids = max_ids, end_time = end_time, delta = delta
    )
    if (is.null(sim_profile) || is.null(est_profile)) return(NULL)

    dplyr::bind_rows(
      compute_sse_mrgsolve_exposure_metrics(
        sim_profile,
        preflight$concentration_output,
        trough_times = trough_times
      ),
      compute_sse_mrgsolve_exposure_metrics(
        est_profile,
        preflight$concentration_output,
        trough_times = trough_times
      )
    )
  })

  metrics <- dplyr::bind_rows(metric_rows)
  if (nrow(metrics) == 0L) {
    return(tibble::tibble(
      sample = integer(),
      ID = integer(),
      metric = character(),
      sim_value = numeric(),
      est_value = numeric(),
      absolute_error = numeric(),
      relative_error = numeric()
    ))
  }

  wide <- tidyr::pivot_wider(
    metrics,
    id_cols = c("sample", "ID", "metric"),
    names_from = "kind",
    values_from = "value"
  )
  names(wide)[names(wide) == "simulation"] <- "sim_value"
  names(wide)[names(wide) == "estimation"] <- "est_value"

  wide$absolute_error <- wide$est_value - wide$sim_value
  wide$relative_error <- ifelse(
    is.finite(wide$sim_value) & abs(wide$sim_value) > 0,
    100 * wide$absolute_error / wide$sim_value,
    NA_real_
  )
  wide
}
