# =============================================================================
# app_state.R -- Atomic session-state contracts
# =============================================================================

new_primary_run_context <- function(
  source = c("none", "upload", "example"),
  source_id = NULL,
  name = "Primary",
  file_paths = list(),
  ext_data = NULL,
  shk_data = NULL,
  coi_data = NULL,
  clt_data = NULL,
  tab_data = NULL,
  ctl_data = NULL,
  cpu_data = NA_real_,
  ext_lines = NULL,
  ctl_lines = NULL,
  summary_data = NULL,
  comparison = NULL,
  table_no_range = NULL,
  param_labels = NULL,
  cmt_labels = NULL,
  groupsize = 1L
) {
  source <- match.arg(source)

  if (length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
    name <- "Primary"
  }

  groupsize <- suppressWarnings(as.integer(groupsize))
  if (length(groupsize) != 1L || is.na(groupsize) || groupsize < 1L) {
    groupsize <- 1L
  }

  structure(
    list(
      source = source,
      source_id = source_id,
      name = name,
      file_paths = file_paths,
      ext_data = ext_data,
      shk_data = shk_data,
      coi_data = coi_data,
      clt_data = clt_data,
      tab_data = tab_data,
      ctl_data = ctl_data,
      cpu_data = cpu_data,
      ext_lines = ext_lines,
      ctl_lines = ctl_lines,
      summary_data = summary_data,
      comparison = comparison,
      table_no_range = table_no_range,
      param_labels = param_labels,
      cmt_labels = cmt_labels,
      groupsize = groupsize
    ),
    class = c("primary_run_context", "list")
  )
}

empty_primary_run_context <- function() {
  new_primary_run_context(source = "none")
}

primary_run_paths_present <- function(paths) {
  is.list(paths) &&
    length(paths) > 0L &&
    any(vapply(paths, function(path) !is.null(path), logical(1L)))
}

activate_primary_run_context <- function(current, candidate) {
  if (!inherits(current, "primary_run_context")) {
    stop("Current primary run context is invalid.", call. = FALSE)
  }
  if (is.null(candidate)) {
    return(current)
  }
  if (!inherits(candidate, "primary_run_context")) {
    stop("Candidate primary run context is invalid.", call. = FALSE)
  }
  if (identical(candidate$source, "none")) {
    stop("Use empty_primary_run_context() explicitly to reset.", call. = FALSE)
  }

  candidate
}

select_primary_run_context <- function(
  source,
  upload_context = NULL,
  example_context = NULL
) {
  source <- match.arg(source, c("none", "upload", "example"))
  context <- switch(
    source,
    upload = upload_context,
    example = example_context,
    NULL
  )

  if (is.null(context)) {
    return(empty_primary_run_context())
  }
  if (!inherits(context, "primary_run_context")) {
    stop(
      "Primary run context must use new_primary_run_context().",
      call. = FALSE
    )
  }
  if (!identical(context$source, source)) {
    stop(
      "Primary run context source does not match the active source.",
      call. = FALSE
    )
  }

  context
}
