# =============================================================================
# sse_individual_pk.R -- PsN keep_tables / patab individual PK outputs
# =============================================================================


# =============================================================================
# .parse_patab_sample() -- sample index + kind from PsN patab filename
# =============================================================================

.parse_patab_sample <- function(path) {
  nm <- basename(path)
  is_sim <- grepl("\\.tab-sim-\\d+$", nm)
  sample <- as.integer(sub("^.*\\.tab(?:-sim)?-(\\d+)$", "\\1", nm))
  list(
    table = sub("^(.*\\.tab)(?:-sim)?-\\d+$", "\\1", nm),
    sample = sample,
    kind = if (is_sim) "simulation" else "estimation"
  )
}


# =============================================================================
# .read_one_patab() -- robust NONMEM table reader
# =============================================================================

.read_one_patab <- function(file) {
  lines <- readLines(file, warn = FALSE)
  header_idx <- grep("^\\s*ID\\s+", lines)[1]
  if (is.na(header_idx)) {
    stop("No patab header found in ", basename(file), call. = FALSE)
  }

  header <- strsplit(trimws(lines[[header_idx]]), "\\s+")[[1]]
  body <- lines[(header_idx + 1L):length(lines)]
  body <- body[!grepl("^\\s*(TABLE NO\\.|ID\\s+)", body)]
  body <- body[nzchar(trimws(body))]
  if (length(body) == 0L) {
    return(tibble::as_tibble(stats::setNames(
      replicate(length(header), numeric(0L), simplify = FALSE),
      header
    )))
  }

  dat <- read.table(text = body, header = FALSE, col.names = header,
                    check.names = FALSE, stringsAsFactors = FALSE)
  dat <- dat[!is.na(dat[[1]]) & dat[[1]] != "ID", , drop = FALSE]
  dat[] <- lapply(dat, function(x) suppressWarnings(as.numeric(x)))
  tibble::as_tibble(dat)
}


# =============================================================================
# read_sse_patab_outputs() -- Read PsN patab files from a zip or directory
# =============================================================================

.is_zip_archive <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(FALSE)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = "raw", n = 4L)
  length(sig) >= 4L &&
    identical(sig[1:2], charToRaw("PK")) &&
    sig[[3]] %in% as.raw(c(3, 5, 7)) &&
    sig[[4]] %in% as.raw(c(4, 6, 8))
}

#' Read PsN `-keep_tables` patab files.
#'
#' Accepts a PsN run directory or a zip archive containing keep_tables outputs.
#' The recommended NONMEM convention is `FILE=pk_individuals.tab`, which PsN
#' keeps as files like `pk_individuals.tab-1` and
#' `pk_individuals.tab-sim-1`. Legacy PsN names such as `patab1.tab-1` and
#' `patab1.tab-sim-1` are also accepted. Repeated subject rows are deduplicated
#' because these tables are often record-level repeats of subject-level PK/ETA
#' values.
#'
#' @param path Path to a directory or zip archive.
#' @param deduplicate Logical; keep one row per table/sample/kind/ID/value set.
#' @param table_pattern Optional regular expression used to keep only matching
#'   table filenames.
#' @return Tibble with `table`, `sample`, `kind`, `source_file`, and patab columns.
#' @export
read_sse_patab_outputs <- function(path, deduplicate = TRUE,
                                   table_pattern = NULL) {
  if (is.null(path) || !file.exists(path)) {
    stop("patab path not found: ", path, call. = FALSE)
  }

  cleanup <- NULL
  root <- path
  if (grepl("\\.zip$", path, ignore.case = TRUE) || .is_zip_archive(path)) {
    root <- tempfile("sse-patab-")
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(path, exdir = root)
    cleanup <- root
    on.exit(unlink(cleanup, recursive = TRUE, force = TRUE), add = TRUE)
  }

  files <- list.files(root, pattern = "\\.tab(-sim)?-\\d+$",
                      recursive = TRUE, full.names = TRUE)
  files <- files[!dir.exists(files)]
  if (!is.null(table_pattern) && length(table_pattern) == 1L &&
      nzchar(table_pattern)) {
    files <- files[grepl(table_pattern, basename(files), ignore.case = TRUE)]
  }
  if (length(files) == 0L) {
    return(tibble::tibble(
      table = character(), sample = integer(), kind = character(),
      source_file = character()
    ))
  }

  rows <- lapply(files, function(f) {
    meta <- .parse_patab_sample(f)
    dat <- .read_one_patab(f)
    if (nrow(dat) == 0L) return(NULL)

    dat$table <- meta$table
    dat$sample <- meta$sample
    dat$kind <- meta$kind
    dat$source_file <- basename(f)
    dat <- dat[, c("table", "sample", "kind", "source_file",
                   setdiff(names(dat), c("table", "sample", "kind",
                                         "source_file")))]
    dat
  })

  out <- tibble::as_tibble(dplyr::bind_rows(rows))
  if (deduplicate && nrow(out) > 0L) {
    out <- dplyr::distinct(out)
  }
  dplyr::arrange(out, table, sample, kind, ID)
}


# =============================================================================
# compute_individual_pk_recovery() -- Estimation vs simulation comparison
# =============================================================================

#' Compare estimated individual PK outputs against simulated individual values.
#'
#' @param patab_data Tibble from `read_sse_patab_outputs()`.
#' @param params Character vector of PK columns to compare.
#' @return Long tibble with `sim`, `est`, and relative error.
#' @export
compute_individual_pk_recovery <- function(
    patab_data,
    params = c("CL", "VC", "Q", "VP", "KA", "F1")) {
  empty <- tibble::tibble(
    table = character(), sample = integer(), ID = numeric(), ARM = numeric(),
    param = character(), sim = numeric(), est = numeric(),
    relative_error = numeric()
  )
  if (is.null(patab_data) || nrow(patab_data) == 0L) return(empty)

  available <- intersect(params, names(patab_data))
  if (length(available) == 0L) return(empty)

  base_cols <- intersect(c("table", "sample", "kind", "ID", "ARM"),
                         names(patab_data))
  long <- patab_data |>
    dplyr::select(dplyr::all_of(base_cols), dplyr::all_of(available)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(available),
      names_to = "param",
      values_to = "value"
    )

  sim <- long |>
    dplyr::filter(kind == "simulation") |>
    dplyr::select(table, sample, ID, param, sim = value)
  est <- long |>
    dplyr::filter(kind == "estimation") |>
    dplyr::select(table, sample, ID, ARM, param, est = value)

  out <- dplyr::inner_join(est, sim,
                           by = c("table", "sample", "ID", "param"))
  if (nrow(out) == 0L) return(empty)

  out$relative_error <- dplyr::if_else(
    !is.na(out$sim) & abs(out$sim) > 1e-15,
    100 * (out$est - out$sim) / abs(out$sim),
    NA_real_
  )
  out |>
    dplyr::select(table, sample, ID, ARM, param, sim, est, relative_error) |>
    dplyr::arrange(table, sample, ID, param)
}


# =============================================================================
# summarize_individual_pk_archive() -- Compact upload status
# =============================================================================

#' Summarise parsed patab output availability.
#'
#' @param patab_data Tibble from `read_sse_patab_outputs()`.
#' @return List with counts used by the Shiny status UI.
#' @export
summarize_individual_pk_archive <- function(patab_data) {
  if (is.null(patab_data) || nrow(patab_data) == 0L) {
    return(list(n_files = 0L, n_est = 0L, n_sim = 0L,
                n_samples = 0L, n_ids = 0L, columns = character()))
  }
  list(
    n_files = length(unique(patab_data$source_file)),
    n_est = length(unique(patab_data$source_file[patab_data$kind == "estimation"])),
    n_sim = length(unique(patab_data$source_file[patab_data$kind == "simulation"])),
    n_samples = length(unique(patab_data$sample)),
    n_ids = length(unique(patab_data$ID)),
    columns = setdiff(names(patab_data),
                      c("table", "sample", "kind", "source_file"))
  )
}


# =============================================================================
# Individual PK plots
# =============================================================================

#' Scatter plot of simulated vs estimated individual PK parameters.
#'
#' @param recovery_data Tibble from `compute_individual_pk_recovery()`.
#' @return ggplot object.
#' @export
plot_individual_pk_recovery <- function(recovery_data, log_axes = FALSE) {
  if (is.null(recovery_data) || nrow(recovery_data) == 0L) {
    return(ggplot() +
      labs(title = "No individual PK patab data available") +
      .theme_design())
  }

  df <- recovery_data[is.finite(recovery_data$sim) &
                        is.finite(recovery_data$est), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(ggplot() +
      labs(title = "No finite individual PK recovery values") +
      .theme_design())
  }

  if (isTRUE(log_axes)) {
    log_axes <- all(df$sim > 0, na.rm = TRUE) && all(df$est > 0, na.rm = TRUE)
  }

  df$error_capped <- pmax(pmin(df$relative_error, 100), -100)
  summary_df <- df |>
    dplyr::filter(!is.na(relative_error)) |>
    dplyr::group_by(param) |>
    dplyr::summarise(
      median_error = stats::median(relative_error, na.rm = TRUE),
      iqr_error = stats::IQR(relative_error, na.rm = TRUE),
      pct_abs20 = mean(abs(relative_error) <= 20, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  summary_df$label <- sprintf(
    "median %+0.1f%%\nIQR %.1f%%\n|err|<=20%%: %.0f%%",
    summary_df$median_error, summary_df$iqr_error, summary_df$pct_abs20
  )

  p <- ggplot(df, aes(x = sim, y = est)) +
    geom_abline(slope = 1, intercept = 0, color = "grey55",
                linewidth = 0.4) +
    geom_point(aes(color = error_capped), alpha = 0.45, size = 1.25) +
    geom_label(
      data = summary_df,
      aes(x = -Inf, y = Inf, label = label),
      inherit.aes = FALSE,
      hjust = -0.05, vjust = 1.08,
      linewidth = 0, fill = "white", alpha = 0.82,
      size = 2.45, lineheight = 0.92
    ) +
    facet_wrap(~ param, scales = "free", ncol = 3) +
    scale_color_gradient2(
      low = "#2563eb", mid = "#64748b", high = "#dc2626",
      midpoint = 0, limits = c(-100, 100),
      name = "Relative error (%)\ncapped at +/-100"
    ) +
    labs(
      title = "Individual PK Recovery",
      subtitle = "Estimated table values vs simulated values; identity line marks perfect recovery",
      x = "Simulated individual value",
      y = "Estimated individual value"
    ) +
    .theme_design() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 8)
    )

  if (isTRUE(log_axes)) {
    p <- p +
      scale_x_log10() +
      scale_y_log10() +
      labs(subtitle = paste(
        "Log-log scale; estimated table values vs simulated values;",
        "identity line marks perfect recovery"
      ))
  }
  p
}


#' Boxplot of individual PK relative errors.
#'
#' @param recovery_data Tibble from `compute_individual_pk_recovery()`.
#' @return ggplot object.
#' @export
plot_individual_pk_error_distribution <- function(recovery_data) {
  if (is.null(recovery_data) || nrow(recovery_data) == 0L) {
    return(ggplot() +
      labs(title = "No individual PK error data available") +
      .theme_design())
  }

  df <- recovery_data[!is.na(recovery_data$relative_error), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(ggplot() +
      labs(title = "No finite individual PK relative errors") +
      .theme_design())
  }

  summary_df <- df |>
    dplyr::group_by(param) |>
    dplyr::summarise(
      median_abs_error = stats::median(abs(relative_error), na.rm = TRUE),
      .groups = "drop"
    )
  df <- dplyr::left_join(df, summary_df, by = "param")
  df$param_ordered <- stats::reorder(df$param, df$median_abs_error)

  ggplot(df, aes(x = param_ordered, y = relative_error)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
    geom_hline(yintercept = c(-20, 20), linetype = "dashed",
               color = "#d97706", linewidth = 0.35) +
    geom_hline(yintercept = c(-50, 50), linetype = "dotted",
               color = "#dc2626", linewidth = 0.35) +
    geom_jitter(width = 0.12, height = 0, alpha = 0.08, size = 0.65,
                color = "#334155") +
    geom_boxplot(aes(fill = median_abs_error), alpha = 0.82,
                 outlier.shape = NA, width = 0.58) +
    coord_flip() +
    scale_fill_gradient(low = "#d1fae5", high = "#dc2626",
                        name = "Median |error| (%)") +
    labs(
      title = "Individual PK Relative Error",
      subtitle = "100 x (estimated - simulated) / |simulated|; dashed = +/-20%, dotted = +/-50%",
      x = NULL,
      y = "Relative error (%)"
    ) +
    .theme_design() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 8)
    )
}
