#!/usr/bin/env Rscript

# =============================================================================
# explore_sse_individual_pk.R
#
# Standalone exploratory diagnostics for PsN SSE individual PK keep_tables.
#
# Example:
#   Rscript scripts/explore_sse_individual_pk.R
#   Rscript scripts/explore_sse_individual_pk.R \
#     --raw=docs/results/sse_further_explore/raw_results_psm_eval_sparseSSE_TABLE.csv \
#     --patab=docs/results/sse_further_explore/m1.zip \
#     --out=docs/results/sse_further_explore/individual_pk_diagnostics
# =============================================================================

parse_args <- function(args) {
  out <- list()
  for (arg in args) {
    if (!grepl("^--", arg)) next
    key <- sub("^--([^=]+).*$", "\\1", arg)
    value <- if (grepl("=", arg, fixed = TRUE)) {
      sub("^--[^=]+=", "", arg)
    } else {
      TRUE
    }
    out[[key]] <- value
  }
  out
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(current, "DESCRIPTION")) &&
        dir.exists(file.path(current, "R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

safe_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

save_plot <- function(plot, path, width = 9, height = 6) {
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = 160,
    bg = "white"
  )
  invisible(path)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
root <- find_project_root()

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

source(file.path(root, "R", "sse_individual_pk.R"))
source(file.path(root, "R", "sse_individual_pk_diagnostics.R"))

default_data_dir <- file.path(root, "docs", "results", "sse_further_explore")
raw_path <- args$raw %||%
  file.path(default_data_dir, "raw_results_psm_eval_sparseSSE_TABLE.csv")
patab_path <- args$patab %||% file.path(default_data_dir, "m1.zip")
out_dir <- args$out %||%
  file.path(default_data_dir, "individual_pk_diagnostics")
params <- args$params %||% "CL,VC,Q,VP,KA,F1"
params <- trimws(strsplit(params, ",", fixed = TRUE)[[1]])
top_n <- as.integer(args[["top-n"]] %||% "25")
if (is.na(top_n) || top_n < 1L) top_n <- 25L

if (!file.exists(raw_path)) {
  stop("raw_results file not found: ", raw_path, call. = FALSE)
}
if (!file.exists(patab_path)) {
  stop("PsN patab archive/directory not found: ", patab_path, call. = FALSE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading raw_results: ", raw_path)
raw <- utils::read.csv(raw_path, check.names = FALSE)
message("Reading PsN keep_tables: ", patab_path)
patab <- read_sse_patab_outputs(patab_path)

diagnostics <- build_individual_pk_diagnostics(raw, patab, params = params)

rds_path <- file.path(out_dir, "individual_pk_diagnostics.rds")
saveRDS(diagnostics, rds_path)

safe_write_csv(
  diagnostics$individual_pk_recovery_long,
  file.path(out_dir, "individual_pk_recovery_long.csv")
)
safe_write_csv(
  diagnostics$individual_pk_summary_by_param,
  file.path(out_dir, "individual_pk_summary_by_param.csv")
)
safe_write_csv(
  diagnostics$individual_pk_summary_by_id_param,
  file.path(out_dir, "individual_pk_summary_by_id_param.csv")
)
safe_write_csv(
  diagnostics$run_pk_error_burden,
  file.path(out_dir, "run_pk_error_burden.csv")
)

writeLines(
  diagnostics$prediction_columns,
  file.path(out_dir, "prediction_columns_detected.txt")
)

save_plot(
  plot_individual_pk_error_forest(
    diagnostics$individual_pk_summary_by_param,
    status_filter = "minimization_successful"
  ),
  file.path(out_dir, "individual_pk_error_forest.png"),
  width = 8.5,
  height = 5.5
)
save_plot(
  plot_individual_pk_error_heatmap(
    diagnostics$individual_pk_summary_by_id_param
  ),
  file.path(out_dir, "individual_pk_error_heatmap.png"),
  width = 8.5,
  height = 10
)
save_plot(
  plot_individual_pk_outliers(
    diagnostics$individual_pk_summary_by_id_param,
    top_n = top_n
  ),
  file.path(out_dir, "individual_pk_outliers.png"),
  width = 9,
  height = 7
)
save_plot(
  plot_run_pk_error_burden(diagnostics$run_pk_error_burden),
  file.path(out_dir, "run_pk_error_burden.png"),
  width = 9,
  height = 5.5
)
save_plot(
  plot_individual_pk_recovery_by_status(
    diagnostics$individual_pk_recovery_long,
    converged_only = FALSE,
    log_axes = TRUE
  ),
  file.path(out_dir, "individual_pk_recovery_by_status.png"),
  width = 9,
  height = 7
)

prediction_plot <- plot_individual_pk_prediction_diagnostics(patab)
if (is.null(prediction_plot)) {
  writeLines(
    c(
      "Prediction diagnostics skipped.",
      "Required kept-table columns were not all present: TIME and IPRED.",
      paste(
        "Detected columns:",
        paste(diagnostics$prediction_columns, collapse = ", ")
      )
    ),
    file.path(out_dir, "prediction_diagnostics_skipped.txt")
  )
} else {
  save_plot(
    prediction_plot,
    file.path(out_dir, "individual_prediction_profiles.png"),
    width = 9,
    height = 6
  )
}

cat("\nIndividual PK SSE diagnostics written to:\n")
cat(normalizePath(out_dir, winslash = "/", mustWork = TRUE), "\n\n")
cat("Rows:\n")
cat("  recovery_long: ",
    nrow(diagnostics$individual_pk_recovery_long), "\n", sep = "")
cat("  summary_by_param: ",
    nrow(diagnostics$individual_pk_summary_by_param), "\n", sep = "")
cat("  summary_by_id_param: ",
    nrow(diagnostics$individual_pk_summary_by_id_param), "\n", sep = "")
cat("  run_pk_error_burden: ",
    nrow(diagnostics$run_pk_error_burden), "\n", sep = "")

failed <- diagnostics$individual_pk_recovery_long |>
  dplyr::filter(.data$run_qc_status == "minimization_failed") |>
  dplyr::pull(.data$sample) |>
  unique() |>
  sort()
cat("  minimization_failed_samples: ",
    paste(failed, collapse = ", "), "\n", sep = "")
