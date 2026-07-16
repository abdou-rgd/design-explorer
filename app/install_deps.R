script_path <- tryCatch(
  {
    candidate <- sys.frame(1)$ofile
    if (
      is.null(candidate) ||
        length(candidate) != 1L ||
        is.na(candidate) ||
        !nzchar(candidate)
    ) {
      NULL
    } else {
      normalizePath(candidate, mustWork = TRUE)
    }
  },
  error = function(e) NULL
)

project_root <- if (!is.null(script_path)) {
  dirname(dirname(script_path))
} else if (file.exists("renv.lock")) {
  normalizePath(".", mustWork = TRUE)
} else {
  normalizePath("..", mustWork = TRUE)
}

activate <- file.path(project_root, "renv", "activate.R")
if (!file.exists(activate)) {
  stop("renv/activate.R is missing from the project", call. = FALSE)
}

source(activate, local = TRUE)
renv::restore(project = project_root, prompt = FALSE)
