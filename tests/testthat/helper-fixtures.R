# tests/testthat/helper-fixtures.R — shared fixture loaders for .tab/.ctl

PROJECT_ROOT <- Sys.getenv("DESIGN_EXPLORER_ROOT", unset = normalizePath("."))

fixture_path <- function(rel) {
  p <- file.path(PROJECT_ROOT, rel)
  if (!file.exists(p)) {
    testthat::skip(paste("Fixture missing:", rel))
  }
  p
}

load_tab <- function(rel) {
  read_tab(fixture_path(rel))
}

load_ctl_lines <- function(rel) {
  readr::read_lines(fixture_path(rel), progress = FALSE)
}

# Synthesize a minimal "classical population" .tab in-memory (no fixture file).
make_classical_tab <- function(n_ids = 10, n_obs_per_id = 5) {
  tibble::tibble(
    table_no = 1L,
    ID       = rep(seq_len(n_ids), each = n_obs_per_id + 1L),
    TIME     = c(rbind(0, replicate(n_ids, sort(runif(n_obs_per_id, 1, 24))))),
    EVID     = rep(c(1L, rep(0L, n_obs_per_id)), times = n_ids),
    MDV      = rep(c(1L, rep(0L, n_obs_per_id)), times = n_ids),
    DV       = c(rbind(0, replicate(n_ids, round(runif(n_obs_per_id, 0.5, 12), 3)))),
    IPRED    = c(rbind(0, replicate(n_ids, round(runif(n_obs_per_id, 0.5, 12), 3))))
  )
}
