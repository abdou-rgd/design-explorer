# =============================================================================
# design_utils.R
# Utilitaires partages pour le parsing NONMEM $DESIGN
#
# Contenu :
#   %||%                — Operateur null-coalesce
#   .EXT_ITER           — Constantes iterations speciales .ext
#   .parse_table_blocks — Parser generique pour fichiers a blocs TABLE NO.
#   prepare_tab_obs     — Preparer les observations d'un .tab
#
# Prerequis : aucun (packages readr, dplyr, stringr, purrr, tidyr)
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)


# =============================================================================
# Utilitaires internes
# =============================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

# Constantes : numéros d'itération spéciaux du fichier .ext
.EXT_ITER <- list(
  final       = -1000000000L,
  se          = -1000000001L,
  eigenvalues = -1000000002L,
  condition   = -1000000003L,
  sd_corr     = -1000000004L,
  se_sd_corr  = -1000000005L,
  fixed_flags = -1000000006L,
  termination = -1000000007L,
  gradient    = -1000000008L
)

# Parser générique pour les fichiers à blocs TABLE NO. (ext, shk, tab...)
.parse_table_blocks <- function(lines, col_line_pattern) {
  table_idx <- which(str_starts(lines, "TABLE NO\\."))
  if (length(table_idx) == 0L) return(list())

  parse_one <- function(tbl_i) {
    start <- table_idx[tbl_i]
    end   <- if (tbl_i < length(table_idx)) table_idx[tbl_i + 1L] - 1L else length(lines)
    block <- lines[start:end]

    tbl_no <- as.integer(str_extract(block[1L], "(?<=TABLE NO\\.\\s{0,10})\\d+"))

    col_line_i <- which(str_detect(block, col_line_pattern))[1L]
    if (is.na(col_line_i)) return(NULL)

    col_names <- str_split(str_trim(block[col_line_i]), "\\s+")[[1L]]

    data_lines <- block[(col_line_i + 1L):length(block)]
    data_lines <- data_lines[str_detect(data_lines, "^\\s*-?[0-9]")]
    if (length(data_lines) == 0L) return(NULL)

    rows <- data_lines |>
      str_trim() |>
      str_split("\\s+") |>
      map(as.numeric) |>
      keep(~ length(.x) == length(col_names))

    if (length(rows) == 0L) return(NULL)

    dat <- as_tibble(do.call(rbind, rows), .name_repair = "minimal")
    names(dat) <- col_names
    mutate(dat, table_no = tbl_no, .before = 1L)
  }

  blocks <- map(seq_along(table_idx), parse_one) |> compact()

  # Fix NONMEM quirk: .tab files may label all blocks "TABLE NO. 1"
  # When multiple blocks share the same table_no, assign sequential indices
  if (length(blocks) > 1L) {
    tbl_nos <- vapply(blocks, function(b) b$table_no[1L], integer(1L))
    if (length(unique(tbl_nos)) == 1L) {
      for (i in seq_along(blocks)) {
        blocks[[i]]$table_no <- i
      }
    }
  }
  blocks
}

#' Prépare les observations d'un fichier .tab pour l'analyse des temps
#'
#' Filtre les doses (EVID != 0 si la colonne existe), exclut la ligne
#' baseline TIME=0 (artefact $DESIGN), et ajoute TSTRAT=1 si absent.
#'
#' @param tab data.frame lu par read_tab()
#' @return data.frame nettoyé, sans ligne de dose
prepare_tab_obs <- function(tab) {
  if ("EVID" %in% names(tab)) tab <- dplyr::filter(tab, EVID == 0)
  # Drop TIME=0 baseline row PER ID (NONMEM $DESIGN artefact).
  # Safe when ID is absent — fallback to a single global group.
  if (nrow(tab) > 1L && "TIME" %in% names(tab)) {
    id_col <- if ("ID" %in% names(tab)) tab$ID else rep(1L, nrow(tab))
    first_row_mask <- !duplicated(id_col)
    drop_mask <- first_row_mask & tab$TIME == 0
    if (any(drop_mask)) tab <- tab[!drop_mask, , drop = FALSE]
  }
  if (!"TSTRAT" %in% names(tab)) tab$TSTRAT <- 1L
  tab
}
