# =============================================================================
# design_io.R
# Lecteurs de fichiers de sortie NONMEM (.ext, .shk, .coi, .cov, .cor, .clt, .tab, .cpu)
#
# Contenu :
#   read_cpu()           — Temps CPU depuis .cpu
#   format_cpu()         — Formatage temps CPU
#   read_ext()           — Parametres + OBJ par iteration depuis .ext
#   read_shk()           — Shrinkages depuis .shk
#   .read_named_matrix() — Parser interne pour .coi/.cov/.cor
#   read_coi()           — FIM nommee depuis .coi
#   read_cov()           — Variance-covariance depuis .cov
#   read_cor()           — Correlation + SE diagonale depuis .cor
#   read_clt()           — FIM triangulaire inferieure depuis .clt
#   read_tab()           — Temps/doses optimises depuis .tab
#   read_summary_tab()   — Parser summary.tab ($SIM TRUE=PRIOR)
#
# Prerequis : source('design_utils.R')
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)


# =============================================================================
# read_cpu() — Lecture du fichier .cpu NONMEM (temps de calcul)
# =============================================================================

#' Lire un fichier .cpu NONMEM
#'
#' @param file Chemin vers le fichier .cpu
#' @return Numeric scalaire : temps de calcul en secondes, ou NA_real_
#' @export
read_cpu <- function(file) {
  if (!file.exists(file)) return(NA_real_)
  val <- tryCatch(
    as.numeric(trimws(readLines(file, n = 1L, warn = FALSE))),
    warning = function(w) NA_real_,
    error   = function(e) NA_real_
  )
  if (length(val) != 1L || is.na(val)) NA_real_ else val
}

#' Formater un temps CPU en chaine lisible
#'
#' @param secs Numeric : temps en secondes
#' @return Character : "Xs" si <60s, "Xm Ys" sinon
#' @export
format_cpu <- function(secs) {
  if (is.na(secs)) return(NA_character_)
  if (secs < 60) return(paste0(round(secs, 1), "s"))
  paste0(floor(secs / 60), "m ", round(secs %% 60), "s")
}


# =============================================================================
# read_ext() — Lecture du fichier .ext NONMEM $DESIGN
# =============================================================================

#' Lire un fichier .ext NONMEM $DESIGN
#'
#' @param file     Chemin vers le fichier .ext
#' @param sentinel Valeur sentinelle pour paramètres fixes/non-estimables (défaut : 1e10)
#'
#' @return Tibble avec colonnes : table_no, type, ITERATION, [paramètres], OBJ
#'   - type : "iteration" | "final" | "se" | "eigenvalues" | "condition" |
#'             "sd_corr" | "se_sd_corr" | "fixed_flags" | "termination" | "gradient"
#'   - Valeurs sentinelles (1e10) remplacées par NA
#' @export
read_ext <- function(file, sentinel = 1e10) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- read_lines(file, progress = FALSE)
  blocks <- .parse_table_blocks(lines, "^\\s*ITERATION\\b")
  if (length(blocks) == 0L) stop("Aucun bloc TABLE NO. valide dans : ", file)

  # Lookup vectorisé : itération spéciale → type (compatible dplyr < 1.1)
  # as.numeric() ensures consistent character representation regardless of
  # integer vs double storage in .EXT_ITER (e.g. "-1e+09" not "-1000000000")
  .iter_to_type <- setNames(names(.EXT_ITER), as.character(as.numeric(unlist(.EXT_ITER))))

  result <- map(blocks, function(dat) {
    types <- .iter_to_type[as.character(dat$ITERATION)]
    types[is.na(types)] <- "iteration"

    dat |>
      mutate(type = types, .after = table_no) |>
      mutate(
        across(
          -c(table_no, type, ITERATION, OBJ),
          ~ if_else(abs(.x - sentinel) < 1, NA_real_, .x)
        )
      )
  }) |>
    bind_rows()

  # Stocker les lignes brutes comme attribut pour detect_criterion() en aval
  attr(result, "ext_lines") <- lines
  result
}


# =============================================================================
# read_shk() — Lecture du fichier .shk NONMEM $DESIGN
# =============================================================================

#' Lire un fichier .shk NONMEM $DESIGN
#'
#' Le type 11 contient RELATIVEINF(%) — information relative par ETA (%).
#'
#' Types disponibles :
#'   1=ETABAR, 2=ETABARSE, 3=??, 4=ETASHRINKSD, 5=ETASHRINKVR,
#'   6=EBVSHRINKSD, 7=??, 8=ETASHRINKSD(nm74), 9=EBVSHRINKVR,
#'   10=ETASHRINKVR(nm74), 11=RELATIVEINF(%)
#'
#' @param file Chemin vers le fichier .shk
#' @return Tibble avec colonnes : table_no, type_id, subpop, ETA1, ETA2, ...
#' @export
read_shk <- function(file) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- read_lines(file, progress = FALSE)
  blocks <- .parse_table_blocks(lines, "^\\s*TYPE\\b")
  if (length(blocks) == 0L) stop("Aucun bloc TYPE valide dans : ", file)

  map(blocks, function(dat) {
    # Renommer TYPE→type_id, SUBPOP→subpop, ETA(x)→ETAx
    dat |>
      rename(type_id = TYPE, subpop = SUBPOP) |>
      rename_with(
        ~ str_replace_all(.x, "ETA\\(\\s*(\\d+)\\s*\\)", "ETA\\1"),
        starts_with("ETA")
      ) |>
      mutate(type_id = as.integer(type_id), subpop = as.integer(subpop))
  }) |>
    bind_rows()
}


# =============================================================================
# .read_named_matrix() — Parser interne pour .coi, .cov, .cor
# =============================================================================
#
# Format commun NONMEM : TABLE NO. header, NAME + param names, puis lignes
# nom + valeurs. Utilise par read_coi(), read_cov(), read_cor().

.read_named_matrix <- function(file, table_no = 1L, caller = "read_named_matrix") {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- read_lines(file, progress = FALSE)

  # Trouver les blocs TABLE NO.
  table_idx <- which(str_starts(lines, "TABLE NO\\."))
  if (length(table_idx) == 0L) stop("Aucun bloc TABLE NO. dans : ", file)

  # Selectionner le bon bloc (fallback sur la derniere table si introuvable)
  tbl_nums <- as.integer(str_extract(lines[table_idx], "\\d+"))
  tbl_i <- which(tbl_nums == table_no)[1L]
  if (is.na(tbl_i)) {
    warning(
      caller, "(): TABLE NO. ", table_no, " introuvable dans : ", basename(file),
      " -- repli sur la derniere table (TABLE NO. ", tbl_nums[length(tbl_nums)], ")."
    )
    tbl_i <- length(table_idx)
  }

  start <- table_idx[tbl_i]
  end   <- if (tbl_i < length(table_idx)) table_idx[tbl_i + 1L] - 1L else length(lines)
  block <- lines[(start + 1L):end]  # skip TABLE NO. line

  # Ligne 1 = header : NAME THETA1 THETA2 ... OMEGA(1,1) ...
  header_tokens <- str_split(str_trim(block[1L]), "\\s+")[[1L]]
  param_names   <- header_tokens[-1L]  # drop "NAME"
  n <- length(param_names)

  # Lignes suivantes : row_name val1 val2 ... valn
  data_lines <- block[2L:length(block)]
  data_lines <- data_lines[nzchar(str_trim(data_lines))]

  mat <- matrix(NA_real_, nrow = n, ncol = n,
                dimnames = list(param_names, param_names))

  for (i in seq_along(data_lines)) {
    tokens <- str_split(str_trim(data_lines[i]), "\\s+")[[1L]]
    vals   <- as.numeric(tokens[-1L])  # drop row name
    if (length(vals) == n) {
      mat[i, ] <- vals
    } else {
      warning(caller, "() ligne ", i, " : ", length(vals),
              " valeurs au lieu de ", n, " attendues -- ligne ignoree")
    }
  }

  mat
}


# =============================================================================
# read_coi() — Lecture du fichier .coi (Fisher Information Matrix nommee)
# =============================================================================

#' Lire un fichier .coi NONMEM $DESIGN
#'
#' Le .coi contient la FIM complete sous forme de matrice nommee.
#'
#' @param file     Chemin vers le fichier .coi
#' @param table_no Numero de table a lire (defaut : 1). Si introuvable,
#'   la derniere table du fichier est utilisee avec un avertissement.
#' @return Matrice numerique nommee (symetrique)
#' @export
read_coi <- function(file, table_no = 1L) {
  .read_named_matrix(file, table_no, caller = "read_coi")
}


# =============================================================================
# read_cov() — Lecture du fichier .cov (variance-covariance)
# =============================================================================

#' Lire un fichier .cov NONMEM $DESIGN
#'
#' Le .cov contient la matrice variance-covariance calculee par NONMEM
#' (inverse de la FIM). Meme format que .coi.
#'
#' @param file     Chemin vers le fichier .cov
#' @param table_no Numero de table a lire (defaut : 1)
#' @return Matrice numerique nommee (symetrique)
#' @export
read_cov <- function(file, table_no = 1L) {
  .read_named_matrix(file, table_no, caller = "read_cov")
}


# =============================================================================
# read_cor() — Lecture du fichier .cor (correlation + SE sur la diagonale)
# =============================================================================

#' Lire un fichier .cor NONMEM $DESIGN
#'
#' Le .cor contient la matrice de correlation calculee par NONMEM.
#' Attention : la diagonale contient les SE (pas 1.0).
#' Off-diagonale = correlations entre parametres.
#'
#' @param file     Chemin vers le fichier .cor
#' @param table_no Numero de table a lire (defaut : 1)
#' @return Matrice numerique nommee (diag = SE, off-diag = correlations)
#' @export
read_cor <- function(file, table_no = 1L) {
  .read_named_matrix(file, table_no, caller = "read_cor")
}


# =============================================================================
# read_clt() — Lecture du fichier .clt (FIM triangulaire inférieure)
# =============================================================================

#' Lire un fichier .clt NONMEM $DESIGN
#'
#' Le .clt contient la FIM en format triangulaire inférieur (ordre TOSL).
#' Format : TABLE NO header, param names, puis lignes triangulaires (1, 2, 3... vals).
#'
#' @param file     Chemin vers le fichier .clt
#' @param table_no Numéro de table à lire (défaut : 1). Si introuvable,
#'   la dernière table du fichier est utilisée avec un avertissement.
#' @return Matrice numérique nommée (symétrique)
#' @export
read_clt <- function(file, table_no = 1L) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- read_lines(file, progress = FALSE)

  table_idx <- which(str_starts(lines, "TABLE NO\\."))
  if (length(table_idx) == 0L) stop("Aucun bloc TABLE NO. dans : ", file)

  # Fallback sur la dernière table si introuvable
  tbl_nums <- as.integer(str_extract(lines[table_idx], "\\d+"))
  tbl_i <- which(tbl_nums == table_no)[1L]
  if (is.na(tbl_i)) {
    warning(
      "read_clt(): TABLE NO. ", table_no, " introuvable dans : ", basename(file),
      " -- repli sur la derniere table (TABLE NO. ", tbl_nums[length(tbl_nums)], ")."
    )
    tbl_i <- length(table_idx)
  }

  start <- table_idx[tbl_i]
  end   <- if (tbl_i < length(table_idx)) table_idx[tbl_i + 1L] - 1L else length(lines)
  block <- lines[(start + 1L):end]

  # Ligne 1 = noms des paramètres (ordre TOSL : THETA, OMEGA, SIGMA)
  param_names <- str_split(str_trim(block[1L]), "\\s+")[[1L]]
  n <- length(param_names)

  # Lignes suivantes : triangulaire inférieur (ligne i a i valeurs)
  data_lines <- block[2L:length(block)]
  data_lines <- data_lines[nzchar(str_trim(data_lines))]

  # Extraire toutes les valeurs
  all_vals <- numeric(0)
  for (line in data_lines) {
    vals <- as.numeric(str_split(str_trim(line), "\\s+")[[1L]])
    all_vals <- c(all_vals, vals)
  }

  # Reconstruire la matrice symétrique depuis le triangulaire inférieur
  expected_n <- n * (n + 1L) / 2L
  if (length(all_vals) != expected_n) {
    warning("Nombre de valeurs (", length(all_vals), ") != attendu (", expected_n, ")")
  }

  mat <- matrix(0, nrow = n, ncol = n,
                dimnames = list(param_names, param_names))
  idx <- 1L
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      if (idx <= length(all_vals)) {
        mat[i, j] <- all_vals[idx]
        mat[j, i] <- all_vals[idx]
        idx <- idx + 1L
      }
    }
  }

  mat
}


# =============================================================================
# read_tab() — Lecture du fichier .tab NONMEM $DESIGN
# =============================================================================

#' Lire un fichier .tab NONMEM $DESIGN
#'
#' Le .tab contient les temps/doses optimisés après $DESIGN.
#' Colonnes variables selon le modèle (ID, TIME, TSTRAT, IPRED, EVID...).
#'
#' @param file     Chemin vers le fichier .tab
#' @param table_no Numéro de table (défaut : NULL = toutes)
#' @return Tibble avec colonnes originales + table_no
#' @export
read_tab <- function(file, table_no = NULL) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- read_lines(file, progress = FALSE)

  # Détecter le pattern de la ligne de colonnes
  # Les .tab $DESIGN ont des colonnes commençant par ID, TIME, DOSE, etc.
  col_pattern <- "^\\s*(ID|TIME|DOSE|DV|PRED)\\b"
  blocks <- .parse_table_blocks(lines, col_pattern)

  if (length(blocks) == 0L) stop("Aucun bloc TABLE valide dans : ", file)

  result <- bind_rows(blocks)

  if (!is.null(table_no)) {
    result <- filter(result, .data$table_no == table_no)
  }

  result
}


# =============================================================================
# read_summary_tab() — Parser for summary.tab from $SIM TRUE=PRIOR
# =============================================================================

#' Parse a summary.tab file produced by the summary.exe Fortran program
#' (robust design with $SIM TRUE=PRIOR SUBPROB=N)
#'
#' Format: blocks of VARIABLE_NAME:\n Header row\n Data rows
#' Each block has: Row, Mean, STD, RSTD, Low, High, percentiles...
#'
#' @param file Path to the summary.tab file
#' @return List of tibbles, one per variable, with columns Row, Mean, STD, RSTD, 2.5%, 97.5%
#' @export
read_summary_tab <- function(file) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- readr::read_lines(file, progress = FALSE)

  # Find variable headers (lines ending with ":")
  var_idx <- which(str_detect(lines, "^[A-Za-z][A-Za-z0-9_]*:\\s*$"))
  if (length(var_idx) == 0L) return(NULL)

  result <- list()
  for (k in seq_along(var_idx)) {
    var_name <- str_replace(lines[var_idx[k]], ":\\s*$", "")
    header_line <- var_idx[k] + 1L

    # Find data lines (start with whitespace + number)
    data_start <- header_line + 1L
    data_end <- if (k < length(var_idx)) var_idx[k + 1L] - 1L else length(lines)

    data_lines <- lines[data_start:data_end]
    data_lines <- data_lines[str_detect(data_lines, "^\\s+\\d")]

    if (length(data_lines) == 0L) next

    # Parse header to get column names
    header_tokens <- str_split(str_trim(lines[header_line]), "\\s+")[[1]]

    # Parse data
    parsed <- lapply(data_lines, function(l) {
      as.numeric(str_split(str_trim(l), "\\s+")[[1]])
    })
    mat <- do.call(rbind, parsed)

    # Use header names, keeping only: Row, Mean, STD, RSTD, Low, High, 2.50%, 97.50%
    if (ncol(mat) >= length(header_tokens)) {
      colnames(mat) <- header_tokens[seq_len(ncol(mat))]
    }
    df <- as_tibble(mat, .name_repair = "minimal")

    # Keep key columns
    keep_cols <- intersect(c("Row", "Mean", "STD", "RSTD", "Low", "High", "2.50%", "97.50%"), names(df))
    if (length(keep_cols) > 0) df <- df[, keep_cols]

    result[[var_name]] <- df
  }

  result
}
