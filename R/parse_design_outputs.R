# =============================================================================
# parse_design_outputs.R
# Lecture et extraction des sorties NONMEM $DESIGN (fichiers .ext et .shk)
#
# Fonctions principales :
#   read_ext()         — Lire le fichier .ext (paramètres + OBJ par itération)
#   read_shk()         — Lire le fichier .shk (shrinkages, TYPE 11 = RELATIVEINF)
#
# Fonctions d'extraction :
#   get_final_params() — Paramètres finaux depuis .ext
#   get_se()           — Erreurs standard prédites par la FIM depuis .ext
#   get_ofv()          — Valeur de l'OFV final (critère d'optimalité)
#   get_rse()          — RSE (%) prédits : tibble param / estimate / se / rse_pct
#   get_relativeinf()  — Information relative (%) par ETA depuis .shk (TYPE 11)
#
# Fonction de synthèse :
#   summary_design()   — Affichage formaté des métriques clés d'un run $DESIGN
#
# Compatibilité : NONMEM 7.5+, R 4.1+, tidyverse
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
    mutate(distinct(dat), table_no = tbl_no, .before = 1L)
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
#' Filtre les doses (EVID != 0 si la colonne existe), exclut la première
#' ligne (dose initiale TIME=0), et ajoute TSTRAT=1 si absent.
#'
#' @param tab data.frame lu par read_tab()
#' @return data.frame nettoyé, sans ligne de dose
prepare_tab_obs <- function(tab) {
  if ("EVID" %in% names(tab)) tab <- dplyr::filter(tab, EVID == 0)
  if (nrow(tab) > 1L) tab <- tab[-1L, , drop = FALSE]
  if (!"TSTRAT" %in% names(tab)) tab$TSTRAT <- 1L
  tab
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
# Fonctions d'extraction
# =============================================================================

#' Extraire les paramètres finaux depuis read_ext()
#' @param ext      Tibble retourné par read_ext()
#' @param table_no Numéro de table (défaut : dernier)
#' @export
get_final_params <- function(ext, table_no = NULL) {
  tbl <- table_no %||% max(ext$table_no)
  ext |> filter(type == "final", .data$table_no == tbl)
}

#' Extraire les erreurs standard prédites par la FIM
#' @param ext      Tibble retourné par read_ext()
#' @param table_no Numéro de table (défaut : dernier)
#' @export
get_se <- function(ext, table_no = NULL) {
  tbl <- table_no %||% max(ext$table_no)
  result <- ext |> filter(type == "se", .data$table_no == tbl)
  if (nrow(result) == 0L) return(tibble())
  result
}

#' Extraire la valeur du critère d'optimalité final
#' @param ext      Tibble retourné par read_ext()
#' @param table_no Numéro de table (défaut : dernier)
#' @export
get_ofv <- function(ext, table_no = NULL) {
  result <- get_final_params(ext, table_no)
  if (nrow(result) == 0L) return(NA_real_)
  result$OBJ
}

#' Calculer les RSE (%) prédits par la FIM
#'
#' RSE = |SE / estimate| × 100. Les paramètres fixés (SE = NA) sont exclus.
#'
#' @param ext      Tibble retourné par read_ext()
#' @param table_no Numéro de table (défaut : dernier)
#' @return Tibble long : param, estimate, se, rse_pct
#' @export
get_rse <- function(ext, table_no = NULL) {
  tbl <- table_no %||% max(ext$table_no)

  fp <- get_final_params(ext, tbl)
  if (nrow(fp) == 0L) return(tibble(param = character(), estimate = numeric(), se = numeric(), rse_pct = numeric()))

  params <- fp |>
    select(-c(table_no, type, ITERATION, OBJ)) |>
    pivot_longer(everything(), names_to = "param", values_to = "estimate")

  se_row <- get_se(ext, tbl)
  if (nrow(se_row) == 0L) return(tibble(param = character(), estimate = numeric(), se = numeric(), rse_pct = numeric()))

  se_vals <- se_row |>
    select(-c(table_no, type, ITERATION, OBJ)) |>
    pivot_longer(everything(), names_to = "param", values_to = "se")

  left_join(params, se_vals, by = "param") |>
    filter(!is.na(estimate), !is.na(se)) |>
    mutate(rse_pct = if_else(abs(estimate) < 1e-12, NA_real_, abs(se / estimate) * 100))
}

#' Extraire les informations relatives (%) depuis read_shk()
#'
#' @param shk      Tibble retourné par read_shk()
#' @param table_no Numéro de table (défaut : dernier)
#' @return Tibble : eta, relativeinf_pct
#' @export
get_relativeinf <- function(shk, table_no = NULL) {
  tbl <- table_no %||% max(shk$table_no)
  shk |>
    filter(.data$table_no == tbl, type_id == 11L) |>
    select(-c(table_no, type_id, subpop)) |>
    pivot_longer(everything(), names_to = "eta", values_to = "relativeinf_pct")
}

#' Extraire les shrinkages EBV (TYPE 6 = EBVSHRINKSD) depuis read_shk()
#'
#' TYPE 6 = EBVSHRINKSD : meaningful en contexte $DESIGN.
#' TYPE 4 = ETASHRINKSD : toujours 100% en $DESIGN (pas de donnees reelles).
#'
#' @param shk      Tibble retourne par read_shk()
#' @param table_no Numero de table (defaut : dernier)
#' @return Tibble : eta, shrinkage_pct
#' @export
get_shrinkage <- function(shk, table_no = NULL) {
  tbl <- table_no %||% max(shk$table_no)
  shk |>
    filter(.data$table_no == tbl, type_id == 6L) |>
    select(-c(table_no, type_id, subpop)) |>
    pivot_longer(everything(), names_to = "eta", values_to = "shrinkage_pct")
}


# =============================================================================
# summary_design() — Résumé formaté d'un run $DESIGN
# =============================================================================

#' Afficher un résumé structuré d'un run NONMEM $DESIGN
#'
#' Combine les RSE prédits (.ext) et les informations relatives (.shk) en un
#' tableau lisible par paramètre, avec un code couleur textuel pour la qualité.
#'
#' Codes RSE : [+] < 20% (bon)  [~] 20–50% (acceptable)  [!] > 50% (médiocre)
#'
#' @param ext          Tibble retourné par read_ext()
#' @param shk          Tibble retourné par read_shk() (optionnel, pour RELATIVEINF)
#' @param table_no     Numéro de table (défaut : dernier)
#' @param run_name     Nom du run pour l'en-tête (optionnel)
#' @param param_labels Vecteur nommé pour renommer les THETAs
#'                     Ex : c(THETA1 = "CL", THETA2 = "V", THETA3 = "KA")
#' @export
summary_design <- function(ext, shk = NULL, table_no = NULL,
                           run_name = NULL, param_labels = NULL) {

  tbl   <- table_no %||% max(ext$table_no)
  ofv   <- get_ofv(ext, tbl)
  rse   <- get_rse(ext, tbl)
  n_tbl <- max(ext$table_no)

  # RELATIVEINF depuis .shk (TYPE 11), indexé par position ETA
  ri <- if (!is.null(shk)) get_relativeinf(shk, tbl) else tibble(eta = character(), relativeinf_pct = numeric())

  # Séparation par type de paramètre (AVANT renommage)
  thetas     <- rse |> filter(str_starts(param, "THETA"))
  omega_diag <- rse |> filter(str_detect(param, "^OMEGA\\((\\d+),\\1\\)$"))
  omega_off  <- rse |> filter(str_starts(param, "OMEGA"), !str_detect(param, "^OMEGA\\((\\d+),\\1\\)$"))
  sigmas_d   <- rse |> filter(str_detect(param, "^SIGMA\\((\\d+),\\1\\)$"))

  # Renommage optionnel des THETAs (appliqué après filtrage)
  if (!is.null(param_labels)) {
    thetas <- thetas |>
      mutate(param = if_else(param %in% names(param_labels), param_labels[param], param))
  }

  # Joindre RELATIVEINF aux OMEGA diagonaux (ETAi ↔ i-ème OMEGA diagonal)
  if (nrow(omega_diag) > 0 && nrow(ri) > 0) {
    omega_diag <- omega_diag |>
      mutate(eta_idx = row_number()) |>
      left_join(
        ri |> mutate(eta_idx = as.integer(str_extract(eta, "\\d+"))),
        by = "eta_idx"
      )
  } else {
    omega_diag <- omega_diag |> mutate(relativeinf_pct = NA_real_)
  }

  # ── Helpers d'affichage ──────────────────────────────────────────────────
  rse_badge <- function(x) {
    case_when(
      is.na(x) ~ "    ",
      x < 20   ~ "[+] ",
      x < 50   ~ "[~] ",
      TRUE     ~ "[!] "
    )
  }

  hr_thick <- function(n = 60) cat(strrep("\u2550", n), "\n")
  hr_thin  <- function(n = 60) cat(strrep("\u2500", n), "\n")
  section  <- function(title) { cat("\n\u2500\u2500 ", title, " ", strrep("\u2500", max(0, 55 - nchar(title))), "\n", sep = "") }

  print_params <- function(dat, with_ri = FALSE) {
    if (nrow(dat) == 0L) { cat("  (aucun paramètre estimable)\n"); return(invisible(NULL)) }
    if (with_ri) {
      cat(sprintf("  %-16s %11s %11s %8s %5s %10s\n",
                  "Paramètre", "Estimé", "SE (FIM)", "RSE (%)", "", "RelInf (%)"))
    } else {
      cat(sprintf("  %-16s %11s %11s %8s\n",
                  "Paramètre", "Estimé", "SE (FIM)", "RSE (%)"))
    }
    hr_thin(if (with_ri) 70 else 58)
    for (i in seq_len(nrow(dat))) {
      r   <- dat[i, ]
      bdg <- rse_badge(r$rse_pct)
      if (with_ri) {
        ri_str <- if (!is.na(r$relativeinf_pct)) sprintf("%9.1f%%", r$relativeinf_pct) else "         "
        cat(sprintf("  %-16s %11.4g %11.4g %7.1f%% %s %s\n",
                    r$param, r$estimate, r$se, r$rse_pct, bdg, ri_str))
      } else {
        cat(sprintf("  %-16s %11.4g %11.4g %7.1f%% %s\n",
                    r$param, r$estimate, r$se, r$rse_pct, bdg))
      }
    }
  }

  # ── Affichage ────────────────────────────────────────────────────────────
  cat("\n")
  hr_thick(70)
  run_label <- if (!is.null(run_name)) run_name else "NONMEM $DESIGN"
  cat(sprintf("  DESIGN SUMMARY — %s  (Table %d/%d)\n", run_label, tbl, n_tbl))
  hr_thick(70)
  cat(sprintf("  OFV  : %g\n", ofv))
  cat("  RSE  : [+] < 20%   [~] 20-50%   [!] > 50%\n")

  section("Effets fixes (THETA)")
  print_params(thetas, with_ri = FALSE)

  section("Effets aléatoires — OMEGA diagonal")
  print_params(omega_diag |> select(param, estimate, se, rse_pct, relativeinf_pct),
               with_ri = TRUE)

  if (nrow(omega_off) > 0) {
    section("Effets aléatoires — OMEGA hors-diagonal")
    print_params(omega_off, with_ri = FALSE)
  }

  section("Variance résiduelle — SIGMA diagonal")
  print_params(sigmas_d, with_ri = FALSE)

  cat("\n")
  invisible(list(thetas = thetas, omega_diag = omega_diag, sigmas = sigmas_d))
}


# =============================================================================
# read_coi() — Lecture du fichier .coi (Fisher Information Matrix nommée)
# =============================================================================

#' Lire un fichier .coi NONMEM $DESIGN
#'
#' Le .coi contient la FIM complète sous forme de matrice nommée.
#' Format : TABLE NO header, puis NAME + param names, puis lignes nom + valeurs.
#'
#' @param file     Chemin vers le fichier .coi
#' @param table_no Numéro de table à lire (défaut : 1)
#' @return Matrice numérique nommée (symétrique)
#' @export
read_coi <- function(file, table_no = 1L) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- read_lines(file, progress = FALSE)

  # Trouver les blocs TABLE NO.
  table_idx <- which(str_starts(lines, "TABLE NO\\."))
  if (length(table_idx) == 0L) stop("Aucun bloc TABLE NO. dans : ", file)

  # Sélectionner le bon bloc
  tbl_i <- which(as.integer(str_extract(lines[table_idx], "\\d+")) == table_no)[1L]
  if (is.na(tbl_i)) stop("TABLE NO. ", table_no, " introuvable dans : ", file)

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
      warning("read_coi() ligne ", i, " : ", length(vals), " valeurs au lieu de ", n, " attendues — ligne ignorée")
    }
  }

  mat
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
#' @param table_no Numéro de table à lire (défaut : 1)
#' @return Matrice numérique nommée (symétrique)
#' @export
read_clt <- function(file, table_no = 1L) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- read_lines(file, progress = FALSE)

  table_idx <- which(str_starts(lines, "TABLE NO\\."))
  if (length(table_idx) == 0L) stop("Aucun bloc TABLE NO. dans : ", file)

  tbl_i <- which(as.integer(str_extract(lines[table_idx], "\\d+")) == table_no)[1L]
  if (is.na(tbl_i)) stop("TABLE NO. ", table_no, " introuvable dans : ", file)

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
# get_eigenvalues() — Eigenvalues depuis .ext
# =============================================================================

#' Extraire les eigenvalues de la matrice de corrélation depuis read_ext()
#'
#' @param ext      Tibble retourné par read_ext()
#' @param table_no Numéro de table (défaut : dernier)
#' @return Tibble : index, eigenvalue
#' @export
get_eigenvalues <- function(ext, table_no = NULL) {
  tbl <- table_no %||% max(ext$table_no)
  row <- ext |>
    filter(type == "eigenvalues", .data$table_no == tbl) |>
    select(-c(table_no, type, ITERATION, OBJ))

  if (nrow(row) == 0L) return(tibble(index = integer(), eigenvalue = numeric()))

  vals <- as.numeric(row[1L, ])
  vals <- vals[!is.na(vals)]

  tibble(
    index      = seq_along(vals),
    eigenvalue = vals
  )
}


# =============================================================================
# get_condition_number() — Condition number depuis .ext
# =============================================================================

#' Extraire le condition number et min/max eigenvalues depuis read_ext()
#'
#' @param ext      Tibble retourné par read_ext()
#' @param table_no Numéro de table (défaut : dernier)
#' @return Liste : condition_number, min_eigenvalue, max_eigenvalue
#' @export
get_condition_number <- function(ext, table_no = NULL) {
  tbl <- table_no %||% max(ext$table_no)
  row <- ext |>
    filter(type == "condition", .data$table_no == tbl) |>
    select(-c(table_no, type, ITERATION, OBJ))

  if (nrow(row) == 0L) return(list(condition_number = NA, min_eigenvalue = NA, max_eigenvalue = NA))

  vals <- as.numeric(row[1L, ])
  vals <- vals[!is.na(vals)]

  # NONMEM stores: condition_number in first non-NA, then eigenvalue range
  list(
    condition_number = vals[1L] %||% NA_real_,
    min_eigenvalue   = vals[2L] %||% NA_real_,
    max_eigenvalue   = vals[3L] %||% NA_real_
  )
}


# =============================================================================
# get_d_criterion() — D-critère d'optimalité
# =============================================================================

#' Calculer le D-critère à partir de l'OFV et du nombre de paramètres
#'
#' D-criterion = exp(-OFV / n_params)
#' Interprétation : plus le D-critère est élevé, meilleure est la capacité du
#' design à estimer les paramètres.
#'
#' @param ofv      Valeur de l'OFV (critère d'optimalité, typiquement -log(det(FIM)))
#' @param n_params Nombre de paramètres estimables
#' @return Numérique : D-critère
#' @export
get_d_criterion <- function(ofv, n_params) {
  if (is.na(ofv) || n_params <= 0L) return(NA_real_)
  exp(-ofv / n_params)
}


# =============================================================================
# read_prior_nwpri() — Parser $PRIOR NWPRI depuis un .ctl
# =============================================================================

#' Parser le bloc $PRIOR NWPRI depuis un fichier .ctl NONMEM
#'
#' @param file Chemin vers le fichier .ctl
#' @return Liste avec has_prior, plev, thetap, thetapv, omega_df, raw_prior_line
#' @export
read_prior_nwpri <- function(file) {
  if (!file.exists(file)) stop("Fichier introuvable : ", file)

  lines <- readr::read_lines(file, progress = FALSE)
  lines_clean <- str_replace(lines, ";.*$", "")

  result <- list(
    has_prior = FALSE, plev = NA_real_,
    thetap = numeric(), thetapv = NULL,
    omega_df = NA_real_, raw_prior_line = ""
  )

  prior_idx <- which(str_detect(lines_clean, "^\\s*\\$PRIOR\\s+NWPRI"))
  if (length(prior_idx) == 0L) return(result)

  result$has_prior <- TRUE
  prior_line <- lines_clean[prior_idx[1]]
  result$raw_prior_line <- trimws(lines[prior_idx[1]])

  plev_match <- str_extract(prior_line, "PLEV\\s*=\\s*[0-9.]+")
  if (!is.na(plev_match)) {
    result$plev <- as.numeric(str_extract(plev_match, "[0-9.]+$"))
  }

  # Find $THETAP block
  thetap_idx <- which(str_detect(lines_clean, "^\\s*\\$THETAP\\b"))
  if (length(thetap_idx) > 0) {
    tp_line <- lines_clean[thetap_idx[1]]
    vals <- str_extract_all(tp_line, "\\(\\s*(-?[0-9.eEdD]+)")[[1]]
    vals <- as.numeric(str_extract(vals, "-?[0-9.eEdD]+"))
    result$thetap <- vals
  }

  # Find $THETAPV BLOCK
  thetapv_idx <- which(str_detect(lines_clean, "^\\s*\\$THETAPV\\b"))
  if (length(thetapv_idx) > 0) {
    dim_match <- str_extract(lines_clean[thetapv_idx[1]], "BLOCK\\s*\\(\\s*(\\d+)\\s*\\)")
    if (!is.na(dim_match)) {
      n <- as.integer(str_extract(dim_match, "\\d+"))
      all_vals <- numeric()
      i <- thetapv_idx[1] + 1
      expected <- n * (n + 1) / 2
      while (length(all_vals) < expected && i <= length(lines_clean)) {
        if (str_detect(lines_clean[i], "^\\s*\\$")) break
        nums <- str_extract_all(lines_clean[i], "-?[0-9.eEdD]+(?:[eEdD][+-]?\\d+)?")[[1]]
        nums <- nums[!nums %in% c("FIX", "FIXED")]
        all_vals <- c(all_vals, as.numeric(nums))
        i <- i + 1
      }
      mat <- matrix(0, n, n)
      idx <- 1
      for (row in seq_len(n)) {
        for (col in seq_len(row)) {
          if (idx <= length(all_vals)) {
            mat[row, col] <- all_vals[idx]
            mat[col, row] <- all_vals[idx]
            idx <- idx + 1
          }
        }
      }
      result$thetapv <- mat
    }
  }

  # Find $OMEGAPD
  omegapd_idx <- which(str_detect(lines_clean, "^\\s*\\$OMEGAPD\\b"))
  if (length(omegapd_idx) > 0) {
    df_vals <- str_extract_all(lines_clean[omegapd_idx[1]], "[0-9.]+")[[1]]
    if (length(df_vals) > 0) result$omega_df <- as.numeric(df_vals[1])
  }

  result
}


# =============================================================================
# get_cor_matrix() — Matrice de correlation depuis la FIM
# =============================================================================

#' Calculer la matrice de correlation a partir de la FIM
#'
#' @param fim_matrix Matrice numerique nommee (FIM)
#' @return Matrice de correlation, ou NULL si FIM singuliere
#' @export
get_cor_matrix <- function(fim_matrix) {
  if (is.null(fim_matrix) || nrow(fim_matrix) == 0L) return(NULL)
  nonzero <- diag(fim_matrix) != 0
  if (sum(nonzero) < 2L) return(NULL)
  fim_sub <- fim_matrix[nonzero, nonzero]
  tryCatch({
    vcov <- solve(fim_sub)
    cov2cor(vcov)
  }, error = function(e) NULL)
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

# =============================================================================
# parse_theta_labels — Extrait les noms THETA depuis un control stream NONMEM
# =============================================================================
#' Extrait les labels THETA depuis les lignes d'un control stream NONMEM.
#'
#' Supporte les formats courants :
#'   $THETA 0.15 ;[CL]
#'   $THETA 0.15 ; CL
#'   $THETA 0.15 ;CL
#'   THETA multi-lignes (continuation apres $THETA)
#'
#' @param lines Character vector de lignes du fichier .ctl/.mod/.con
#' @return Named character vector c("THETA1"="CL", "THETA2"="V", ...) ou NULL
#' @export
parse_theta_labels <- function(lines) {
  theta_start <- which(str_detect(lines, "^\\$THETA\\b"))
  if (length(theta_start) == 0L) return(NULL)

  next_block <- which(str_detect(lines, "^\\$") & seq_along(lines) > theta_start[1])
  theta_end  <- if (length(next_block) > 0L) next_block[1] - 1L else length(lines)
  theta_lines <- lines[theta_start[1]:theta_end]

  labels <- character(0)
  for (ln in theta_lines) {
    comment_match <- regmatches(ln, regexpr(";\\s*\\[?([A-Za-z][A-Za-z0-9_]*)\\]?", ln))
    if (length(comment_match) == 0L || nchar(comment_match) == 0L) next
    lbl <- str_trim(sub("^;\\s*\\[?([A-Za-z][A-Za-z0-9_]*)\\]?.*", "\\1", comment_match))
    if (nchar(lbl) > 0L) labels <- c(labels, lbl)
  }

  if (length(labels) == 0L) return(NULL)
  setNames(labels, paste0("THETA", seq_along(labels)))
}

# =============================================================================
# parse_design_summary — Extrait un resume des arguments $DESIGN pour nommage
# =============================================================================
#' Construit un label court des options $DESIGN cles pour nommer une run.
#'
#' Exemple : "$DESIGN FIMTYPE=1 APPROX=FO VARCROSS=1 MAXEVAL=0"
#'           -> "FT=1/FO/VC=1 (eval)"
#'
#' @param lines Character vector de lignes du fichier .ctl/.mod/.con
#' @return Character scalar, ou NULL si aucun $DESIGN trouve
#' @export
parse_design_summary <- function(lines) {
  design_start <- which(str_detect(lines, "^\\$DESIGN\\b"))
  if (length(design_start) == 0L) return(NULL)

  design_lines <- character(0)
  for (i in seq(design_start[1], length(lines))) {
    ln <- lines[i]
    if (i > design_start[1] && str_detect(ln, "^\\$")) break
    design_lines <- c(design_lines, ln)
  }
  block <- paste(design_lines, collapse = " ")

  get_arg <- function(key) {
    m <- regmatches(block, regexpr(paste0("\\b", key, "\\s*=\\s*([A-Za-z0-9]+)"), block, perl = TRUE))
    if (length(m) == 0L || nchar(m) == 0L) return(NULL)
    sub(paste0(".*", key, "\\s*=\\s*"), "", m)
  }

  parts <- character(0)

  fimtype <- get_arg("FIMTYPE") %||% get_arg("FIMDIAG")
  if (!is.null(fimtype)) parts <- c(parts, paste0("FT=", fimtype))

  ofvtype <- get_arg("OFVTYPE")
  if (!is.null(ofvtype) && ofvtype != "1") parts <- c(parts, paste0("OFV=", ofvtype))

  approx <- get_arg("APPROX")
  if (!is.null(approx)) parts <- c(parts, approx) else parts <- c(parts, "FO")

  vc <- get_arg("VARCROSS")
  if (!is.null(vc) && vc != "0") parts <- c(parts, paste0("VC=", vc))

  desel <- get_arg("DESEL")
  if (!is.null(desel)) parts <- c(parts, paste0("DESEL=", desel))

  maxeval <- get_arg("MAXEVAL")
  suffix <- if (!is.null(maxeval) && maxeval == "0") " (eval)" else if (!is.null(maxeval)) " (optim)" else ""

  if (length(parts) == 0L) return(NULL)
  paste0(paste(parts, collapse = "/"), suffix)
}

# =============================================================================
# parse_cmt_labels — Extrait les noms de compartiments depuis $MODEL
# =============================================================================
#' Extrait les labels de compartiments depuis un control stream NONMEM.
#'
#' Supporte le format :
#'   $MODEL COMP=(DEPOT,DEFDOSE) COMP=(CENTRAL,DEFOBS) COMP=(EFFECT)
#'   $MODEL COMP=DEPOT COMP=CENTRAL
#'
#' @param lines Character vector de lignes du fichier .ctl/.mod/.con
#' @return Named character vector c("1"="DEPOT", "2"="CENTRAL", ...) ou NULL
#' @export
parse_cmt_labels <- function(lines) {
  model_start <- which(str_detect(lines, "^\\$MODEL\\b"))
  if (length(model_start) == 0L) return(NULL)

  next_block <- which(str_detect(lines, "^\\$") & seq_along(lines) > model_start[1])
  model_end  <- if (length(next_block) > 0L) next_block[1] - 1L else length(lines)
  block      <- paste(lines[model_start[1]:model_end], collapse = " ")

  # Extraire tous les COMP=(NOM,...) ou COMP=NOM
  matches <- gregexpr("COMP\\s*=\\s*\\(?([A-Za-z][A-Za-z0-9_]*)", block, perl = TRUE)
  m       <- regmatches(block, matches)[[1]]
  if (length(m) == 0L) return(NULL)

  names_vec <- sub(".*COMP\\s*=\\s*\\(?", "", m)
  names_vec <- trimws(names_vec)
  names_vec <- names_vec[nchar(names_vec) > 0L]
  if (length(names_vec) == 0L) return(NULL)

  setNames(names_vec, as.character(seq_along(names_vec)))
}
