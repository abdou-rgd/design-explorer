# =============================================================================
# design_metrics.R
# Extraction de metriques et algebre FIM depuis les sorties NONMEM parsees
#
# Contenu :
#   get_final_params()        — Parametres finaux depuis .ext
#   get_se()                  — Erreurs standard predites par la FIM
#   get_ofv()                 — Valeur OFV finale
#   .param_type()             — Type de parametre depuis son nom NONMEM
#   get_rse()                 — RSE (%) predits
#   get_relativeinf()         — Information relative (%) par ETA
#   get_shrinkage()           — Shrinkage EBV (TYPE 6)
#   get_eigenvalues()         — Eigenvalues matrice de correlation
#   get_condition_number()    — Condition number + min/max eigenvalues
#   get_d_criterion()         — D-critere d'optimalite
#   get_robust_d_criterion()  — D-critere robuste (multi-subprob)
#   get_cor_matrix()          — Matrice de correlation depuis FIM
#   scale_fim()               — Mise a l'echelle FIM par taille d'echantillon
#   vcov_from_fim()           — Variance-covariance depuis FIM
#   compute_robust_summary()  — Reimplementation R de summary.f90
#
# Prerequis : source('design_io.R')
# =============================================================================

library(dplyr)
library(stringr)
library(purrr)
library(tidyr)


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

#' Determiner le type de parametre depuis son nom NONMEM
#'
#' @param param Vecteur de noms de parametres NONMEM
#' @return Vecteur de types : THETA, OMEGA (diag.), OMEGA (off-diag.), SIGMA (diag.), SIGMA (off-diag.), Autre
#' @export
.param_type <- function(param) {
  case_when(
    str_starts(param, "THETA")                              ~ "THETA",
    str_detect(param, "^OMEGA\\((\\d+),\\1\\)$")           ~ "OMEGA (diag.)",
    str_starts(param, "OMEGA")                             ~ "OMEGA (off-diag.)",
    str_detect(param, "^SIGMA\\((\\d+),\\1\\)$")           ~ "SIGMA (diag.)",
    str_starts(param, "SIGMA")                             ~ "SIGMA (off-diag.)",
    TRUE                                                   ~ "Autre"
  )
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
# get_robust_d_criterion() — D-critère robuste sur design Monte Carlo
# =============================================================================

#' Résumer le D-critère sur un design robuste (SUBPROB > 1)
#'
#' Approche standard (Nyberg et al., Bauer 2021) :
#'   D-critère robuste = exp(-mean(OFV_i) / p)  [= moyenne géométrique de det(FIM)^(1/p)]
#' Bornes : exp(-P90(OFV_i)/p) [P10 D-crit] et exp(-P10(OFV_i)/p) [P90 D-crit]
#' Note : OFV élevé <=> D-critère faible, donc les bornes OFV s'inversent.
#'
#' @param ext      Tibble retourné par read_ext() (multi-table)
#' @param n_params Nombre de paramètres estimables (depuis get_rse())
#' @return Liste : d_robust, d_p10, d_p90, ofv_mean, ofv_sd, n_subprob
#'         ou NULL si ext n'est pas multi-table ou n_params invalide
#' @export
get_robust_d_criterion <- function(ext, n_params) {
  if (is.null(ext) || is.na(n_params) || n_params <= 0L) return(NULL)

  tbl_nos <- sort(unique(ext$table_no))
  if (length(tbl_nos) <= 1L) return(NULL)

  ofv_vec <- vapply(tbl_nos, function(tbl) {
    val <- tryCatch(get_ofv(ext, tbl), error = function(e) NA_real_)
    if (length(val) == 0L) NA_real_ else val
  }, numeric(1))

  ofv_vec <- ofv_vec[!is.na(ofv_vec)]
  if (length(ofv_vec) < 2L) return(NULL)

  list(
    d_robust  = exp(-mean(ofv_vec)               / n_params),
    d_p10     = exp(-quantile(ofv_vec, 0.90)[[1]] / n_params),  # P90 OFV -> P10 D-crit
    d_p90     = exp(-quantile(ofv_vec, 0.10)[[1]] / n_params),  # P10 OFV -> P90 D-crit
    ofv_mean  = mean(ofv_vec),
    ofv_sd    = sd(ofv_vec),
    n_subprob = length(ofv_vec)
  )
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
    if (is.na(rcond(fim_sub)) || rcond(fim_sub) < 1e-15) return(NULL)
    vcov <- chol2inv(chol(fim_sub))
    dimnames(vcov) <- dimnames(fim_sub)
    cov2cor(vcov)
  }, error = function(e) NULL)
}


# =============================================================================
# scale_fim() — Mise a l'echelle de la FIM par taille d'echantillon
# =============================================================================

#' Mettre a l'echelle la FIM pour une nouvelle taille d'echantillon
#'
#' FIM est additive par sujet : FIM(N2) = FIM(N1) * N2/N1.
#' Utile pour le calcul de NSN (Number of Subjects Needed).
#'
#' @param fim    Matrice numerique nommee (FIM)
#' @param n_from Taille d'echantillon d'origine (GROUPSIZE du run)
#' @param n_to   Taille d'echantillon cible
#' @return Matrice FIM mise a l'echelle
#' @export
scale_fim <- function(fim, n_from, n_to) {
  if (is.null(fim)) return(NULL)
  if (n_from <= 0) stop("n_from doit etre > 0")
  fim * (n_to / n_from)
}


# =============================================================================
# vcov_from_fim() — Variance-covariance depuis la FIM
# =============================================================================

#' Calculer la matrice variance-covariance a partir de la FIM
#'
#' VCOV = FIM^{-1}. Point d'entree canonique pour Wald power, TOST,
#' intervalles de confiance par la methode delta.
#'
#' @param fim Matrice numerique nommee (FIM)
#' @return Matrice VCOV nommee, ou NULL si FIM singuliere
#' @export
vcov_from_fim <- function(fim) {
  if (is.null(fim) || nrow(fim) == 0L) return(NULL)
  tryCatch({
    if (is.na(rcond(fim)) || rcond(fim) < 1e-15) {
      warning("FIM singuliere ou mal conditionnee, inversion impossible")
      return(NULL)
    }
    vcov <- chol2inv(chol(fim))
    dimnames(vcov) <- dimnames(fim)
    vcov
  }, error = function(e) {
    warning("FIM non definie positive, inversion impossible : ", e$message)
    NULL
  })
}


# =============================================================================
# compute_robust_summary — Reimplementation R de summary.f90 (Bauer 2021)
# =============================================================================

#' Calcule les statistiques robustes a partir d'un .tab multi-subproblemes
#'
#' Reimplemente en R la logique du programme Fortran summary.f90 de Bauer (2021).
#' Pour chaque variable et chaque position de ligne, calcule Mean, STD, RSTD,
#' Low, High, et percentiles 2.5%/97.5% a travers N subproblemes.
#'
#' @param tab Tibble retourne par read_tab(), avec colonne table_no
#' @return Liste de tibbles (un par variable), meme format que read_summary_tab(),
#'         ou NULL si <= 1 subprobleme
#' @export
compute_robust_summary <- function(tab) {
  if (is.null(tab) || nrow(tab) == 0L) return(NULL)

  n_sub <- n_distinct(tab$table_no)
  if (n_sub <= 1L) return(NULL)

  # Row position within each subproblem
  tab <- tab |>
    group_by(table_no) |>
    mutate(row_pos = row_number()) |>
    ungroup()

  # Numeric columns to summarize (exclude metadata)
  skip_cols <- c("table_no", "row_pos", "EVID", "MDV", "ID")
  var_cols <- setdiff(
    names(tab)[vapply(tab, is.numeric, logical(1))],
    skip_cols
  )

  # Warn if subproblems have unequal row counts (summary.f90 aborts)
  rows_per_sub <- tapply(tab$row_pos, tab$table_no, max)
  if (length(unique(rows_per_sub)) > 1L) {
    warning("compute_robust_summary: subproblems have unequal row counts (",
            paste(sort(unique(rows_per_sub)), collapse = ", "),
            "). Statistics for sparse rows are averaged over fewer replications.")
  }

  result <- list()
  for (vc in var_cols) {
    # Build matrix: rows = row positions, cols = subproblems
    vals <- tab[[vc]]
    row_pos <- tab$row_pos
    tbl_no <- tab$table_no
    n_rows <- max(row_pos)

    mat <- matrix(NA_real_, nrow = n_rows, ncol = n_sub)
    unique_tbl <- sort(unique(tbl_no))
    for (j in seq_along(unique_tbl)) {
      idx <- which(tbl_no == unique_tbl[j])
      rp <- row_pos[idx]
      mat[rp, j] <- vals[idx]
    }

    row_mean <- rowMeans(mat, na.rm = TRUE)
    row_sd   <- apply(mat, 1, sd, na.rm = TRUE)
    row_rstd <- ifelse(row_mean != 0,
                       abs(100 * row_sd / row_mean),
                       NA_real_)

    summ_df <- tibble(
      Row      = seq_len(n_rows),
      Mean     = row_mean,
      STD      = row_sd,
      RSTD     = row_rstd,
      Low      = apply(mat, 1, min, na.rm = TRUE),
      High     = apply(mat, 1, max, na.rm = TRUE),
      `2.50%`  = apply(mat, 1, quantile, probs = 0.025, na.rm = TRUE),
      `97.50%` = apply(mat, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
    result[[vc]] <- summ_df
  }
  result[["n_sub"]] <- as.integer(n_sub)
  result
}
