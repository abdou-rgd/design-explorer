# =============================================================================
# design_summary.R
# Affichage formate des metriques cles d'un run $DESIGN
#
# Contenu :
#   summary_design() — Resume structure d'un run NONMEM $DESIGN
#
# Prerequis : source('design_metrics.R')
# =============================================================================

library(dplyr)
library(stringr)
library(tidyr)


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
      mutate(eta_idx = as.integer(str_extract(param, "(?<=\\()\\d+"))) |>
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
