# =============================================================================
# fim_metrics.R
# Fonctions de decision basees sur la FIM : Power, NSN, courbe Power(N)
#
# Sources :
#   - PopED evaluate_power.R (Retout et al. 2007, Mentre & Rousseau 2011)
#   - PopED optimize_n_rse() (Ueckert et al. 2013)
#
# Prerequis : source design_utils.R, design_io.R, design_metrics.R, report_design.R
# Compatibilite : R 4.1+, ggplot2, dplyr
# =============================================================================

library(ggplot2)
library(dplyr)


# =============================================================================
# compute_power_wald() — Puissance du test de Wald
# =============================================================================

#' Puissance du test de Wald pour un parametre
#'
#' Calcule la puissance du test H0: theta = h0 vs H1: theta != h0 (bilateral)
#' ou H1: theta > h0 (unilateral), basee sur l'erreur standard predite par la FIM.
#'
#' @param theta_val Valeur estimee du parametre (scalaire)
#' @param rse_pct   RSE en pourcentage (e.g. 20 pour 20%)
#' @param h0        Valeur sous H0 (defaut 0)
#' @param alpha     Niveau de significativite (defaut 0.05)
#' @param two_sided Test bilateral (defaut TRUE)
#' @return Puissance [0,1] ou NA_real_ si theta_val ~ 0 ou rse_pct manquant
compute_power_wald <- function(theta_val, rse_pct, h0 = 0,
                               alpha = 0.05, two_sided = TRUE) {
  if (is.na(theta_val) || abs(theta_val) < 1e-12 || is.na(rse_pct) || rse_pct <= 0) {
    return(NA_real_)
  }
  if (two_sided) alpha <- alpha / 2
  z_alpha <- qnorm(1 - alpha)
  se <- abs(theta_val) * rse_pct / 100
  W <- (h0 - theta_val) / se
  1 - pnorm(W + z_alpha) + pnorm(W - z_alpha)
}


# =============================================================================
# compute_n_needed() — Nombre de sujets necessaire (NSN)
# =============================================================================

#' Nombre de sujets necessaire pour atteindre une puissance cible
#'
#' Utilise la propriete de scaling lineaire de la FIM :
#'   FIM(N) = N * FIM(1)  =>  RSE(N) = RSE(N0) * sqrt(N0 / N)
#'
#' @param theta_val         Valeur estimee du parametre
#' @param rse_pct           RSE en % au groupsize actuel
#' @param groupsize_current Nombre de sujets actuel (GROUPSIZE du $DESIGN)
#' @param h0                Valeur sous H0 (defaut 0)
#' @param power_target      Puissance cible (defaut 0.80)
#' @param alpha             Niveau de significativite (defaut 0.05)
#' @param two_sided         Test bilateral (defaut TRUE)
#' @return Liste : n_needed (integer), rse_current, rse_needed
compute_n_needed <- function(theta_val, rse_pct, groupsize_current,
                             h0 = 0, power_target = 0.80,
                             alpha = 0.05, two_sided = TRUE) {
  if (is.na(theta_val) || abs(theta_val) < 1e-12 || is.na(rse_pct) || rse_pct <= 0) {
    return(list(n_needed = NA_integer_, rse_current = rse_pct, rse_needed = NA_real_))
  }
  if (two_sided) alpha <- alpha / 2
  z_alpha <- qnorm(1 - alpha)
  z_beta  <- qnorm(power_target)
  # SE necessaire : |h0 - theta| / (z_alpha + z_beta)
  se_needed <- abs(h0 - theta_val) / (z_alpha + z_beta)
  rse_needed <- se_needed / abs(theta_val) * 100
  # Scaling : N = N0 * (RSE_current / RSE_needed)^2
  n_needed <- ceiling((rse_pct / rse_needed)^2 * groupsize_current)
  list(
    n_needed   = as.integer(max(1L, n_needed)),
    rse_current = rse_pct,
    rse_needed  = rse_needed
  )
}


# =============================================================================
# compute_power_table() — Tableau power/NSN pour tous les parametres
# =============================================================================

#' Tableau de puissance et NSN pour tous les parametres d'un run
#'
#' @param ext       Tibble retourne par read_ext()
#' @param table_no  Numero de table (NULL = derniere)
#' @param groupsize Nombre de sujets (GROUPSIZE)
#' @param h0        Valeur sous H0 (defaut 0)
#' @param alpha     Niveau alpha (defaut 0.05)
#' @param two_sided Bilateral (defaut TRUE)
#' @param power_target Puissance cible pour NSN (defaut 0.80)
#' @param param_labels Vecteur nomme THETA1=CL, ... (optionnel)
#' @return Tibble : param, label, estimate, rse_pct, se, power, n_needed, rse_needed
compute_power_table <- function(ext, table_no = NULL, groupsize = 1L,
                                h0 = 0, alpha = 0.05, two_sided = TRUE,
                                power_target = 0.80, param_labels = NULL) {
  rse_df <- get_rse(ext, table_no = table_no)
  if (is.null(rse_df) || nrow(rse_df) == 0L) return(NULL)

  result <- rse_df |>
    mutate(
      label = if (!is.null(param_labels)) {
        ifelse(param %in% names(param_labels), param_labels[param], param)
      } else {
        param
      },
      power = mapply(compute_power_wald,
        theta_val = estimate, rse_pct = rse_pct,
        MoreArgs = list(h0 = h0, alpha = alpha, two_sided = two_sided)
      ),
      n_needed = mapply(function(tv, rp) {
        compute_n_needed(tv, rp, groupsize, h0, power_target, alpha, two_sided)$n_needed
      }, tv = estimate, rp = rse_pct),
      rse_needed = mapply(function(tv, rp) {
        compute_n_needed(tv, rp, groupsize, h0, power_target, alpha, two_sided)$rse_needed
      }, tv = estimate, rp = rse_pct),
      param_type = .param_type(param)
    )

  result
}


# =============================================================================
# plot_power_curve() — Courbe Puissance vs N pour un parametre
# =============================================================================

#' Courbe Power(N) pour un parametre donne
#'
#' FIM scaling : RSE(N) = RSE(N0) * sqrt(N0 / N)
#'
#' @param theta_val    Valeur estimee du parametre
#' @param rse_at_n     RSE (%) au groupsize actuel
#' @param n_current    Groupsize actuel
#' @param h0           Valeur sous H0 (defaut 0)
#' @param alpha        Niveau alpha (defaut 0.05)
#' @param two_sided    Bilateral (defaut TRUE)
#' @param power_target Puissance cible (defaut 0.80)
#' @param n_max        N max sur le graphe (defaut 5 * n_current)
#' @param param_name   Nom du parametre (pour le titre)
#' @return ggplot2 object
plot_power_curve <- function(theta_val, rse_at_n, n_current,
                             h0 = 0, alpha = 0.05, two_sided = TRUE,
                             power_target = 0.80, n_max = NULL,
                             param_name = NULL) {
  if (is.na(theta_val) || abs(theta_val) < 1e-12 || is.na(rse_at_n)) {
    return(
      ggplot() +
        labs(title = paste0("Power(N) non calculable",
                            if (!is.null(param_name)) paste0(" — ", param_name) else "")) +
        .theme_design()
    )
  }

  if (is.null(n_max)) n_max <- max(10L, as.integer(n_current * 5))
  n_range <- seq(1L, n_max, by = max(1L, n_max %/% 200L))

  # FIM scaling
  powers <- vapply(n_range, function(n) {
    rse_n <- rse_at_n * sqrt(n_current / n)
    compute_power_wald(theta_val, rse_n, h0, alpha, two_sided)
  }, numeric(1))

  df <- data.frame(N = n_range, Power = powers)


  # N needed
  nsn <- compute_n_needed(theta_val, rse_at_n, n_current,
                          h0, power_target, alpha, two_sided)

  title_txt <- if (!is.null(param_name)) {
    paste0("Puissance vs N — ", param_name)
  } else {
    "Puissance vs N"
  }
  subtitle_txt <- sprintf("RSE actuel = %.1f%% | N actuel = %d | N necessaire = %s",
                           rse_at_n, n_current,
                           if (is.na(nsn$n_needed)) "N/A" else as.character(nsn$n_needed))

  p <- ggplot(df, aes(x = N, y = Power)) +
    geom_line(color = "#2563eb", size = 1) +
    geom_hline(yintercept = power_target, linetype = "dashed",
               color = "#dc2626", size = 0.6) +
    geom_vline(xintercept = n_current, linetype = "dotted",
               color = "#6b7280", size = 0.6) +
    annotate("text", x = n_current, y = 0.05,
             label = paste0("N=", n_current), hjust = -0.15,
             size = 3.2, color = "#6b7280") +
    annotate("text", x = max(n_range) * 0.95, y = power_target + 0.03,
             label = sprintf("Cible = %g%%", power_target * 100),
             hjust = 1, size = 3.2, color = "#dc2626") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = function(x) paste0(x * 100, "%")) +
    labs(x = "Nombre de sujets (N)", y = "Puissance",
         title = title_txt, subtitle = subtitle_txt) +
    .theme_design()

  # Marker for N needed
  if (!is.na(nsn$n_needed) && nsn$n_needed <= n_max) {
    power_at_nsn <- compute_power_wald(
      theta_val, rse_at_n * sqrt(n_current / nsn$n_needed), h0, alpha, two_sided
    )
    p <- p +
      geom_vline(xintercept = nsn$n_needed, linetype = "dotted",
                 color = "#16a34a", size = 0.6) +
      geom_point(data = data.frame(N = nsn$n_needed, Power = power_at_nsn),
                 aes(x = N, y = Power), color = "#16a34a", size = 3) +
      annotate("text", x = nsn$n_needed, y = 0.05,
               label = paste0("N=", nsn$n_needed), hjust = -0.15,
               size = 3.2, color = "#16a34a")
  }

  p
}
