# =============================================================================
# report_design.R
# Visualisations des sorties NONMEM $DESIGN
#
# Fonctions principales :
#   plot_relativeinf() — Barplot RELATIVEINF(%) par ETA (informativité design)
#   plot_rse()         — Barplot RSE(%) par paramètre, facetté par type
#   plot_convergence() — Courbe OFV vs itération (convergence optimisation)
#
# Prérequis : source("parse_design_outputs.R") avant d'utiliser ces fonctions
# Compatibilité : R 4.1+, ggplot2, dplyr, stringr
# =============================================================================

library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)


# =============================================================================
# Utilitaires internes
# =============================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

# Palette qualité RelInf : rouge (bas) → orange → vert (élevé)
.ri_quality <- function(ri_pct) {
  case_when(
    is.na(ri_pct)  ~ "Inconnu",
    ri_pct >= 50   ~ "> 50% (bon)",
    ri_pct >= 20   ~ "20-50% (acceptable)",
    TRUE           ~ "< 20% (insuffisant)"
  )
}

# Palette qualité RSE : vert (bon) → orange → rouge (médiocre)
.rse_quality <- function(rse_pct) {
  case_when(
    is.na(rse_pct) ~ "Inconnu",
    rse_pct < 20   ~ "< 20% (bon)",
    rse_pct < 50   ~ "20-50% (acceptable)",
    TRUE           ~ "> 50% (médiocre)"
  )
}

# Détermine le type de paramètre depuis son nom NONMEM
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

# Thème commun
.theme_design <- function() {
  theme_bw(base_size = 11) +
    theme(
      legend.position    = "bottom",
      panel.grid.minor   = element_blank(),
      strip.background   = element_rect(fill = "grey95", color = "grey70"),
      strip.text         = element_text(face = "bold", size = 10)
    )
}

# Couleurs qualité
.COLORS_RI  <- c(
  "> 50% (bon)"         = "#4CAF50",
  "20-50% (acceptable)" = "#FF9800",
  "< 20% (insuffisant)" = "#F44336",
  "Inconnu"             = "#AAAAAA"
)
.COLORS_RSE <- c(
  "< 20% (bon)"         = "#4CAF50",
  "20-50% (acceptable)" = "#FF9800",
  "> 50% (médiocre)"    = "#F44336",
  "Inconnu"             = "#AAAAAA"
)


# =============================================================================
# plot_relativeinf() — Informativité du design par ETA
# =============================================================================

#' Barplot de l'information relative (%) par ETA
#'
#' Visualise dans quelle mesure le design est informatif pour chaque effet
#' aléatoire. La RELATIVEINF(%) mesure la réduction d'incertitude sur chaque
#' ETA apportée par le design par rapport au prior (OMEGA).
#'
#' Seuils indicatifs :
#'   > 50%  → bon (design très informatif sur cet ETA)
#'   20-50% → acceptable
#'   < 20%  → insuffisant (design peu informatif, fort a priori nécessaire)
#'
#' @param shk          Tibble retourné par read_shk()
#' @param table_no     Numéro de table (défaut : dernier bloc $DESIGN)
#' @param param_labels Vecteur nommé ETA → label  (ex : c(ETA1 = "CL", ETA2 = "V"))
#' @param title        Titre du graphique (NULL = titre automatique)
#'
#' @return Objet ggplot2
#' @export
#'
#' @examples
#' shk <- read_shk("warfarin.shk")
#' plot_relativeinf(shk, param_labels = c(ETA1 = "CL", ETA2 = "V", ETA3 = "KA"))
plot_relativeinf <- function(shk, table_no = NULL, param_labels = NULL, title = NULL) {

  ri <- get_relativeinf(shk, table_no)

  if (nrow(ri) == 0L) {
    warning("Aucune donnée RELATIVEINF (TYPE 11) dans le fichier .shk fourni.")
    return(ggplot() + labs(title = "Pas de données RELATIVEINF") + .theme_design())
  }

  # Renommage optionnel des ETAs
  if (!is.null(param_labels)) {
    ri <- ri |>
      mutate(eta = if_else(eta %in% names(param_labels), param_labels[eta], eta))
  }

  ri <- ri |>
    mutate(
      quality = factor(
        .ri_quality(relativeinf_pct),
        levels = c("> 50% (bon)", "20-50% (acceptable)", "< 20% (insuffisant)", "Inconnu")
      )
    )

  y_max <- max(c(ri$relativeinf_pct, 100), na.rm = TRUE) * 1.18
  ttl   <- title %||% "Information relative (%) par ETA"

  ggplot(ri, aes(x = reorder(eta, relativeinf_pct), y = relativeinf_pct, fill = quality)) +
    geom_col(width = 0.65, color = "white", size = 0.3) +
    geom_hline(yintercept = c(20, 50), linetype = "dashed",
               color = "grey40", size = 0.45) +
    geom_text(
      aes(label = sprintf("%.2f%%", relativeinf_pct)),
      hjust = -0.12, size = 3.2, color = "grey25"
    ) +
    annotate("text", x = 0.4, y = 20, label = "20%", hjust = 0,
             vjust = -0.4, size = 2.8, color = "grey50") +
    annotate("text", x = 0.4, y = 50, label = "50%", hjust = 0,
             vjust = -0.4, size = 2.8, color = "grey50") +
    scale_fill_manual(values = .COLORS_RI, name = NULL, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, y_max),
      labels = function(x) paste0(x, "%")
    ) +
    coord_flip() +
    labs(
      title = ttl,
      x     = NULL,
      y     = "RELATIVEINF (%)"
    ) +
    .theme_design() +
    theme(panel.grid.major.y = element_blank())
}


# =============================================================================
# plot_rse() — RSE prédits par la FIM
# =============================================================================

#' Barplot des RSE (%) prédits par la FIM
#'
#' Affiche le RSE prédit (= |SE_FIM / estimate| × 100) pour tous les
#' paramètres estimables, facetté par type (THETA / OMEGA / SIGMA).
#' Lignes de référence à 20% et 50%.
#'
#' @param ext          Tibble retourné par read_ext()
#' @param table_no     Numéro de table (défaut : dernier bloc $DESIGN)
#' @param param_labels Vecteur nommé THETA → label  (ex : c(THETA1 = "CL", THETA2 = "V"))
#' @param title        Titre du graphique (NULL = titre automatique)
#' @param log_scale    Si TRUE, axe Y en échelle log10 (utile si RSE très dispersés)
#' @param free_y       Si TRUE, chaque facette a son propre axe Y (défaut FALSE)
#'
#' @return Objet ggplot2
#' @export
#'
#' @examples
#' ext <- read_ext("warfarin.ext")
#' plot_rse(ext, param_labels = c(THETA1 = "CL", THETA2 = "V", THETA3 = "KA"))
plot_rse <- function(ext, table_no = NULL, param_labels = NULL,
                     title = NULL, log_scale = FALSE, free_y = FALSE) {

  rse <- get_rse(ext, table_no)

  if (nrow(rse) == 0L) {
    warning("Aucun paramètre estimable trouvé dans le fichier .ext fourni.")
    return(ggplot() + labs(title = "Pas de données RSE") + .theme_design())
  }

  # Déterminer le type de paramètre AVANT renommage
  rse <- rse |>
    mutate(
      param_type = factor(
        .param_type(param),
        levels = c("THETA", "OMEGA (diag.)", "OMEGA (off-diag.)",
                   "SIGMA (diag.)", "SIGMA (off-diag.)", "Autre")
      )
    )

  # Renommage optionnel des THETAs (après filtrage type)
  if (!is.null(param_labels)) {
    rse <- rse |>
      mutate(param = if_else(param %in% names(param_labels), param_labels[param], param))
  }

  rse <- rse |>
    mutate(
      quality = factor(
        case_when(
          is.na(rse_pct)  ~ "Inconnu",
          rse_pct < 20    ~ "< 20% (bon)",
          rse_pct < 50    ~ "20-50% (acceptable)",
          rse_pct < 100   ~ "50-100% (mauvais)",
          TRUE            ~ "> 100% (tres mauvais)"
        ),
        levels = c("< 20% (bon)", "20-50% (acceptable)",
                   "50-100% (mauvais)", "> 100% (tres mauvais)", "Inconnu")
      )
    )

  .colors_rse_4 <- c(
    "< 20% (bon)"          = "#16a34a",
    "20-50% (acceptable)"  = "#d97706",
    "50-100% (mauvais)"    = "#dc2626",
    "> 100% (tres mauvais)" = "#7f1d1d",
    "Inconnu"              = "#AAAAAA"
  )

  ttl        <- title %||% "RSE predit par la FIM (%)"
  facet_scales <- if (free_y) "free" else "free_x"

  p <- ggplot(rse, aes(x = param, y = rse_pct, fill = quality)) +
    geom_col(width = 0.65, color = "white", size = 0.3) +
    geom_hline(yintercept = c(20, 50), linetype = "dashed",
               color = "grey40", size = 0.45) +
    geom_text(
      aes(label = sprintf("%.2f%%", rse_pct)),
      vjust = -0.35, size = 2.9, color = "grey25"
    ) +
    scale_fill_manual(values = .colors_rse_4, name = NULL, drop = FALSE) +
    facet_wrap(~ param_type, scales = facet_scales) +
    labs(
      title = ttl,
      x     = NULL,
      y     = "RSE (%)"
    ) +
    .theme_design() +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(angle = 30, hjust = 1, size = 9)
    )

  if (log_scale) {
    p <- p + scale_y_log10(labels = function(x) paste0(x, "%"))
  } else if (!free_y) {
    y_max <- max(rse$rse_pct, na.rm = TRUE) * 1.18
    p <- p + scale_y_continuous(
      limits = c(0, y_max),
      labels = function(x) paste0(x, "%")
    )
  } else {
    p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
  }

  p
}


# =============================================================================
# plot_convergence() — Courbe OFV par itération
# =============================================================================

#' Courbe de convergence du critère d'optimalité (OFV)
#'
#' Trace l'évolution de l'OFV ($DESIGN objective) au fil des itérations.
#' Chaque bloc TABLE NO. dans le .ext (= chaque bloc $DESIGN dans le .ctl)
#' correspond à une ligne. Utile pour diagnostiquer la convergence d'un run
#' d'optimisation multi-étapes (ex : RS → STGR → NELDER enchaînés).
#'
#' Note : les runs d'évaluation (MAXEVAL=0) n'ont pas d'itérations.
#'
#' @param ext      Tibble retourné par read_ext()
#' @param log_iter Si TRUE, axe X en échelle log10 (utile pour MAXEVAL très élevé)
#' @param title    Titre du graphique (NULL = titre automatique)
#'
#' @return Objet ggplot2
#' @export
#'
#' @examples
#' ext <- read_ext("warfarin2.ext")
#' plot_convergence(ext)
plot_convergence <- function(ext, log_iter = FALSE, title = NULL) {

  dat <- ext |>
    filter(type == "iteration") |>
    select(table_no, ITERATION, OBJ) |>
    filter(!is.na(OBJ), !is.na(ITERATION)) |>
    arrange(table_no, ITERATION)

  if (nrow(dat) == 0L) {
    warning("Aucune ligne d'itération dans .ext (run d'évaluation MAXEVAL=0 ?).")
    return(ggplot() + labs(title = "Pas de données de convergence (MAXEVAL=0)") + .theme_design())
  }

  n_blocs <- n_distinct(dat$table_no)
  dat <- dat |>
    mutate(
      bloc_label = factor(
        paste0("Bloc ", table_no),
        levels = paste0("Bloc ", sort(unique(table_no)))
      )
    )

  ttl <- title %||% "Convergence — critère d'optimalité par itération"

  p <- ggplot(dat, aes(x = ITERATION, y = OBJ, color = bloc_label, group = bloc_label)) +
    geom_line(size = 0.75, alpha = 0.9) +
    geom_point(size = 0.6, alpha = 0.4) +
    labs(
      title = ttl,
      x     = "Itération",
      y     = "OFV (critère d'optimalité)",
      color = NULL
    ) +
    .theme_design()

  # Palette : un seul bloc → couleur unique sans légende
  if (n_blocs == 1L) {
    p <- p +
      scale_color_manual(values = "#2196F3") +
      theme(legend.position = "none")
  } else {
    p <- p + scale_color_brewer(palette = "Set1")
  }

  if (log_iter) {
    p <- p + scale_x_log10()
  }

  p
}


# =============================================================================
# plot_fim_heatmap() — Heatmap de corrélation de la FIM
# =============================================================================

#' Heatmap de la matrice de corrélation dérivée de la FIM
#'
#' Prend la FIM (depuis .coi), l'inverse pour obtenir la variance-covariance,
#' puis convertit en corrélations. Affiche une heatmap avec palette divergente.
#'
#' @param fim_matrix Matrice numérique nommée (FIM, depuis read_coi() ou read_clt())
#' @param labels     Vecteur nommé pour renommer les paramètres (optionnel)
#' @param title      Titre du graphique (NULL = titre automatique)
#'
#' @return Objet ggplot2
#' @export
plot_fim_heatmap <- function(fim_matrix, labels = NULL, title = NULL) {
  if (is.null(fim_matrix) || nrow(fim_matrix) == 0L) {
    return(ggplot() + labs(title = "Pas de matrice FIM disponible") + .theme_design())
  }

  # Filtrer les paramètres avec des valeurs non-nulles sur la diagonale
  nonzero <- diag(fim_matrix) != 0
  if (sum(nonzero) < 2L) {
    return(ggplot() + labs(title = "FIM trop creuse pour une heatmap") + .theme_design())
  }
  fim_sub <- fim_matrix[nonzero, nonzero]

  # Inverser la FIM pour obtenir la variance-covariance, puis corrélation
  corr_mat <- tryCatch({
    vcov <- solve(fim_sub)
    cov2cor(vcov)
  }, error = function(e) {
    warning("FIM singulière, impossible de calculer les corrélations : ", e$message)
    return(NULL)
  })

  if (is.null(corr_mat)) {
    return(ggplot() + labs(title = "FIM singulière — corrélations non calculables") + .theme_design())
  }

  # Renommer les paramètres si labels fournis
  pnames <- rownames(corr_mat)
  if (!is.null(labels)) {
    pnames <- ifelse(pnames %in% names(labels), labels[pnames], pnames)
    rownames(corr_mat) <- pnames
    colnames(corr_mat) <- pnames
  }

  # Convertir en format long pour ggplot
  n <- nrow(corr_mat)
  df <- expand.grid(row = pnames, col = pnames, stringsAsFactors = FALSE)
  df$value <- as.vector(corr_mat)
  df$row <- factor(df$row, levels = rev(pnames))
  df$col <- factor(df$col, levels = pnames)

  ttl <- title %||% "Matrice de corrélation (FIM)"

  ggplot(df, aes(x = col, y = row, fill = value)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.2f", value)),
              size = 2.8, color = "grey20") +
    scale_fill_gradient2(
      low = "#2166ac", mid = "white", high = "#b2182b",
      midpoint = 0, limits = c(-1, 1),
      name = "Corrélation"
    ) +
    labs(title = ttl, x = NULL, y = NULL) +
    .theme_design() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      panel.grid   = element_blank()
    )
}


# =============================================================================
# plot_optimal_times() — Gantt des temps d'échantillonnage optimaux
# =============================================================================

#' Gantt plot des temps d'échantillonnage optimaux par groupe
#'
#' Visualise les temps d'échantillonnage optimisés issus du .tab $DESIGN.
#' Groupes (TSTRAT) sur l'axe Y, temps sur l'axe X.
#'
#' @param tab_data   Tibble retourné par read_tab()
#' @param group_col  Colonne de groupement (défaut : "TSTRAT")
#' @param time_col   Colonne de temps (défaut : "TIME")
#' @param title      Titre du graphique (NULL = titre automatique)
#'
#' @return Objet ggplot2
#' @export
plot_optimal_times <- function(tab_data, group_col = "TSTRAT", time_col = "TIME",
                               cmt_col = NULL, title = NULL) {

  if (is.null(tab_data) || nrow(tab_data) == 0L) {
    return(ggplot() + labs(title = "Pas de données .tab disponibles") + .theme_design())
  }

  # Filtrer observations uniquement (EVID == 0)
  obs <- tab_data
  if ("EVID" %in% names(obs)) {
    obs <- filter(obs, EVID == 0)
  }

  if (nrow(obs) == 0L) {
    return(ggplot() + labs(title = "Aucune observation (EVID=0) dans le .tab") + .theme_design())
  }

  # Vérifier que les colonnes existent
  if (!group_col %in% names(obs)) {
    warning("Colonne '", group_col, "' absente du .tab. Utilisation d'un groupe unique.")
    obs[[group_col]] <- 1
  }
  if (!time_col %in% names(obs)) {
    stop("Colonne '", time_col, "' absente du .tab")
  }

  use_cmt <- !is.null(cmt_col) && cmt_col %in% names(obs) &&
             n_distinct(obs[[cmt_col]]) > 1L

  obs <- obs |>
    mutate(
      group = factor(paste0("Strate ", .data[[group_col]])),
      time  = .data[[time_col]]
    )

  if (use_cmt) {
    obs <- obs |> mutate(cmt_lbl = paste0("CMT=", .data[[cmt_col]]))
  }

  ttl <- title %||% "Temps de prelevement optimaux par groupe (TSTRAT)"
  cap  <- "Un point = temps de prelevement optimal pour ce groupe de patients (TSTRAT)"

  if (use_cmt) {
    p <- ggplot(obs, aes(x = time, y = group, color = cmt_lbl, shape = cmt_lbl)) +
      geom_point(size = 3.5, alpha = 0.85) +
      scale_color_manual(values = c("#2563eb", "#dc2626", "#16a34a", "#d97706"),
                         name = "Reponse") +
      scale_shape_manual(values = c(16L, 17L, 15L, 18L), name = "Reponse")
  } else {
    p <- ggplot(obs, aes(x = time, y = group, color = group)) +
      geom_point(size = 3.5, alpha = 0.85) +
      scale_color_brewer(palette = "Set2", name = NULL)
  }

  p +
    labs(title = ttl, x = "Temps (h)", y = NULL, caption = cap) +
    .theme_design() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.caption = element_text(size = 8, color = "#6b7280"),
      legend.position = if (!use_cmt && n_distinct(obs$group) <= 1L) "none" else "bottom"
    )
}


# =============================================================================
# plot_se() — Barplot SE absolues par paramètre
# =============================================================================

#' Barplot des erreurs standard (SE) absolues prédites par la FIM
#'
#' Même structure que plot_rse() mais affiche les SE brutes au lieu des RSE (%).
#' Utile pour contextualiser les RSE.
#'
#' @param ext          Tibble retourné par read_ext()
#' @param table_no     Numéro de table (défaut : dernier)
#' @param param_labels Vecteur nommé pour renommer les THETAs
#' @param title        Titre du graphique (NULL = titre automatique)
#'
#' @return Objet ggplot2
#' @export
plot_se <- function(ext, table_no = NULL, param_labels = NULL, title = NULL) {

  rse <- get_rse(ext, table_no)

  if (nrow(rse) == 0L) {
    return(ggplot() + labs(title = "Pas de données SE") + .theme_design())
  }

  rse <- rse |>
    mutate(
      param_type = factor(
        .param_type(param),
        levels = c("THETA", "OMEGA (diag.)", "OMEGA (off-diag.)",
                   "SIGMA (diag.)", "SIGMA (off-diag.)", "Autre")
      )
    )

  if (!is.null(param_labels)) {
    rse <- rse |>
      mutate(param = if_else(param %in% names(param_labels), param_labels[param], param))
  }

  ttl <- title %||% "Erreurs standard (SE) prédites par la FIM"

  ggplot(rse, aes(x = param, y = se)) +
    geom_col(width = 0.65, fill = "#1976d2", color = "white", size = 0.3) +
    geom_text(
      aes(label = sprintf("%.4f", se)),
      vjust = -0.35, size = 2.9, color = "grey25"
    ) +
    facet_wrap(~ param_type, scales = "free") +
    labs(
      title = ttl,
      x     = NULL,
      y     = "SE"
    ) +
    .theme_design() +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(angle = 30, hjust = 1, size = 9)
    )
}


# =============================================================================
# plot_rse_waterfall() — Waterfall plot RSE (barres horizontales triees)
# =============================================================================

#' Waterfall plot des RSE (barres horizontales triees)
#'
#' @param ext          Tibble retourne par read_ext()
#' @param table_no     Numero de table
#' @param param_labels Vecteur nomme
#' @param title        Titre
#' @return Objet ggplot2
#' @export
plot_rse_waterfall <- function(ext, table_no = NULL, param_labels = NULL, title = NULL) {
  rse <- get_rse(ext, table_no)
  if (nrow(rse) == 0L) {
    return(ggplot() + labs(title = "Pas de donnees RSE") + .theme_design())
  }

  if (!is.null(param_labels)) {
    rse <- rse |> mutate(param = if_else(param %in% names(param_labels), param_labels[param], param))
  }

  rse <- rse |>
    mutate(quality = factor(
      .rse_quality(rse_pct),
      levels = c("< 20% (bon)", "20-50% (acceptable)", "> 50% (mediocre)", "Inconnu")
    )) |>
    arrange(desc(rse_pct))

  ttl <- title %||% "RSE predit par la FIM (%) -- Waterfall"

  ggplot(rse, aes(x = reorder(param, rse_pct), y = rse_pct, fill = quality)) +
    geom_col(width = 0.65, color = "white", size = 0.3) +
    geom_hline(yintercept = c(20, 50), linetype = "dashed", color = "grey40", size = 0.45) +
    geom_text(aes(label = sprintf("%.2f%%", rse_pct)),
              hjust = -0.12, size = 3, color = "grey25") +
    scale_fill_manual(values = .COLORS_RSE, name = NULL, drop = FALSE) +
    coord_flip() +
    labs(title = ttl, x = NULL, y = "RSE (%)") +
    .theme_design() +
    theme(panel.grid.major.y = element_blank())
}


# =============================================================================
# plot_model_prediction() — Courbe(s) PK/PD avec points de sampling
# =============================================================================

#' Courbe(s) PK/PD predite(s) avec points de sampling optimaux
#'
#' Trace la courbe concentration-temps (ou effet-temps) a partir des predictions
#' du modele (.tab $DESIGN). Si la colonne CMT est presente avec plusieurs
#' compartiments, des courbes separees sont tracees (ex: PK + PD).
#' Les points de sampling sont marques et etiquetes par strate (TSTRAT).
#'
#' @param tab_data   Tibble retourne par read_tab()
#' @param group_col  Colonne de groupement (defaut : "TSTRAT")
#' @param title      Titre (NULL = automatique)
#' @return Objet ggplot2
#' @export
plot_model_prediction <- function(tab_data, group_col = "TSTRAT", title = NULL) {
  if (is.null(tab_data) || nrow(tab_data) == 0L) {
    return(ggplot() + labs(title = "Pas de donnees .tab") + .theme_design())
  }

  obs <- tab_data
  if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
  if (nrow(obs) == 0L) {
    return(ggplot() + labs(title = "Aucune observation") + .theme_design())
  }

  # Determine Y variable
  y_col <- intersect(c("IPRED", "PRED", "DV", "CONC"), names(obs))[1]
  if (is.na(y_col)) {
    return(ggplot() + labs(title = "Colonne IPRED/PRED/DV absente") + .theme_design())
  }

  if (!group_col %in% names(obs)) obs[[group_col]] <- 1

  obs <- obs |>
    mutate(y_val = .data[[y_col]],
           strate = paste0("Strate ", .data[[group_col]]))

  # Detect multi-response (PK-PD) via CMT column
  has_cmt <- "CMT" %in% names(obs) && n_distinct(obs$CMT) > 1
  if (has_cmt) {
    obs <- obs |>
      mutate(response = paste0("Reponse CMT=", CMT)) |>
      arrange(response, TIME)
  } else {
    obs <- obs |>
      mutate(response = "Prediction") |>
      arrange(TIME)
  }

  y_label <- if (has_cmt) "Prediction (IPRED)" else y_col
  ttl <- title %||% if (has_cmt) "Predictions PK/PD aux temps de sampling optimaux" else
                     paste0("Predictions (", y_col, ") aux temps de sampling optimaux")

  p <- ggplot(obs, aes(x = TIME, y = y_val))

  # Draw curve(s) — connect points sorted by TIME within each response
  if (has_cmt) {
    p <- p +
      geom_line(aes(color = response, group = response),
                size = 0.7, alpha = 0.35, linetype = "dashed") +
      geom_point(aes(fill = response), shape = 21, size = 3.5,
                 color = "white", stroke = 0.8) +
      scale_color_manual(values = c("#2563eb", "#dc2626", "#16a34a", "#d97706"),
                         name = NULL) +
      scale_fill_manual(values = c("#2563eb", "#dc2626", "#16a34a", "#d97706"),
                        name = NULL)
  } else {
    p <- p +
      geom_line(color = "#2563eb", size = 0.7, alpha = 0.35, linetype = "dashed") +
      geom_point(fill = "#2563eb", shape = 21, size = 3.5,
                 color = "white", stroke = 0.8)
  }

  # Label sampling points with strate number
  p <- p +
    geom_text(aes(label = strate), size = 2.8, color = "#374151",
              vjust = -1.3, hjust = 0.5) +
    labs(title = ttl, x = "Temps (h)", y = y_label,
         caption = "Chaque point = prediction du modele a un temps optimal | Tirets = connexion des points (pas une courbe PK continue)") +
    .theme_design() +
    theme(plot.caption = element_text(size = 8, color = "#6b7280"))

  if (has_cmt) {
    p <- p + facet_wrap(~ response, scales = "free_y", ncol = 1)
  }

  p
}
