# =============================================================================
# report_design.R
# Visualisations des sorties NONMEM $DESIGN
#
# Fonctions principales :
#   plot_relativeinf() — Barplot RELATIVEINF(%) par ETA (informativité design)
#   plot_rse()         — Barplot RSE(%) par paramètre, facetté par type
#   plot_convergence() — Courbe OFV vs itération (convergence optimisation)
#
# Prerequis : source design_utils.R, design_io.R, design_metrics.R
# Compatibilité : R 4.1+, ggplot2, dplyr, stringr
# =============================================================================

library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)


# =============================================================================
# Utilitaires internes
# =============================================================================

# Palette qualité RelInf : rouge (bas) → orange → vert (élevé)
.ri_quality <- function(ri_pct) {
  case_when(
    is.na(ri_pct)  ~ "Inconnu",
    ri_pct >= 50   ~ "> 50% (bon)",
    ri_pct >= 20   ~ "20-50% (acceptable)",
    TRUE           ~ "< 20% (insuffisant)"
  )
}

# Palette qualite RSE 4 tiers : vert -> orange -> rouge -> rouge fonce
.rse_quality <- function(rse_pct) {
  case_when(
    is.na(rse_pct)  ~ "Inconnu",
    rse_pct < 20    ~ "< 20% (bon)",
    rse_pct < 50    ~ "20-50% (acceptable)",
    rse_pct < 100   ~ "50-100% (mauvais)",
    TRUE            ~ "> 100% (tres mauvais)"
  )
}

# .param_type() est defini dans design_metrics.R

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

# Placeholder vide pour donnees manquantes
.empty_plot <- function(msg) ggplot() + labs(title = msg) + .theme_design()

# Prepare obs/CTP points from .tab data (shared by plot_pk_profile)
.prep_points <- function(df, time_div) {
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  if ("EVID" %in% names(df)) df <- dplyr::filter(df, EVID == 0)
  if ("table_no" %in% names(df) && dplyr::n_distinct(df$table_no) > 1L)
    df <- dplyr::filter(df, table_no == 1L)
  y_col <- intersect(c("IPRED", "PRED", "DV", "CONC"), names(df))[1]
  if (is.na(y_col)) return(NULL)
  df |> dplyr::mutate(
    y_val     = .data[[y_col]],
    time_plot = TIME / time_div,
    arm       = if ("ID"  %in% names(df)) df$ID  else 1,
    cmt       = if ("CMT" %in% names(df)) df$CMT else 1L
  )
}

# Select representative IDs when dataset has >max_ids unique IDs
.select_representative_ids <- function(obs, group_col, max_ids = 4L) {
  n_ids <- dplyr::n_distinct(obs$ID)
  if (n_ids <= max_ids) return(unique(obs$ID))

  rep_ids <- NULL
  if (group_col %in% names(obs)) {
    arm_sig <- obs |>
      dplyr::group_by(ID) |>
      dplyr::summarise(sig = paste(sort(unique(.data[[group_col]])),
                                   collapse = ","),
                       .groups = "drop")
    rep_ids <- arm_sig |>
      dplyr::group_by(sig) |>
      dplyr::slice_min(ID, n = 1L) |>
      dplyr::ungroup() |>
      dplyr::pull(ID)
  }
  if (is.null(rep_ids) || length(rep_ids) == 0L) {
    rep_ids <- sort(unique(obs$ID))[1:2L]
  }
  if (length(rep_ids) > max_ids) {
    rep_ids <- sort(rep_ids)[1:max_ids]
  }
  rep_ids
}

# Couleurs qualité
.COLORS_RI  <- c(
  "> 50% (bon)"         = "#4CAF50",
  "20-50% (acceptable)" = "#FF9800",
  "< 20% (insuffisant)" = "#F44336",
  "Inconnu"             = "#AAAAAA"
)
.COLORS_RSE <- c(
  "< 20% (bon)"            = "#16a34a",
  "20-50% (acceptable)"    = "#d97706",
  "50-100% (mauvais)"      = "#dc2626",
  "> 100% (tres mauvais)"  = "#7f1d1d",
  "Inconnu"                = "#AAAAAA"
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
    return(.empty_plot("Pas de donnees RELATIVEINF"))
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
    geom_col(width = 0.65, color = "white", linewidth = 0.3) +
    geom_hline(yintercept = c(20, 50), linetype = "dashed",
               color = "grey40", linewidth = 0.45) +
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
#' Lignes de référence à 20%, 50% et 100%.
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
    return(.empty_plot("Pas de donnees RSE"))
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
        .rse_quality(rse_pct),
        levels = names(.COLORS_RSE)
      )
    )

  ttl        <- title %||% "RSE prédit par la FIM (%)"
  facet_scales <- if (free_y) "free" else "free_x"

  p <- ggplot(rse, aes(x = param, y = rse_pct, fill = quality)) +
    geom_col(width = 0.65, color = "white", linewidth = 0.3) +
    geom_hline(yintercept = c(20, 50, 100), linetype = "dashed",
               color = "grey40", linewidth = 0.45) +
    geom_text(
      aes(label = sprintf("%.2f%%", rse_pct)),
      vjust = -0.35, size = 2.9, color = "grey25"
    ) +
    scale_fill_manual(values = .COLORS_RSE, name = NULL, drop = FALSE) +
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
    return(.empty_plot("Pas de donnees de convergence (MAXEVAL=0)"))
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
    geom_line(linewidth = 0.75, alpha = 0.9) +
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
# build_convergence_steps() — Resume par phase d'optimisation
# =============================================================================

#' Construit un data.frame resume de la convergence par phase.
#'
#' Chaque TABLE NO. dans le .ext correspond a une phase d'optimisation.
#' Cette fonction extrait l'OBJ final de chaque phase pour un step chart.
#'
#' @param ext         Tibble retourne par read_ext()
#' @param cpu_secs    Temps CPU total (scalaire) ou NA
#' @param method_labels Named character vector de parse_design_methods() ou NULL
#'
#' @return data.frame avec colonnes: step (factor), obj, cpu, table_no, is_eval
#' @export
build_convergence_steps <- function(ext, cpu_secs = NA_real_,
                                    method_labels = NULL) {
  finals <- ext |>
    filter(type == "final") |>
    group_by(table_no) |>
    summarise(obj = OBJ[1L], .groups = "drop") |>
    filter(!is.na(obj)) |>
    arrange(table_no)

  if (nrow(finals) == 0L) return(NULL)

  # Apply method labels or fallback
  if (!is.null(method_labels)) {
    finals$label <- vapply(as.character(finals$table_no), function(tn) {
      method_labels[tn] %||% paste0("Bloc ", tn)
    }, character(1L))
  } else {
    finals$label <- paste0("Bloc ", finals$table_no)
  }

  # Detect eval step (first step is usually eval if label starts with "Eval")
  finals$is_eval <- grepl("^Eval", finals$label, ignore.case = TRUE)

  # CPU: total only, displayed on last step
  finals$cpu <- NA_character_
  if (!is.na(cpu_secs) && cpu_secs > 0) {
    cpu_fmt <- if (cpu_secs < 60) {
      sprintf("%.1fs", cpu_secs)
    } else if (cpu_secs < 3600) {
      sprintf("%.0f min", cpu_secs / 60)
    } else {
      sprintf("%.1fh", cpu_secs / 3600)
    }
    finals$cpu[nrow(finals)] <- paste0("CPU total: ", cpu_fmt)
  }

  finals$step <- factor(finals$label, levels = finals$label)
  finals[, c("step", "obj", "cpu", "table_no", "is_eval")]
}


# =============================================================================
# plot_convergence_steps() — Step chart du critere D par phase
# =============================================================================

#' Trace un step chart montrant l'OBJ final par phase d'optimisation.
#'
#' Reproduit le style du plot de presentation : points connectes par une ligne,
#' labels OBJ au-dessus, CPU en-dessous, annotation du gain total.
#'
#' @param steps_df data.frame de build_convergence_steps()
#' @param title    Titre du graphique (NULL = titre automatique)
#'
#' @return Objet ggplot2
#' @export
plot_convergence_steps <- function(steps_df, title = NULL) {
  if (is.null(steps_df) || nrow(steps_df) == 0L) {
    return(.empty_plot("Pas de donnees de convergence par phase"))
  }

  col_eval <- "#D4883A"
  col_opti <- "#6C2B91"
  col_gain <- "#1B8C4E"

  n <- nrow(steps_df)
  point_colors <- ifelse(steps_df$is_eval, col_eval, col_opti)

  # Gain calculation
  gain <- steps_df$obj[n] - steps_df$obj[1L]

  # Y-axis range: use the actual data range with moderate padding for labels
  obj_range <- max(steps_df$obj) - min(steps_df$obj)
  obj_range <- max(obj_range, 1)  # minimum 1 unit range
  y_pad <- obj_range * 0.6  # room for OBJ labels above + CPU labels below

  # Build subtitle: chain of algos + CPU per step
  chain_parts <- steps_df$step
  sub_txt <- paste("Chainage :", paste(chain_parts, collapse = " -> "))
  # Add total CPU if available
  cpu_vals <- steps_df$cpu[!is.na(steps_df$cpu)]
  if (length(cpu_vals) > 0L) {
    sub_txt <- paste0(sub_txt, " | ", cpu_vals[length(cpu_vals)])
  }

  ttl <- title %||% "Convergence de l'optimisation : D-critere"

  p <- ggplot(steps_df, aes(x = step, y = obj)) +
    # Shaded gain region
    annotate("rect", xmin = 0.5, xmax = n + 0.5,
             ymin = min(steps_df$obj), ymax = max(steps_df$obj),
             fill = col_gain, alpha = 0.08) +
    # Line connecting points
    geom_line(aes(group = 1), color = col_opti, linewidth = 1.2) +
    # Points with per-step colors
    geom_point(size = 5, color = point_colors) +
    # OBJ labels above
    geom_text(aes(label = sprintf("%.2f", obj)),
              vjust = -1.5, size = 4.2, fontface = "bold",
              color = point_colors) +
    scale_y_continuous(
      breaks = pretty(c(min(steps_df$obj) - y_pad, max(steps_df$obj) + y_pad), n = 6),
      limits = c(min(steps_df$obj) - y_pad, max(steps_df$obj) + y_pad)
    ) +
    labs(
      title = ttl,
      subtitle = sub_txt,
      x = NULL,
      y = "-log(det(FIM))"
    ) +
    .theme_design() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11))

  # CPU labels below points (where available)
  cpu_data <- steps_df[!is.na(steps_df$cpu), , drop = FALSE]
  if (nrow(cpu_data) > 0L) {
    p <- p +
      geom_text(data = cpu_data, aes(label = cpu),
                vjust = 2.8, size = 3.5, color = "grey50")
  }

  # Gain annotation (only if > 1 step)
  if (n > 1L) {
    x_arrow <- n + 0.4
    y_mid <- (steps_df$obj[1L] + steps_df$obj[n]) / 2

    p <- p +
      annotate("segment", x = x_arrow, xend = x_arrow,
               y = steps_df$obj[1L], yend = steps_df$obj[n],
               arrow = arrow(ends = "both", length = unit(0.15, "cm")),
               color = col_gain, linewidth = 0.8) +
      annotate("text", x = x_arrow + 0.15, y = y_mid,
               label = sprintf("Gain\n%.2f", gain), color = col_gain,
               fontface = "bold", size = 4, hjust = 0) +
      coord_cartesian(xlim = c(0.5, n + 0.9), clip = "off")
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
    return(.empty_plot("Pas de matrice FIM disponible"))
  }

  # Utiliser get_cor_matrix() pour filtrage + inversion + correlation
  corr_mat <- get_cor_matrix(fim_matrix)
  if (is.null(corr_mat)) {
    return(.empty_plot("FIM singuliere -- correlations non calculables"))
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
    geom_tile(color = "white", linewidth = 0.5) +
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
    return(.empty_plot("Pas de donnees .tab disponibles"))
  }

  # Filtrer observations uniquement (EVID == 0)
  obs <- tab_data
  if ("EVID" %in% names(obs)) {
    obs <- filter(obs, EVID == 0)
  }

  if (nrow(obs) == 0L) {
    return(.empty_plot("Aucune observation (EVID=0) dans le .tab"))
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
    return(.empty_plot("Pas de donnees SE"))
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
    geom_col(width = 0.65, fill = "#1976d2", color = "white", linewidth = 0.3) +
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
    return(.empty_plot("Pas de donnees RSE"))
  }

  if (!is.null(param_labels)) {
    rse <- rse |> mutate(param = if_else(param %in% names(param_labels), param_labels[param], param))
  }

  rse <- rse |>
    mutate(quality = factor(
      .rse_quality(rse_pct),
      levels = names(.COLORS_RSE)
    ))

  ttl <- title %||% "RSE prédit par la FIM (%) -- Waterfall"

  ggplot(rse, aes(x = reorder(param, rse_pct), y = rse_pct, fill = quality)) +
    geom_col(width = 0.65, color = "white", linewidth = 0.3) +
    geom_hline(yintercept = c(20, 50, 100), linetype = "dashed", color = "grey40", linewidth = 0.45) +
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
#' @param time_unit  "hours" ou "days" (divise TIME par 24 si "days")
#' @param show_doses Si TRUE, affiche des lignes verticales aux temps de dose
#' @param arm_labels Named character vector pour renommer les facettes ID
#'                   (ex: c("1"="IV", "2"="SC"))
#' @param cmt_labels Named character vector pour renommer les CMT
#' @return Objet ggplot2
#' @export
plot_model_prediction <- function(tab_data, group_col = "TSTRAT", title = NULL,
                                  time_unit = "hours", show_doses = TRUE,
                                  arm_labels = NULL, cmt_labels = NULL) {
  if (is.null(tab_data) || nrow(tab_data) == 0L) {
    return(.empty_plot("Pas de donnees .tab"))
  }

  # Extract dose times BEFORE filtering (for dose markers)
  dose_times <- NULL
  if (show_doses && "EVID" %in% names(tab_data)) {
    if ("AMT" %in% names(tab_data)) {
      dose_rows <- tab_data |>
        dplyr::filter(EVID == 1 | (!is.na(AMT) & AMT > 0))
    } else {
      dose_rows <- tab_data |> dplyr::filter(EVID == 1)
    }
    if (nrow(dose_rows) > 0L) {
      dose_times <- dose_rows
    }
  }

  obs <- tab_data
  if ("EVID" %in% names(obs)) obs <- filter(obs, EVID == 0)
  if (nrow(obs) == 0L) {
    return(.empty_plot("Aucune observation"))
  }

  # Determine Y variable
  y_col <- intersect(c("IPRED", "PRED", "DV", "CONC"), names(obs))[1]
  if (is.na(y_col)) {
    return(.empty_plot("Colonne IPRED/PRED/DV absente"))
  }

  if (!group_col %in% names(obs)) obs[[group_col]] <- 1

  obs <- obs |>
    mutate(y_val = .data[[y_col]],
           strate_label = as.character(.data[[group_col]]))

  # Detect multi-ID (e.g. IV vs SC elementary designs or dataset classique)
  has_multi_id <- "ID" %in% names(obs) && n_distinct(obs$ID) > 1
  if (has_multi_id) {
    if (n_distinct(obs$ID) > 4L) {
      rep_ids <- .select_representative_ids(obs, group_col, max_ids = 4L)
      obs <- obs |> dplyr::filter(ID %in% rep_ids)
    }
    obs <- obs |> mutate(id_label = paste0("ID ", ID))
  }

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

  # Sort within each ID for correct line drawing
  if (has_multi_id) obs <- obs |> arrange(id_label, TIME)

  y_label <- if (has_cmt) "Prediction (IPRED)" else y_col
  ttl <- title %||% if (has_cmt) "Predictions PK/PD aux temps de sampling optimaux" else
                     paste0("Predictions (", y_col, ") aux temps de sampling optimaux")

  # --- Time unit conversion (BEFORE ggplot captures the data) ---
  use_days <- identical(time_unit, "days")
  time_label <- if (use_days) "Temps (jours)" else "Temps (h)"
  sec_label  <- if (use_days) "Temps de sampling (jours)" else "Temps de sampling (h)"
  if (use_days) {
    obs$TIME <- obs$TIME / 24
    if (!is.null(dose_times)) dose_times$TIME <- dose_times$TIME / 24
  }

  p <- ggplot(obs, aes(x = TIME, y = y_val))

  if (has_cmt) {
    p <- p +
      geom_line(aes(color = response, group = response),
                linewidth = 0.7, alpha = 0.35, linetype = "dashed") +
      geom_point(aes(fill = response), shape = 21, size = 3.5,
                 color = "white", stroke = 0.8) +
      scale_color_manual(values = c("#2563eb", "#dc2626", "#16a34a", "#d97706"),
                         name = NULL) +
      scale_fill_manual(values = c("#2563eb", "#dc2626", "#16a34a", "#d97706"),
                        name = NULL)
  } else if (has_multi_id) {
    p <- p +
      geom_line(aes(group = id_label),
                color = "#2563eb", linewidth = 0.7, alpha = 0.35, linetype = "dashed") +
      geom_point(fill = "#2563eb", shape = 21, size = 3.5,
                 color = "white", stroke = 0.8)
  } else {
    p <- p +
      geom_line(color = "#2563eb", linewidth = 0.7, alpha = 0.35, linetype = "dashed") +
      geom_point(fill = "#2563eb", shape = 21, size = 3.5,
                 color = "white", stroke = 0.8)
  }

  # Label sampling points with strate number (compact, no "Strate" prefix)
  p <- p +
    geom_text(aes(label = strate_label), size = 2.8, color = "#374151",
              vjust = -1.3, hjust = 0.5)

  # --- Dose markers ---
  if (!is.null(dose_times) && nrow(dose_times) > 0L) {
    dose_df <- dose_times
    # If multi-ID with arm labels, match dose times to facets
    if (has_multi_id && "ID" %in% names(dose_df)) {
      dose_df <- dose_df |> dplyr::mutate(id_label = paste0("ID ", ID))
      if (!is.null(arm_labels)) {
        dose_df$id_label <- ifelse(
          as.character(dose_df$ID) %in% names(arm_labels),
          arm_labels[as.character(dose_df$ID)],
          dose_df$id_label
        )
      }
    }
    p <- p +
      geom_vline(data = dose_df, aes(xintercept = TIME),
                 linetype = "dotted", color = "firebrick3",
                 linewidth = 0.3, alpha = 0.6)
  }

  # --- Arm labels (rename ID facets) ---
  if (has_multi_id && !is.null(arm_labels)) {
    obs$id_label <- ifelse(
      as.character(obs$ID) %in% names(arm_labels),
      arm_labels[as.character(obs$ID)],
      obs$id_label
    )
  }

  # --- CMT labels ---
  if (has_cmt && !is.null(cmt_labels)) {
    obs$response <- ifelse(
      as.character(obs$CMT) %in% names(cmt_labels),
      cmt_labels[as.character(obs$CMT)],
      obs$response
    )
  }

  # Secondary x-axis with exact sampling times
  sampling_breaks <- sort(unique(round(obs$TIME, 1)))
  p <- p +
    scale_x_continuous(
      sec.axis = dup_axis(breaks = sampling_breaks, name = sec_label)
    ) +
    labs(title = ttl, x = time_label, y = y_label,
         caption = if (has_multi_id) "Chaque facette = un elementary design (ID) | Tirets = connexion des points"
                   else "Chaque point = prediction du modele a un temps optimal") +
    .theme_design() +
    theme(plot.caption = element_text(size = 8, color = "#6b7280"),
          axis.text.x.top = element_text(size = 6, angle = 45, hjust = 0,
                                         color = "#9ca3af"))

  # Facetting: multi-ID takes priority, then multi-CMT
  if (has_multi_id) {
    p <- p + facet_wrap(~ id_label, ncol = 1, scales = "free_y")
  } else if (has_cmt) {
    p <- p + facet_wrap(~ response, scales = "free_y", ncol = 1)
  }

  p
}


# =============================================================================
# plot_pk_profile — Profil PK lisse via mrgsolve + points d'echantillonnage
# =============================================================================

#' Profil PK simule (courbe lisse) avec points d'echantillonnage superposes
#'
#' @param sim_data        Tibble mrgsolve: time, IPRED, cmt, arm
#' @param obs_points      Tibble .tab: TIME, IPRED/PRED/DV, TSTRAT, CMT, ID, EVID
#' @param dose_times      Vecteur numerique des temps de dose (pour les lignes rouges)
#' @param time_unit       "hours" ou "days"
#' @param cmt_labels      Vecteur nomme: c("1"="Depot", "2"="Central")
#' @param arm_labels      Vecteur nomme: c("1"="IV", "2"="SC")
#' @param title           Titre du plot (NULL = auto)
#' @param compare_points  Tibble optionnel: 2e jeu de points (ex: CTP avant optimisation)
#' @return ggplot
plot_pk_profile <- function(sim_data, obs_points, dose_times = NULL,
                            time_unit = "hours", cmt_labels = NULL,
                            arm_labels = NULL, title = NULL,
                            compare_points = NULL) {
  if (is.null(sim_data) || nrow(sim_data) == 0L) {
    return(.empty_plot("Pas de donnees de simulation"))
  }

  # -- Time unit conversion ---------------------------------------------------
  time_div <- if (time_unit == "days") 24 else 1
  time_label <- if (time_unit == "days") "Temps (jours)" else "Temps (heures)"

  sim <- sim_data |>
    dplyr::mutate(time_plot = time / time_div)

  # -- Prepare obs + compare points (CTP) via shared helper -------------------
  obs <- .prep_points(obs_points, time_div)
  ctp <- .prep_points(compare_points, time_div)

  # -- Apply arm/cmt labels ---------------------------------------------------
  .apply_arm_label <- function(df, labels) {
    if (is.null(df)) return(NULL)
    if (!is.null(labels)) {
      df$arm_label <- ifelse(
        as.character(df$arm) %in% names(labels),
        labels[as.character(df$arm)],
        paste("Arm", df$arm)
      )
    } else {
      df$arm_label <- paste("Arm", df$arm)
    }
    df
  }
  sim <- .apply_arm_label(sim, arm_labels)
  obs <- .apply_arm_label(obs, arm_labels)
  ctp <- .apply_arm_label(ctp, arm_labels)

  .apply_cmt_label <- function(df, labels) {
    if (is.null(df)) return(NULL)
    if (!is.null(labels)) {
      df$cmt_label <- ifelse(
        as.character(df$cmt) %in% names(labels),
        labels[as.character(df$cmt)],
        paste("CMT", df$cmt)
      )
    } else {
      df$cmt_label <- paste("CMT", df$cmt)
    }
    df
  }
  sim <- .apply_cmt_label(sim, cmt_labels)
  obs <- .apply_cmt_label(obs, cmt_labels)

  # -- Detect multi-dimensions ------------------------------------------------
  n_arms <- dplyr::n_distinct(sim$arm)
  n_cmts <- dplyr::n_distinct(sim$cmt)
  has_multi_arm <- n_arms > 1L
  has_multi_cmt <- n_cmts > 1L

  # -- Build plot -------------------------------------------------------------
  p <- ggplot(sim, aes(x = time_plot, y = IPRED)) +
    geom_line(
      aes(group = interaction(arm, cmt)),
      color = "grey30", linewidth = 0.7, alpha = 0.9
    )

  # Overlay optimized sampling points
  has_ctp <- !is.null(ctp)
  if (!is.null(obs)) {
    p <- p + geom_point(
      data = obs, aes(x = time_plot, y = y_val),
      color = "#7c3aed", shape = 16, size = 3, alpha = 0.9
    )
  }
  if (has_ctp) {
    p <- p + geom_point(
      data = ctp, aes(x = time_plot, y = y_val),
      color = "#e67e22", shape = 17, size = 3, alpha = 0.9
    )
  }

  # -- Dose markers -----------------------------------------------------------
  if (!is.null(dose_times) && length(dose_times) > 0L) {
    dose_t <- unique(dose_times / time_div)
    p <- p + geom_vline(
      xintercept = dose_t, linetype = "dotted",
      color = "#e74c3c", alpha = 0.5, linewidth = 0.4
    )
  }

  # -- Secondary x-axis with sampling time ticks ------------------------------
  if (!is.null(obs)) {
    samp_breaks <- sort(unique(round(obs$time_plot, 1)))
    if (length(samp_breaks) > 15L) {
      idx <- seq(1, length(samp_breaks), length.out = 15)
      samp_breaks <- samp_breaks[round(idx)]
    }
    p <- p + scale_x_continuous(
      sec.axis = dup_axis(breaks = samp_breaks,
                          name = "Points de prelevement")
    )
  }

  # -- Labels & theme ---------------------------------------------------------
  ttl <- title %||% "Profil PK predit et points de prelevement"
  sub <- if (has_ctp) {
    "Simulation population (ETA=0) | Ronds = Optimise | Triangles = CTP"
  } else {
    "Simulation population (ETA=0) | Points = temps optimaux"
  }

  caption_text <- "Pointilles rouges = doses"

  p <- p +
    labs(title = ttl, subtitle = sub, x = time_label,
         y = "Concentration predite", caption = caption_text) +
    .theme_design() +
    theme(
      plot.caption = element_text(size = 8, color = "#6b7280"),
      axis.text.x.top = element_text(size = 6, angle = 45, hjust = 0,
                                      color = "#9ca3af")
    )

  # -- Facetting ---------------------------------------------------------------
  if (has_multi_arm && has_multi_cmt) {
    p <- p + facet_grid(arm_label ~ cmt_label, scales = "free_y")
  } else if (has_multi_arm) {
    p <- p + facet_wrap(~ arm_label, ncol = 1, scales = "free_y")
  } else if (has_multi_cmt) {
    p <- p + facet_wrap(~ cmt_label, ncol = 1, scales = "free_y")
  }

  p
}
