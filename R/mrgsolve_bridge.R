# =============================================================================
# mrgsolve_bridge.R
# Interface entre les sorties NONMEM $DESIGN et mrgsolve pour simulation PK
#
# Contenu :
#   has_mrgsolve()                — Detecte si mrgsolve + compilateur sont dispo
#   extract_params_for_mrgsolve() — Extrait THETA/OMEGA/SIGMA depuis .ext
#   validate_param_mapping()      — Compare noms mrgsolve vs THETA labels
#   extract_dosing_from_tab()     — Extrait les evenements de dose depuis .tab
#   build_dosing_events()         — Construit le data frame de dose mrgsolve
#   compile_mrgsolve_model()      — Compile un modele mrgsolve depuis du code
#   simulate_pk_profile()         — Simule un profil PK haute resolution
#
# Prerequis : design_io.R, design_metrics.R (pour get_final_params)
# Optionnel : mrgsolve (graceful degradation si absent)
# =============================================================================

library(dplyr)
library(stringr)


# =============================================================================
# Detection mrgsolve
# =============================================================================

#' Detecte si mrgsolve est installe et fonctionnel (compilateur C++ OK)
#'
#' Memorise le resultat pour la duree de la session R.
#' @return Liste: list(available = logical, reason = character)
has_mrgsolve <- function() {
  # Memoization via package environment

  env <- environment(has_mrgsolve)
  if (!is.null(env$.mrg_cache)) {
    return(env$.mrg_cache)
  }

  if (!requireNamespace("mrgsolve", quietly = TRUE)) {
    result <- list(available = FALSE, reason = "mrgsolve non installe")
    env$.mrg_cache <- result
    return(result)
  }

  if (
    identical(.Platform$OS.type, "windows") &&
      !nzchar(Sys.which("make"))
  ) {
    result <- list(
      available = FALSE,
      reason = "Compilateur C++ non disponible (Rtools requis sous Windows)"
    )
    env$.mrg_cache <- result
    return(result)
  }

  # Test compilation with a trivial model
  test_ok <- tryCatch(
    {
      mod <- mrgsolve::mcode(
        "__mrg_test__",
        "$PARAM CL = 1\n$CMT CENTRAL\n$ODE dxdt_CENTRAL = -CL * CENTRAL;",
        compile = TRUE,
        quiet = TRUE
      )
      TRUE
    },
    error = function(e) FALSE
  )

  if (!test_ok) {
    result <- list(
      available = FALSE,
      reason = "Compilateur C++ non disponible (Rtools requis sous Windows)"
    )
    env$.mrg_cache <- result
    return(result)
  }

  result <- list(available = TRUE, reason = "OK")
  env$.mrg_cache <- result
  result
}


# =============================================================================
# Extraction des parametres depuis .ext
# =============================================================================

#' Extrait THETA/OMEGA/SIGMA depuis le .ext pour injection dans mrgsolve
#'
#' @param ext_data   Tibble retourne par read_ext()
#' @param theta_labels Vecteur nomme optionnel : c(THETA1 = "CL", THETA2 = "V")
#' @param table_no   Numero de table (defaut: max)
#' @return Liste : list(params = named_list, omega = matrix, sigma = matrix)
extract_params_for_mrgsolve <- function(
  ext_data,
  theta_labels = NULL,
  table_no = NULL
) {
  if (is.null(ext_data) || nrow(ext_data) == 0L) {
    return(NULL)
  }

  final <- get_final_params(ext_data, table_no)
  if (nrow(final) == 0L) {
    return(NULL)
  }

  # Identify parameter columns (exclude metadata)
  meta_cols <- c("table_no", "type", "ITERATION", "OBJ")
  param_cols <- setdiff(names(final), meta_cols)

  vals <- as.numeric(final[1L, param_cols])
  names(vals) <- param_cols

  # Split by type
  theta_mask <- str_starts(param_cols, "THETA")
  omega_mask <- str_starts(param_cols, "OMEGA")
  sigma_mask <- str_starts(param_cols, "SIGMA")

  # THETAs as named list
  theta_vals <- vals[theta_mask]
  if (!is.null(theta_labels) && length(theta_labels) > 0L) {
    # Map THETA1 -> CL etc. using provided labels
    mapped <- numeric(0)
    for (nm in names(theta_vals)) {
      if (nm %in% names(theta_labels)) {
        mapped[theta_labels[[nm]]] <- theta_vals[[nm]]
      } else {
        mapped[nm] <- theta_vals[[nm]]
      }
    }
    theta_list <- as.list(mapped)
  } else {
    theta_list <- as.list(theta_vals)
  }

  # OMEGA -> square matrix
  omega_mat <- .triangular_to_matrix(vals[omega_mask], names(vals)[omega_mask])

  # SIGMA -> square matrix
  sigma_mat <- .triangular_to_matrix(vals[sigma_mask], names(vals)[sigma_mask])

  list(params = theta_list, omega = omega_mat, sigma = sigma_mat)
}

#' Reconstruit une matrice carree depuis les elements triangulaires NONMEM
#' @param vals  Vecteur numerique des elements
#' @param nms   Noms NONMEM : "OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", ...
#' @return Matrice carree symetrique
.triangular_to_matrix <- function(vals, nms) {
  if (length(vals) == 0L) {
    return(matrix(0, 0, 0))
  }

  # Extract row,col indices from names like "OMEGA(2,1)"
  indices <- str_match(nms, "\\((\\d+),(\\d+)\\)")
  rows <- as.integer(indices[, 2])
  cols <- as.integer(indices[, 3])
  n <- max(c(rows, cols))

  mat <- matrix(0, nrow = n, ncol = n)
  for (k in seq_along(vals)) {
    v <- vals[k]
    if (is.na(v)) {
      v <- 0
    }
    mat[rows[k], cols[k]] <- v
    mat[cols[k], rows[k]] <- v # symmetric
  }
  mat
}


# =============================================================================
# Validation du mapping parametres
# =============================================================================

#' Compare les noms de parametres mrgsolve vs les labels THETA du .ctl
#'
#' @param mrg_param_names Vecteur des noms de $PARAM dans le modele mrgsolve
#' @param theta_labels    Vecteur nomme : c(THETA1 = "CL", THETA2 = "V", ...)
#' @return Liste : list(matched, unmatched_mrg, unmatched_theta, mapping_table)
validate_param_mapping <- function(mrg_param_names, theta_labels) {
  if (is.null(theta_labels) || length(theta_labels) == 0L) {
    return(list(
      matched = character(0),
      unmatched_mrg = mrg_param_names,
      unmatched_theta = character(0),
      mapping_table = data.frame(
        mrgsolve = mrg_param_names,
        theta = rep(NA_character_, length(mrg_param_names)),
        status = rep("non mappe", length(mrg_param_names)),
        stringsAsFactors = FALSE
      )
    ))
  }

  theta_names <- unname(theta_labels)
  matched <- intersect(mrg_param_names, theta_names)
  unmatched_mrg <- setdiff(mrg_param_names, theta_names)
  unmatched_theta <- setdiff(theta_names, mrg_param_names)

  # Build mapping table
  rows <- lapply(mrg_param_names, function(p) {
    idx <- which(theta_names == p)
    if (length(idx) > 0L) {
      data.frame(
        mrgsolve = p,
        theta = names(theta_labels)[idx[1]],
        status = "OK",
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        mrgsolve = p,
        theta = NA_character_,
        status = "non mappe",
        stringsAsFactors = FALSE
      )
    }
  })
  mapping_table <- do.call(rbind, rows)

  list(
    matched = matched,
    unmatched_mrg = unmatched_mrg,
    unmatched_theta = unmatched_theta,
    mapping_table = mapping_table
  )
}


# =============================================================================
# Extraction dosing depuis .tab
# =============================================================================

#' Extrait les evenements de dose depuis le .tab
#'
#' Cherche AMT (example7), puis DOSE (example6), puis retourne amt = NA.
#' Groupe par ID pour detecter les bras (IV vs SC).
#'
#' @param tab_data Tibble retourne par read_tab()
#' @return Tibble : id, time, cmt, evid, amt, rate (amt = NA si absent du .tab)
extract_dosing_from_tab <- function(tab_data) {
  if (is.null(tab_data) || nrow(tab_data) == 0L) {
    return(NULL)
  }

  # Filter dose events
  if ("EVID" %in% names(tab_data)) {
    doses <- dplyr::filter(tab_data, EVID == 1)
  } else {
    return(NULL) # Cannot identify doses without EVID
  }

  if (nrow(doses) == 0L) {
    return(NULL)
  }

  # Use table_no == 1 for robust designs (avoid duplicating across subproblems)
  if ("table_no" %in% names(doses) && dplyr::n_distinct(doses$table_no) > 1L) {
    doses <- dplyr::filter(doses, table_no == 1L)
  }

  # Build standardized dosing tibble
  result <- tibble::tibble(
    id = if ("ID" %in% names(doses)) doses$ID else 1,
    time = doses$TIME,
    cmt = if ("CMT" %in% names(doses)) doses$CMT else 1L,
    evid = 1L
  )

  # AMT priority: AMT column > DOSE column > NA

  if ("AMT" %in% names(doses)) {
    result$amt <- doses$AMT
  } else if ("DOSE" %in% names(doses)) {
    result$amt <- doses$DOSE
  } else {
    result$amt <- NA_real_
  }

  # Keep missing/invalid RATE explicit so a manual per-arm value can fill it.
  # Remaining missing values fall back to bolus (RATE = 0) downstream.
  result$rate <- if ("RATE" %in% names(doses)) {
    suppressWarnings(as.numeric(doses$RATE))
  } else {
    rep(NA_real_, nrow(doses))
  }
  result$rate[!is.finite(result$rate) | result$rate < 0] <- NA_real_

  result
}


# =============================================================================
# Construction d'evenements de dose mrgsolve
# =============================================================================

#' Combine le schedule de dose (.tab) avec la config utilisateur (AMT/RATE)
#'
#' @param dose_schedule Tibble de extract_dosing_from_tab()
#' @param dose_config   Liste optionnelle par bras : list("1" = list(amt=300, rate=0))
#' @return Data frame compatible mrgsolve (colonnes: ID, time, amt, cmt, evid, rate)
build_dosing_events <- function(dose_schedule, dose_config = NULL) {
  if (is.null(dose_schedule) || nrow(dose_schedule) == 0L) {
    return(NULL)
  }

  result <- dose_schedule

  if (!"rate" %in% names(result)) {
    result$rate <- rep(NA_real_, nrow(result))
  }
  result$rate <- suppressWarnings(as.numeric(result$rate))
  result$rate[!is.finite(result$rate) | result$rate < 0] <- NA_real_

  # Fill missing AMT and RATE values from the per-arm user configuration.
  if (!is.null(dose_config)) {
    for (arm_id in names(dose_config)) {
      arm_rows <- !is.na(result$id) & result$id == as.numeric(arm_id)
      cfg <- dose_config[[arm_id]]
      if (!is.null(cfg$amt) && any(arm_rows, na.rm = TRUE)) {
        result$amt[arm_rows & is.na(result$amt)] <- cfg$amt
      }
      cfg_rate <- suppressWarnings(as.numeric(cfg$rate))
      valid_rate <- length(cfg_rate) == 1L &&
        !is.na(cfg_rate) &&
        is.finite(cfg_rate) &&
        cfg_rate >= 0
      if (valid_rate && any(arm_rows, na.rm = TRUE)) {
        result$rate[arm_rows & is.na(result$rate)] <- cfg_rate
      }
    }
  }

  # RATE = 0 is the mrgsolve bolus default when neither the table nor the
  # manual configuration provides a usable infusion rate.
  result$rate[is.na(result$rate)] <- 0

  # Rename to mrgsolve conventions
  data.frame(
    ID = as.integer(result$id),
    time = result$time,
    amt = result$amt,
    cmt = as.integer(result$cmt),
    evid = 1L,
    rate = result$rate,
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Compilation du modele mrgsolve
# =============================================================================

#' Calcule un hash deterministe du code mrgsolve
#'
#' @param code_text Code mrgsolve en texte (contenu d'un fichier .cpp).
#' @param n_chars Nombre de caracteres MD5 a conserver, entre 1 et 32.
#' @return Chaine hexadecimale identifiant le contenu du code.
mrgsolve_code_hash <- function(code_text, n_chars = 8L) {
  if (is.null(code_text)) {
    code_text <- ""
  }
  n_chars <- suppressWarnings(as.integer(n_chars))
  if (
    length(n_chars) != 1L || is.na(n_chars) || n_chars < 1L || n_chars > 32L
  ) {
    stop("n_chars must be an integer between 1 and 32.", call. = FALSE)
  }
  code_text <- paste(code_text, collapse = "\n")
  tmp <- tempfile(fileext = ".cpp")
  con <- file(tmp, open = "wb")
  on.exit(
    {
      try(close(con), silent = TRUE)
      unlink(tmp)
    },
    add = TRUE
  )
  writeBin(charToRaw(enc2utf8(code_text)), con)
  close(con)
  unname(substr(tools::md5sum(tmp), 1L, n_chars))
}

mrgsolve_sim_to_data_frame <- function(sim) {
  if (inherits(sim, "mrgsims") && methods::is(sim, "mrgsims")) {
    return(as.data.frame(sim@data))
  }
  as.data.frame(sim)
}

#' Compile un modele mrgsolve depuis du code texte
#'
#' Utilise mrgsolve::mcode() (pas mcode_cache pour eviter le stale cache).
#' Capture les erreurs de compilation C++.
#'
#' @param code_text Code mrgsolve en texte (contenu d'un fichier .cpp).
#' @param model_name Nom unique pour le modele (defaut: hash du code).
#' @return Objet modele mrgsolve, ou character (message d'erreur).
compile_mrgsolve_model <- function(code_text, model_name = NULL) {
  if (!has_mrgsolve()$available) {
    return(has_mrgsolve()$reason)
  }

  if (is.null(model_name)) {
    model_name <- paste0("mod_", mrgsolve_code_hash(code_text))
  }

  tryCatch(
    {
      mod <- mrgsolve::mcode(
        model_name,
        code_text,
        compile = TRUE,
        quiet = TRUE
      )
      mod
    },
    error = function(e) {
      msg <- conditionMessage(e)
      # Clean up typical C++ compilation noise
      paste("Erreur de compilation:", msg)
    }
  )
}


# =============================================================================
# Simulation PK
# =============================================================================

#' Simule un profil PK haute resolution avec mrgsolve
#'
#' @param mod      Objet modele mrgsolve compile
#' @param params   Liste nommee de parametres (THETAs)
#' @param omega    Matrice OMEGA (sera mise a zero pour sujet typique)
#' @param sigma    Matrice SIGMA (sera mise a zero pour sujet typique)
#' @param events   Data frame d'evenements de dose (de build_dosing_events)
#' @param end_time Temps de fin de simulation (en heures)
#' @param delta    Pas de temps pour la resolution (defaut: 0.5h)
#' @return Tibble : time, IPRED, cmt, arm (ID original)
simulate_pk_profile <- function(
  mod,
  params,
  omega = NULL,
  sigma = NULL,
  events,
  end_time = NULL,
  delta = 0.5
) {
  if (is.null(mod) || is.character(mod)) {
    return(NULL)
  }
  if (is.null(events) || nrow(events) == 0L) {
    return(NULL)
  }

  # Set parameter values (only those present in the model)
  mod_params <- names(mrgsolve::param(mod))
  params_to_set <- params[names(params) %in% mod_params]
  if (length(params_to_set) > 0L) {
    mod <- mrgsolve::param(mod, params_to_set)
  }

  # Zero out random effects for typical subject (ETA=0, EPS=0)
  mod <- mrgsolve::zero_re(mod)

  # Determine end time
  if (is.null(end_time)) {
    end_time <- max(events$time, na.rm = TRUE) * 1.15
  }

  # Simulate per arm (unique ID)
  arm_ids <- sort(unique(events$ID))
  results <- lapply(arm_ids, function(arm) {
    arm_events <- events[events$ID == arm, , drop = FALSE]

    # Remove rows with NA amt (user hasn't configured doses)
    arm_events <- arm_events[!is.na(arm_events$amt), , drop = FALSE]
    if (nrow(arm_events) == 0L) {
      return(NULL)
    }

    tryCatch(
      {
        sim <- mrgsolve::mrgsim(
          mod,
          data = arm_events,
          end = end_time,
          delta = delta,
          carry_out = "cmt,evid",
          recover = "ID"
        )
        out <- mrgsolve_sim_to_data_frame(sim)
        # Keep only observation rows (evid == 0 in mrgsolve output)
        out <- out[out$evid == 0, , drop = FALSE]
        # Detect Y variable: prefer IPRED, then DV, then first CMT column
        y_col <- intersect(c("IPRED", "DV", "Y", "CP"), names(out))[1]
        if (is.na(y_col)) {
          # Use the first compartment column from mrgsolve output
          cmt_cols <- setdiff(
            names(out),
            c("ID", "time", "evid", "cmt", "amt", "rate")
          )
          if (length(cmt_cols) > 0L) y_col <- cmt_cols[1]
        }
        if (is.na(y_col)) {
          return(NULL)
        }

        tibble::tibble(
          time = out$time,
          IPRED = out[[y_col]],
          cmt = if ("cmt" %in% names(out)) out$cmt else 1L,
          arm = arm
        )
      },
      error = function(e) {
        warning("Simulation echouee pour arm ", arm, ": ", conditionMessage(e))
        NULL
      }
    )
  })

  dplyr::bind_rows(results)
}
