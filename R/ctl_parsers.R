# =============================================================================
# ctl_parsers.R
# Parseurs de control stream NONMEM (.ctl / .mod / .con)
#
# Contenu :
#   read_prior_nwpri()     — Parser $PRIOR NWPRI depuis un .ctl
#   parse_theta_labels()   — Extraire les labels THETA
#   parse_design_summary() — Resume des arguments $DESIGN pour nommage
#   parse_cmt_labels()     — Extraire les noms de compartiments depuis $MODEL
#   parse_groupsize()      — Extraire GROUPSIZE du bloc $DESIGN
#
# Prerequis : aucun (utilise seulement stringr et readr)
# =============================================================================

library(readr)
library(stringr)


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
    vals <- as.numeric(gsub("[dD]", "E", str_extract(vals, "-?[0-9.eEdD]+")))
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
        all_vals <- c(all_vals, as.numeric(gsub("[dD]", "E", nums)))
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

  # Collect ALL lines belonging to $THETA blocks (handles multiple $THETA blocks)
  dollar_lines <- which(str_detect(lines, "^\\$"))
  theta_lines_idx <- integer(0)
  for (ts in theta_start) {
    # Block ends at next non-$THETA $ line or end of file
    later <- dollar_lines[dollar_lines > ts]
    non_theta_later <- later[!later %in% theta_start]
    block_end <- if (length(non_theta_later) > 0L) non_theta_later[1] - 1L else length(lines)
    # Ranges may overlap when $THETA blocks are adjacent; sort(unique()) deduplicates
    theta_lines_idx <- c(theta_lines_idx, ts:block_end)
  }
  theta_lines_idx <- sort(unique(theta_lines_idx))

  # Helper: does this line contain a THETA value (number, bounds, or FIXED)?
  # Assumption: every THETA value line contains at least one digit.
  # Multi-value lines ($THETA 0.15 8.0 1.0) are counted as one THETA.
  has_value <- function(ln) {
    stripped <- sub(";.*", "", ln)          # remove comment
    stripped <- sub("^\\$THETA\\s*", "", stripped)  # remove $THETA keyword
    grepl("[0-9]", stripped)                # contains at least one digit
  }

  # Helper: extract label from comment after ;
  extract_label <- function(ln) {
    if (!grepl(";", ln)) return(NULL)
    comment <- sub("^[^;]*;\\s*", "", ln)
    if (nchar(comment) == 0L) return(NULL)
    # Strip Sanofi prefix: --thN- or --thN-- or similar
    comment <- sub("^[-]+\\s*(th\\d+)?[-]*\\s*", "", comment)
    # Strip brackets: [LABEL] -> LABEL
    comment <- sub("^\\[([^]]+)\\].*", "\\1", comment)
    # Take first word-like token (letters, digits, underscores, starting with letter)
    m <- regmatches(comment, regexpr("[A-Za-z][A-Za-z0-9_]*", comment))
    if (length(m) == 0L || nchar(m) == 0L) return(NULL)
    m
  }

  theta_idx <- 0L
  labels <- character(0)
  label_names <- character(0)
  for (i in theta_lines_idx) {
    ln <- lines[i]
    if (!has_value(ln)) next   # skip bare "$THETA" line or blank/comment-only lines
    theta_idx <- theta_idx + 1L
    lbl <- extract_label(ln)
    if (!is.null(lbl)) {
      labels <- c(labels, lbl)
      label_names <- c(label_names, paste0("THETA", theta_idx))
    }
  }

  if (length(labels) == 0L) return(NULL)
  setNames(labels, label_names)
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


# =============================================================================
# parse_groupsize() — Extraire GROUPSIZE du bloc $DESIGN
# =============================================================================

# =============================================================================
# parse_design_methods() — Extraire les methodes d'optimisation de chaque $DESIGN
# =============================================================================

#' Identifie la methode de chaque bloc $DESIGN dans un control stream.
#'
#' Chaque $DESIGN dans le .ctl produit un TABLE NO. dans le .ext.
#' Cette fonction retourne un vecteur nomme mappant table_no -> label methode.
#'
#' @param lines Character vector des lignes du fichier .ctl
#' @return Named character vector c("1" = "Eval (CTP)", "2" = "RS", ...)
#'         ou NULL si aucun $DESIGN trouve
#' @export
parse_design_methods <- function(lines) {
  if (is.null(lines) || length(lines) == 0L) return(NULL)

  lines_clean <- str_replace(lines, ";.*$", "")
  design_starts <- which(str_detect(lines_clean, "^\\s*\\$DESIGN\\b"))
  if (length(design_starts) == 0L) return(NULL)

  dollar_lines <- which(str_detect(lines_clean, "^\\s*\\$"))

  labels <- character(length(design_starts))
  for (k in seq_along(design_starts)) {
    ds <- design_starts[k]
    later <- dollar_lines[dollar_lines > ds]
    block_end <- if (length(later) > 0L) later[1L] - 1L else length(lines_clean)
    block <- paste(lines_clean[ds:block_end], collapse = " ")
    block <- toupper(block)

    # Extract key arguments
    get_val <- function(key) {
      m <- regmatches(block, regexpr(paste0("\\b", key, "\\s*=\\s*([A-Za-z0-9]+)"),
                                     block, perl = TRUE))
      if (length(m) == 0L || nchar(m) == 0L) return(NULL)
      toupper(sub(paste0(".*", key, "\\s*=\\s*"), "", m))
    }

    maxeval <- get_val("MAXEVAL")
    is_eval <- !is.null(maxeval) && maxeval == "0"

    # Detect method from METHOD= or from optimization keywords
    method <- NULL
    if (grepl("\\bNELDER\\b", block) || grepl("\\bSIMPLEX\\b", block)) {
      method <- "NELDER"
    } else if (grepl("\\bSTGR\\b", block) || grepl("\\bSTAGGER\\b", block)) {
      method <- "STGR"
    } else if (grepl("\\bFEDOROV\\b", block)) {
      method <- "FEDOROV"
    } else if (grepl("\\bDISCRETE\\b", block)) {
      method <- "DISCRETE"
    } else if (grepl("\\bRS\\b", block) || grepl("\\bRANDOM\\b", block)) {
      method <- "RS"
    }

    # Fallback: check METHOD= argument
    if (is.null(method)) {
      method_val <- get_val("METHOD")
      if (!is.null(method_val)) {
        method <- switch(method_val,
          "RANDOM" = "RS", "STAGGER" = "STGR",
          "SIMPLEX" = "NELDER", "NELDER" = "NELDER",
          "FEDOROV" = "FEDOROV",
          method_val  # fallback to raw value
        )
      }
    }

    if (is_eval) {
      labels[k] <- "Eval"
    } else if (!is.null(method)) {
      labels[k] <- method
    } else {
      labels[k] <- paste0("Bloc ", k)
    }
  }

  setNames(labels, as.character(seq_along(design_starts)))
}


#' Extrait la valeur GROUPSIZE= du bloc $DESIGN d'un control stream
#'
#' @param lines Vecteur de lignes du fichier .ctl/.mod/.con
#' @return Integer (GROUPSIZE) ou NA_integer_ si non trouve
parse_groupsize <- function(lines) {
  if (is.null(lines) || length(lines) == 0L) return(NA_integer_)
  design_idx <- which(stringr::str_detect(lines, "^\\s*\\$DESIGN"))
  if (length(design_idx) == 0L) return(NA_integer_)
  # Scan du bloc $DESIGN jusqu'au prochain bloc $ ou fin de fichier
  next_block <- which(stringr::str_detect(lines, "^\\s*\\$") &
                        seq_along(lines) > design_idx[1L])
  end_idx <- if (length(next_block) > 0L) next_block[1L] - 1L else length(lines)
  block <- paste(lines[design_idx[1L]:end_idx], collapse = " ")
  m <- regmatches(block, regexpr("GROUPSIZE\\s*=\\s*([0-9]+)", block, perl = TRUE))
  if (length(m) == 0L) return(NA_integer_)
  as.integer(sub(".*=\\s*", "", m))
}
