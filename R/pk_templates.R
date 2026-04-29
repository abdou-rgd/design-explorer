# =============================================================================
# pk_templates.R
# Closed-form concentration-time functions for standard NONMEM ADVANs.
# Vectorized over `times`. Multi-dose via linear superposition.
#
# Exports:
#   pk_1cpt_iv, pk_1cpt_oral, pk_2cpt_iv, pk_2cpt_oral
#   pk_template_for_advan() — dispatch on ADVANn TRANSm
# =============================================================================


# ---- Utilities ---------------------------------------------------------------

.superpose <- function(times, dose_times, single_dose_fn) {
  # Sum single-dose curves shifted by each dose time. Pre-dose contribution
  # is zeroed so a future dose doesn't leak into past times:
  #   C_total(t) = sum_k C(t - t_k) * I(t >= t_k)
  mat <- vapply(dose_times, function(dt) {
    out <- single_dose_fn(pmax(times - dt, 0))
    out[times < dt] <- 0
    out
  }, numeric(length(times)))
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1)
  rowSums(mat)
}


# ---- 1-cpt IV bolus / infusion ----------------------------------------------

#' 1-compartment IV concentration-time.
#'
#' @param times Numeric vector of time points (>=0).
#' @param dose  Scalar dose amount.
#' @param rate  NULL for bolus; positive scalar for zero-order infusion.
#' @param CL    Clearance.
#' @param V     Volume of distribution.
#' @param dose_times Optional numeric vector for multi-dose superposition.
#'                   Default = 0 (single dose at t=0).
#' @return Numeric vector of concentrations, same length as `times`.
#' @export
pk_1cpt_iv <- function(times, dose, rate = NULL, CL, V, dose_times = 0) {
  ke <- CL / V
  single <- if (is.null(rate) || !is.finite(rate) || rate <= 0) {
    function(t) (dose / V) * exp(-ke * t)
  } else {
    tinf <- dose / rate
    function(t) {
      during <- t <= tinf
      c_inf  <- (rate / (V * ke)) * (1 - exp(-ke * t))
      c_post <- (rate / (V * ke)) * (1 - exp(-ke * tinf)) * exp(-ke * (t - tinf))
      ifelse(during, c_inf, c_post)
    }
  }
  .superpose(times, dose_times, single)
}


# ---- 1-cpt oral (first-order absorption) ------------------------------------

#' 1-compartment first-order oral absorption — Bateman equation.
#'
#' @export
pk_1cpt_oral <- function(times, dose, CL, V, KA, F = 1, dose_times = 0) {
  ke <- CL / V
  single <- if (abs(KA - ke) < 1e-10) {
    # Flip-flop / equal rates — use the singular-case formula
    function(t) (F * dose * ke / V) * t * exp(-ke * t)
  } else {
    function(t) (F * dose * KA) / (V * (KA - ke)) * (exp(-ke * t) - exp(-KA * t))
  }
  .superpose(times, dose_times, single)
}


# ---- 2-cpt IV ---------------------------------------------------------------

# Internal: macro-rate constants (alpha, beta, A, B) from micro (CL, V2, Q, V3).
.two_cpt_macro <- function(CL, V2, Q, V3) {
  k10 <- CL / V2
  k12 <- Q  / V2
  k21 <- Q  / V3
  s   <- k10 + k12 + k21
  disc <- sqrt(max(s^2 - 4 * k10 * k21, 0))
  alpha <- (s + disc) / 2
  beta  <- (s - disc) / 2
  list(alpha = alpha, beta = beta, k21 = k21)
}

#' 2-compartment IV concentration-time (bolus).
#'
#' @export
pk_2cpt_iv <- function(times, dose, rate = NULL, CL, V2, Q, V3, dose_times = 0) {
  if (!is.null(rate) && is.finite(rate) && rate > 0) {
    stop("pk_2cpt_iv() closed-form template currently supports IV bolus only; use mrgsolve for infusion designs.")
  }
  mac <- .two_cpt_macro(CL, V2, Q, V3)
  alpha <- mac$alpha; beta <- mac$beta; k21 <- mac$k21
  A <- (dose / V2) * (alpha - k21) / (alpha - beta)
  B <- (dose / V2) * (k21   - beta ) / (alpha - beta)
  single <- function(t) A * exp(-alpha * t) + B * exp(-beta * t)
  .superpose(times, dose_times, single)
}


# ---- 2-cpt oral (1st-order absorption) --------------------------------------

#' 2-compartment first-order oral absorption.
#'
#' @export
pk_2cpt_oral <- function(times, dose, CL, V2, Q, V3, KA, F = 1, dose_times = 0) {
  mac <- .two_cpt_macro(CL, V2, Q, V3)
  alpha <- mac$alpha; beta <- mac$beta; k21 <- mac$k21
  coef_a <- (F * dose * KA * (k21 - alpha)) / (V2 * (KA - alpha) * (beta - alpha))
  coef_b <- (F * dose * KA * (k21 - beta )) / (V2 * (KA - beta ) * (alpha - beta))
  coef_k <- (F * dose * KA * (k21 - KA   )) / (V2 * (alpha - KA) * (beta - KA))
  single <- function(t) coef_a * exp(-alpha * t) +
                        coef_b * exp(-beta  * t) +
                        coef_k * exp(-KA    * t)
  .superpose(times, dose_times, single)
}


# ---- Dispatch on ADVAN/TRANS ------------------------------------------------

#' Return the template function matching a parsed ADVAN/TRANS pair.
#'
#' @param advan_trans Output of parse_advan_trans(), e.g. list(advan="ADVAN4", trans="TRANS4")
#' @return Named list: list(fn = function, required_params = character vector), or NULL.
#' @export
pk_template_for_advan <- function(advan_trans) {
  if (is.null(advan_trans)) return(NULL)
  `%||%` <- function(x, y) if (is.null(x)) y else x
  key <- paste(advan_trans$advan, advan_trans$trans %||% "")
  switch(trimws(key),
    "ADVAN1 TRANS1" = list(fn = pk_1cpt_iv,   required = c("CL", "V")),
    "ADVAN1 TRANS2" = list(fn = pk_1cpt_iv,   required = c("CL", "V")),
    "ADVAN2 TRANS1" = list(fn = pk_1cpt_oral, required = c("CL", "V", "KA")),
    "ADVAN2 TRANS2" = list(fn = pk_1cpt_oral, required = c("CL", "V", "KA")),
    "ADVAN3 TRANS3" = list(fn = pk_2cpt_iv,   required = c("CL", "V2", "Q", "V3")),
    "ADVAN3 TRANS4" = list(fn = pk_2cpt_iv,   required = c("CL", "V2", "Q", "V3")),
    "ADVAN4 TRANS3" = list(fn = pk_2cpt_oral, required = c("CL", "V2", "Q", "V3", "KA")),
    "ADVAN4 TRANS4" = list(fn = pk_2cpt_oral, required = c("CL", "V2", "Q", "V3", "KA")),
    NULL
  )
}
