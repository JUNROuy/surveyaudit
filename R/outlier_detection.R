#' Hidiroglou-Berthelot outlier detection for panel/longitudinal survey data
#'
#' Detects register errors in continuous variables by analyzing the ratio
#' between two measurements of the same unit (e.g. two time periods).
#' Implements the standard used in official statistics agencies.
#'
#' @param yt1 Numeric vector. First measurement (e.g. period t1).
#' @param yt2 Numeric vector. Second measurement (e.g. period t2).
#'   Must be the same length as \code{yt1}.
#' @param U Numeric (0-1). Magnitude weight. Default 0.5 (standard).
#' @param C Numeric. Interval width multiplier. Default 7. Use 4 for stricter
#'   detection.
#' @param A Numeric. Small positive constant to handle concentrated ratios.
#'   Default 0.05.
#' @param pct Numeric. Quantile to use. Default 0.25 (quartiles). Use 0.10
#'   when more than 1/4 of units share the same ratio (Hidiroglou-Emond 2018).
#'
#' @return A list with:
#' \describe{
#'   \item{outlier}{Logical vector. TRUE = potential outlier.}
#'   \item{score}{Numeric vector. E_i scores (sort by abs value for severity).}
#'   \item{limits}{Named numeric vector with lower and upper bounds.}
#'   \item{method}{Character string with method name and parameters used.}
#' }
#'
#' @examples
#' set.seed(42)
#' yt1 <- c(100, 200, 150, 300, 250, 180, 220, 99999, 170, 190)
#' yt2 <- c(105, 210, 148, 315, 245, 185, 225, 100500, 168, 195)
#' result <- detect_hb(yt1, yt2)
#' result$outlier
#'
#' @export
detect_hb <- function(yt1, yt2, U = 0.5, C = 7, A = 0.05, pct = 0.25) {
  if (!is.numeric(yt1) || !is.numeric(yt2))
    stop("`yt1` and `yt2` must be numeric vectors.")
  if (length(yt1) != length(yt2))
    stop("`yt1` and `yt2` must have the same length.")
  if (U < 0 || U > 1) stop("`U` must be between 0 and 1.")
  if (C <= 0)          stop("`C` must be positive.")

  # Remove zeros and NAs (undefined ratios)
  valid <- !is.na(yt1) & !is.na(yt2) & yt1 != 0 & yt2 != 0
  n     <- length(yt1)
  score <- rep(NA_real_, n)

  if (sum(valid) < 4) {
    warning("Too few valid observations for HB method.")
    return(.hb_empty_result(n, U, C, A))
  }

  r  <- yt2[valid] / yt1[valid]
  rM <- stats::median(r)

  # Step 1: symmetry transformation (s_i)
  s <- ifelse(r < rM, 1 - rM / r, r / rM - 1)

  # Step 2: magnitude score (E_i)
  mag   <- pmax(yt1[valid], yt2[valid])
  E     <- s * mag ^ U

  # Robust quantiles for the interval
  EQ1 <- stats::quantile(E, pct,       names = FALSE)
  EM  <- stats::median(E)
  EQ3 <- stats::quantile(E, 1 - pct,   names = FALSE)

  dQ1 <- max(EM - EQ1, abs(A * EM))
  dQ3 <- max(EQ3 - EM, abs(A * EM))

  lower <- EM - C * dQ1
  upper <- EM + C * dQ3

  score[valid]   <- E
  outlier        <- rep(FALSE, n)
  outlier[valid] <- E < lower | E > upper
  outlier[!valid] <- NA

  list(
    outlier = outlier,
    score   = score,
    limits  = c(lower = lower, upper = upper),
    method  = paste0("Hidiroglou-Berthelot (U=", U, ", C=", C,
                     ", A=", A, ", pct=", pct, ")")
  )
}

.hb_empty_result <- function(n, U, C, A) {
  list(
    outlier = rep(NA, n),
    score   = rep(NA_real_, n),
    limits  = c(lower = NA_real_, upper = NA_real_),
    method  = paste0("Hidiroglou-Berthelot (U=", U, ", C=", C, ", A=", A, ")")
  )
}


#' Skewness-Adjusted Boxplot outlier detection (SABP)
#'
#' Detects outliers in univariate distributions using the adjusted boxplot
#' method (Hubert & Vandervieren, 2008). Uses the medcouple (M) measure of
#' skewness to adjust whisker limits, avoiding false alarms in naturally
#' skewed distributions with long tails.
#'
#' @param x Numeric vector. The variable to analyze.
#' @param k Numeric. Whisker multiplier. Fixed at 1.5 per the SABP definition.
#'
#' @return A list with:
#' \describe{
#'   \item{outlier}{Logical vector. TRUE = potential outlier.}
#'   \item{score}{Numeric vector. Signed distance from nearest fence (negative
#'     = below lower, positive = above upper). NA for non-outliers.}
#'   \item{limits}{Named numeric vector with lower and upper fence values.}
#'   \item{medcouple}{Numeric. M skewness value used (-1 to 1).}
#'   \item{method}{Character string describing the method.}
#' }
#'
#' @examples
#' set.seed(42)
#' x <- c(rlnorm(95, meanlog = 5), 1e7, 1e8, 2e8, 3e8, 4e8)
#' result <- detect_sabp(x)
#' sum(result$outlier, na.rm = TRUE)
#'
#' @export
detect_sabp <- function(x, k = 1.5) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")
  if (k != 1.5)       warning("SABP requires k=1.5. Other values alter method validity.")

  valid   <- !is.na(x)
  n       <- length(x)
  outlier <- rep(NA, n)
  score   <- rep(NA_real_, n)

  x_valid <- x[valid]
  if (length(x_valid) < 4) {
    warning("Too few valid observations for SABP.")
    return(.sabp_empty_result(n))
  }

  Q1  <- stats::quantile(x_valid, 0.25, names = FALSE)
  Q3  <- stats::quantile(x_valid, 0.75, names = FALSE)
  IQR <- Q3 - Q1
  M   <- .medcouple(x_valid)

  if (abs(M) > 0.6) {
    warning("Medcouple M=", round(M, 3), " outside [-0.6, 0.6]; SABP fences may be unreliable.")
  }

  if (M >= 0) {
    lower <- Q1 - k * exp(-4 * M) * IQR
    upper <- Q3 + k * exp( 3 * M) * IQR
  } else {
    lower <- Q1 - k * exp(-3 * M) * IQR
    upper <- Q3 + k * exp( 4 * M) * IQR
  }

  is_out          <- x_valid < lower | x_valid > upper
  outlier[valid]  <- is_out

  # Score: signed distance from nearest fence for outliers only
  dist_lower <- lower - x_valid
  dist_upper <- x_valid - upper
  s <- ifelse(is_out & x_valid < lower, -dist_lower,
         ifelse(is_out & x_valid > upper, dist_upper, NA_real_))
  score[valid] <- s

  list(
    outlier   = outlier,
    score     = score,
    limits    = c(lower = lower, upper = upper),
    medcouple = M,
    method    = paste0("SABP - Skewness-Adjusted Boxplot (k=", k,
                       ", M=", round(M, 4), ")")
  )
}

.sabp_empty_result <- function(n) {
  list(
    outlier   = rep(NA, n),
    score     = rep(NA_real_, n),
    limits    = c(lower = NA_real_, upper = NA_real_),
    medcouple = NA_real_,
    method    = "SABP - insufficient data"
  )
}

# Medcouple: robust measure of skewness (Brys et al. 2004)
# MC = median { sign(x_j - x_i) * h(x_i, x_j) } for all x_i < median < x_j
.medcouple <- function(x) {
  x   <- sort(x[!is.na(x)])
  med <- stats::median(x)
  z_p <- x[x > med]  # above median
  z_m <- x[x < med]  # below median

  if (length(z_p) == 0 || length(z_m) == 0) return(0)

  h_vals <- outer(z_m, z_p, function(xi, xj) {
    denom <- xj - xi
    ifelse(denom == 0, sign(length(z_p) - length(z_m)),
           ((xj - med) - (med - xi)) / denom)
  })

  stats::median(as.vector(h_vals))
}


#' Apply both HB and SABP outlier methods to multiple survey variables
#'
#' Convenience wrapper that runs outlier detection across a set of numeric
#' variables in a data.frame and returns a tidy summary table.
#'
#' @param data A data.frame with raw survey data.
#' @param vars Character vector of numeric column names to analyze.
#' @param method One of \code{"sabp"}, \code{"hb"}, or \code{"both"}.
#'   For \code{"hb"}, \code{vars} must be a list of two-element character
#'   vectors: \code{list(c("yt1_var", "yt2_var"), ...)}.
#' @param ... Additional arguments passed to \code{detect_hb} or
#'   \code{detect_sabp}.
#'
#' @return A data.frame with columns: variable, method, n_outliers,
#'   pct_outliers, lower_limit, upper_limit.
#'
#' @examples
#' df <- data.frame(
#'   ingreso  = c(rlnorm(48, 10), 1e9, 2e9),
#'   edad     = c(sample(18:80, 49, TRUE), 200)
#' )
#' outlier_summary(df, c("ingreso", "edad"), method = "sabp")
#'
#' @export
outlier_summary <- function(data, vars, method = "sabp", ...) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  method <- match.arg(method, c("sabp", "hb", "both"))

  results <- lapply(vars, function(v) {
    if (!v %in% names(data)) stop("Variable not found: ", v)
    x <- data[[v]]
    if (!is.numeric(x)) {
      warning("Skipping non-numeric variable: ", v)
      return(NULL)
    }
    res <- detect_sabp(x, ...)
    n_out <- sum(res$outlier, na.rm = TRUE)
    n_val <- sum(!is.na(x))
    data.frame(
      variable     = v,
      metodo       = "SABP",
      n_outliers   = n_out,
      pct_outliers = round(n_out / n_val * 100, 2),
      limite_inf   = round(res$limits["lower"], 4),
      limite_sup   = round(res$limits["upper"], 4),
      medcouple    = round(res$medcouple, 4),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, Filter(Negate(is.null), results))
  rownames(out) <- NULL
  out
}
