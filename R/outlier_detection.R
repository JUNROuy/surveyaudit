#' Detección de outliers por el método Hidiroglou-Berthelot (HB)
#'
#' Detecta errores de registro en variables continuas analizando la razón entre
#' dos mediciones de la misma unidad (p.ej. dos periodos de tiempo). Es el
#' estándar usado en institutos de estadística oficiales.
#'
#' @param yt1 Vector numérico. Primera medición (p.ej. periodo t1).
#' @param yt2 Vector numérico. Segunda medición (p.ej. periodo t2).
#'   Debe tener el mismo largo que \code{yt1}.
#' @param U Numérico (0-1). Peso de magnitud. Por defecto 0.5 (estándar).
#' @param C Numérico. Multiplicador de amplitud del intervalo. Por defecto 7.
#'   Usar 4 para detección más estricta.
#' @param A Numérico. Constante positiva pequeña para razones muy concentradas.
#'   Por defecto 0.05 (Hidiroglou & Berthelot 1986).
#' @param pct Numérico. Cuantil a usar. Por defecto 0.25 (cuartiles). Usar
#'   0.10 cuando más de 1/4 de las unidades comparten la misma razón
#'   (Hidiroglou & Emond 2018).
#'
#' @return Una lista con:
#' \describe{
#'   \item{outlier}{Vector lógico. TRUE = posible outlier.}
#'   \item{score}{Vector numérico con los scores E_i. Ordenar por valor
#'     absoluto para priorizar por gravedad.}
#'   \item{limits}{Vector numérico nombrado con límites inferior y superior.}
#'   \item{method}{Cadena con el nombre del método y parámetros usados.}
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
    stop("`yt1` y `yt2` deben ser vectores numéricos.")
  if (length(yt1) != length(yt2))
    stop("`yt1` y `yt2` deben tener el mismo largo.")
  if (U < 0 || U > 1) stop("`U` debe estar entre 0 y 1.")
  if (C <= 0)          stop("`C` debe ser positivo.")

  valid <- !is.na(yt1) & !is.na(yt2) & yt1 != 0 & yt2 != 0
  n     <- length(yt1)
  score <- rep(NA_real_, n)

  if (sum(valid) < 4) {
    warning("Observaciones válidas insuficientes para el método HB.")
    return(.hb_empty_result(n, U, C, A))
  }

  r  <- yt2[valid] / yt1[valid]
  rM <- stats::median(r)

  s   <- ifelse(r < rM, 1 - rM / r, r / rM - 1)
  mag <- pmax(yt1[valid], yt2[valid])
  E   <- s * mag ^ U

  EQ1 <- stats::quantile(E, pct,       names = FALSE)
  EM  <- stats::median(E)
  EQ3 <- stats::quantile(E, 1 - pct,   names = FALSE)

  dQ1 <- max(EM - EQ1, abs(A * EM))
  dQ3 <- max(EQ3 - EM, abs(A * EM))

  lower <- EM - C * dQ1
  upper <- EM + C * dQ3

  score[valid]    <- E
  outlier         <- rep(FALSE, n)
  outlier[valid]  <- E < lower | E > upper
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


#' Detección de outliers por Boxplot ajustado por asimetría (SABP)
#'
#' Detecta outliers univariados usando el método de boxplot ajustado
#' (Hubert & Vandervieren, 2008). Utiliza el medcouple (M) como medida de
#' asimetría para ajustar los límites de los bigotes, evitando falsas alarmas
#' en distribuciones con colas largas naturales.
#'
#' @param x Vector numérico. La variable a analizar.
#' @param k Numérico. Multiplicador de bigotes. Fijo en 1.5 por definición del
#'   método SABP.
#'
#' @return Una lista con:
#' \describe{
#'   \item{outlier}{Vector lógico. TRUE = posible outlier.}
#'   \item{score}{Vector numérico con la distancia firmada al límite más
#'     cercano (negativo = bajo límite inferior, positivo = sobre límite
#'     superior). NA para no-outliers.}
#'   \item{limits}{Vector numérico nombrado con los límites inferior y superior.}
#'   \item{medcouple}{Numérico. Valor M de asimetría usado (-1 a 1).}
#'   \item{method}{Cadena con el nombre del método y parámetros.}
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
  if (!is.numeric(x)) stop("`x` debe ser un vector numérico.")
  if (k != 1.5) warning("SABP requiere k=1.5. Otros valores alteran la validez del método.")

  valid   <- !is.na(x)
  n       <- length(x)
  outlier <- rep(NA, n)
  score   <- rep(NA_real_, n)

  x_valid <- x[valid]
  if (length(x_valid) < 4) {
    warning("Observaciones válidas insuficientes para SABP.")
    return(.sabp_empty_result(n))
  }

  Q1  <- stats::quantile(x_valid, 0.25, names = FALSE)
  Q3  <- stats::quantile(x_valid, 0.75, names = FALSE)
  IQR <- Q3 - Q1
  M   <- .medcouple(x_valid)

  if (abs(M) > 0.6) {
    warning("Medcouple M=", round(M, 3),
            " fuera de [-0.6, 0.6]; los límites SABP pueden no ser confiables.")
  }

  if (M >= 0) {
    lower <- Q1 - k * exp(-4 * M) * IQR
    upper <- Q3 + k * exp( 3 * M) * IQR
  } else {
    lower <- Q1 - k * exp(-3 * M) * IQR
    upper <- Q3 + k * exp( 4 * M) * IQR
  }

  is_out         <- x_valid < lower | x_valid > upper
  outlier[valid] <- is_out

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
    method    = paste0("SABP - Boxplot ajustado por asimetría (k=", k,
                       ", M=", round(M, 4), ")")
  )
}

.sabp_empty_result <- function(n) {
  list(
    outlier   = rep(NA, n),
    score     = rep(NA_real_, n),
    limits    = c(lower = NA_real_, upper = NA_real_),
    medcouple = NA_real_,
    method    = "SABP - datos insuficientes"
  )
}

# Medcouple: medida robusta de asimetría (Brys et al. 2004)
.medcouple <- function(x) {
  x   <- sort(x[!is.na(x)])
  med <- stats::median(x)
  z_p <- x[x > med]
  z_m <- x[x < med]

  if (length(z_p) == 0 || length(z_m) == 0) return(0)

  h_vals <- outer(z_m, z_p, function(xi, xj) {
    denom <- xj - xi
    ifelse(denom == 0, sign(length(z_p) - length(z_m)),
           ((xj - med) - (med - xi)) / denom)
  })

  stats::median(as.vector(h_vals))
}


#' Resumen de outliers para múltiples variables de la encuesta
#'
#' Función de conveniencia que aplica detección de outliers a un conjunto de
#' variables numéricas de un data.frame y devuelve una tabla resumen ordenada.
#'
#' @param data Un data.frame con los datos crudos de la encuesta.
#' @param vars Vector de caracteres con los nombres de las columnas a analizar.
#' @param method Uno de \code{"sabp"}, \code{"hb"} o \code{"both"}.
#' @param ... Argumentos adicionales pasados a \code{detect_sabp}.
#'
#' @return Un data.frame con columnas: variable, metodo, n_outliers,
#'   pct_outliers, limite_inf, limite_sup, medcouple.
#'
#' @examples
#' df <- data.frame(
#'   ingreso = c(rlnorm(48, 10), 1e9, 2e9),
#'   edad    = c(sample(18:80, 49, TRUE), 200)
#' )
#' outlier_summary(df, c("ingreso", "edad"), method = "sabp")
#'
#' @export
outlier_summary <- function(data, vars, method = "sabp", ...) {
  if (!is.data.frame(data)) stop("`data` debe ser un data.frame.")
  method <- match.arg(method, c("sabp", "hb", "both"))

  results <- lapply(vars, function(v) {
    if (!v %in% names(data)) stop("Variable no encontrada: ", v)
    x <- data[[v]]
    if (!is.numeric(x)) {
      warning("Omitiendo variable no numérica: ", v)
      return(NULL)
    }
    res   <- detect_sabp(x, ...)
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
