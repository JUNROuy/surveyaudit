#' Auditoría completa de encuesta con clasificación automática de variables
#'
#' Función principal del paquete. Clasifica automáticamente el tipo de cada
#' variable del dataframe y aplica el método de auditoría correspondiente:
#' flujo y parteaguas para todas las variables, SABP para las continuas,
#' tablas de frecuencia para las categóricas y binarias.
#'
#' @param data Un data.frame con los datos crudos de la encuesta.
#' @param ordered_vars Vector de caracteres con las columnas en el orden lógico
#'   del cuestionario. Si es NULL, usa todas las columnas en el orden que
#'   aparecen en el dataframe.
#' @param umbral_categorica Entero. Número máximo de valores únicos para que
#'   una variable numérica sea tratada como categórica ordinal en lugar de
#'   continua. Por defecto 10.
#' @param threshold_low Numérico (0-1). Umbral bajo para alerta de flujo.
#'   Por defecto 0.90.
#' @param threshold_high Numérico (0-1). Umbral alto para alerta de flujo.
#'   Por defecto 0.80.
#'
#' @return Una lista de clase \code{survey_audit} con:
#' \describe{
#'   \item{clasificacion}{data.frame con el tipo detectado por variable.}
#'   \item{flujo}{Resultado de \code{flow_audit()}: parteaguas y alertas.}
#'   \item{outliers}{data.frame con outliers SABP para variables continuas.}
#'   \item{frecuencias}{Lista de tablas de frecuencia para categóricas/binarias.}
#'   \item{resumen}{data.frame integrado: tipo + alerta de flujo + outliers.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   edad     = c(25, 34, 45, 28, 60, 32, NA, 41, 38, 29),
#'   sexo     = c("M","F","M","F","M","F","M",NA,"F","M"),
#'   ingreso  = c(25000, 40000, NA, 31000, 55000, 28000, 33000, 47000, NA, 29000),
#'   satisf   = c(3, 4, 5, NA, 2, 4, 3, 5, NA, 4)
#' )
#' resultado <- survey_audit(df, c("edad", "sexo", "ingreso", "satisf"))
#' print(resultado)
#'
#' @export
survey_audit <- function(data,
                         ordered_vars    = NULL,
                         umbral_categorica = 10,
                         threshold_low   = 0.90,
                         threshold_high  = 0.80) {

  if (!is.data.frame(data)) stop("`data` debe ser un data.frame.")
  if (is.null(ordered_vars)) ordered_vars <- names(data)

  missing <- setdiff(ordered_vars, names(data))
  if (length(missing) > 0)
    stop("Variables no encontradas en data: ", paste(missing, collapse = ", "))

  # ── 1. Clasificar variables ───────────────────────────────────────────────
  clasificacion <- classify_vars(data[, ordered_vars, drop = FALSE],
                                 umbral_categorica = umbral_categorica)

  # ── 2. Auditoría de flujo (todas las variables) ───────────────────────────
  flujo <- flow_audit(data, ordered_vars,
                      threshold_low  = threshold_low,
                      threshold_high = threshold_high)

  # ── 3. Outliers SABP (solo variables continuas) ───────────────────────────
  vars_continuas <- clasificacion$variable[clasificacion$tipo == "continua"]
  outliers <- NULL
  if (length(vars_continuas) > 0) {
    outliers <- outlier_summary(data, vars_continuas, method = "sabp")
  }

  # ── 4. Frecuencias (categóricas, binarias y discretas) ───────────────────
  vars_freq <- clasificacion$variable[
    clasificacion$tipo %in% c("categorica", "binaria", "discreta")
  ]
  frecuencias <- lapply(stats::setNames(vars_freq, vars_freq), function(v) {
    x  <- data[[v]]
    tb <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
    data.frame(
      valor     = names(tb),
      n         = as.integer(tb),
      pct       = round(as.numeric(tb) / sum(!is.na(x)) * 100, 1),
      stringsAsFactors = FALSE
    )
  })

  # ── 5. Resumen integrado ──────────────────────────────────────────────────
  resumen <- merge(
    clasificacion[, c("variable", "tipo", "n_validos", "pct_na")],
    flujo[, c("variable", "parteaguas", "pct_entrada", "alerta")],
    by = "variable", sort = FALSE
  )

  if (!is.null(outliers)) {
    resumen <- merge(
      resumen,
      outliers[, c("variable", "n_outliers", "pct_outliers")],
      by = "variable", all.x = TRUE
    )
  } else {
    resumen$n_outliers   <- NA_integer_
    resumen$pct_outliers <- NA_real_
  }

  resumen$n_outliers[is.na(resumen$n_outliers)] <- 0L
  resumen <- resumen[match(ordered_vars, resumen$variable), ]
  rownames(resumen) <- NULL

  estructura <- list(
    clasificacion = clasificacion,
    flujo         = flujo,
    outliers      = outliers,
    frecuencias   = frecuencias,
    resumen       = resumen
  )
  class(estructura) <- "survey_audit"
  estructura
}


#' Clasificar automáticamente el tipo de cada variable de una encuesta
#'
#' Determina si cada columna es continua, discreta, categórica, binaria o fecha,
#' usando la clase R y la cardinalidad de valores únicos.
#'
#' @param data Un data.frame.
#' @param umbral_categorica Entero. Valores únicos máximos para considerar
#'   una variable numérica como discreta en lugar de continua. Por defecto 10.
#'
#' @return Un data.frame con columnas: variable, clase_r, tipo, n_validos,
#'   pct_na, n_unicos.
#'
#' @examples
#' df <- data.frame(
#'   edad    = c(25, 34, 45, 28, 60),
#'   sexo    = c("M", "F", "M", "F", "M"),
#'   satisf  = c(1L, 2L, 3L, 4L, 5L),
#'   activo  = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' )
#' classify_vars(df)
#'
#' @export
classify_vars <- function(data, umbral_categorica = 10) {
  if (!is.data.frame(data)) stop("`data` debe ser un data.frame.")

  resultado <- lapply(names(data), function(v) {
    x        <- data[[v]]
    clase_r  <- class(x)[1]
    n_total  <- length(x)
    n_na     <- sum(is.na(x))
    n_validos <- n_total - n_na
    x_validos <- x[!is.na(x)]
    n_unicos  <- length(unique(x_validos))

    tipo <- dplyr::case_when(
      inherits(x, c("Date", "POSIXct", "POSIXlt")) ~ "fecha",
      is.logical(x)                                 ~ "binaria",
      is.character(x) || is.factor(x)               ~ {
        if (n_unicos <= 2) "binaria" else "categorica"
      },
      is.numeric(x) && n_unicos <= 2                ~ "binaria",
      is.numeric(x) && n_unicos <= umbral_categorica ~ "discreta",
      is.numeric(x)                                 ~ "continua",
      TRUE                                          ~ "otro"
    )

    data.frame(
      variable  = v,
      clase_r   = clase_r,
      tipo      = tipo,
      n_validos = n_validos,
      pct_na    = round(n_na / n_total * 100, 1),
      n_unicos  = n_unicos,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, resultado)
}


#' Método print para objetos survey_audit
#'
#' @param x Un objeto survey_audit.
#' @param ... Ignorado.
#' @export
print.survey_audit <- function(x, ...) {
  cat("╔══════════════════════════════════════════════════════╗\n")
  cat("║         AUDITORÍA DE ENCUESTA — surveyaudit          ║\n")
  cat("╚══════════════════════════════════════════════════════╝\n\n")

  cat("── Clasificación de variables ──────────────────────────\n")
  print(x$clasificacion[, c("variable","tipo","n_validos","pct_na","n_unicos")],
        row.names = FALSE)

  cat("\n── Flujo y variable parteaguas ─────────────────────────\n")
  flujo_print <- x$flujo
  flujo_print$pct_entrada <- paste0(flujo_print$pct_entrada, "%")
  print(flujo_print, row.names = FALSE)

  if (!is.null(x$outliers) && nrow(x$outliers) > 0) {
    cat("\n── Outliers en variables continuas (SABP) ──────────────\n")
    print(x$outliers, row.names = FALSE)
  }

  if (length(x$frecuencias) > 0) {
    cat("\n── Frecuencias (variables categóricas/discretas) ───────\n")
    for (v in names(x$frecuencias)) {
      cat("\n  [", v, "]\n", sep = "")
      tb <- x$frecuencias[[v]]
      print(head(tb, 8), row.names = FALSE)
      if (nrow(tb) > 8) cat("  ... (", nrow(tb) - 8, " categorías más)\n", sep = "")
    }
  }

  cat("\n── Resumen integrado ───────────────────────────────────\n")
  print(x$resumen, row.names = FALSE)

  invisible(x)
}
