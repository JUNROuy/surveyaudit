#' Auditar el flujo de respuesta y detectar la variable parteaguas
#'
#' Reconstruye la secuencia de contestación a partir de un orden de variables
#' definido por el usuario, calcula el flujo poblacional (n) en cada paso,
#' identifica la variable "parteaguas" que provocó cada caída en el tamaño de
#' muestra y clasifica cada transición como salto legítimo o alerta de
#' no-respuesta.
#'
#' @param data Un data.frame con los datos crudos de la encuesta.
#' @param ordered_vars Vector de caracteres con los nombres de las columnas en
#'   el orden lógico en que aparecen en el cuestionario.
#' @param threshold_low Numérico (0-1). Porcentaje de entrada por debajo del
#'   cual se genera alerta "Baja". Por defecto 0.90.
#' @param threshold_high Numérico (0-1). Porcentaje de entrada por debajo del
#'   cual se genera alerta "Alta". Por defecto 0.80.
#'
#' @return Un data.frame de clase \code{flow_audit} con una fila por variable,
#'   conteniendo: variable, parteaguas, n_esperado, n_real, pct_entrada y alerta.
#'
#' @examples
#' df <- data.frame(
#'   p1 = c(1, 2, 1, 2, 1, 1, 2, 1, 2, 1),
#'   p2 = c(1, NA, 1, NA, 1, 1, NA, 1, NA, 1),
#'   p3 = c(NA, NA, 1, NA, 2, 1, NA, 2, NA, 1)
#' )
#' flow_audit(df, c("p1", "p2", "p3"))
#'
#' @export
flow_audit <- function(data, ordered_vars, threshold_low = 0.90,
                       threshold_high = 0.80) {
  if (!is.data.frame(data)) stop("`data` debe ser un data.frame.")
  missing_vars <- setdiff(ordered_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables no encontradas en data: ", paste(missing_vars, collapse = ", "))
  }

  n_total <- nrow(data)
  results <- vector("list", length(ordered_vars))

  current_parteaguas   <- "(Inicio)"
  current_parteaguas_n <- n_total

  for (i in seq_along(ordered_vars)) {
    var     <- ordered_vars[i]
    n_real  <- sum(!is.na(data[[var]]))
    n_esperado  <- current_parteaguas_n
    pct_entrada <- if (n_esperado > 0) n_real / n_esperado else NA_real_

    alerta <- .classify_alert(pct_entrada, threshold_low, threshold_high)

    results[[i]] <- data.frame(
      variable         = var,
      parteaguas       = current_parteaguas,
      n_esperado       = n_esperado,
      n_real           = n_real,
      pct_entrada      = round(pct_entrada * 100, 1),
      alerta           = alerta,
      stringsAsFactors = FALSE
    )

    if (!is.na(pct_entrada) && pct_entrada < threshold_low) {
      current_parteaguas   <- var
      current_parteaguas_n <- n_real
    }
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  class(out) <- c("flow_audit", "data.frame")
  out
}

#' @keywords internal
.classify_alert <- function(pct, threshold_low, threshold_high) {
  if (is.na(pct))            return("Sin datos")
  if (pct >= threshold_low)  return("OK")
  if (pct >= threshold_high) return("Baja (Perdida leve)")
  return("Alta (No respuesta)")
}

#' Método print para objetos flow_audit
#'
#' @param x Un objeto flow_audit.
#' @param ... Ignorado.
#' @export
print.flow_audit <- function(x, y, ...) {
  cat("== Auditoría de Flujo de Encuesta ==\n\n")
  x_print             <- x
  x_print$pct_entrada <- paste0(x$pct_entrada, "%")
  print.data.frame(x_print, row.names = FALSE)
  invisible(x)
}
