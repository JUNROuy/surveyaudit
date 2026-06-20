#' Audit survey response flow and detect parteaguas variables
#'
#' Reconstructs the response sequence from a user-defined variable order,
#' calculates population flow (n) at each step, identifies the "parteaguas"
#' (watershed) variable that caused each drop in sample size, and classifies
#' each transition as a legitimate skip or a non-response quality alert.
#'
#' @param data A data.frame with raw survey data.
#' @param ordered_vars A character vector of column names in the logical order
#'   they appear in the questionnaire.
#' @param threshold_low Numeric (0-1). % entry below this triggers "Baja" alert.
#'   Default 0.90.
#' @param threshold_high Numeric (0-1). % entry below this triggers "Alta" alert.
#'   Default 0.80.
#'
#' @return A data.frame with one row per variable containing: variable name,
#'   parteaguas variable, expected n, real n, % entry, and quality alert.
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
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  missing_vars <- setdiff(ordered_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  n_total <- nrow(data)
  results <- vector("list", length(ordered_vars))

  current_parteaguas <- "(Inicio)"
  current_parteaguas_n <- n_total

  for (i in seq_along(ordered_vars)) {
    var <- ordered_vars[i]
    n_real <- sum(!is.na(data[[var]]))
    n_esperado <- current_parteaguas_n
    pct_entrada <- if (n_esperado > 0) n_real / n_esperado else NA_real_

    alerta <- .classify_alert(pct_entrada, threshold_low, threshold_high)

    results[[i]] <- data.frame(
      variable          = var,
      parteaguas        = current_parteaguas,
      n_esperado        = n_esperado,
      n_real            = n_real,
      pct_entrada       = round(pct_entrada * 100, 1),
      alerta            = alerta,
      stringsAsFactors  = FALSE
    )

    # Update parteaguas only when a new significant drop occurs
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
  if (is.na(pct))              return("Sin datos")
  if (pct >= threshold_low)    return("OK")
  if (pct >= threshold_high)   return("Baja (Perdida leve)")
  return("Alta (No respuesta)")
}

#' Print method for flow_audit objects
#'
#' @param x A flow_audit object.
#' @param ... Ignored.
#' @export
print.flow_audit <- function(x, y, ...) {
  cat("== Auditoria de Flujo de Encuesta ==\n\n")
  x_print <- x
  x_print$pct_entrada <- paste0(x$pct_entrada, "%")
  print.data.frame(x_print, row.names = FALSE)
  invisible(x)
}
