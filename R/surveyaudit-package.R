#' surveyaudit: Mapeo de flujos y detección de sesgo de medición en encuestas
#'
#' Provee herramientas para auditar datos crudos de encuestas reconstruyendo
#' la secuencia de contestación a partir del orden de variables definido por el
#' usuario. Permite visualizar flujos poblacionales, detectar anomalías lógicas
#' y estadísticas, e identificar sesgo de medición antes de cualquier etapa de
#' inferencia.
#'
#' ## Funciones principales
#'
#' | Función | Propósito |
#' |---|---|
#' | [flow_audit()] | Reconstruir el flujo y detectar la variable parteaguas |
#' | [detect_hb()] | Detección de outliers Hidiroglou-Berthelot (datos de panel) |
#' | [detect_sabp()] | Detección de outliers con boxplot ajustado por asimetría |
#' | [outlier_summary()] | Resumen tidy de outliers para múltiples variables |
#' | [plot_flow_tree()] | Diagrama de árbol jerárquico de la secuencia de respuesta |
#' | [plot_flow_sankey()] | Diagrama de Sankey con mapeo cromático de alertas |
#'
#' ## Flujo de trabajo típico
#'
#' ```r
#' # 1. Definir el orden de variables del cuestionario
#' vars <- c("edad", "nivel_educativo", "anios_univ", "salario")
#'
#' # 2. Auditar el flujo de respuesta
#' audit <- flow_audit(encuesta, vars)
#' print(audit)
#'
#' # 3. Detectar outliers
#' outlier_summary(encuesta, c("salario", "anios_univ"), method = "sabp")
#'
#' # 4. Visualizar
#' plot_flow_sankey(audit)
#' plot_flow_tree(audit)
#' ```
#'
#' @keywords internal
"_PACKAGE"
