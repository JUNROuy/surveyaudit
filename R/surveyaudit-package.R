#' surveyaudit: Flow Mapping and Measurement Bias Detection in Survey Data
#'
#' Provides tools to audit raw survey data by reconstructing the response
#' sequence from a user-defined variable order. Enables visualization of
#' population flows, detection of logical and statistical anomalies (outliers),
#' and identification of measurement bias before any inference stage.
#'
#' ## Main functions
#'
#' | Function | Purpose |
#' |---|---|
#' | [flow_audit()] | Reconstruct response flow and detect parteaguas variables |
#' | [detect_hb()] | Hidiroglou-Berthelot outlier detection (panel data) |
#' | [detect_sabp()] | Skewness-adjusted boxplot outlier detection |
#' | [outlier_summary()] | Tidy summary of outliers across multiple variables |
#' | [plot_flow_tree()] | Hierarchical tree diagram of response sequence |
#' | [plot_flow_sankey()] | Sankey diagram with chromatic alert mapping |
#'
#' ## Typical workflow
#'
#' ```r
#' # 1. Define variable order from questionnaire
#' vars <- c("edad", "nivel_educativo", "anios_univ", "salario")
#'
#' # 2. Audit the response flow
#' audit <- flow_audit(encuesta, vars)
#' print(audit)
#'
#' # 3. Detect outliers
#' outlier_summary(encuesta, c("salario", "anios_univ"), method = "sabp")
#'
#' # 4. Visualize
#' plot_flow_sankey(audit)
#' plot_flow_tree(audit)
#' ```
#'
#' @keywords internal
"_PACKAGE"
