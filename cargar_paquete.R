# Carga manual del paquete surveyaudit para uso en sesión de R
# Ejecutar con: source("cargar_paquete.R")

pkg_dir <- "/home/juancho/Documentos/survey_analisis"

# orden importa: theme primero, survey_audit último
archivos <- c(
  "theme_survey.R",
  "flow_audit.R",
  "outlier_detection.R",
  "plot_flow_tree.R",
  "plot_flow_sankey.R",
  "survey_audit.R"
)
for (f in archivos) source(file.path(pkg_dir, "R", f))

cat("surveyaudit cargado. Función principal:\n\n")
cat("  survey_audit(data, ordered_vars)\n\n")
cat("  → clasifica variables automáticamente\n")
cat("  → audita flujo y detecta parteaguas\n")
cat("  → corre SABP en continuas\n")
cat("  → tabula frecuencias en categóricas\n")
cat("  → devuelve resumen integrado\n\n")
cat("Funciones individuales:\n")
cat("  classify_vars()    flow_audit()     detect_sabp()\n")
cat("  detect_hb()        outlier_summary() \n")
cat("  plot_flow_sankey() plot_flow_tree()\n")
