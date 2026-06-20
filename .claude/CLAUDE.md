Proyecto: Paquete de R para Auditoría de Flujos y Detección de Sesgos de Medición

0. operativa de vinculacion: preguntale siempre primero al notebooklm que debers vicular a via cli de github, trabajando en el cuanderno: Flow Mapping and Survey Anomalies in R

1. Objetivo General
Desarrollar un paquete de R diseñado para realizar una auditoría técnica del dato bruto de encuestas. El sistema debe reconstruir la secuencia de contestación a partir de un orden de variables definido por el usuario, permitiendo visualizar flujos poblacionales, detectar anomalías lógicas y estadísticas (outliers), e identificar sesgos de medición (measurement bias) antes de cualquier etapa de inferencia.
2. Pilares Metodológicos
A. Lógica de la Variable "Parteaguas" (Origen/Filtro)
El paquete no solo analizará la variable anterior, sino que identificará la variable parteaguas: aquella que definió el universo actual de encuestados al provocar la última caída o segmentación significativa en el tamaño de la muestra (n).

    Detección de saltos lógicos: Identifica si la pérdida de flujo es por diseño (salto legítimo) o por error (no respuesta/fatiga).
    Persistencia del filtro: Realiza el seguimiento del n a través del árbol jerárquico para marcar alertas si el volumen no coincide con los criterios de origen.

B. Detección de Anomalías y Rarezas

    Método Hidiroglou-Berthelot (HB): Implementación de este estándar de estadísticas oficiales para detectar errores de registro en variables continuas mediante el análisis de razones y puntuaciones de magnitud.
    Boxplots ajustados por asimetría (SABP): Para identificar outliers en distribuciones con colas largas, evitando falsas alarmas en datos asimétricos naturales.
    Validación Lógica: Marcado de "transiciones imposibles" e inconsistencias temporales (ej. tiempos de observación negativos).
    Análisis de Paradatos: Detección de fatiga del encuestado mediante el monitoreo del goteo constante de n o frecuencias de estados inusuales.

3. Productos y Visualizaciones
Visualizaciones Gráficas

    Diagrama de Árbol Jerárquico: Representación ramificada de la secuencia completa de contestación. Incluirá infografías de volumen de respuesta en nodos y ramas para ver la densidad de participación en cada camino.
    Diagrama de Sankey (Aluvial): Visualización de transiciones de estados con mapeo cromático de alertas, resaltando las ramas donde se concentran errores de consistencia u outliers detectados.

Informes Tabulares

    Diagnóstico Estadístico (Tipo skimr): Tabla resumen que incluya estadísticos base, identificación de la variable parteaguas, n esperado vs. real, y porcentaje de entrada.
    Registro de Alertas: Listado detallado de rarezas detectadas categorizadas por criticidad.

4. Estándares de Desarrollo
Estructura del Paquete

    Metadata: Configuración estricta del archivo DESCRIPTION (título en Title Case, roles cre, aut, cph y declaración de dependencias como ggplot2, ggraph, univOutl y skimr).
    Documentación: Uso mandatorio de roxygen2 para documentar funciones (etiquetas @param, @return, @examples y @export) y datos de ejemplo.
    Calidad: Pruebas automáticas con el framework testthat y chequeos rigurosos mediante devtools::check() para asegurar la compatibilidad con los estándares de CRAN.
    Guía de Usuario: Creación de viñetas (vignettes) en R Markdown que funcionen como tutoriales paso a paso para el análisis de flujos.

5. Hoja de Ruta (Roadmap)

    Fase 1: Configuración de arquitectura, lógica de variable parteaguas y funciones base de frecuencias.
    Fase 2: Implementación de algoritmos de detección de outliers (HB y SABP) y validaciones lógicas.
    Fase 3: Desarrollo de motores gráficos para el Diagrama de Árbol y Sankey con alertas integradas.
    Fase 4: Documentación técnica, creación de viñetas y publicación en GitHub/CRAN.

Instrucción para Claude: Utiliza esta estructura como contexto base para generar código, resolver dudas arquitectónicas y asegurar que todas las funciones mantengan la coherencia con la auditoría de sesgos de medición y la lógica de flujo definida.
