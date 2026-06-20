# surveyaudit

> Paquete de R para mapeo de flujos y detección de sesgo de medición en encuestas

[![R-CMD-check](https://github.com/JUNROuy/surveyaudit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JUNROuy/surveyaudit/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## ¿Para qué sirve?

`surveyaudit` reconstruye la secuencia de contestación de una encuesta a partir del orden de variables definido por el usuario. Permite:

- **Rastrear el flujo poblacional** — quién llegó a cada pregunta y quién no
- **Detectar la variable parteaguas** — cuál provocó cada caída en el tamaño de muestra
- **Identificar outliers** — mediante Hidiroglou-Berthelot (HB) y Boxplot ajustado por asimetría (SABP)
- **Visualizar con alertas cromáticas** — diagramas de Sankey y árbol con colores por nivel de alerta

Todo esto ocurre **antes de cualquier etapa de inferencia**, auditando el dato crudo en busca de sesgo de medición y errores de recolección.

## Instalación

```r
# Versión de desarrollo desde GitHub
# install.packages("remotes")
remotes::install_github("JUNROuy/surveyaudit")
```

## Uso rápido

```r
library(surveyaudit)

# 1. Definir el orden de variables tal como aparecen en el cuestionario
vars <- c("edad", "nivel_educativo", "anios_univ", "salario_actual")

# 2. Auditar el flujo — detecta la variable parteaguas y genera alertas
audit <- flow_audit(encuesta, vars)
print(audit)
#>          variable        parteaguas n_esperado n_real pct_entrada              alerta
#>              edad          (Inicio)       1000   1000       100.0%                  OK
#>   nivel_educativo              edad       1000    998        99.8%                  OK
#>        anios_univ   nivel_educativo        379    409       107.9%  Alta (No respuesta)
#>    salario_actual        anios_univ        409    271        66.3%  Alta (No respuesta)

# 3. Detectar outliers con SABP (ideal para ingresos y variables asimétricas)
res <- detect_sabp(encuesta$salario_actual)
encuesta[res$outlier & !is.na(res$outlier), c("edad", "salario_actual")]

# 4. Visualizar el flujo
plot_flow_sankey(audit)
plot_flow_tree(audit)
```

## Metodología

| Método | Uso | Referencia |
|---|---|---|
| **Algoritmo parteaguas** | Rastreo de flujo y detección de errores de salto | Meyer (2026) |
| **Hidiroglou-Berthelot (HB)** | Errores de registro en datos de panel | Hidiroglou & Berthelot (1986) |
| **SABP** | Outliers univariados en distribuciones asimétricas | Hubert & Vandervieren (2008) |

## Paleta de alertas

| Alerta | Color | Significado |
|---|---|---|
| OK | `#4682B4` Azul acero | Flujo dentro de lo esperado |
| Baja | `#FFA500` Ámbar | Pérdida leve — posible salto o fatiga |
| Alta | `#CC0000` Rojo intenso | Alta no-respuesta — riesgo de sesgo de medición |

## Hoja de ruta

- [x] Fase 1 — Lógica de flujo y variable parteaguas
- [x] Fase 2 — Detección de outliers HB y SABP
- [x] Fase 3 — Motores de visualización (árbol y Sankey)
- [x] Fase 4 — Documentación roxygen2 y viñeta
- [ ] Fase 5 — Publicación en CRAN

## Licencia

MIT © Juan Meyer
