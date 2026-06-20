# surveyaudit <img src="man/figures/logo.png" align="right" height="139" alt="" />

> R package for survey flow mapping and measurement bias detection

[![R-CMD-check](https://github.com/JUNROuy/surveyaudit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JUNROuy/surveyaudit/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

`surveyaudit` reconstructs the response sequence of a survey from a user-defined
variable order, enabling:

- **Population flow tracking** — who went where in the questionnaire
- **Parteaguas detection** — which variable caused each drop in sample size
- **Outlier detection** — Hidiroglou-Berthelot (HB) and Skewness-Adjusted Boxplot (SABP)
- **Chromatic visualization** — Sankey and tree diagrams with alert color mapping

All of this happens **before any inference stage**, auditing the raw data for
measurement bias and collection errors.

## Installation

```r
# Development version from GitHub
# install.packages("remotes")
remotes::install_github("JUNROuy/surveyaudit")
```

## Quick start

```r
library(surveyaudit)

# 1. Define variable order from the questionnaire
vars <- c("edad", "nivel_educativo", "anios_univ", "salario_actual")

# 2. Audit the response flow — detect parteaguas and quality alerts
audit <- flow_audit(encuesta, vars)
print(audit)
#>       variable   parteaguas n_esperado n_real pct_entrada              alerta
#>           edad     (Inicio)       1000   1000       100.0%                  OK
#> nivel_educativo         edad       1000    998        99.8%                  OK
#>     anios_univ nivel_educativo        379    409       107.9%   Alta (No respuesta)
#>  salario_actual    anios_univ        409    271        66.3%   Alta (No respuesta)

# 3. Detect outliers with SABP (ideal for skewed income variables)
res <- detect_sabp(encuesta$salario_actual)
encuesta[res$outlier & !is.na(res$outlier), c("edad", "salario_actual")]

# 4. Visualize the flow
plot_flow_sankey(audit)
plot_flow_tree(audit)
```

## Methodology

| Method | Use case | Reference |
|---|---|---|
| **Parteaguas algorithm** | Track population flow, detect skip errors | Meyer (2026) |
| **Hidiroglou-Berthelot (HB)** | Panel/longitudinal register errors | Hidiroglou & Berthelot (1986) |
| **SABP** | Univariate outliers in skewed distributions | Hubert & Vandervieren (2008) |

## Visualization palette

| Alert | Color | Meaning |
|---|---|---|
| OK | `#4682B4` Steel Blue | Flow within expected bounds |
| Baja | `#FFA500` Amber | Mild loss — possible skip or fatigue |
| Alta | `#CC0000` Deep Red | High non-response — measurement bias risk |

## Roadmap

- [x] Phase 1 — Parteaguas flow logic
- [x] Phase 2 — HB and SABP outlier detection
- [x] Phase 3 — Tree and Sankey visualization engines
- [x] Phase 4 — roxygen2 docs and vignette
- [ ] Phase 5 — CRAN submission

## License

MIT © Juan Meyer
