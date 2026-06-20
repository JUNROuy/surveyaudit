# Shared palette and theme for all surveyaudit visualizations

#' @keywords internal
.PALETTE <- list(
  ok       = "#4682B4",   # steel blue  – normal flow
  baja     = "#FFA500",   # amber       – mild alert
  alta     = "#CC0000",   # deep red    – critical alert
  sin_dato = "#AAAAAA",   # light grey  – no data
  flujo    = "#D3D3D3",   # pale grey   – background ribbons
  fondo    = "#FFFFFF",
  texto    = "#2B2B2B"
)

#' Map alert label to hex color
#' @keywords internal
.alert_color <- function(alerta) {
  dplyr::case_when(
    grepl("Alta",    alerta) ~ .PALETTE$alta,
    grepl("Baja",    alerta) ~ .PALETTE$baja,
    grepl("OK",      alerta) ~ .PALETTE$ok,
    TRUE                     ~ .PALETTE$sin_dato
  )
}

#' Base ggplot2 theme for surveyaudit plots
#' @keywords internal
.theme_audit <- function(base_size = 11) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(
                           family = "sans", color = .PALETTE$texto),
      plot.title       = ggplot2::element_text(
                           size   = base_size + 4,
                           face   = "bold",
                           margin = ggplot2::margin(b = 8)),
      plot.subtitle    = ggplot2::element_text(
                           size   = base_size,
                           color  = "#555555",
                           margin = ggplot2::margin(b = 12)),
      plot.caption     = ggplot2::element_text(
                           size   = base_size - 2,
                           color  = "#888888",
                           hjust  = 1,
                           margin = ggplot2::margin(t = 8)),
      plot.background  = ggplot2::element_rect(
                           fill = .PALETTE$fondo, color = NA),
      plot.margin      = ggplot2::margin(16, 16, 16, 16),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(
                           size = base_size - 1, face = "bold"),
      legend.text      = ggplot2::element_text(size = base_size - 2)
    )
}
