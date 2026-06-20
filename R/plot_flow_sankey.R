#' Diagrama de Sankey del flujo poblacional de la encuesta
#'
#' Genera un diagrama de Sankey donde cada bloque vertical representa una
#' variable de la encuesta y el ancho de las cintas muestra la cantidad de
#' encuestados que transitaron entre cada paso. Las cintas están coloreadas
#' según la alerta de calidad: gris/azul = OK, naranja = pérdida leve,
#' rojo = alta no-respuesta.
#'
#' @param audit Un data.frame de clase \code{flow_audit} producido por
#'   \code{\link{flow_audit}}.
#' @param title Cadena de texto. Título del gráfico.
#' @param alpha_ribbon Numérico (0-1). Transparencia de las cintas. Por
#'   defecto 0.72.
#' @param bar_width Numérico. Ancho de las barras verticales. Por defecto 0.08.
#' @param label_size Numérico. Tamaño de fuente de las etiquetas. Por defecto 3.
#'
#' @return Un objeto \code{ggplot} (invisible). Imprimirlo o guardarlo con
#'   \code{ggplot2::ggsave()}.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   p1 = c(1,2,1,2,1,1,2,1,2,1),
#'   p2 = c(1,NA,1,NA,1,1,NA,1,NA,1),
#'   p3 = c(NA,NA,1,NA,2,1,NA,2,NA,1)
#' )
#' audit <- flow_audit(df, c("p1","p2","p3"))
#' plot_flow_sankey(audit)
#' }
#'
#' @export
plot_flow_sankey <- function(audit,
                             title        = "Flujo Poblacional de la Encuesta (Sankey)",
                             alpha_ribbon = 0.72,
                             bar_width    = 0.08,
                             label_size   = 3) {
  if (!inherits(audit, "data.frame") ||
      !all(c("variable","n_real","alerta","pct_entrada") %in% names(audit)))
    stop("`audit` debe ser la salida de flow_audit().")

  .check_pkg("ggplot2")
  .check_pkg("dplyr")

  vars <- audit$variable
  nv   <- length(vars)

  ribbons <- lapply(seq_len(nv - 1), function(i) {
    data.frame(
      xmin   = i     - bar_width / 2,
      xmax   = i + 1 + bar_width / 2,
      ymax_l = audit$n_real[i],
      ymax_r = audit$n_real[i + 1],
      alerta = audit$alerta[i + 1],
      stringsAsFactors = FALSE
    )
  })
  ribbons <- do.call(rbind, ribbons)

  ribbon_polys <- lapply(seq_len(nrow(ribbons)), function(i) {
    r  <- ribbons[i, ]
    xs <- seq(r$xmin, r$xmax, length.out = 60)
    t  <- (xs - r$xmin) / (r$xmax - r$xmin)
    s  <- t^2 * (3 - 2 * t)   # curva S cúbica
    data.frame(
      x      = c(xs, rev(xs)),
      y      = c(r$ymax_l + s * (r$ymax_r - r$ymax_l),
                 rep(0, 60)),
      group  = i,
      alerta = r$alerta,
      stringsAsFactors = FALSE
    )
  })
  ribbon_df <- do.call(rbind, ribbon_polys)

  bars <- data.frame(
    x      = seq_len(nv),
    n_real = audit$n_real,
    label  = paste0(vars, "\nn=", audit$n_real, "\n", audit$pct_entrada, "%"),
    alerta = audit$alerta,
    stringsAsFactors = FALSE
  )

  alert_colors <- c(
    "OK"                  = .PALETTE$ok,
    "Baja (Perdida leve)" = .PALETTE$baja,
    "Alta (No respuesta)" = .PALETTE$alta,
    "Sin datos"           = .PALETTE$sin_dato
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data    = ribbon_df,
      mapping = ggplot2::aes(x = x, y = y, group = group, fill = alerta),
      alpha   = alpha_ribbon, color = NA
    ) +
    ggplot2::geom_rect(
      data    = bars,
      mapping = ggplot2::aes(
        xmin = x - bar_width / 2, xmax = x + bar_width / 2,
        ymin = 0, ymax = n_real, fill = alerta
      ),
      color = "white", linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data     = bars,
      mapping  = ggplot2::aes(x = x, y = n_real, label = label),
      vjust    = -0.4, size = label_size,
      color    = .PALETTE$texto, fontface = "bold", lineheight = 0.9
    ) +
    ggplot2::scale_fill_manual(name = "Alerta de calidad", values = alert_colors) +
    ggplot2::scale_x_continuous(
      breaks = seq_len(nv), labels = vars,
      expand = ggplot2::expansion(mult = 0.12)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = "Ancho de cinta = encuestados que transitan entre variables",
      caption  = "Colores: azul = OK  •  naranja = pérdida leve  •  rojo = alta no-respuesta",
      x        = NULL,
      y        = "N encuestados"
    ) +
    .theme_audit() +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(
                             size = 9, face = "bold",
                             color = .PALETTE$texto,
                             margin = ggplot2::margin(t = 6)),
      axis.text.y        = ggplot2::element_text(size = 8, color = "#666666"),
      axis.title.y       = ggplot2::element_text(
                             size = 9, color = "#555555", angle = 90,
                             margin = ggplot2::margin(r = 8)),
      panel.grid.major.y = ggplot2::element_line(color = "#EEEEEE", linewidth = 0.4),
      legend.position    = "bottom"
    )

  invisible(p)
}
