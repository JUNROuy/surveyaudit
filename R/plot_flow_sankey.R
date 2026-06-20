#' Plot Sankey / alluvial diagram of survey population flow
#'
#' Renders a Sankey diagram where each vertical block represents a survey
#' variable and the ribbon width shows the number of respondents that
#' transitioned through each step. Ribbons are color-coded by quality alert:
#' grey = OK, orange = mild loss, red = high non-response.
#'
#' @param audit A \code{flow_audit} data.frame from \code{\link{flow_audit}}.
#' @param title Character. Plot title.
#' @param alpha_ribbon Numeric (0-1). Ribbon transparency. Default 0.72.
#' @param bar_width Numeric. Width of the vertical variable bars. Default 0.08.
#' @param label_size Numeric. Font size for bar labels. Default 3.
#'
#' @return A \code{ggplot} object (invisible). Print it or save with
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
                             title        = "Survey Population Flow (Sankey)",
                             alpha_ribbon = 0.72,
                             bar_width    = 0.08,
                             label_size   = 3) {
  if (!inherits(audit, "data.frame") || !all(c("variable","n_real","alerta","pct_entrada") %in% names(audit)))
    stop("`audit` must be the output of flow_audit().")

  .check_pkg("ggplot2")
  .check_pkg("dplyr")

  vars  <- audit$variable
  nv    <- length(vars)

  # ── build ribbon segments between consecutive variables ──────────────────
  ribbons <- lapply(seq_len(nv - 1), function(i) {
    x_left  <- i
    x_right <- i + 1
    h_left  <- audit$n_real[i]
    h_right <- audit$n_real[i + 1]
    alerta  <- audit$alerta[i + 1]
    data.frame(
      xmin    = x_left  - bar_width / 2,
      xmax    = x_right + bar_width / 2,
      ymin_l  = 0,
      ymax_l  = h_left,
      ymin_r  = 0,
      ymax_r  = h_right,
      alerta  = alerta,
      stringsAsFactors = FALSE
    )
  })
  ribbons <- do.call(rbind, ribbons)

  # ── expand ribbon to polygon points ──────────────────────────────────────
  ribbon_polys <- lapply(seq_len(nrow(ribbons)), function(i) {
    r    <- ribbons[i, ]
    xs   <- seq(r$xmin, r$xmax, length.out = 60)
    t    <- (xs - r$xmin) / (r$xmax - r$xmin)
    # smooth cubic interpolation (S-curve)
    s    <- t^2 * (3 - 2 * t)
    ytop <- r$ymax_l + s * (r$ymax_r - r$ymax_l)
    ybot <- r$ymin_l + s * (r$ymin_r - r$ymin_l)
    data.frame(
      x      = c(xs, rev(xs)),
      y      = c(ytop, rev(ybot)),
      group  = i,
      alerta = r$alerta,
      stringsAsFactors = FALSE
    )
  })
  ribbon_df <- do.call(rbind, ribbon_polys)

  # ── bars (one per variable) ───────────────────────────────────────────────
  bars <- data.frame(
    x      = seq_len(nv),
    n_real = audit$n_real,
    label  = paste0(vars, "\nn=", audit$n_real, "\n", audit$pct_entrada, "%"),
    alerta = audit$alerta,
    stringsAsFactors = FALSE
  )

  # ── color mapping ─────────────────────────────────────────────────────────
  alert_colors <- c(
    "OK"                  = .PALETTE$ok,
    "Baja (Perdida leve)" = .PALETTE$baja,
    "Alta (No respuesta)" = .PALETTE$alta,
    "Sin datos"           = .PALETTE$sin_dato
  )

  p <- ggplot2::ggplot() +
    # Ribbons
    ggplot2::geom_polygon(
      data    = ribbon_df,
      mapping = ggplot2::aes(x = x, y = y, group = group, fill = alerta),
      alpha   = alpha_ribbon,
      color   = NA
    ) +
    # Variable bars
    ggplot2::geom_rect(
      data    = bars,
      mapping = ggplot2::aes(
        xmin = x - bar_width / 2,
        xmax = x + bar_width / 2,
        ymin = 0,
        ymax = n_real,
        fill = alerta
      ),
      color = "white",
      size  = 0.4
    ) +
    # Labels above bars
    ggplot2::geom_text(
      data    = bars,
      mapping = ggplot2::aes(x = x, y = n_real, label = label),
      vjust   = -0.4,
      size    = label_size,
      color   = .PALETTE$texto,
      fontface = "bold",
      lineheight = 0.9
    ) +
    ggplot2::scale_fill_manual(
      name   = "Alerta de calidad",
      values = alert_colors
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_len(nv),
      labels = vars,
      expand = ggplot2::expansion(mult = 0.12)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = "Ribbon width = respondents flowing between variables",
      caption  = "Colors: blue = OK • orange = mild loss • red = high non-response",
      x        = NULL,
      y        = "N respondents"
    ) +
    .theme_audit() +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(
                           size     = 9,
                           face     = "bold",
                           color    = .PALETTE$texto,
                           margin   = ggplot2::margin(t = 6)),
      axis.text.y      = ggplot2::element_text(
                           size   = 8,
                           color  = "#666666"),
      axis.title.y     = ggplot2::element_text(
                           size   = 9,
                           color  = "#555555",
                           angle  = 90,
                           margin = ggplot2::margin(r = 8)),
      panel.grid.major.y = ggplot2::element_line(
                           color = "#EEEEEE", linewidth = 0.4),
      legend.position  = "bottom"
    )

  invisible(p)
}
