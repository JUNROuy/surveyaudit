#' Diagrama de árbol jerárquico del flujo de respuesta
#'
#' Genera un árbol dirigido de la secuencia de contestación producida por
#' \code{\link{flow_audit}}. El tamaño de cada nodo es proporcional al n real;
#' el color de nodos y aristas codifica el nivel de alerta de calidad.
#'
#' @param audit Un data.frame de clase \code{flow_audit} producido por
#'   \code{\link{flow_audit}}.
#' @param title Cadena de texto. Título del gráfico.
#' @param node_scale Vector numérico de largo 2. Rango mínimo/máximo del radio
#'   de los nodos. Por defecto \code{c(4, 18)}.
#' @param label_size Numérico. Tamaño de fuente de las etiquetas. Por defecto 3.2.
#' @param show_pct Lógico. Mostrar % de entrada en las aristas. Por defecto TRUE.
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
#' plot_flow_tree(audit)
#' }
#'
#' @export
plot_flow_tree <- function(audit,
                           title      = "Árbol de Secuencia de Contestación",
                           node_scale = c(4, 18),
                           label_size = 3.2,
                           show_pct   = TRUE) {
  if (!inherits(audit, "data.frame") ||
      !all(c("variable","parteaguas","n_real","alerta") %in% names(audit)))
    stop("`audit` debe ser la salida de flow_audit().")

  .check_pkg("igraph")
  .check_pkg("ggraph")
  .check_pkg("ggplot2")
  .check_pkg("dplyr")

  edges <- data.frame(
    from   = audit$parteaguas,
    to     = audit$variable,
    pct    = audit$pct_entrada,
    alerta = audit$alerta,
    stringsAsFactors = FALSE
  )
  edges <- edges[edges$from != edges$to, ]

  all_nodes <- unique(c("(Inicio)", audit$variable))
  nodes <- data.frame(
    name   = all_nodes,
    n_real = c(max(audit$n_esperado),
               audit$n_real[match(all_nodes[-1], audit$variable)]),
    alerta = c("OK",
               audit$alerta[match(all_nodes[-1], audit$variable)]),
    stringsAsFactors = FALSE
  )

  g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  p <- ggraph::ggraph(g, layout = "tree") +
    ggraph::geom_edge_diagonal(
      ggplot2::aes(color = alerta, width = after_stat(index) * 0.6 + 0.2),
      alpha = 0.65, show.legend = FALSE
    ) +
    ggraph::scale_edge_color_manual(
      values = c(
        "OK"                  = .PALETTE$ok,
        "Baja (Perdida leve)" = .PALETTE$baja,
        "Alta (No respuesta)" = .PALETTE$alta,
        "Sin datos"           = .PALETTE$sin_dato
      )
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(size = n_real, color = alerta), alpha = 0.92
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = paste0(name, "\nn=", n_real)),
      size = label_size, repel = TRUE, color = .PALETTE$texto,
      fontface = "bold", point.padding = ggplot2::unit(0.25, "lines")
    ) +
    ggplot2::scale_color_manual(
      name   = "Alerta de calidad",
      values = c(
        "OK"                  = .PALETTE$ok,
        "Baja (Perdida leve)" = .PALETTE$baja,
        "Alta (No respuesta)" = .PALETTE$alta,
        "Sin datos"           = .PALETTE$sin_dato
      )
    ) +
    ggplot2::scale_size_continuous(range = node_scale, guide = "none") +
    ggplot2::labs(
      title    = title,
      subtitle = "Tamaño del nodo proporcional al n de encuestados que llegaron a esa variable",
      caption  = "Colores: azul = OK  •  naranja = pérdida leve  •  rojo = alta no-respuesta"
    ) +
    .theme_audit() +
    ggplot2::theme(legend.position = "bottom")

  invisible(p)
}

