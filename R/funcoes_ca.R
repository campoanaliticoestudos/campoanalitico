#' Grid UEFA Oficial (18 Zonas)
#' @export
ca_grid_uefa <- function() {
  data.frame(
    x_idx = rep(1:6, each = 3),
    y_idx = rep(1:3, times = 6)
  ) %>%
    dplyr::mutate(
      xmin = (x_idx - 1) * (100/6),
      xmax = x_idx * (100/6),
      ymin = dplyr::case_when(y_idx == 1 ~ 0, y_idx == 2 ~ 21.1, y_idx == 3 ~ 78.9),
      ymax = dplyr::case_when(y_idx == 1 ~ 21.1, y_idx == 2 ~ 78.9, y_idx == 3 ~ 100),
      zona = (x_idx - 1) * 3 + y_idx
    )
}

#' Plot de Perdas de Posse Campo Analítico
#' @export
plot_ca_perdas <- function(time_nome, n_erros, z_id, partida_txt) {
  df_zonas_uefa <- ca_grid_uefa()
  
  ggplot2::ggplot() +
    ggsoccer::annotate_pitch(dimension = ggsoccer::pitch_opta, fill = "#F5F2EB", colour = "#222222") +
    ggsoccer::theme_pitch() +
    ggplot2::coord_flip() +
    ggplot2::geom_rect(data = df_zonas_uefa, 
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       fill = NA, colour = "#D1D1D1", linetype = "dotted") +
    ggplot2::geom_rect(data = df_zonas_uefa %>% dplyr::filter(zona == z_id),
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       fill = "#BB0000", alpha = 0.6) +
    ggplot2::geom_text(data = df_zonas_uefa %>% dplyr::filter(zona == z_id),
                       ggplot2::aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, 
                                    label = paste0(n_erros, "\nERROS")),
                       color = "white", size = 5, fontface = "bold", family = "space") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#F5F2EB", color = NA),
      panel.background = ggplot2::element_rect(fill = "#F5F2EB", color = NA),
      text = ggplot2::element_text(family = "space")
    ) +
    ggplot2::labs(title = toupper(time_nome), subtitle = partida_txt)
}
