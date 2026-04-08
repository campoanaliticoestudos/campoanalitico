#' Listar Bases de Dados Disponíveis
#' @export
ca_listar_bases <- function() {
  url <- "https://api.github.com/repos/campoanaliticoestudos/Banco-de-dados-futebol/contents/"
  res <- httr::GET(url)
  conteudo <- httr::content(res)
  
  arquivos <- sapply(conteudo, function(x) x$name)
  return(arquivos)
}

#' Carregar Partida do Banco de Dados
#' @param nome_arquivo Nome do arquivo (ex: "partida_final.rds" ou "dados_scout.csv")
#' @export
ca_importar_dados <- function(nome_arquivo) {
  url_base <- "https://raw.githubusercontent.com/campoanaliticoestudos/Banco-de-dados-futebol/main/"
  url_final <- paste0(url_base, nome_arquivo)
  
  if (grepl(".rds$", nome_arquivo)) {
    dados <- readRDS(url(url_final))
  } else {
    dados <- readr::read_csv(url_final)
  }
  
  return(dados)
}

#' Gerar Grid UEFA Oficial (18 Zonas)
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

#' Plot de Perdas de Posse (Padrão Campo Analítico)
#' @export
plot_ca_perdas_v2 <- function(time_nome, n_erros, z_id, partida_txt = "Partida") {
  df_zonas_uefa <- ca_grid_uefa()
  
  ggplot2::ggplot() +
    ggsoccer::annotate_pitch(dimension = ggsoccer::pitch_opta, fill = "#F5F2EB", colour = "#222222") +
    ggsoccer::theme_pitch() +
    ggplot2::coord_flip() +
    ggplot2::geom_rect(data = df_zonas_uefa, 
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       fill = NA, colour = "#D1D1D1", linetype = "dotted", size = 0.4) +
    ggplot2::geom_rect(data = df_zonas_uefa %>% dplyr::filter(zona == z_id),
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       fill = "#BB0000", alpha = 0.6) +
    ggplot2::geom_text(data = df_zonas_uefa %>% dplyr::filter(zona == z_id),
                       ggplot2::aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, 
                                    label = paste0(n_erros, "\nERROS")),
                       color = "white", size = 5, fontface = "bold", family = "space") +
    ggplot2::geom_text(data = df_zonas_uefa %>% dplyr::filter(zona != z_id),
                       ggplot2::aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label = zona),
                       color = "#555555", size = 5, family = "space") +
    ggplot2::theme(
      aspect.ratio = 1.6,
      plot.title = ggplot2::element_text(family = "space", face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(family = "space", size = 10, hjust = 0.5, color = "#555555"),
      panel.background = ggplot2::element_rect(fill = "#F5F2EB", color = NA),
      plot.background = ggplot2::element_rect(fill = "#F5F2EB", color = NA)
    ) +
    ggplot2::labs(title = toupper(time_nome), subtitle = partida_txt)
}

#' Relatório de Eficiência no Último Terço
#' @export
ca_relatorio_terco_final <- function(data, nomes_times, titulo = "RELATÓRIO: EFICIÊNCIA NO ÚLTIMO TERÇO") {
  df_zonas_uefa <- ca_grid_uefa()
  zonas_ataque <- c(13, 14, 15, 16, 17, 18)

  # Processamento da Tabela
  tabela_top5 <- data %>%
    dplyr::mutate(nome_time = nomes_times[as.character(teamId)]) %>%
    dplyr::filter(type == "Pass" & outcome == "Unsuccessful") %>%
    dplyr::mutate(
      z_x = cut(x, breaks = seq(0, 100, length.out = 7), labels = FALSE, include.lowest = TRUE),
      z_y = dplyr::case_when(y <= 21.1 ~ 1, y > 21.1 & y <= 78.9 ~ 2, TRUE ~ 3),
      zona_id = (z_x - 1) * 3 + z_y
    ) %>%
    dplyr::filter(zona_id %in% zonas_ataque) %>%
    dplyr::group_by(jogador, nome_time, zona_id) %>%
    dplyr::summarise(erros_zona = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(jogador) %>%
    dplyr::mutate(total_erros = sum(erros_zona)) %>%
    dplyr::slice_max(erros_zona, n = 1, with_ties = FALSE) %>%
    dplyr::arrange(dplyr::desc(total_erros)) %>%
    head(5) %>%
    dplyr::select(Jogador = jogador, Time = nome_time, Zona = zona_id, Erros = total_erros)

  # Plot do Campo
  p_campo <- ggplot2::ggplot() +
    ggsoccer::annotate_pitch(dimension = ggsoccer::pitch_opta, fill = "white", colour = "#222222") +
    ggsoccer::theme_pitch() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse() +
    ggplot2::geom_rect(data = df_zonas_uefa, ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, colour="#D1D1D1", linetype="dotted") +
    ggplot2::geom_rect(data = df_zonas_uefa %>% dplyr::filter(zona %in% zonas_ataque), ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#BB0000", alpha=0.3) +
    ggplot2::geom_text(data = df_zonas_uefa, ggplot2::aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=zona), family="space", size=4, col="#333333", fontface="bold") +
    ggplot2::theme(aspect.ratio = 1.6, text = ggplot2::element_text(family = "space")) +
    ggplot2::labs(title = "ZONAS DE ATAQUE", subtitle = "Foco: Terço Final")

  # Tabela formatada
  theme_ca <- gridExtra::ttheme_minimal(
    base_family = "space",
    colhead = list(fg_params = list(col = "white", fontface = "bold"), bg_params = list(fill = "#BB0000")),
    core = list(fg_params = list(cex = 0.8))
  )
  g_tabela <- gridExtra::tableGrob(tabela_top5, rows = NULL, theme = theme_ca)

  # Timeline
  jogadores_destaque <- tabela_top5$Jogador
  p_minuto <- data %>%
    dplyr::filter(jogador %in% jogadores_destaque & outcome == "Unsuccessful") %>%
    dplyr::mutate(intervalo = cut(minute, breaks = seq(0, 105, by = 5), labels = seq(5, 105, by = 5))) %>%
    dplyr::group_by(intervalo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    ggplot2::ggplot(ggplot2::aes(x = intervalo, y = n)) +
    ggplot2::geom_col(fill = "#BB0000", alpha = 0.8) +
    ggplot2::theme_minimal(base_family = "space") +
    ggplot2::labs(title = "DISTRIBUIÇÃO TEMPORAL", x = "Minutos", y = "Erros")

  # Layout Final
  layout_final <- "AAABBB\nAAACCC"
  
  (p_campo + g_tabela + p_minuto) + 
    patchwork::plot_layout(design = layout_final) +
    patchwork::plot_annotation(title = titulo, theme = ggplot2::theme(plot.title = ggplot2::element_text(family = "space", size = 18, face = "bold", hjust = 0.5)))
}

#' Análise de Passes Comparativa
#' @export
ca_plot_passes <- function(data, jogadores, partida_txt = "Análise de Passes") {
  df_zonas_uefa <- ca_grid_uefa()
  
  df_plot <- data %>%
    dplyr::filter(jogador %in% jogadores, type == "Pass") %>%
    dplyr::mutate(cor_passo = ifelse(outcome == "Successful", "#BB0000", "#545454"))

  ggplot2::ggplot(df_plot) +
    ggsoccer::annotate_pitch(dimension = ggsoccer::pitch_opta, fill = "#f5f5f2", colour = "#a1a1a1") +
    ggplot2::geom_rect(data = df_zonas_uefa, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, color = "#d1d1d1", linetype = "dotted", alpha = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = endX, yend = endY, color = cor_passo), arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed"), linewidth = 0.5, alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_color_identity() +
    ggplot2::facet_wrap(~jogador, ncol = 2) +
    ggsoccer::theme_pitch() +
    ggplot2::theme(aspect.ratio = 1.6, strip.text = ggplot2::element_text(family = "space", face = "bold", size = 12), plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA)) +
    ggplot2::labs(title = "ANÁLISE DE PASSES", subtitle = partida_txt, caption = "Visual: Campo Analítico")
}
