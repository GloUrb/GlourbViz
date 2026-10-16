


    # Onglet indicator
# calcul la freq des mots pour le contexte
calculate_wordcloud_frequency <- function(data) {
  data |>
    dplyr::mutate(
      freq = n / sum(n, na.rm = TRUE) * 100
    )
}


    # Onglet landscapes changes
# fonciton pour le graphique des pixels 
plot_landscape_pixel <- function(data) {
  data_graph <- data |>
    tidyr::pivot_longer(
      cols = c(perc_pos, perc_neg, perc_neu),
      names_to = "pixel_type",
      values_to = "proportion"
    ) |>
    dplyr::mutate(
      decoupage = factor(
        dplyr::if_else(
          is.na(COL_DECOUPAGE),
          "entire_zone",
          as.character(COL_DECOUPAGE)
        ),
        levels = c("upstream", "city", "downstream", "entire_zone"),
        labels = c(
          "Upstream",
          "City",
          "Downstream",
          "Entire selected zone"
        )
      ),
      pixel_type = factor(
        pixel_type,
        levels = c("perc_neu", "perc_neg", "perc_pos"),
        labels = c("Neutral", "Negative", "Positive")
      )
    )
    #  classement (A,B,C...)
  classements <- data_graph |>
    dplyr::distinct(decoupage, code_prop)
    ggplot2::ggplot(
    data_graph,
    ggplot2::aes(
      x = decoupage,
      y = proportion,
      fill = pixel_type
    )
  ) +
    ggplot2::geom_col(width = 0.6) +
      ggplot2::geom_text(
  ggplot2::aes(
    label = scales::percent(proportion, accuracy = 1)
  ),
  position = ggplot2::position_stack(vjust = 0.5),
  size = 3.5,
  color = "black",
  fontface = "bold"
) +
    ggplot2::geom_text(
      data = classements,
      ggplot2::aes(
        x = decoupage,
        y = -0.08,
        label = code_prop
      ),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 5
    ) +
        ggplot2::scale_fill_manual(
      values = c(
        "Positive" = "#56a88d",
        "Negative" = "#dc6b6b",
        "Neutral" = "#a3a3a3"
      )
    ) +
      ggplot2::scale_y_continuous(
        labels = function(x) {
          ifelse(x < 0, "", scales::percent(x))
        },
        breaks = seq(0, 1, 0.25),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      )+
      ggplot2::coord_cartesian(
        ylim = c(-0.12, 1.05),
        clip = "off"
      ) +
      ggplot2::labs(
      x = NULL,
      y = "Proportion of pixels",
      fill = NULL
    ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 11),
      axis.title.y = ggplot2::element_text(size = 11)
    )
}
