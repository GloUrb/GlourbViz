##### FONCTIONS


    # Onglet indicator
# calcul la freq des mots pour le contexte
calculate_wordcloud_frequency <- function(data) {
  data |>
    dplyr::mutate(
      freq = n / sum(n, na.rm = TRUE) * 100
    )
}


    # Onglet landscapes changes
# fonction pour le graphique des pixels 
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
          is.na(reach),
          "entire_zone",
          as.character(reach)
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
        "Neutral" = "#616161"
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


#fonction permettant de decomposer les valeurs de change spatiality 
parse_landscape_spatiality <- function(x) {
  if (length(x) == 0 || is.na(x[1]) ||
      !nzchar(trimws(x[1]))) {
    return(list(
      styles = character(0),
      secteur = NA_character_,
      statut = "no_data"
    ))
  }
  x <- tolower(trimws(as.character(x[1])))
  if (x %in% c("no_data", "no_change")) {
    return(list(
      styles = character(0),
      secteur = NA_character_,
      statut = x
    ))
  }
  parties <- trimws(strsplit(x, "_", fixed = TRUE)[[1]])
  styles_possibles <- c("local", "reach", "occasional")
  secteurs_possibles <- c("upstream", "city", "downstream", "all")
  # Vérifier que tous les éléments sont reconnus
  if (any(!parties %in% c(styles_possibles, secteurs_possibles))) {
    warning(paste("Spatialité non reconnue :", x))
    return(list(
      styles = character(0),
      secteur = NA_character_,
      statut = "invalid"
    ))
  }
  styles <- unique(parties[parties %in% styles_possibles])
  secteurs <- unique(parties[parties %in% secteurs_possibles])
  # Sans secteur explicite, on considère "all"
  if (length(secteurs) == 0) {
    secteurs <- "all"
  }
  if (length(styles) == 0 || length(secteurs) != 1) {
    warning(paste("Combinaison spatiale ambiguë :", x))
    return(list(
      styles = character(0),
      secteur = NA_character_,
      statut = "invalid"
    ))
  }
  list(
    styles = styles,
    secteur = secteurs,
    statut = "change"
  )
}



#Legende pour la carte dans la view locl et petit (i)
landscape_map_legend <- function(show_spatiality = FALSE) {
  htmltools::tags$div(
    class = "landscape-map-legend",
    # htmltools::tags$div(
    #   class = "landscape-legend-title",
    #   "Map legend"
    # ),
          # petit (i)
    htmltools::tags$span(
      class = "landscape-legend-info",
      title = "About the map legend",
      onclick = "toggleLandscapeInfo(event, this);",
      "\u24D8"
    ),
    htmltools::tags$div(
      class = "landscape-legend-popup",
      htmltools::tags$strong("Understanding spatiality"),
      htmltools::tags$p("Local : The change is observed only in the study area"),
      htmltools::tags$p("Extended : The change is observed beyond of the study area. Sometimes, all the lenght of the corridor "),
      htmltools::tags$p("Scattered : The change is observed that scaterred maners in the study area"),
      htmltools::tags$p("Reach boundaries : Represents the boundaries of the downstream, urban, and upstream sections."),
      htmltools::tags$p("Sometimes, we have multiple spatialities for a single area")
    ),
    
    #Sometimes, we have multiple spatialities for a single area
          #fin petit (i)
    htmltools::tags$div(
      class = "landscape-legend-item",
      htmltools::tags$span(class = "legend-line-yellow"),
      "Study area"
    ),
    htmltools::tags$div(
      class = "landscape-legend-item",
      htmltools::tags$span(class = "legend-line-grey"),
      "Reach boundaries"
    ),
    if (show_spatiality) {
      htmltools::tagList(
        htmltools::tags$div(
          class = "landscape-legend-item",
          htmltools::tags$span(class = "legend-line-pink"),
          "Local change"
        ),
        htmltools::tags$div(
          class = "landscape-legend-item",
          htmltools::tags$span(class = "legend-line-dashed"),
          "Extended change"
        ),
        htmltools::tags$div(
          class = "landscape-legend-item",
          htmltools::tags$span(class = "legend-hatched"),
          "Scattered change"
        )
      )
    }
  )
}
