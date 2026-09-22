################################## COTE SERVER ##################################################

server <- function(input, output, session) {
  
  ############################ onglet N°2 ##################################
  
  
  # Bloc d'information
  output$notes <- renderText({
    if (input$Ind == "discours") {
      paste(
        "Selected indicator:",
        input$Ind,
        "-",
        input$word_discours
      )
    } else {
      paste(
        "Selected indicator :",
        input$Ind,
        "-",
        input$Var
      )
    }
  })
  
  
  # Sélecteur dynamique de variable / mot
  output$variable_selector <- renderUI({
    if (input$Ind == "discours") {
      selectInput(
        "word_discours","Word",
        choices = sort(
          unique(lexique_discourses$en_word)
        )
      )
    } else {
      selectInput(
        "Var","Variable",
        choices = c(
          "oiseau","sediment","others"
        )
      )
    }
  })
  
  
  # Titre dynamique du nuage
  output$cloud_title <- renderUI({
    req(input$word_discours)
    if (is.null(selected_city_id())) {
      tags$strong(
        paste0(
          'Global context cloud for "',
          input$word_discours,
          '"'
        )
      )
    } else {
      ville <- selected_city()
      tags$strong(
        paste0(
          'Context cloud for "',
          input$word_discours,
          '" in ',
          ville$urban_aggl[1]
        )
      )
    }
  })
  
  
  # Contenu dynamique du nuage
  output$cloud_content <- renderUI({
    req(input$word_discours)
    data_cloud <- context_cloud_data()
    if (nrow(data_cloud) == 0) {
      if (is.null(selected_city_id())) {
        tags$div(
          style = "padding: 10px; font-size: 13px;",
          tags$strong(
            "No significant context words identified"
          ),
          tags$br(),
          tags$br(),
          paste0(
            'No context words meet the thresholds for “',
            input$word_discours,
            '” (specificity ≥ 2; n ≥ 5).'
          )
        )
      } else {
        ville <- selected_city()
        tags$div(
          style = "padding: 10px; font-size: 13px;",
          tags$strong(
            "No significant context words identified"
          ),
          
          tags$br(),
          tags$br(),
          paste0(
            '“',
            input$word_discours,
            '” occurs in the corpus for ',
            ville$urban_aggl[1],
            ', but no context words meet the thresholds ',
            '(specificity ≥ 2; n ≥ 5).'
          )
        )
      }
    } else {
      plotOutput(
        "mini_plot",
        height = "150px"
      )
    }
  })
  
  
                    ## REACTIVES
  
  
  # Données cartographiques selon le mot sélectionné
  discours_map_data <- reactive({
    req(input$word_discours)
    lexique_city_global |>
      dplyr::filter(
        word == input$word_discours
      )
  })
  
  
  # Palette de couleurs pour la carte des discours (orange mais bon...)
  discours_palette <- reactive({
    leaflet::colorBin(
      palette = "Oranges",
      domain = c(0, 1),
      bins = c(
        0,
        0.01,
        0.025,
        0.05,
        0.10,
        0.20,
        1
      ),
      na.color = "transparent",
      right = FALSE
    )
  })
  
  
  # Aucune ville sélectionnée au départ (avec celle du dessous)
  selected_city_id <- reactiveVal(NULL)
  
  
  # Informations sur la ville sélectionnée
  selected_city <- reactive({
    req(selected_city_id())
    discours_map_data() |>
      dplyr::filter(
        city_id == selected_city_id()
      )
  })
  
  
  # Données du nuage :
  # global si aucune ville sélectionnée
  # local si une ville est sélectionnée (nuage de la ville quoi)
  context_cloud_data <- reactive({
    req(input$word_discours)
    if (is.null(selected_city_id())) {
      context_specificities_selected |>
        dplyr::filter(
          lexicon_word == input$word_discours
        )
    } else {
      context_specificities_selected_city |>
        dplyr::filter(
          lexicon_word == input$word_discours,
          city_id == selected_city_id()
        )
    }
  })
  
  
  ## GRANDE CARTE 
  
  
  output$map <- leaflet::renderLeaflet({
    if (input$Ind == "discours") {
      data_map <- discours_map_data()
      pal <- discours_palette()
      leaflet::leaflet(data_map) |>
        leaflet::addTiles() |>
        leaflet::setView(
          lng = 0.3458,
          lat = 50.6253,
          zoom = 3
        ) |>
        leaflet::addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          radius = 6,
          fillColor = ~pal(freq_pages),
          fillOpacity = 0.8,
          color = "black",
          weight = 0.5,
          layerId = ~city_id,
          popup = ~paste0(
            "<b>", urban_aggl, "</b><br>",
            "Word : ", word, "<br>",
            "Pages concerned : ", nb_pages_concerned, "<br>",
            "Analysable pages : ", nb_pages_total_analysable, "<br>",
            "Frequency : ", round(freq_pages * 100, 1), "%"
          )
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors = pal(c(
            0.005,
            0.015,
            0.035,
            0.075,
            0.15,
            0.30
          )),
          labels = c(
            "< 1%",
            "1–2.5%",
            "2.5–5%",
            "5–10%",
            "10–20%",
            "≥ 20%"
          ),
          title = "Pages mentioning the word (frequency)",
          opacity = 1
        )
    } else {
      # Biodiversity / Economy pour le moment
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::setView(
          lng = 0.3458,
          lat = 50.6253,
          zoom = 3
        )
    }
  })
  
  
  ## MINI CARTE
  
  
  # Mini-carte initiale
  output$mini_map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(
        lng = 0.3458,
        lat = 50.6253,
        zoom = 3
      )
  })
  
  
                    # EVENTS 
  
  
  # Quand on change de mot :
  # on retire la ville précédemment sélectionnée
  observeEvent(input$word_discours, {
    selected_city_id(NULL)
  })
  
  
  # Interaction grande carte -> mini-carte
  observeEvent(input$map_marker_click, {
    selected_city_id(
      input$map_marker_click$id
    )
    ville <- selected_city()
    # Zones correspondant à la ville
    zone_selected <- zone |>
      dplyr::filter(
        CityCode == ville$citycode_clean[1]
      )
    # Transformation en longitude / latitude
    zone_selected <- sf::st_transform(
      zone_selected,
      4326
    )
    
    # puis GRANDE CARTE 
    leaflet::leafletProxy("map") |>
      leaflet::flyTo(
        lng = ville$lon[1],
        lat = ville$lat[1],
        zoom = 8
      ) |>
      leaflet::clearGroup(
        "selected_city"
      ) |>
      leaflet::addCircleMarkers(
        lng = ville$lon[1],
        lat = ville$lat[1],
        radius = 6,
        color = "blue",
        weight = 1,
        fillOpacity = 0,
        group = "selected_city"
      )
    # puis MINI-CARTE
    leaflet::leafletProxy("mini_map") |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::addPolygons(
        data = zone_selected,
        fillColor = ~pal(reach),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~reach
      ) |>
      leaflet::addCircleMarkers(
        lng = ville$lon[1],
        lat = ville$lat[1],
        radius = 4,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.7,
        popup = ville$urban_aggl[1]
      )
  
    # Cadrage auto  mini-carte
    if (nrow(zone_selected) > 0) {
      bbox <- sf::st_bbox(
        zone_selected
      )
      leaflet::leafletProxy("mini_map") |>
        leaflet::fitBounds(
          lng1 = unname(bbox["xmin"]),
          lat1 = unname(bbox["ymin"]),
          lng2 = unname(bbox["xmax"]),
          lat2 = unname(bbox["ymax"])
        )
    }
  })
  
  
  # sortie du NUAGE DE MOTS 
  output$mini_plot <- renderPlot({
    data_cloud <- context_cloud_data()
    req(nrow(data_cloud) > 0)
    data_cloud <- calculate_wordcloud_frequency(
      data_cloud
    )
    data_cloud |>
      dplyr::slice_max(
        order_by = spec,
        n = 20,
        with_ties = FALSE
      ) |>
      ggplot2::ggplot(
        ggplot2::aes(
          label = context_word,
          size = freq
        )
      ) +
      ggwordcloud::geom_text_wordcloud() +
      ggplot2::scale_size_area(
        max_size = 10
      ) +
      ggplot2::theme_void()
  })
  
  
  
  
  ############################ onglet N°3 ##################################
  
  output$Explication <- renderText({
    paste("Explanation of the", input$ipd)
  })
  
  
  pal <- leaflet::colorFactor(
    palette = c("pink", "lightblue", "grey"),
    domain = zone$reach
  )
  output$mini_map3 <- leaflet::renderLeaflet({
    leaflet::leaflet(zone) %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(
        fillColor = ~pal(reach),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~reach
      ) %>%
      setView(lng = 2.35, lat = 48.85, zoom = 10)
  })
  
  output$mini_plot3 <- renderPlot({
    df <- data.frame(x = 1:8, y = rnorm(8))
    ggplot(df, aes(x = factor(x), y = y)) + geom_col(fill = "steelblue") + theme_minimal() + labs(x = NULL, y = NULL)
  })
  output$map3 <- leaflet::renderLeaflet({
    leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::setView(lng = 0.3458, lat = 50.6253, zoom = 3)
  })
  
  
  output$graphe_zones <- renderPlot({
    
    data <- data.frame(
      zone = c("Amont", "Ville", "Aval"),
      valeur = c(15, -5, -12)
    )
    
    # variable propre pour la légende
    data$classe <- ifelse(data$valeur > 10, "Augmentation des surfaces en eau",
                          ifelse(data$valeur < -10, "Diminution des surfaces en eau", "Pas de changement"))
    
    ggplot(data, aes(x = zone, y = 1, fill = classe)) +
      geom_col(width = 1, color = "grey40") +
      
      scale_fill_manual(
        values = c(
          "Augmentation des surfaces en eau" = "green",
          "Diminution des surfaces en eau" = "red",
          "Pas de changement" = "gray"
        ),
        name = NULL
      ) +
      
      labs(x = NULL, y = NULL) +
      
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()
      )
  })
}