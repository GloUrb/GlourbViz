################################## COTE SERVER ##################################################

server <- function(input, output, session) {
  
  ############################ onglet indicator ##################################
  
  
  # Bloc d'information
  output$notes <- renderUI({
    
    if (input$Ind == "Discourses of the Web") {
      
      tags$div(
        class = "indicator-panel",
        
        # Les deux colonnes
        tags$div(
          class = "indicator-columns",
          
          # Colonne 1
          tags$div(
            class = "indicator-column",
            
            tags$strong("About the indicator"),
            
            tags$p(
              "Web discourses provide insights into how urban rivers are 
            represented and discussed in digital spaces. This indicator 
            is based on web pages related to urban rivers worldwide, 
            collected through web scraping. These data can be used to 
            explore the digital visibility and representations of urban 
            rivers across cities and regions of the world."
            )
          ),
          
          # Colonne 2
          tags$div(
            class = "indicator-column",
            
            tags$strong("Interpretation & limitations"),
            
            tags$p(
              "Digital representations of urban rivers are unevenly produced."
            ),
            
            tags$p(
              "Web discourses about water can be understood as a digital 
            extension of the hydrosocial cycle."
            )
          )
        ),
        
        # Sélection actuelle
        tags$div(
          class = "current-selection",
          
          'Selected indicator "',
          tags$strong(input$Ind),
          '" to study the word ↬ "',
          tags$strong(input$word_discours),
          '"'
        )
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
    if (input$Ind == "Discourses of the Web") {
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
            '” (specificity ≥ 2).'
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
            '(specificity ≥ 2).'
          )
        )
      }
    } else {
      plotOutput(
        "mini_plot",
        height = "25vh" # Emilie oublies pas c'est la taille du nuage
      )
    }
  })
  
  # La petite boite sous le nuage de mots qui explique 
  output$cloud_explanation <- renderUI({
    req(input$word_discours)
    data_cloud <- context_cloud_data()
    # S'il n'y a pas de nuage de mots, pas de bloc explicatif
    if (nrow(data_cloud) == 0) {
      return(NULL)
    }
    tags$div(
      class = "mini-box cloud-explanation",
      tags$strong("How to read this word cloud?"),
      tags$p(
        "This word cloud displays context words that are specifically ",
        "associated with the selected term (specificity ≥ 2)."
      ),
      tags$p(
        "Word size represents its relative frequency among the selected ",
        "context words, while color indicates its number of occurrences (n)."
      )
    )
  })
  
# le bouton dans le nuage qui permet de revenir au global 
  output$global_view_button <- renderUI({
    if (is.null(selected_city_id())) {
      return(NULL)
    }
    actionLink(
      "global_view",
      "↩ Global view"
    )
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
    if (input$Ind == "Discourses of the Web") {
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
    # puis MINI-CARTE (d'abord on recup la ville puis on regarde si elle à une geom dans "zone")
    mini_map <- leaflet::leafletProxy("mini_map") |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::addCircleMarkers(
        lng = ville$lon[1],
        lat = ville$lat[1],
        radius = 4,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.7,
        popup = ville$urban_aggl[1]
      )
    if (nrow(zone_selected) > 0) {
      mini_map |>
        leaflet::addPolygons(
          data = zone_selected,
          fillColor = ~pal(reach),
          color = "black",
          weight = 1,
          fillOpacity = 0.7,
          label = ~reach
        )
      bbox <- sf::st_bbox(zone_selected)
      mini_map |>
        leaflet::fitBounds(
          lng1 = unname(bbox["xmin"]),
          lat1 = unname(bbox["ymin"]),
          lng2 = unname(bbox["xmax"]),
          lat2 = unname(bbox["ymax"])
        )
    } else {
      mini_map |>
        leaflet::setView(
          lng = ville$lon[1],
          lat = ville$lat[1],
          zoom = 8
        )
    }
    

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
  
  #Go gO GOOO revient sur la carte et le nuage global (surtout le nuage enfaite) clic dans le fond de carte 
  observeEvent(input$map_click, {
    selected_city_id(NULL)
    # Supp le contour bleu de la ville séle
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("selected_city")
    # Remet la mini-carte en global
    leaflet::leafletProxy("mini_map") |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::setView(
        lng = 0,
        lat = 20,
        zoom = 1
      )
  })
  
  # la m^me que celle du dessus mais enfaite sur le bouton global view comme aç les gens pvt revenir au nuage global en cliquant
  observeEvent(input$global_view, {
    selected_city_id(NULL)
    # Supp le contour bleu de la ville sélec
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("selected_city")
    # Remet la mini-carte
    leaflet::leafletProxy("mini_map") |>
      leaflet::clearShapes() |>
      leaflet::clearMarkers() |>
      leaflet::setView(
        lng = 0,
        lat = 30,
        zoom = 1
      )
  })
  
  
  # Sortie du NUAGE DE MOTS
  output$mini_plot <- renderPlot({
    data_cloud <- context_cloud_data()
    req(nrow(data_cloud) > 0)
  #mince la legende est trop bizarre on va la gerer comme ça --> 
    # Largeur actuelle du graphique dans le navigateur
    plot_width <- session$clientData$output_mini_plot_width
    # Taille dynamique de la légende
    legend_title_size <- max(
      7,
      min(11, plot_width * 0.03)
    )
    legend_text_size <- max(
      6,
      min(10, plot_width * 0.025)
    )
    # Calcul de la fréquence 
    data_cloud <- calculate_wordcloud_frequency(data_cloud)
    # Classes de n différentes selon l'échelle
    if (is.null(selected_city_id())) {
      
      # GLOBAL (monde)
      data_cloud <- data_cloud |>
        dplyr::mutate(
          n_class = dplyr::case_when(
            n < 25 ~ "n < 25",
            n < 100 ~ "25 ≤ n < 100",
            TRUE ~ "n ≥ 100"
          )
        )
      colors_n <- c(
        "n < 25" = "grey70",
        "25 ≤ n < 100" = "black",
        "n ≥ 100" = "red"
      )
      
    } else {
      
      # VILLE
      data_cloud <- data_cloud |>
        dplyr::mutate(
          n_class = dplyr::case_when(
            n <= 2 ~ "n ≤ 2",
            n <= 5 ~ "3 ≤ n ≤ 5",
            TRUE ~ "n ≥ 6"
          )
        )
      colors_n <- c(
        "n ≤ 2" = "grey70",
        "3 ≤ n ≤ 5" = "black",
        "n ≥ 6" = "red"
      )
    }
    
    # Sélection des 20 mots ayant les fréquences les plus élevées
    data_cloud <- data_cloud |>
      dplyr::slice_max(
        order_by = freq,
        n = 20,
        with_ties = FALSE
      )
    # le Nuage de mots : comment il est fait 
    ggplot2::ggplot(
      data_cloud,
      ggplot2::aes(
        label = context_word,
        size = freq,
        color = n_class
      )
    ) +
      ggwordcloud::geom_text_wordcloud(
        show.legend = TRUE,
        key_glyph = "point"
      ) +
      ggplot2::scale_size_continuous(
        range = c(5,10),
        guide = "none"
      ) +
      ggplot2::scale_color_manual(
        name = "Number of occurrences of a word (n)",
        values = colors_n,
        breaks = names(colors_n)
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = ggplot2::element_text(
          size = legend_title_size
        ),
        legend.text = ggplot2::element_text(
          size = legend_text_size
        )
      )
  })
  
  

  ############################ onglet landscapes changes ##################################
  
  # La vue qui est sélec
  landscape_view_selected <- reactiveVal("global")
  observeEvent(input$landscape_global, {
    landscape_view_selected("global")
  })
  observeEvent(input$landscape_city, {
    landscape_view_selected("city")
  })
  
  # Afficher la bonne vue
  observe({
    if (landscape_view_selected() == "global") {
      shinyjs::show("landscapeGlobal")
      shinyjs::hide("landscapeCity")
    } else {
      shinyjs::hide("landscapeGlobal")
      shinyjs::show("landscapeCity")
    }
  })
  
  
  # les 2 boutons 
  output$landscape_view_buttons <- renderUI({
    tags$div(
      id = "landscapeViewButtons",
      #bt 1
      actionButton(
        "landscape_global",
        "Global view",
        class = if (landscape_view_selected() == "global") {
          "landscape-view-btn active"
        } else {
          "landscape-view-btn"
        }
      ),
      #bt 2
      actionButton(
        "landscape_city",
        "City view",
        class = if (landscape_view_selected() == "city") {
          "landscape-view-btn active"
        } else {
          "landscape-view-btn"
        }
      )
    )
  })
  
  
  #creat variables
  landscape_global_selected <- reactive({
    variable_name <- paste0(
      "n_",
      input$landscape_global_variable,
      "_",
      input$landscape_global_zone
    )
    
    change_name <- paste0(
      "perc_change_",
      input$landscape_global_zone
    )
    
    indicator_label <- switch(
      input$landscape_global_variable,
      "impact" = "Number of impacts",
      "pressure" = "Number of pressures",
      "driver" = "Number of drivers"
    )
    
    zone_label <- switch(
      input$landscape_global_zone,
      "tot" = "Whole area",
      "cor" = "Corridor",
      "pl" = "Plain"
    )
    
    landscape_global |>
      dplyr::mutate(
        value = .data[[variable_name]],
        change = .data[[change_name]],
        indicator_label = indicator_label,
        zone_label = zone_label
      )
  })
  
  
  
  landscape_radius <- function(x) {
    sqrt(x) * 4
  }

  # la map centrale dans "GLOBAL VIEW"
  output$landscape_global_map <- leaflet::renderLeaflet({
    data_map <- landscape_global_selected() |>
      dplyr::filter(
        !is.na(value),
        value > 0
      )
    # cercle prp
    max_value <- max(data_map$value, na.rm = TRUE)
    
    legend_values <- sort(
      unique(
        pmax(1, round(c(
          max_value / 4,
          max_value / 2,
          max_value
        )))
      ),
      decreasing = TRUE
    )
    legend_radii <- sqrt(legend_values) * 4

    # Palette 
    pal_change <- leaflet::colorNumeric(
      palette = c(
        "#eff3ff",
        "#bdd7e7",
        "#6baed6",
        "#2171b5",
        "#084594"
      ),
      domain = c(0, 100),
      na.color = "#BDBDBD"
    )
    
    # Légende des cercles pro
    max_radius <- max(legend_radii)
    center_x <- max_radius + 12
    bottom_y <- 2 * max_radius + 12
    svg_width <- 2 * max_radius + 110
    label_y <- seq(
      from = 15,
      by = 25,
      length.out = length(legend_values)
    )
    svg_height <- max(
      bottom_y + 12,
      max(label_y) + 15
    )
    circles <- paste0(
      '<circle cx="', center_x,
      '" cy="', bottom_y - legend_radii,
      '" r="', legend_radii,
      '" fill="none" stroke="#333" stroke-width="1"/>',
      collapse = ""
    )
    # Traits de lecture en cascade
    labels <- paste0(
      '<path d="M ', center_x, ' ',
      bottom_y - 2 * legend_radii,
      ' H ', center_x + max_radius + 15,
      ' V ', label_y,
      ' H ', center_x + max_radius + 35,
      '" fill="none" stroke="#555" stroke-width="0.8"/>',
      '<text x="', center_x + max_radius + 42,
      '" y="', label_y + 4,
      '" font-size="12" fill="#333">',
      legend_values,
      '</text>',
      collapse = ""
    )
    legend_html <- paste0(
      '<div style="
    background: white;
    padding: 10px;
    border-radius: 5px;
    box-shadow: 0 1px 5px rgba(0,0,0,0.3);
  ">',
      '<strong>',
      data_map$indicator_label[1],
      '</strong>',
      '<div>',
      '<svg width="', svg_width,
      '" height="', svg_height,
      '" viewBox="0 0 ', svg_width, ' ', svg_height, '">',
      circles,
      labels,
      '</svg>',
      '</div></div>'
    )
    
    leaflet::leaflet(data = data_map) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(value) * 4,
        fillColor = ~pal_change(change),
        color = "#333333",
        weight = 1,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<strong>", Urban_aggl, "</strong><br>",
          "Zone: ", zone_label, "<br>",
          indicator_label, ": ", value, "<br>",
          "Proportion of pixels that changed: ",
          ifelse(
            is.na(change),
            "No data",
            paste0(round(change, 1), "%")
          )
        )
      )|>
      leaflet::addLegend(
        position = "bottomright",
        pal = pal_change,
        values = c(0, 100),
        labFormat = leaflet::labelFormat(
          suffix = " %",
          digits = 0
        ),
        title = "Proportion of pixels that changed",
        opacity = 0.8
      ) |>
      leaflet::addControl(
        html = htmltools::HTML(legend_html),
        position = "bottomleft"
      )
  })
  
  
  
  
  
  
  
  #  ---------------------------------------------
  
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