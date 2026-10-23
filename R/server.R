################################## COTE SERVER ##################################################

server <- function(input, output, session) {
  
  ############################ onglet indicator ##################################
  
      ## Render UI
  
# Les blocs d'infos (en haut de l'appli) (phrase + 2 blocs textes)
  output$notes <- renderUI({
    if (input$Ind == "Discourses of the Web") {
      tags$div(
        class = "indicator-panel",
        # Les deux colonnes
        tags$div(
          class = "indicator-columns",
          
          # bloc 1
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
            ) # à revoir 
          ),
          
          # bloc 2 (colonne)
          tags$div(
            class = "indicator-column",
            tags$strong("Interpretation & limitations"),
            tags$p(
              "Digital representations of urban rivers are unevenly produced."
            ),
            tags$p(
              "What people say about rivers online reflects how they perceive and interact with them."
            )
          )
        ),
        
        # cette petite phrase permet d'avoir une vue sur le mot et l'indicateur qu'on rg
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
          "oiseau","sediment","others","..." # reprendre 
        )
      )
    }
  })
  
  
# Titre dynamique du nuage selon le mot mais aussi si on regarde une ville ou le monde
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
  
  
# le nuage de mot aussi est dynamique selon le mot choisi dans la liste déroulante et la ville ... 
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
        height = "25vh" # c'est la taille du nuage
      )
    }
  })
  
  
# La petite boite sous le nuage de mots qui explique ce qu'on voit... 
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
  
  
# le bouton dans le nuage qui permet de revenir au global c'est à dire sur une vue monde
# Apparait une fois qu'on a cliquer sur une ville 
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
  
# Changmt des données cartographiques selon le mot sélectionné
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
  
  
  ## Render Leaflet
  
## La grande map
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
  
  
# la petit carte de loc à gauche
  # initiale
  output$mini_map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(
        lng = 0.3458,
        lat = 50.6253,
        zoom = 3
      )
  })
  
  
        ## Les events 
  
# Quand on change de mot :on retire la ville précédemment sélectionnée
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
    ## puis La GRANDE CARTE --> à revoir le flyto le zoom qui se fait la...(j'ai rg ducoup j'ai mis 4.. à voir)
    leaflet::leafletProxy("map") |>
      leaflet::flyTo(
        lng = ville$lon[1],
        lat = ville$lat[1],
        zoom = 4
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
    # puis la PETITE CARTE (d'abord on recup la ville puis on regarde si elle à une geom dans "zone")
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
    # Cadrage auto de la petite carte
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
  
# la m^me que celle du dessus mais enfaite sur le bouton "global" view comme ça les gens pvt revenir au nuage global en cliquant
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
  
    ## Render plot
  
# Sortie du nuage de mots
  output$mini_plot <- renderPlot({
    data_cloud <- context_cloud_data()
    req(nrow(data_cloud) > 0)
  # légende on va la gerer comme ça --> 
    # Largeur actuelle du graphique dans le navigateur
    plot_width <- session$clientData$output_mini_plot_width
    # Taille dynamique de la légende
    legend_title_size <- 
      max(7,min(11, plot_width * 0.03))
    legend_text_size <- 
      max(6,min(10, plot_width * 0.025))
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
  
      ### Fin de l'onglet indocator


  ############################ Onglet landscapes changes ##################################
  
# La vue qui est sélec (le monde ou local)
  landscape_view_selected <- reactiveVal("global")
  observeEvent(input$landscape_global, {
    landscape_view_selected("global")
    })
  observeEvent(input$landscape_city, {
    landscape_view_selected("city")
  })
  
# Afficher la bonne vue (local ou monde) selon le choix
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
        "Local view",
        class = if (landscape_view_selected() == "city") {
          "landscape-view-btn active"
        } else {
          "landscape-view-btn"
        }
      )
    )
  })
  
  ### Global View
  
  ## Reactives
  
# Récupération de la définition associée à l'impact, la pression ou le driver
  landscape_category_info <- reactive({
    req(input$landscape_global_category)
    selected <- input$landscape_global_category
    matches <- infos |>
      dplyr::filter(
        !is.na(NomVar),
        !is.na(Def),
        trimws(Def) != "",
        selected == NomVar |
          startsWith(selected, paste0(NomVar, "_"))
      ) |>
      dplyr::mutate(
        longueur = nchar(NomVar)
      ) |>
      dplyr::arrange(dplyr::desc(longueur))
    
    if (nrow(matches) == 0) {
      return(NA_character_)
    }
    matches$Def[1]
  })
  
# Définition générale des indicateurs Landscape (impact,driver,pression)
  landscape_indicator_info <- reactive({
    switch(
      input$landscape_global_variable,
      "impact" = paste(
        "An observed consequence of environmental changes",
        "affecting the structure, functioning or ecological",
        "conditions of river landscapes."
      ),
      "pressure" = paste(
        "A direct influence or stress exerted on a river system",
        "by natural processes or human activities, contributing",
        "to changes in its physical and ecological characteristics."
      ),
      "driver" = paste(
        "An underlying natural or human-related factor that",
        "drives changes in river systems, influencing the",
        "pressures exerted on river landscapes."
      )
    )
  })
  
  
# Affichage dynamique de la boîte Information
  output$landscape_category_definition <- renderUI({
    req(input$landscape_global_mode)
    req(input$landscape_global_variable)
    indicator_label <- switch(
      input$landscape_global_variable,
      "impact" = "Impacts",
      "pressure" = "Pressures",
      "driver" = "Drivers"
    )
    # Définition générale
    general_info <- tagList(
      tags$strong(indicator_label),
      tags$p(landscape_indicator_info())
    )
    
  # En mode Number : définition générale uniquement
    if (input$landscape_global_mode != "typology") {
      return(general_info)
    }
  # En mode Typology : définition générale + catégorie
    req(input$landscape_global_category)
    definition <- landscape_category_info()
    category_info <- if (
      is.na(definition) || !nzchar(trimws(definition))
    ) {
      tags$p("No definition available for this category.")
    } else {
      tags$p(definition)
    }
    tagList(
      general_info,
      tags$hr(),
      tags$strong(input$landscape_global_category),
      category_info
    )
  })
  
  
#variable nombre de ... et les pixels qui ont pas changé 
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
  
  
# l'onglet typo :  pour choisir un (impact/driver/pression) 
  landscape_typology_selected <- reactive({
    req(input$landscape_global_category)
    variable_name <- switch(
      input$landscape_global_variable,
      "impact" = "Impact",
      "pressure" = "Pressure",
      "driver" = "Driver"
    )
    zone_name <- switch(
      input$landscape_global_zone,
      "tot" = "all",
      "cor" = "corridor",
      "pl" = "plain"
    )
    data_typology <- landscape_impacts
    if (zone_name != "all") {
      data_typology <- data_typology |>
        dplyr::filter(zone == zone_name)
    }
    
    # Séparer les lignes avec des choix multpl (',')
    data_categories <- data_typology |>
      dplyr::select(citycode, dplyr::all_of(variable_name)) |>
      tidyr::separate_longer_delim(
        cols = dplyr::all_of(variable_name),
        delim = ","
      ) |>
      dplyr::mutate(
        category = stringr::str_trim(
          as.character(.data[[variable_name]])
        )
      )
    city_status <- data_categories |>
      dplyr::group_by(citycode) |>
      dplyr::summarise(
        
        # 'Catégorie' sélectionnée présente dans la ville
        present = any(
          category == input$landscape_global_category &
            !is.na(category) &
            !tolower(category) %in% c("no_data", "null", "na", ""),
          na.rm = TRUE
        ),
        # Au moins une catégorie exploitable
        has_data = any(
          !is.na(category) &
            !tolower(category) %in% c("no_data", "null", "na", ""),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        status = dplyr::case_when(
          present ~ "Present",
          has_data ~ "Not found",
          TRUE ~ "No data"
        )
      )
    
    # Conserver toutes les villes
    landscape_global |>
      dplyr::left_join(
        city_status |> dplyr::select(citycode, status),
        by = "citycode"
      ) |>
      dplyr::mutate(
        status = dplyr::coalesce(status, "No data")
      )
  })
  
  
 ## Events
  
# Liste dynamique des differents choix (impact, pression, driver)
  observeEvent(input$landscape_global_variable, {
    variable_name <- switch(
      input$landscape_global_variable,
      "impact" = "Impact",
      "pressure" = "Pressure",
      "driver" = "Driver"
    )
    # Récup
    categories <- landscape_impacts |>
      dplyr::pull(dplyr::all_of(variable_name)) |>
      as.character() |>
      stringr::str_split(",") |>
      unlist() |>
      stringr::str_trim() |>
      unique()
    # Suppression quand rien
    categories <- sort(
      categories[
        !is.na(categories) &
          !tolower(categories) %in% c("", "no_data", "null", "na")
      ]
    )
    category_label <- switch(
      input$landscape_global_variable,
      "impact" = "Impacts",
      "pressure" = "Pressures",
      "driver" = "Drivers"
    )
    # MAJ
    updateSelectInput(
      session,
      "landscape_global_category",
      label = category_label,
      choices = categories,
      selected = categories[1]
    )
  })
  
  

  # la map centrale dans "GLOBAL VIEW"
  output$landscape_global_map <- leaflet::renderLeaflet({
    
    # LA CARTE pour Typologie ...
    if (identical(input$landscape_global_mode, "typology")) {
      data_map <- landscape_typology_selected() |>
        dplyr::filter(
          !is.na(Longitude),
          !is.na(Latitude)
        )
      return(
        leaflet::leaflet(data = data_map) |>
          leaflet::addTiles() |>
          leaflet::addCircleMarkers(
            lng = ~Longitude,
            lat = ~Latitude,
            radius = 5,
            fillColor = ~dplyr::case_when(
              status == "Present" ~ "#2171b5",
              status == "Not found" ~ "#888888",
              TRUE ~ "#dddddd"
            ),            color = "#333333",
            weight = 1,
            fillOpacity = 0.8,
            popup = ~paste0(
              "<strong>", Urban_aggl, "</strong><br>",
              "Category: ", input$landscape_global_category, "<br>",
              "Status: ", status)
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            colors = c("#2171b5", "#888888", "#dddddd"),
            labels = c("Present", "Not found", "No data"),
            title = "Selected category",
            opacity = 0.8
          )
      )
    }
    
    #LA MAP pour la carte NUMBER de .....
    data_map <- landscape_global_selected() |>
      dplyr::filter(
        !is.na(value),
        value > 0
      )
    # cercle prop
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
 # Légende des cercles prop (un bout du code permet de faire des 'traits' pour la legende)
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
    # Traits de lecture en cascade : petit trafic bizarre mais qui fonctionne ...
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
          "Proportion of pixels<br>that changed: ",
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
        title = "Proportion of pixels<br>that changed",
        opacity = 0.8
      ) |>
      leaflet::addControl(
        html = htmltools::HTML(legend_html),
        position = "bottomleft"
      )
  })
  
  
  
  # react pour l'onglet typo : choisir une zone et voir les villes concernées 
  landscape_typology_selected <- reactive({
    req(input$landscape_global_category)
    variable_name <- switch(
      input$landscape_global_variable,
      "impact" = "Impact",
      "pressure" = "Pressure",
      "driver" = "Driver"
    )
    zone_name <- switch(
      input$landscape_global_zone,
      "tot" = "all",
      "cor" = "corridor",
      "pl" = "plain"
    )
    data_typology <- landscape_impacts
    if (zone_name != "all") {
      data_typology <- data_typology |>
        dplyr::filter(zone == zone_name)
    }
    # Séparer les catégories multiples
    data_categories <- data_typology |>
      dplyr::select(citycode, dplyr::all_of(variable_name)) |>
      tidyr::separate_longer_delim(
        cols = dplyr::all_of(variable_name),
        delim = ","
      ) |>
      dplyr::mutate(
        category = stringr::str_trim(
          as.character(.data[[variable_name]])
        )
      )
    city_status <- data_categories |>
      dplyr::group_by(citycode) |>
      dplyr::summarise(
        
        # Catégorie sélectionnée présente
        present = any(
          category == input$landscape_global_category &
            !is.na(category) &
            !tolower(category) %in% c("no_data", "null", "na", ""),
          na.rm = TRUE
        ),
        # Au moins une catégorie exploitable
        has_data = any(
          !is.na(category) &
            !tolower(category) %in% c("no_data", "null", "na", ""),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        status = dplyr::case_when(
          present ~ "Present",
          has_data ~ "Not found",
          TRUE ~ "No data"
        )
      )
    # Conserver toutes les villes
    landscape_global |>
      dplyr::left_join(
        city_status |> dplyr::select(citycode, status),
        by = "citycode"
      ) |>
      dplyr::mutate(
        status = dplyr::coalesce(status, "No data")
      )
  })
  
  
           ### Local View
  
  ## Render UI 

#Permet d'afficher sur le website les impacts sous forme de boutons ...
  output$landscape_city_impacts_ui <- renderUI({
    impacts <- landscape_city_impacts()$Impact
    if (length(impacts) == 0) {
      return(tags$p("No impact data available for this selection."))
    }
    tags$div(
      class = "landscape-impacts-list",
      lapply(seq_along(impacts), function(i) {
        actionButton(
          inputId = paste0("landscape_impact_", i),
          label = impacts[i],
          class = "landscape-impact-button"
        )
      })
    )
  })
  
# L"emplacement de l'arbre dans l'app
  output$landscape_city_tree_ui <- renderUI({
    impact <- landscape_selected_impact()
    if (is.null(impact)) {
      return(NULL)
    }
    tags$div(
      class = "landscape-tree-container",
      tags$h4(impact),
      visNetwork::visNetworkOutput(
        "landscape_city_tree_graph",
        width = "100%",
        height = "45vh"
      )
    )
  })
  
  
  

  ## observe
  
# ce code va servir à alimenter le selecteur des villes
  observe({
    villes <- landscape_pixel |>
      dplyr::distinct(citycode, Urban.Aggl) |>
      dplyr::arrange(Urban.Aggl)

    shiny::updateSelectInput(
      session = session,
      inputId = "landscape_city_selected",
      choices = stats::setNames(
        villes$citycode,
        villes$Urban.Aggl),
        selected = "699745_20997"
    )
  })
  
# Permet de faire fonctionner le bouton (i) d'infos à coté du titre
  observeEvent(input$landscape_classification_info, {
    showModal(
      modalDialog(
        title = "Classification legend",
        tags$p(
          "Each letter below the bars represents a class of riverscape change."
        ),
        tags$p(tags$strong("A — "), "Major increase and minor decrease"),
        tags$p(tags$strong("B — "), "Medium increase and minor decrease"),
        tags$p(tags$strong("C — "), "Medium increase and medium decrease"),
        tags$p(tags$strong("D — "), "Minor increase and minor decrease"),
        tags$p(tags$strong("E — "), "Minor increase and medium decrease"),
        tags$p(tags$strong("F — "), "Minor increase and major decrease"),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  
  # Réinitialiser la sélection si on change de ville ou de zone
  observeEvent(
    list(input$landscape_city_selected, input$landscape_zone_selected),
    {
      landscape_selected_impact(NULL)
    },
    ignoreInit = TRUE
  ) 
  
  
  ## Reactive
  
# On va filtrer la donnée selon les choix de l'utilisateur 
  landscape_city_selected_data <- reactive({
    req(
      input$landscape_city_selected,
      input$landscape_zone_selected
    )
    landscape_pixel |>
      dplyr::filter(
        citycode == input$landscape_city_selected,
        zone == input$landscape_zone_selected
      )
  })
  
#Va permettre de créer une couche ou les zones subdivisés --> fusion pour corridor/plain pour l'affichage
  landscape_city_map_data <- reactive({
    req(input$landscape_city_selected, input$landscape_zone_selected)
    # Sélectionner les polygones
    subdivisions <- zone |>
      dplyr::filter(
        CityCode == input$landscape_city_selected,
        zone == input$landscape_zone_selected
      )
    if (nrow(subdivisions) == 0) {return(NULL)}
    # Récupérer les coordonnées de la ville
    coord_ville <- landscape_global |>
      dplyr::filter(citycode == input$landscape_city_selected)
    req(nrow(coord_ville) > 0)
    lon <- coord_ville$Longitude[1]
    lat <- coord_ville$Latitude[1]
    req(is.finite(lon), is.finite(lat))
    # Déterminer le fuseau UTM
    fuseau <- max(1, min(60, floor((lon + 180) / 6) + 1))
    epsg_utm <- if (lat >= 0) {
      32600 + fuseau
    } else {
      32700 + fuseau
    }
    # Reprojeter et réparer les géométries
    subdivisions_proj <- subdivisions |>
      sf::st_transform(epsg_utm) |>
      sf::st_make_valid()
    # Fusionner pour obtenir le périmètre extérieur
    perimetre_proj <- subdivisions_proj |>
      sf::st_geometry() |>
      sf::st_union()
    # Revenir en WGS84
    list(
      subdivisions = sf::st_transform(subdivisions_proj, 4326),
      perimetre = sf::st_transform(perimetre_proj, 4326)
    )
  }) 
  
  
#ce code va servir à faire bouger le bloc d'interpretation   
  landscape_city_impacts <- reactive({
    req(input$landscape_city_selected, input$landscape_zone_selected)
    landscape_impacts |>
      dplyr::filter(
        citycode == input$landscape_city_selected,
        zone == input$landscape_zone_selected
      ) |>
      tidyr::separate_longer_delim(
        cols = Impact,
        delim = ","
      ) |>
      dplyr::mutate(
        Impact = stringr::str_trim(Impact)
      ) |>
      dplyr::filter(
        !is.na(Impact),
        !tolower(Impact) %in% c("no_data", "null", "na", "")
      ) |>
      dplyr::distinct(Impact) |>
      dplyr::arrange(Impact)
  }) 
  
  
#Récuperer l'impact sur lequel on a cliqué 
  landscape_selected_impact <- reactiveVal(NULL)
  observe({
    impacts <- landscape_city_impacts()$Impact
    lapply(seq_along(impacts), function(i) {
      local({
        index <- i
        
        observeEvent(input[[paste0("landscape_impact_", index)]], {
          landscape_selected_impact(impacts[index])
        }, ignoreInit = TRUE)
        
      })
    })
  })
  
  
# Associations correspondant à l'impact sélectionné
  landscape_city_associations <- reactive({
    req(
      input$landscape_city_selected,
      input$landscape_zone_selected
    )
    impact <- landscape_selected_impact()
    req(impact)
    landscape_impacts |>
      dplyr::mutate(
        association_id = dplyr::row_number()
      ) |>
      dplyr::filter(
        citycode == input$landscape_city_selected,
        zone == input$landscape_zone_selected
      ) |>
      tidyr::separate_longer_delim(
        Impact,
        delim = ","
      ) |>
      dplyr::mutate(
        Impact = stringr::str_trim(as.character(Impact))
      ) |>
      dplyr::filter(Impact == impact) |>
      dplyr::select(
        association_id,
        Impact,
        Pressure,
        Driver,
        change_spatiality
      )
  })
  
  
# Recup l'association sélectionnée par l'utilisateur
  landscape_selected_association <- reactiveVal(NULL)
  # Réinitialiser la sélection quand les associations changent
  observeEvent(landscape_city_associations(), {
    landscape_selected_association(NULL)
  })
  # Afficher les associations de l'impact sélectionné
  output$landscape_city_associations_ui <- renderUI({
    donnees <- landscape_city_associations()
    if (nrow(donnees) == 0) {
      return(NULL)
    }
    choix <- stats::setNames(
      as.character(donnees$association_id),
      paste0(
        "Association ", seq_len(nrow(donnees)),
        " — ",
        dplyr::coalesce(as.character(donnees$Pressure), "No pressure"),
        " / ",
        dplyr::coalesce(as.character(donnees$Driver), "No driver")
      )
    )
    shiny::selectInput(
      inputId = "landscape_association_selected",
      label = "Select an association",
      choices = c("Choose an association" = "", choix),
      selected = ""
    )
  })
  # Enregistrer l'association choisie
  observeEvent(input$landscape_association_selected, {
    id <- input$landscape_association_selected
    if (is.null(id) || !nzchar(id)) {
      landscape_selected_association(NULL)
    } else {
      landscape_selected_association(as.integer(id))
    }
    
  })
  
  
  
# Va faire le lien-Relations --> Impact / Pressure / Driver de l'impact sélec
  landscape_city_tree_data <- reactive({
    req(input$landscape_city_selected,
        input$landscape_zone_selected)
    impact <- landscape_selected_impact()
    req(impact)
    # Lignes associées à l'impact sélectionné (+reactive plus haut)
    donnees <- landscape_city_associations() |>
      dplyr::mutate(
        ligne_id = association_id
      )
    # Séparer les pressions
    pressions <- donnees |>
      dplyr::select(ligne_id, Pressure) |>
      tidyr::separate_longer_delim(Pressure, delim = ",") |>
      dplyr::mutate(
        Pressure = stringr::str_trim(as.character(Pressure))
      ) |>
      dplyr::filter(
        !is.na(Pressure),
        !tolower(Pressure) %in% c("", "na", "null", "no_data")
      )
    # Séparer les drivers
    drivers <- donnees |>
      dplyr::select(ligne_id, Driver) |>
      tidyr::separate_longer_delim(Driver, delim = ",") |>
      dplyr::mutate(
        Driver = stringr::str_trim(as.character(Driver))
      ) |>
      dplyr::filter(
        !is.na(Driver),
        !tolower(Driver) %in% c("", "na", "null", "no_data")
      )
    # Pressions et drivers appartenant à la même ligne
    liens_pd <- pressions |>
      dplyr::inner_join(
        drivers,
        by = "ligne_id",
        relationship = "many-to-many"
      ) |>
      dplyr::distinct(Pressure, Driver)
    # Drivers dont la ligne ne possède aucune pression
    drivers_directs <- drivers |>
      dplyr::anti_join(
        pressions,
        by = "ligne_id"
      ) |>
      dplyr::distinct(Driver)
    list(
      impact = impact,
      pressions = unique(pressions$Pressure),
      drivers = unique(drivers$Driver),
      liens_pd = liens_pd,
      drivers_directs = drivers_directs
    )
  })

  
  ## Render(s)
  
  #Render plot 
  
  #On appelle la fonction du plot pour créér le graphe
  output$landscape_pixel_graph <- renderPlot({
    data_graph <- landscape_city_selected_data()
    validate(
      need(
        nrow(data_graph) > 0,
        "No landscape data available for this selection."
      )
    )
    plot_landscape_pixel(data_graph)
  })
  
  
  # render leaflet
  
# sortie de la carte   
  output$landscape_city_map <- leaflet::renderLeaflet({
    req(input$landscape_city_selected)
    coord_ville <- landscape_global |>
      dplyr::filter(citycode == input$landscape_city_selected)
    req(nrow(coord_ville) > 0)
    lon <- coord_ville$Longitude[1]
    lat <- coord_ville$Latitude[1]
    req(is.finite(lon), is.finite(lat))
    donnees <- landscape_city_map_data()
    carte <- leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(lng = lon, lat = lat, zoom = 10)
    
    if (is.null(donnees)) {
      return(carte)
    }
    
    # Carte de base
    carte <- carte |>
      leaflet::addPolygons(
        data = donnees$subdivisions,
        fill = FALSE,
        color = "#64748b",
        weight = 1.5,
        group = "subdivisions"
      ) |>
      leaflet::addPolygons(
        data = donnees$perimetre,
        fill = FALSE,
        color = "#FFD700",
        weight = 4,
        group = "perimetre"
      )
    
    # Association actuellement sélectionnée
    association_id <- landscape_selected_association()
    if (is.null(association_id)) {
      return(carte)
    }
    association <- landscape_city_associations() |>
      dplyr::filter(association_id == !!association_id)
    if (nrow(association) == 0) {
      return(carte)
    }
    # Lire la spatialité de l'association
    spatialite <- parse_landscape_spatiality(
      association$change_spatiality[1]
    )
    
    if (spatialite$statut == "no_change") {
      carte <- carte |>
        leaflet::addControl(
          html = paste0(
            "<div style='background:white; padding:10px 15px;",
            "border-radius:6px; font-weight:bold;'>",
            "No visible change",
            "</div>"
          ),
          position = "topright"
        )
      return(carte)
    }
    
    if (spatialite$statut != "change") {
      return(carte)
    }
    
    # Sélection de la géométrie
    geometrie_changement <- if (spatialite$secteur == "all") {
      donnees$perimetre
    } else {
      donnees$subdivisions |>
        dplyr::filter(reach == spatialite$secteur)
    }
    
    if (length(sf::st_geometry(geometrie_changement)) == 0) {
      return(carte)
    }
    
    styles <- spatialite$styles
    
    # FRAISE : contour épais continu
    if ("local" %in% styles) {
      carte <- carte |>
        leaflet::addPolygons(
          data = geometrie_changement,
          fill = FALSE,
          color = "#DB2777",
          weight = 6,
          group = "changement"
        )
    }
    
    # BANANE : contour pointillé
    if ("reach" %in% styles) {
      carte <- carte |>
        leaflet::addPolygons(
          data = geometrie_changement,
          fill = FALSE,
          color = "#DB2777",
          weight = 3,
          dashArray = "8,6",
          group = "changement"
        )
    }
    
    # KIWI : couche qui recevra les hachures
    if ("occasional" %in% styles) {
      
      carte <- carte |>
        leaflet::addPolygons(
          data = geometrie_changement,
          fill = FALSE,
          color = "#DB2777",
          weight = if ("banane" %in% styles) 0 else 2,
          layerId = "hachures_changement",
          group = "changement"
        )
      
      # On reprend ton JavaScript fonctionnel
      carte <- htmlwidgets::onRender(
        carte,
        "
      function(el, x) {

        var map = this;

        var pattern = new L.StripePattern({
          weight: 2,
          spaceWeight: 7,
          color: '#DB2777',
          opacity: 1,
          angle: 45
        });

        pattern.addTo(map);

        var layer = map.layerManager.getLayer(
  'shape',
  'hachures_changement'
);

if (layer) {
  layer.setStyle({
    fill: true,
    fillOpacity: 1,
    fillPattern: pattern
  });
}

      }
      "
      )
    }
    
    carte
  })
  
  
  # render arbre
#ce code va contruire le graphique de l'arbre de liens
  output$landscape_city_tree_graph <- visNetwork::renderVisNetwork({
    donnees <- landscape_city_tree_data()
    # Nœuds
    # Nœuds
    nodes <- data.frame(
      id = paste0("I:", donnees$impact),
      label = donnees$impact,
      group = "Impact"
    )
    
    if (length(donnees$pressions) > 0) {
      nodes <- dplyr::bind_rows(
        nodes,
        data.frame(
          id = paste0("P:", donnees$pressions),
          label = donnees$pressions,
          group = "Pressure"
        )
      )
    }
    
    if (length(donnees$drivers) > 0) {
      nodes <- dplyr::bind_rows(
        nodes,
        data.frame(
          id = paste0("D:", donnees$drivers),
          label = donnees$drivers,
          group = "Driver"
        )
      )
    }
    
    # Liens Impact -> Pressure
    edges_ip <- data.frame(from = character(), to = character())
    
    if (length(donnees$pressions) > 0) {
      edges_ip <- data.frame(
        from = rep(paste0("I:", donnees$impact), length(donnees$pressions)),
        to = paste0("P:", donnees$pressions)
      )
    }
    
    # Liens Pressure -> Driver
    edges_pd <- donnees$liens_pd |>
      dplyr::transmute(
        from = paste0("P:", Pressure),
        to = paste0("D:", Driver)
      )
    
    # Liens directs Impact -> Driver
    edges_id <- data.frame(from = character(), to = character())
    
    if (nrow(donnees$drivers_directs) > 0) {
      edges_id <- donnees$drivers_directs |>
        dplyr::transmute(
          from = rep(paste0("I:", donnees$impact), dplyr::n()),
          to = paste0("D:", Driver)
        )
    }
    
    # Tous les liens
    edges <- dplyr::bind_rows(edges_ip, edges_pd, edges_id) |>
      dplyr::distinct()
    visNetwork::visNetwork(
      nodes,
      edges,
      width = "100%",
      height = "45vh"
    ) |>
      visNetwork::visNodes(
        shape = "box",
        margin = 12,
        font = list(size = 16)
      ) |>
      visNetwork::visGroups(
        groupname = "Impact",
        color = list(background = "#dbeafe", border = "#60a5fa")
      ) |>
      visNetwork::visGroups(
        groupname = "Pressure",
        color = list(background = "#fef3c7", border = "#d97706")
      ) |>
      visNetwork::visGroups(
        groupname = "Driver",
        color = list(background = "#f3e8ff", border = "#a78bfa")
      ) |>
      visNetwork::visEdges(
        arrows = "",
        color = list(color = "#94a3b8")
      ) |>
      visNetwork::visHierarchicalLayout(
        direction = "UD",
        levelSeparation = 130,
        nodeSpacing = 180,
        sortMethod = "directed"
      ) |>
      visNetwork::visInteraction(
        dragNodes = FALSE,
        dragView = FALSE,
        zoomView = FALSE
      )
  })
  
  observe({
    req(landscape_city_associations())
    
    print(landscape_city_associations())
  })
  
  
  ### Fin de l'onglet landscapes
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #  --------------------------------------------- onglet autre
  
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