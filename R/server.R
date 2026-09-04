################################## COTE SERVER ##################################################


server <- function(input, output, session) {
  
  ############################ onglet N°2 ##################################
  
  output$notes <- renderText({
    paste("Thème sélectionné :", input$Ind, "-", input$Var)
  })
  
  output$mini_map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::setView(lng = 2.35, lat = 48.85, zoom = 10)
  })
  
  output$mini_plot <- renderPlot({
    df <- data.frame(x = 1:8, y = rnorm(8))
    ggplot(df, aes(x = factor(x), y = y)) + geom_col(fill = "steelblue") + theme_minimal() + labs(x = NULL, y = NULL)
  })
  
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::setView(lng = 0.3458, lat = 50.6253, zoom = 3)
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