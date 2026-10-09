################################## COTE UI ##################################################

ui <- fluidPage(includeCSS("www/css.css"),
                shinyjs::useShinyjs(),
                navbarPage(
                  title = div("GloUrbViz"),
                  windowTitle = "GloUrbViz",
                  id = "nav",
                  
                  
                  ########################### ONGLET indicator  ###################################
                  tabPanel(
                    "Indicator",
                    tags$div(
                      id = "indicatorLayout",
                      
                      # SIDEBAR GAUCHE
                      tags$div(
                        id = "leftSidebar",
                        # Filtres
                        tags$div(
                          class = "mini-box",
                          tags$strong("Filters"),
                          selectInput(
                            "Ind","Indicator",
                            choices = c("Biodiversity","Economy","Discourses of the Web")
                          ),
                          
                          uiOutput("variable_selector")
                        ),
                        
                        # Localisation
                        tags$div(
                          class = "mini-box",
                          tags$strong("Localisation"),
                          leaflet::leafletOutput(
                            "mini_map",
                            height = "25vh"
                          )
                        ),
                        
                        # Nuage de mots
                        tags$div(
                          class = "mini-box",
                          uiOutput("cloud_title"),
                          uiOutput("global_view_button"),
                          uiOutput("cloud_content")
                        ),
                          uiOutput("cloud_explanation")
                      ),
                      
                      # ZONE PRINCIPALE
                      tags$div(
                        id = "mainContent",
                        tags$div(
                          class = "box-info",
                          tags$h4("Indicator and data information"),
                          uiOutput("notes") #verbatimTextOutput met une boite | textOutput si je souhaite enlever la box autour de thème selectionné...se fait nat à cause de bootstrap
                        ),
                        tags$div(
                          leaflet::leafletOutput(
                            "map",
                            height = "75vh"
                          )
                        )
                      )
                    )
                  ),
                  
                  ############################ ONGLET landscape changes ##################################
                
                  tabPanel(
                    "Landscape changes",
                    # Boutons 
                    uiOutput("landscape_view_buttons"),
                    
                                                        #le bt global view (onglet)
                    tags$div(
                      id = "landscapeGlobal",
                      # Colonne gauche
                      tags$div(
                        id = "landscapeGlobalSidebar",
                        # bloc Information
                        tags$div(
                          class = "landscape-box",
                          tags$h4("Information"),
                          tags$p(
                            "Information about the selected landscape indicator."
                          )
                        ),
                        
                        # bloc choix donnée
                        tags$div(
                          class = "landscape-box",
                          tags$h4("Display"),
                          # Choix du mode de cartographie
                          tags$div(
                            class = "landscape-mode-switch",
                            radioButtons(
                              "landscape_global_mode",
                              NULL,
                              choices = c(
                                "Number" = "number",
                                "Typology" = "typology"
                              ),
                              selected = "number",
                              inline = TRUE
                            )
                          ),
                          tags$hr(class = "landscape-display-divider"),
                          # Choix de l'indicateur
                          tags$strong("Indicator"),
                          radioButtons(
                            "landscape_global_variable",
                            NULL,
                            choices = c(
                              "Impacts" = "impact",
                              "Pressures" = "pressure",
                              "Drivers" = "driver"
                            ),
                            selected = "impact"
                          ),
                          # Liste des catégories (mode Typology uniquement)
                          conditionalPanel(
                            condition = "input.landscape_global_mode == 'typology'",
                            tags$strong("Category"),
                            selectInput(
                              "landscape_global_category",
                              NULL,
                              choices = NULL
                            )
                          ),
                          # Choix de la zone
                          tags$strong("Zone"),
                          
                          radioButtons(
                            "landscape_global_zone",
                            NULL,
                            choices = c(
                              "Whole area" = "tot",
                              "Corridor" = "cor",
                              "Plain" = "pl"
                            ),
                            selected = "tot"
                          )
                        )
                      ),
                      
                      # Partie centrale :  la map
                      tags$div(
                        id = "landscapeGlobalMain",
                        tags$div(
                          class = "landscape-map-box",
                          leaflet::leafletOutput(
                            "landscape_global_map",
                            height = "65vh"
                          )
                        )
                      )
                    ),
                  
                                                          # le bt city (onglet)
                    tags$div(
                      id = "landscapeCity",
                      tags$div(
                        id = "landscapeCityLayout",
                        

                        # COLONNE GAUCHE
                        tags$div(
                          id = "landscapeCitySidebar",
                          
                          tags$div(
                            class = "landscape-box",
                            id = "landscapeCityInfo",
                            tags$h4("Information"),
                            tags$p("Select an element to display more information.")
                          ),
                          
                          tags$div(
                            class = "landscape-box",
                            id = "landscapeCitySelection",
                            
                            tags$h4("Selection"),
                            
                            selectInput(
                              "landscape_city_selected",
                              "City",
                              choices = NULL
                            ),
                            
                            tags$strong("Zone"),
                            
                            radioButtons(
                              "landscape_zone_selected",
                              NULL,
                              choices = c(
                                "Corridor" = "corridor",
                                "Plain" = "plain"
                              ),
                              selected = "corridor"
                            )
                          ),
                          
                          tags$div(
                            class = "landscape-box",
                            id = "landscapeDataQuality",
                            
                            tags$h4("Data quality"),
                            
                            tags$p(
                              "Methodological information about the interpretation of landscape changes."
                            )
                          )
                        ),
                        
                        
                        # COLONNE DROITE
                        tags$div(
                          id = "landscapeCityMain",
                          
                          tags$div(
                            class = "landscape-box",
                            id = "landscapeCityGraph",
                            
                            tags$h4("Riverscape changes: status of the zones"),
                            
                            tags$div(
                              class = "landscape-placeholder-small",
                              "LANDSCAPE CHANGE GRAPH"
                            )
                          ),
                          
                          tags$div(
                            class = "landscape-map-box",
                            id = "landscapeCityMap",
                            
                            tags$div(
                              class = "landscape-placeholder",
                              "CITY MAP"
                            )
                          )
                        )
                      ),
                      
                      

                      # INTERPRETATION
                      tags$div(
                        class = "landscape-box",
                        id = "landscapeInterpretation",
                        
                        tags$h4("Interpretation of observed landscape changes"),
                        
                        tags$div(
                          class = "landscape-interpretation-placeholder",
                          "IMPACTS / PRESSURES / DRIVERS"
                        )
                      )
                    )
                  ),
                  
                  ############################ ONGLET N°3 ##################################
                  
                  tabPanel("Global Idea", "Contenu global..."),
                  
                  ############################ ONGLET N°4 ##################################
                  
                  tabPanel("About",
                           # Sidebar gauche (comme dans tes autres onglets)
                           tags$div(
                             id = "leftSidebarAbout",
                             tags$div(
                               class = "mini-box",
                               tags$strong("Navigation"),
                               tags$p("Explorez les autres onglets pour découvrir les fonctionnalités de l'application."),
                               tags$hr(),
                               tags$a("Documentation", href = "#", style = "display: block; margin-bottom: 10px;"),
                               tags$a("Contact", href = "mailto:lala.lala@example.com", style = "display: block;")
                             ),
                             tags$div(
                               class = "mini-box",
                               tags$strong("Projet"),
                               tags$p("GloUrbView est un outil d'analyse des changements dans les paysages urbains et fluviaux."),
                               tags$img(src = "logo.png", style = "width: 100%; margin-top: 10px;")  # Optionnel : ajoute un logo
                             )
                           ),
                           
                           # Zone principale
                           tags$div(
                             id = "mainContentAbout",
                             # Section : Présentation
                             tags$div(
                               class = "box-info",
                               tags$h2("À propos de GloUrbView", style = "color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                               tags$p(
                                 "GloUrbView est une application développée pour analyser les dynamiques des paysages urbains et fluviaux.
        Elle permet de visualiser les indicateurs clés (biodiversité, économie, etc.) et d'étudier les impacts,
        pressions et facteurs de changement."
                               ),
                               tags$p(
                                 "Cette application s'adresse à tous types de publics."
                               )
                             ),
                             
                             # Section : Équipe
                             tags$div(
                               class = "box-info",
                               style = "margin-top: 20px;",
                               tags$h2("L'équipe GloUrb", style = "color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                               tags$div(
                                 class = "row",
                                 tags$div(
                                   class = "col-md-4",
                                   tags$div(
                                     class = "mini-box",
                                     tags$h3("madame fraise"),
                                     tags$p("biologiste"),
                                     tags$p("Contact: ", tags$a("mme.fr@example.com", href = "mailto:mme.fr@example.com"))
                                   )
                                 ),
                                 tags$div(
                                   class = "col-md-4",
                                   tags$div(
                                     class = "mini-box",
                                     tags$h3("Jean Dupont"),
                                     tags$p("Expert en écologie fluviale"),
                                     tags$p("Contact: ", tags$a("jean.dupont@example.com", href = "mailto:jean.dupont@example.com"))
                                   )
                                 ),
                                 tags$div(
                                   class = "col-md-4",
                                   tags$div(
                                     class = "mini-box",
                                     tags$h3("Marie Martin"),
                                     tags$p("Cheffe de projet"),
                                     tags$p("Contact: ", tags$a("marie.martin@example.com", href = "mailto:marie.martin@example.com"))
                                   )
                                 )
                               )
                             ),
                             
                             # Section : Technologies
                             tags$div(
                               class = "box-info",
                               style = "margin-top: 20px;",
                               tags$h2("Technologies utilisées", style = "color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                               tags$div(
                                 class = "row",
                                 tags$div(
                                   class = "col-md-6",
                                   tags$h4("Langages et outils"),
                                   tags$ul(
                                     tags$li("Langage: R"),
                                     tags$li("Framework: Shiny"),
                                     tags$li("Visualisation: ggplot2, leaflet"),
                                     tags$li("Données: CSV, API INRAE")
                                   )
                                 ),
                                 tags$div(
                                   class = "col-md-6",
                                   tags$h4("Fonctionnalités clés"),
                                   tags$ul(
                                     tags$li("Visualisation interactive des indicateurs"),
                                     tags$li("Analyse des changements de paysage"),
                                     tags$li("Filtrage par zone et par variable"),
                                     tags$li("Export des données")
                                   )
                                 )
                               )
                             ),
                             
                             # Section : Instructions
                             tags$div(
                               class = "box-info",
                               style = "margin-top: 20px;",
                               tags$h2("Comment utiliser l'application ?", style = "color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                               tags$ol(
                                 tags$li("Sélectionnez un onglet pour accéder aux différentes fonctionnalités."),
                                 tags$li("Utilisez les filtres dans la sidebar pour affiner vos résultats."),
                                 tags$li("Explorez les cartes et graphes interactifs."),
                                 tags$li("Exportez les données si nécessaire.")
                               )
                             ),
                             
                             # Pied de page
                             tags$div(
                               style = "margin-top: 30px; text-align: center; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
                               tags$p("© 2026 – GloUrbView | Tous droits réservés")
                             )
                           )
                  )
                )
)