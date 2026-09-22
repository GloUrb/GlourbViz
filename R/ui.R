################################## COTE UI ##################################################

ui <- fluidPage(includeCSS("www/css.css"),
                navbarPage(
                  title = div("GloUrbView"),
                  windowTitle = "GloUrbView",
                  id = "nav",
                  
                  ############################ ONGLET N°2 ##################################
                  
                  tabPanel("Global Idea", "Contenu global..."),
                  
                  ########################### ONGLET N°2 ###################################
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
                          tags$strong("Filtres"),
                          selectInput(
                            "Ind","Indicator",
                            choices = c("Biodiversity","economy","discours")
                          ),
                          
                          uiOutput("variable_selector")
                        ),
                        
                        # Localisation
                        tags$div(
                          class = "mini-box",
                          tags$strong("Localisation"),
                          leaflet::leafletOutput(
                            "mini_map",
                            height = "20vh"
                          )
                        ),
                        
                        # Nuage de mots
                        tags$div(
                          class = "mini-box",
                          uiOutput("cloud_title"),
                          uiOutput("cloud_content")
                        )
                      ),
                      
                      # ZONE PRINCIPALE
                      tags$div(
                        id = "mainContent",
                        tags$div(
                          class = "box-info",
                          tags$h4("Indicator and data information"),
                          verbatimTextOutput("notes") #textOutput si je souhaite enlever la box autour de thème selectionné...se fait nat à cause de bootstrap
                        ),
                        
                        tags$div(
                          leaflet::leafletOutput(
                            "map",
                            height = "70vh"
                          )
                        )
                      )
                    )
                  ),
                  
                  ############################ ONGLET N°3 ##################################
                  
                  tabPanel("Landscape changes",
                           tags$div(
                             id = "leftSidebar3",
                             tags$div(class = "mini-box",
                                      selectInput("ipd", "understand and analyze", choices = c("Impact","Pressure","Driver")),
                                      verbatimTextOutput("Explication")
                             ),
                             tags$div(class = "mini-box",
                                      tags$strong("Localisation"),
                                      leaflet::leafletOutput("mini_map3", height = "150px")
                             ),
                             tags$div(class = "mini-box",
                                      tags$strong("Data Quality"),
                                      plotOutput("mini_plot3", height = "150px")
                             )
                           ),
                           
                           # zone principale --> cadre texte en haut, grande carte en bas
                           tags$div(
                             id = "mainContent3",
                             tags$div(class = "box-info",
                                      tags$h4("Riverscape changes : status of the zones")),
                             tags$div(plotOutput("graphe_zones",height = "100px",width = "400px")),
                             tags$div(leaflet::leafletOutput("map3",height = "450px"))
                           ),
                  ),
                  
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