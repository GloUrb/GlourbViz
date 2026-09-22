library(shiny)
library(leaflet)
library(ggplot2)
library(sf)
library(htmlwidgets)
library(ggwordcloud)

source("R/functions.R")
source("R/load_data.R")
source("R/ui.R")
source("R/server.R")


# Fait le lien
shinyApp(
  ui = ui,
  server = server
)