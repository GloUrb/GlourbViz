# Chargement data 

zone <- sf::st_read(
  "data/plain_global_reach/plain_global_reach.shp"
)

lexique_discourses <- readxl::read_excel(
  "data/lexique_discourses.xlsx"
)

lexique_city_global <- readRDS(
  "data/glourbviz_lexicon_discourses_city_global.rds"
)

context_specificities_selected <- readRDS(
  "data/glourbviz_context_specificities_selected.rds"
)

context_specificities_selected_city <- readRDS(
  "data/glourbviz_context_specificities_city_selected.rds"
)