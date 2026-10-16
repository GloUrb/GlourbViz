# Chargement data 

# landscape

zone <- sf::st_read(
  "data/landscapes/study_area_reach_zone/study_area_reach_zone.shp"
)

landscape_global <- readRDS(
  "data/landscapes/glourbviz_landscape_global.rds"
)

landscape_impacts <- readRDS(
  "data/landscapes/glourbviz_landscape_impacts.rds"
)

landscape_pixel <- readRDS("data/landscapes/glourbviz_landscape_pixels.rds"
)

infos <- readxl::read_xlsx("data/landscapes/info.xlsx"
)


# discoourses 

lexique_discourses <- readxl::read_excel(
  "data/discourses/lexique_discourses.xlsx"
)

lexique_city_global <- readRDS(
  "data/discourses/glourbviz_lexicon_discourses_city_global.rds"
)

context_specificities_selected <- readRDS(
  "data/discourses/glourbviz_context_specificities_selected.rds"
)

context_specificities_selected_city <- readRDS(
  "data/discourses/glourbviz_context_specificities_city_selected.rds"
)