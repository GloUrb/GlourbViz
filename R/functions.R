
#onglet 2
# calcul la freq des mots pour le contexte

calculate_wordcloud_frequency <- function(data) {
  
  data |>
    dplyr::mutate(
      freq = n / sum(n, na.rm = TRUE) * 100
    )
}




# Onglet 3 

# Fonction couleur selon la valeur
get_color <- function(valeur) {
  if (valeur > 10) {
    return("green")   # Excédent
  } else if (valeur < -10) {
    return("red")     # Déficit
  } else {
    return("gray")    # Neutre
  }
}

