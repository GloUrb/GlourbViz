
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