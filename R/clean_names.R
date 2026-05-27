#' @title Nettoyage des noms de variables
#' @description Transforme des chaînes de caractères en noms de variables valides,
#' minuscules, sans accents ni caractères spéciaux.
#' @param x Un vecteur de caractères ou un data.frame.
#' @param max_length Longueur maximale des noms (optionnel).
#' @param prefix Préfixe à ajouter si le nom commence par un chiffre (optionnel).
#' @return Un objet de même type que x avec des noms nettoyés.
#' @export
clean_names <- function(x, max_length = NULL, prefix = NULL) {
  if (is.data.frame(x)) {
    old_names <- names(x)
    new_names <- clean_names(old_names, max_length = max_length, prefix = prefix)
    names(x) <- new_names
    return(x)
  }
  
  if (!is.character(x)) {
    stop("`x` must be either a character vector or a data.frame")
  }
  
  # 1. Remplacement manuel de "/" par "_ou_" (cas spécifique demandé par les tests)
  # On ne remplace PAS le point ici car names(df) avec check.names=TRUE (défaut) 
  # transforme les espaces en points, et on veut "nom_patient" pour "Nom Patient".
  clean <- gsub("/", "_ou_", x)
  
  # 2. Conversion en minuscules
  clean <- tolower(clean)
  
  # 3. Suppression des accents
  clean <- iconv(clean, to = "ASCII//TRANSLIT")
  
  # 4. Remplacement des caractères non alphanumériques par des underscores
  # Cela inclut le point "." généré par data.frame()
  clean <- gsub("[^a-z0-9]+", "_", clean)
  
  # 5. Suppression des underscores en début/fin
  clean <- gsub("^_+|_+$", "", clean)
  
  # 6. Ajout du préfixe si commence par un chiffre
  if (is.null(prefix)) {
    clean <- ifelse(grepl("^[0-9]", clean), paste0("v_", clean), clean)
  } else {
    clean <- ifelse(grepl("^[0-9]", clean), paste0(prefix, clean), clean)
  }
  
  # 7. Troncation
  if (!is.null(max_length)) {
    clean <- substr(clean, 1, max_length)
  }
  
  # 8. Gérer les doublons
  if (any(duplicated(clean))) {
    clean <- make.unique(clean, sep = "_")
  }
  
  return(clean)
}
