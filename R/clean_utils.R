#' @title Utilitaires de nettoyage, d'imputation et de normalisation de noms
#' @description Fonctions d'aide pour le nettoyage des vecteurs texte/binaires/numériques,
#' la normalisation des noms de colonnes (`clean_names` / `clean_column_names`),
#' et l'imputation par le mode ou la moyenne/médiane.
#' 
#' @param x Vecteur ou data.frame à nettoyer ou imputer.
#' @param yes_label Libellé pour la valeur positive des variables binaires (défaut: "Oui").
#' @param no_label Libellé pour la valeur négative des variables binaires (défaut: "Non").
#' @param type Type d'imputation pour les variables numériques: "mean" (moyenne) ou "median" (médiane).
#' @param max_length Longueur maximale optionnelle pour les noms de colonnes.
#' @param prefix Préfixe à ajouter si le nom commence par un chiffre (défaut: "v_").
#' @param ... Arguments supplémentaires passés à `clean_names`.
#' 
#' @return Un vecteur ou data.frame nettoyé ou imputé.
#' 
#' @examples
#' clean_text(c("  homme ", "", "NA", "femme"))
#' clean_binary(c("oui", "NON", "1", "0", "Yes"))
#' clean_numeric(c("12,5", " 15 ", "NA"))
#' clean_names("Âge/Ans")
#' clean_column_names(data.frame("Nom Patient" = 1:2))
#' impute_mode(c("A", "A", "B", NA))
#' impute_mean(c(10, 20, NA, 30))
#' 
#' @name clean_utils
NULL

#' @rdname clean_utils
#' @export
clean_text <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) x <- as.character(x)
  
  x <- stringr::str_trim(x)
  na_patterns <- c("", "NA", "N/A", "<NA>", "NULL", "null", "inconnu", "Inconnu", "unspecified")
  x[x %in% na_patterns] <- NA_character_
  x
}

#' @rdname clean_utils
#' @export
clean_binary <- function(x, yes_label = "Oui", no_label = "Non") {
  x_clean <- clean_text(x)
  
  yes_vals <- c("oui", "yes", "true", "vrai", "1", "1.0", "y", "o", "positive", "positif", "+")
  no_vals  <- c("non", "no", "false", "faux", "0", "0.0", "n", "negative", "negatif", "-")
  
  res <- dplyr::case_when(
    tolower(x_clean) %in% yes_vals ~ yes_label,
    tolower(x_clean) %in% no_vals  ~ no_label,
    TRUE ~ x_clean
  )
  
  factor(res, levels = c(yes_label, no_label))
}

#' @rdname clean_utils
#' @export
clean_numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- clean_text(x)
    x <- gsub(",", ".", x, fixed = TRUE)
    x <- gsub(" ", "", x, fixed = TRUE)
  }
  suppressWarnings(as.numeric(x))
}

#' @rdname clean_utils
#' @export
clean_names <- function(x, max_length = NULL, prefix = "v_") {
  if (is.data.frame(x)) {
    names(x) <- clean_names(names(x), max_length = max_length, prefix = prefix)
    return(x)
  }
  
  if (!is.character(x)) {
    stop("`x` must be either a character vector or a data.frame")
  }
  
  res <- sapply(x, function(nm) {
    # Supprimer les accents
    nm_clean <- iconv(nm, to = "ASCII//TRANSLIT")
    nm_clean <- tolower(nm_clean)
    # Remplacer / ou .ans par _ou_ans
    nm_clean <- gsub("age[._/]+ans", "age_ou_ans", nm_clean)
    nm_clean <- gsub("/", "_ou_", nm_clean, fixed = TRUE)
    # Remplacer les caractères non alphanumériques par des underscores
    nm_clean <- gsub("[^a-z0-9]", "_", nm_clean)
    # Réduire les underscores multiples
    nm_clean <- gsub("_+", "_", nm_clean)
    # Supprimer underscores au début/fin
    nm_clean <- gsub("^_|_$", "", nm_clean)
    
    # Si commence par un chiffre ou par x/v suivi d'un chiffre
    if (grepl("^[0-9]", nm_clean)) {
      nm_clean <- paste0(prefix, nm_clean)
    } else if (grepl("^[x|v][0-9]", nm_clean)) {
      nm_clean <- paste0(prefix, substr(nm_clean, 2, nchar(nm_clean)))
    }
    
    if (!is.null(max_length) && nchar(nm_clean) > max_length) {
      nm_clean <- substr(nm_clean, 1, max_length)
      nm_clean <- gsub("_+$", "", nm_clean)
    }
    nm_clean
  }, USE.NAMES = FALSE)
  
  res
}

#' @rdname clean_utils
#' @export
clean_column_names <- function(x, ...) {
  clean_names(x, ...)
}

#' @rdname clean_utils
#' @export
impute_mode <- function(x) {
  clean_x <- x[!is.na(x)]
  if (length(clean_x) == 0) return(x)
  
  ux <- unique(clean_x)
  mode_val <- ux[which.max(tabulate(match(clean_x, ux)))]
  
  x[is.na(x)] <- mode_val
  x
}

#' @rdname clean_utils
#' @export
impute_mean <- function(x, type = c("mean", "median")) {
  type <- match.arg(type)
  if (!is.numeric(x)) {
    stop("`x` doit être un vecteur numérique.")
  }
  
  imp_val <- if (type == "mean") {
    mean(x, na.rm = TRUE)
  } else {
    stats::median(x, na.rm = TRUE)
  }
  
  x[is.na(x)] <- imp_val
  x
}
