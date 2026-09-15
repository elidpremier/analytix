#' @title Le Rapport "Magique" en 1-Clic
#' @description Analyse automatiquement un jeu de données, détecte la variable d'intérêt la plus
#' pertinente si non spécifiée, impute les valeurs manquantes, applique les statistiques 
#' appropriées et génère un rapport Word complet incluant des interprétations en français.
#'
#' @param data data.frame ou chemin vers un fichier Excel/CSV.
#' @param output Chemin du fichier Word de sortie (défaut: "magic_report.docx").
#' @param title Titre du rapport.
#' @param outcome Nom de la variable d'intérêt. Si NULL, la fonction tente de la deviner.
#' @param open_doc Ouvrir le document après génération (défaut: TRUE).
#'
#' @return Le chemin du fichier Word généré (invisible).
#' @export
report_magic <- function(data, output = "magic_report.docx", title = "Rapport Magique Analytix", outcome = NULL, open_doc = TRUE) {
  
  if (is.character(data)) {
    message("Importation des données depuis ", data)
    if (grepl("\\.xlsx?$", data)) {
      data <- prep_import(data)
    } else if (grepl("\\.csv$", data)) {
      data <- read.csv(data, stringsAsFactors = FALSE)
      data <- clean_names(data)
    } else {
      stop("Format de fichier non supporté. Utilisez Excel ou CSV.")
    }
  }
  
  if (!is.data.frame(data)) stop("`data` doit être un data.frame ou un chemin de fichier.")
  if (nrow(data) == 0) stop("Le jeu de données est vide.")

  message("--- Lancement du Magic Report ---")
  
  # 1. Deviner l'outcome si non fourni
  if (is.null(outcome)) {
    message("Analyse des variables pour deviner l'outcome principal...")
    # Stratégie simple : chercher une variable binaire avec peu de NA
    # Ou la dernière colonne si aucune n'est évidente
    noms <- names(data)
    potentiels <- sapply(data, function(x) {
      if (is.logical(x)) return(TRUE)
      if (is.factor(x) || is.character(x) || is.numeric(x)) {
        if (length(unique(na.omit(x))) == 2) return(TRUE)
      }
      return(FALSE)
    })
    
    candidats <- noms[potentiels]
    if (length(candidats) > 0) {
      outcome <- candidats[length(candidats)] # Souvent la dernière variable
      message("Outcome deviné : ", outcome, " (Variable binaire)")
    } else {
      outcome <- noms[length(noms)]
      message("Outcome deviné : ", outcome, " (Dernière colonne)")
    }
  } else {
    if (!outcome %in% names(data)) stop("La variable outcome '", outcome, "' n'existe pas.")
  }

  # 2. Nettoyage basique (imputation simple)
  message("Nettoyage automatique (imputation des valeurs manquantes)...")
  for (var in names(data)) {
    if (any(is.na(data[[var]]))) {
       if (is.numeric(data[[var]])) {
          data[[var]] <- prep_impute_mean(data[[var]])
       } else {
          data[[var]] <- prep_impute_mode(data[[var]])
       }
    }
  }

  # 3. Génération via report_generate avec interprétations activées
  message("Génération du rapport Word via report_generate...")
  
  doc_path <- report_generate(data, 
                              output = output, 
                              title = title, 
                              subtitle = "Généré automatiquement par analytix::report_magic()",
                              outcome = outcome,
                              open_doc = open_doc,
                              verbose = FALSE)
                              
  message("Rapport généré avec succès : ", doc_path)
  
  invisible(doc_path)
}
