#' @title Description des questions à choix multiples (réponses multiples)
#' @description Génère un tableau descriptif et un objet `flextable` pour des séries de variables binaires
#' représentant une question à choix multiples ("Cochez tout ce qui s'applique").
#' 
#' @param data data.frame contenant les colonnes de la question.
#' @param cols Vecteur de noms de colonnes ou sélection de colonnes (caractères ou symboles).
#' @param var_labels Vecteur nommé de libellés pour chaque option/colonne. Si NULL, les noms de colonnes sont utilisés.
#' @param title Titre du tableau (légende du flextable).
#' @param pct_type Type de pourcentage: "respondents" (par rapport au nombre total de personnes N)
#' ou "choices" (par rapport au nombre total de choix cochés).
#' @param digits Nombre de décimales pour l'affichage du pourcentage (défaut: 1).
#' @param color Couleur d'en-tête pour le thème analytique (défaut: "#D3D3D3").
#' 
#' @return Un objet `flextable` formaté.
#' 
#' @examples
#' df <- data.frame(
#'   q1_a = c(1, 1, 0, 0, 1),
#'   q1_b = c(1, 0, 1, 0, 0),
#'   q1_c = c(0, 0, 1, 1, 1)
#' )
#' descr_multi_choice(df, cols = c("q1_a", "q1_b", "q1_c"),
#'                    var_labels = c(q1_a = "Option A", q1_b = "Option B", q1_c = "Option C"))
#' 
#' @export
descr_multi_choice <- function(data, cols, var_labels = NULL,
                               title = "Question à choix multiples",
                               pct_type = c("respondents", "choices"),
                               digits = 1,
                               color = "#D3D3D3") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  
  pct_type <- match.arg(pct_type)
  
  if (is.character(cols)) {
    col_names <- cols
  } else {
    col_names <- names(dplyr::select(data, {{ cols }}))
  }
  
  col_names <- intersect(col_names, names(data))
  if (length(col_names) == 0) {
    stop("Aucune colonne valide trouvée dans `data`.")
  }
  
  n_total_respondents <- nrow(data)
  
  # Conversion des valeurs en 1 (coché) ou 0 (non coché)
  effs <- sapply(col_names, function(cn) {
    vec <- data[[cn]]
    if (is.logical(vec)) {
      sum(vec, na.rm = TRUE)
    } else if (is.numeric(vec)) {
      sum(vec == 1, na.rm = TRUE)
    } else {
      vec_str <- clean_text(vec)
      sum(tolower(vec_str) %in% c("oui", "yes", "true", "1", "coché", "vrai"), na.rm = TRUE)
    }
  })
  
  total_choices <- sum(effs)
  denom <- if (pct_type == "respondents") n_total_respondents else total_choices
  
  pcts <- (effs / denom) * 100
  
  labels_vec <- sapply(col_names, function(cn) {
    if (!is.null(var_labels) && cn %in% names(var_labels)) {
      var_labels[[cn]]
    } else {
      attr_l <- attr(data[[cn]], "label")
      if (!is.null(attr_l)) attr_l else cn
    }
  })
  
  pct_col_name <- if (pct_type == "respondents") "Pourcentage (% répondants)" else "Pourcentage (% choix)"
  
  res_df <- data.frame(
    Option = unname(labels_vec),
    Effectif = as.numeric(effs),
    Pourcentage = unname(pcts),
    stringsAsFactors = FALSE
  )
  
  # Trier par effectif décroissant
  res_df <- res_df[order(-res_df$Effectif), ]
  
  res_df$Pourcentage_str <- format(round(res_df$Pourcentage, digits), nsmall = digits, decimal.mark = ",")
  
  display_df <- data.frame(
    Option = res_df$Option,
    Effectif = res_df$Effectif,
    Pct = paste0(res_df$Pourcentage_str, " %"),
    stringsAsFactors = FALSE
  )
  names(display_df)[3] <- pct_col_name
  
  ft <- flextable::flextable(display_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(paste0(title, " (N = ", n_total_respondents, ")"))
  
  ft
}
