#' @title Description des questions à choix multiples (réponses multiples)
#' @description Génère un tableau descriptif et un objet `analytix_table` pour des séries de variables
#' représentant une question à choix multiples ("Cochez tout ce qui s'applique").
#'
#' @param data data.frame contenant les colonnes de la question.
#' @param cols Vecteur de noms de colonnes ou sélection de colonnes (caractères ou symboles non quotés).
#' @param positive Valeur(s) considérée(s) comme réponse positive / cochée (défaut: 1, TRUE, "Oui", "Yes", "vrai", "coché").
#' @param var_labels Vecteur nommé de libellés pour chaque option/colonne. Si NULL, les noms de colonnes sont utilisés.
#' @param title Titre du tableau (légende du flextable).
#' @param pct_type Type de pourcentage: "respondents" (par rapport au nombre total de personnes N)
#' ou "choices" (par rapport au nombre total de choix cochés).
#' @param digits Nombre de décimales pour l'affichage du pourcentage (défaut: 1).
#' @param color Couleur d'en-tête pour le thème analytique (défaut: "transparent").
#'
#' @return Un objet \code{analytix_table} contenant \code{$data} et \code{$flextable}.
#'
#' @examples
#' df <- data.frame(
#'   q1_a = c("Oui", "Oui", "Non", "Non", "Oui"),
#'   q1_b = c("Oui", "Non", "Oui", "Non", "Non"),
#'   q1_c = c("Non", "Non", "Oui", "Oui", "Oui")
#' )
#' desc_multi_choice(df, cols = c(q1_a, q1_b, q1_c), positive = "Oui")
#'
#' @export
desc_multi_choice <- function(data, cols,
                               positive = c(1, TRUE, "Oui", "Yes", "true", "vrai", "coché"),
                               var_labels = NULL,
                               title = "Question à choix multiples",
                               pct_type = c("respondents", "choices"),
                               digits = 1,
                               color = "transparent") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")

  pct_type <- match.arg(pct_type)

  # Extraction souple de cols (symboles ou caractères)
  cols_enq <- rlang::enquo(cols)
  if (is.character(cols)) {
    col_names <- cols
  } else {
    col_names <- tryCatch({
      names(dplyr::select(data, !!cols_enq))
    }, error = function(e) {
      if (rlang::quo_is_call(cols_enq)) {
        vapply(rlang::quo_get_expr(cols_enq)[-1], rlang::as_name, character(1))
      } else {
        rlang::as_name(cols_enq)
      }
    })
  }

  col_names <- intersect(col_names, names(data))
  if (length(col_names) == 0) {
    stop("Aucune colonne valide trouvée dans `data`.")
  }

  n_total_respondents <- nrow(data)

  pos_clean <- tolower(trimws(as.character(positive)))

  # Conversion / Comptage des réponses positives
  effs <- sapply(col_names, function(cn) {
    vec <- data[[cn]]
    vec_clean <- tolower(trimws(as.character(vec)))
    sum(vec_clean %in% pos_clean, na.rm = TRUE)
  })

  total_choices <- sum(effs)
  denom <- if (pct_type == "respondents") n_total_respondents else total_choices

  pcts <- if (denom > 0) (effs / denom) * 100 else rep(0, length(effs))

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
  rownames(res_df) <- NULL

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

  as_analytix_table(data = res_df, flextable = ft)
}
