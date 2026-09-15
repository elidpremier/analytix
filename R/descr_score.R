#' @title Calcul et description d'un score d'items / indice de complétude
#' @description Calcule le score individuel (nombre d'items validés ou prescrits parmi une liste de colonnes),
#' génère les statistiques descriptives globales du score (moyenne, médiane, min, max) et optionnellement
#' la distribution par tranches de score si des bornes (\code{breaks}) et libellés (\code{labels}) sont fournis.
#'
#' @param data data.frame contenant les données.
#' @param cols Sélection des colonnes représentant les items du score (symboles ou vecteurs de caractères).
#' @param positive Valeur(s) considérée(s) comme item validé/présent (défaut: 1, TRUE, "Oui", "Yes", "vrai", "coché").
#'   Si NULL, toute valeur non-NA et différente de 0/FALSE/"Non" est considérée positive.
#' @param score_name Nom personnalisé pour la variable de score (défaut: "Score").
#' @param breaks (Optionnel) Vecteur numérique définissant les limites des tranches de score pour la catégorisation.
#' @param labels (Optionnel) Vecteur de caractères pour nommer les catégories créées par \code{breaks}.
#' @param digits Nombre de décimales pour l'affichage (défaut: 1).
#' @param caption Titre du tableau (légende du flextable).
#' @param color Couleur d'en-tête pour le thème analytique (défaut: "transparent").
#'
#' @return Un objet \code{analytix_table} contenant \code{$data} et \code{$flextable}.
#'
#' @examples
#' df <- data.frame(
#'   exam1 = c(1, 1, 0, 1, 0),
#'   exam2 = c(1, 0, 0, 1, 1),
#'   exam3 = c(1, 1, 1, 1, 0),
#'   exam4 = c(0, 1, 0, 1, 1)
#' )
#' desc_score(df, cols = c(exam1, exam2, exam3, exam4),
#'            breaks = c(-Inf, 1, 3, Inf),
#'            labels = c("Faible (0-1)", "Moyen (2-3)", "Élevé (4)"),
#'            caption = "Complétude du bilan d'examens")
#'
#' @export
desc_score <- function(data, cols,
                       positive = c(1, TRUE, "Oui", "Yes", "true", "vrai", "coché"),
                       score_name = "Score",
                       breaks = NULL,
                       labels = NULL,
                       digits = 1,
                       caption = NULL,
                       color = "transparent") {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")

  cols_enq <- rlang::enquo(cols)
  col_names <- tryCatch({
    names(dplyr::select(data, !!cols_enq))
  }, error = function(e) {
    if (is.character(cols)) cols else names(dplyr::select(data, {{ cols }}))
  })

  col_names <- intersect(col_names, names(data))
  if (length(col_names) == 0) {
    stop("Aucune colonne valide trouvée dans `data` pour le calcul du score.")
  }

  pos_clean <- tolower(trimws(as.character(positive)))

  # Calcul du score par ligne
  score_matrix <- sapply(col_names, function(cn) {
    vec <- data[[cn]]
    vec_clean <- tolower(trimws(as.character(vec)))
    as.numeric(vec_clean %in% pos_clean)
  })

  score_vec <- rowSums(score_matrix, na.rm = TRUE)
  max_possible <- length(col_names)
  n_total <- length(score_vec)

  # Statistiques descriptives continues
  mean_val <- mean(score_vec, na.rm = TRUE)
  sd_val   <- stats::sd(score_vec, na.rm = TRUE)
  med_val  <- stats::median(score_vec, na.rm = TRUE)
  q1_val   <- stats::quantile(score_vec, 0.25, na.rm = TRUE)
  q3_val   <- stats::quantile(score_vec, 0.75, na.rm = TRUE)
  min_val  <- min(score_vec, na.rm = TRUE)
  max_val  <- max(score_vec, na.rm = TRUE)

  summary_rows <- list(
    data.frame(
      Paramètre = "Nombre d'items analysés",
      Valeur = as.character(max_possible),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Paramètre = "Effectif total (N)",
      Valeur = as.character(n_total),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Paramètre = "Moyenne ± Écart-type",
      Valeur = sprintf("%.*f ± %.*f", digits, mean_val, digits, sd_val),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Paramètre = "Médiane [Q1 - Q3]",
      Valeur = sprintf("%.*f [%.*f - %.*f]", digits, med_val, digits, q1_val, digits, q3_val),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Paramètre = "Min - Max",
      Valeur = sprintf("%g - %g", min_val, max_val),
      stringsAsFactors = FALSE
    )
  )

  summary_df <- dplyr::bind_rows(summary_rows)

  # Catégorisation si breaks fournis
  cat_df <- NULL
  if (!is.null(breaks)) {
    cat_factor <- cut(score_vec, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE)
    counts <- table(cat_factor, useNA = "no")
    pcts <- (counts / n_total) * 100

    cat_df <- data.frame(
      Paramètre = paste0("Tranche : ", names(counts)),
      Valeur = sprintf("%d (%.*f %%)", as.numeric(counts), digits, pcts),
      stringsAsFactors = FALSE
    )
    summary_df <- dplyr::bind_rows(summary_df, cat_df)
  }

  if (is.null(caption)) {
    caption <- paste0("Description du score : ", score_name, " (sur ", max_possible, " items)")
  }

  ft <- flextable::flextable(summary_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(caption) %>%
    flextable::set_header_labels(Paramètre = "Indicateur de score", Valeur = "Résultat")

  data_res <- list(
    scores = score_vec,
    summary = summary_df,
    max_score = max_possible
  )

  as_analytix_table(data = data_res, flextable = ft)
}
