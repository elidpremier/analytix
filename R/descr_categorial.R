#' @title Calcul de fréquences universel avec flextable
#' @description Calcule les fréquences et pourcentages d'une variable et génère un flextable professionnel
#' @param data Le dataframe contenant les données
#' @param var La variable à analyser (peut être character, factor, numeric, logical, etc.)
#' @param var_name Nom personnalisé pour la variable (optionnel)
#' @param subset Expression logique pour filtrer les données (ex: sexe == "M")
#' @param sort TRUE pour trier par fréquence décroissante, FALSE pour ordre naturel
#' @param digits Nombre de décimales pour les pourcentages (défaut: 1)
#' @param include_na TRUE pour inclure les NA dans le calcul, FALSE pour les exclure
#' @param na_label Étiquette pour les valeurs manquantes (défaut: "Manquant")
#' @param total TRUE pour inclure une ligne de total
#' @param caption Titre personnalisé du tableau
#' @param color Couleur de l'en-tête (défaut: "#D3D3D3", (gris))
#' @param compact TRUE pour un affichage compact (n et % sur la même ligne)
#'
#' @return Une liste contenant le tableau de données et le flextable
#'
#' @examples
#' descr_categorial(iris, Species)
#' descr_categorial(iris, Species, subset = Sepal.Length > 5)
#'
#' @export
descr_categorial <- function(data, var, var_name = NULL, subset = NULL, sort = TRUE, digits = 1,
                       include_na = FALSE, na_label = "Manquant", total = TRUE,
                       caption = NULL, color = "#D3D3D3", compact = FALSE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("Package 'flextable' requis")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' requis")

  # Gestion du filtrage
  subset_enq <- rlang::enquo(subset)
  if (!rlang::quo_is_null(subset_enq)) {
    data <- dplyr::filter(data, !!subset_enq)
  }

  var_enq <- rlang::enquo(var)
  var_name_auto <- rlang::as_name(var_enq)
  
  # Récupération du label si var_name est NULL
  if (is.null(var_name)) {
    # Tenter de récupérer l'attribut label
    attr_label <- attr(data[[var_name_auto]], "label")
    if (!is.null(attr_label)) {
      var_name <- attr_label
    } else {
      var_name <- var_name_auto
    }
  }

  if (!var_name_auto %in% names(data)) {
    stop("La variable '", var_name_auto, "' n'existe pas dans le dataframe.")
  }

  x <- data[[var_name_auto]]

  # Conversion en character pour uniformité
  if (is.factor(x)) {
    x <- as.character(x)
  } else if (is.logical(x)) {
    x <- as.character(x)
  } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    x <- as.character(x)
  } else {
    x <- as.character(x)
  }

  if (!include_na) {
    x <- x[!is.na(x)]
  } else {
    x[is.na(x)] <- na_label
  }

  if (length(x) == 0) {
    warning("Aucune donnée à analyser après filtrage ou exclusion des NA.")
    return(NULL)
  }

  df <- tibble::tibble(variable = x)

  # Calcul des fréquences
  freq_data <- df %>%
    dplyr::count(variable, name = "n", sort = FALSE) %>%
    dplyr::mutate(
      pourcentage = (n / sum(n)) * 100
    )

  if (sort) freq_data <- dplyr::arrange(freq_data, dplyr::desc(n))

  # Formater les pourcentages
  format_pct <- function(val, d = digits) {
    base::format(base::round(val, d), nsmall = d, decimal.mark = ",")
  }

  freq_data <- freq_data %>%
    dplyr::mutate(pourcentage_formate = format_pct(pourcentage))

  # Calcul du total
  if (total) {
    total_row <- tibble::tibble(
      variable = "Total",
      n = sum(freq_data$n),
      pourcentage = sum(freq_data$pourcentage),
      pourcentage_formate = format_pct(sum(freq_data$pourcentage))
    )
    freq_data <- dplyr::bind_rows(freq_data, total_row)
  }

  # Nom de la colonne des modalités
  col_label <- var_name

  # Préparation des données pour flextable
  ft_data <- if (compact) {
    freq_data %>%
      dplyr::mutate(
        `Effectif (%)` = dplyr::if_else(
          variable == "Total",
          base::as.character(n),
          base::paste0(n, " (", pourcentage_formate, "%)")
        )
      ) %>%
      dplyr::select(!!rlang::sym(col_label) := variable, `Effectif (%)`)
  } else {
    freq_data %>%
      dplyr::select(
        !!rlang::sym(col_label) := variable,
        Effectif = n,
        `Pourcentage (%)` = pourcentage_formate
      )
  }

  if (is.null(caption)) caption <- base::paste("Distribution de :", col_label)

  # Création du flextable
  ft <- flextable::flextable(ft_data) %>%
    flextable::set_caption(caption) %>%
    theme_analytique(color = color)

  # Mise en forme du Total
  if (total) {
    # Utiliser l'index de la ligne Total
    total_idx <- which(freq_data$variable == "Total")
    ft <- ft %>%
      flextable::bold(i = total_idx)
  }

  # Meta-données de retour
  n_total_final <- if (total) sum(freq_data$n[freq_data$variable != "Total"]) else sum(freq_data$n)

  structure(
    list(
      data = freq_data,
      flextable = ft,
      variable_name = var_name,
      n_total = n_total_final,
      raw_data = x
    ),
    class = "freq_table"
  )
}


