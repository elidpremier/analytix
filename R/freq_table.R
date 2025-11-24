#' @title Calcul de fréquences universel avec flextable
#' @description Calcule les fréquences et pourcentages d'une variable et génère un flextable professionnel
#' @param data Le dataframe contenant les données
#' @param var La variable à analyser (peut être character, factor, numeric, logical, etc.)
#' @param var_name Nom personnalisé pour la variable (optionnel)
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
#' freq_table(iris, Species)
#'
#' @export
freq_table <- function(data, var, var_name = NULL, sort = TRUE, digits = 1,
                       include_na = TRUE, na_label = "Manquant", total = TRUE,
                       caption = NULL, color = "#D3D3D3", compact = FALSE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("Package 'flextable' requis")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' requis")

  var_name_auto <- deparse(substitute(var))
  if (is.null(var_name)) var_name <- var_name_auto

  if (!var_name_auto %in% names(data)) {
    stop("La variable '", var_name_auto, "' n'existe pas dans le dataframe.")
  }

  x <- data[[var_name_auto]]

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

  df <- tibble::tibble(variable = x)

  # Calcul des fréquences
  freq_data <- df %>%
    dplyr::count(variable, name = "n", sort = FALSE) %>%
    dplyr::mutate(
      pourcentage = (n / sum(n)) * 100,
      pourcentage_formate = base::format(base::round(pourcentage, digits),
                                         nsmall = digits,
                                         decimal.mark = ",")
    )

  if (sort) freq_data <- dplyr::arrange(freq_data, dplyr::desc(n))

  # Calcul du total REEL (pas forcé à 100)
  # Fonction interne pour formater intelligemment les pourcentages
  format_pourcentage_intelligent <- function(x, digits = 1) {
    if (base::isTRUE(base::all.equal(x, base::round(x)))) {
      base::as.character(base::as.integer(base::round(x)))
    } else {
      base::format(base::round(x, digits), nsmall = digits, decimal.mark = ",")
    }
  }

  # Calcul du total REEL
  if (total) {
    n_total <- base::sum(freq_data$n)
    pct_total <- base::sum(freq_data$pourcentage)  # Peut être < 100 si NA exclus

    # Formater le pourcentage du Total de façon intelligente
    pct_total_formate <- format_pourcentage_intelligent(pct_total, digits)

    # Formater les pourcentages des catégories (toujours avec décimales si demandé)
    freq_data <- freq_data %>%
      dplyr::mutate(pourcentage_formate = base::format(base::round(pourcentage, digits),
                                                       nsmall = digits,
                                                       decimal.mark = ","))

    total_row <- tibble::tibble(
      variable = "Total",
      n = n_total,
      pourcentage = pct_total,
      pourcentage_formate = pct_total_formate  # ← format intelligent ici
    )
    freq_data <- dplyr::bind_rows(freq_data, total_row)
  } else {
    # Si pas de total, formater les pourcentages normalement
    freq_data <- freq_data %>%
      dplyr::mutate(pourcentage_formate = base::format(base::round(pourcentage, digits),
                                                       nsmall = digits,
                                                       decimal.mark = ","))
  }

  # Nom de la colonne des modalités
  col_label <- var_name  # car var_name est soit personnalisé, soit = var_name_auto

  # Préparation des données pour flextable
  ft_data <- if (compact) {
    freq_data %>%
      dplyr::mutate(
        `Effectif (%)` = dplyr::if_else(
          variable == "Total",
          base::as.character(n),  # ← pas de % pour le Total
          base::paste0(n, " (", pourcentage_formate, "%)")  # ← avec % pour les catégories
        )
      ) %>%
      dplyr::select(!!rlang::sym(col_label) := variable, `Effectif (%)`)
  } else {
    freq_data %>%
      dplyr::select(
        !!rlang::sym(col_label) := variable,
        Effectif = n,
        `Pourcentage (%)` = pourcentage_formate  # ← pas de % dans les cellules, mais dans le nom de colonne
      )
    # → aucune mutation avec paste0("%") ici
  }

  if (is.null(caption)) caption <- base::paste("Distribution de :", col_label)

  # Création du flextable
  ft <- flextable::flextable(ft_data) %>%
    flextable::set_caption(caption) %>%
    flextable::theme_booktabs() %>%
    flextable::bg(bg = color, part = "header") %>%
    flextable::color(color = "black", part = "header") %>%
    flextable::set_table_properties(align = "left") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::align(j = 2:ncol(ft_data), align = "center", part = "all") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::width(width = 16)

  # Mise en forme du Total
  if (total) {
    safe_col <- base::paste0("`", col_label, "`")
    total_condition <- stats::as.formula(paste("~", safe_col, "== 'Total'"))
    ft <- ft %>%
      flextable::bold(i = total_condition)
  }

  ft <- flextable::autofit(ft)

  n_total_final <- if (total) base::sum(freq_data$n[freq_data$variable != "Total"]) else base::sum(freq_data$n)

  structure(
    list(
      data = freq_data,
      flextable = ft,
      variable_name = var_name,
      n_total = n_total_final
    ),
    class = "freq_table"
  )
}
