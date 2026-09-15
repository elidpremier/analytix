#' @title Description des catégories groupées (ex: molécules par classe thérapeutique)
#' @description Génère un tableau récapitulatif (`flextable`) regroupant des sous-catégories/modalités
#' (ex: molécules) sous leurs catégories ou classes thématiques parentes (ex: classes thérapeutiques).
#' Supporte à la fois le format large (plusieurs colonnes) et le format long (deux colonnes groupe/élément).
#'
#' @param data data.frame contenant les données.
#' @param cols Vecteur de noms de colonnes représentant chaque groupe en format large (ex: `c("diuretique", "iec", "ara2")`).
#'   Chaque colonne contient les éléments/modalités (ex: "Amlodipine").
#' @param group_col Nom de la colonne contenant la catégorie parente en format long (ex: `"classe"`).
#' @param sub_col Nom de la colonne contenant le sous-élément en format long (ex: `"molecule"`).
#' @param var_labels Vecteur nommé de libellés pour remplacer les noms des colonnes/groupes.
#' @param pct_type Type de pourcentage: `"total"` (par rapport au N total du jeu de données, défaut)
#'   ou `"group"` (par rapport au total des éléments valides du groupe).
#' @param caption Titre/Légende du tableau.
#' @param digits Nombre de décimales pour l'affichage des pourcentages (défaut: 1).
#' @param color Couleur d'en-tête pour le thème analytique (défaut: `"transparent"`).
#'
#' @return Un objet `flextable` formaté avec lignes de groupes.
#'
#' @examples
#' df <- data.frame(
#'   diuretique = c("Furosemide", "Indapamide", NA, "Furosemide"),
#'   iec        = c("Enalapril", NA, "Ramipril", "Enalapril"),
#'   ara2       = c(NA, "Valsartan", "Candesartan", NA)
#' )
#' desc_grouped(df, cols = c("diuretique", "iec", "ara2"),
#'   var_labels = c(diuretique = "Diurétique", iec = "IEC", ara2 = "ARA II"),
#'   caption = "Molécules prescrites par classe")
#'
#' @export
desc_grouped <- function(data,
                                     cols = NULL,
                                     group_col = NULL,
                                     sub_col = NULL,
                                     var_labels = NULL,
                                     pct_type = c("total", "group"),
                                     caption = NULL,
                                     digits = 1,
                                     color = "transparent") {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("stringr requis")

  pct_type <- match.arg(pct_type)
  n_total <- nrow(data)

  if (!is.null(cols)) {
    if (is.character(cols)) {
      cols_nms <- cols
    } else {
      cols_enq <- rlang::enquos(cols)
      cols_nms <- vapply(cols_enq, rlang::quo_name, character(1))
    }

    cols_exist <- cols_nms[cols_nms %in% names(data)]
    if (length(cols_exist) == 0) {
      stop("Aucune des colonnes spécifiées dans `cols` n'existe dans `data`.")
    }

    df_long <- data %>%
      dplyr::select(dplyr::all_of(cols_exist)) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Classe", values_to = "Element") %>%
      dplyr::filter(!is.na(.data$Element), stringr::str_trim(as.character(.data$Element)) != "")

    # Application des labels
    if (!is.null(var_labels)) {
      df_long$Classe <- dplyr::recode(df_long$Classe, !!!var_labels)
    } else {
      df_long$Classe <- vapply(df_long$Classe, function(cn) .get_label(data, cn, cn), character(1))
    }

  } else if (!is.null(group_col) && !is.null(sub_col)) {
    grp_nm <- if (is.character(group_col)) group_col else rlang::as_name(rlang::enquo(group_col))
    sub_nm <- if (is.character(sub_col)) sub_col else rlang::as_name(rlang::enquo(sub_col))

    if (!grp_nm %in% names(data) || !sub_nm %in% names(data)) {
      stop("`group_col` et `sub_col` doivent correspondre à des colonnes existantes dans `data`.")
    }

    df_long <- data %>%
      dplyr::select(Classe = dplyr::all_of(grp_nm), Element = dplyr::all_of(sub_nm)) %>%
      dplyr::filter(!is.na(.data$Classe), !is.na(.data$Element), stringr::str_trim(as.character(.data$Element)) != "")

    if (!is.null(var_labels)) {
      df_long$Classe <- dplyr::recode(df_long$Classe, !!!var_labels)
    }
  } else {
    stop("Veuillez fournir soit `cols` (mode wide), soit `group_col` et `sub_col` (mode long).")
  }

  if (nrow(df_long) == 0) {
    stop("Aucune donnée disponible à résumer après filtrage des NA.")
  }

  # Calcul des effectifs et pourcentages
  if (pct_type == "total") {
    df_sum <- df_long %>%
      dplyr::count(.data$Classe, .data$Element, name = "Effectif") %>%
      dplyr::mutate(Pourcentage = round(100 * .data$Effectif / n_total, digits))
  } else {
    df_sum <- df_long %>%
      dplyr::group_by(.data$Classe) %>%
      dplyr::mutate(total_grp = dplyr::n()) %>%
      dplyr::group_by(.data$Classe, .data$Element, .data$total_grp) %>%
      dplyr::summarise(Effectif = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(Pourcentage = round(100 * .data$Effectif / .data$total_grp, digits)) %>%
      dplyr::select(-.data$total_grp)
  }

  # Tri : les éléments commençant par 'autre' à la fin, le reste par effectif décroissant
  df_sum <- df_sum %>%
    dplyr::mutate(is_autre = stringr::str_detect(tolower(as.character(.data$Element)), "^autre")) %>%
    dplyr::arrange(.data$Classe, .data$is_autre, dplyr::desc(.data$Effectif)) %>%
    dplyr::select(-.data$is_autre)

  df_sum$pct_fmt <- sprintf(paste0("%.", digits, "f%%"), df_sum$Pourcentage)

  final_df <- df_sum %>%
    dplyr::select(Classe = .data$Classe, Modalite = .data$Element, Effectif = .data$Effectif, `Pourcentage (%)` = .data$pct_fmt)

  grp_data <- flextable::as_grouped_data(final_df, groups = "Classe")
  ft <- flextable::as_flextable(grp_data) %>%
    flextable::bold(i = ~ !is.na(Classe), part = "body") %>%
    flextable::set_header_labels(
      Modalite = "Sous-catégorie / Élément",
      Effectif = "Effectif (n)",
      `Pourcentage (%)` = if (pct_type == "total") "Pourcentage (% de N)" else "Pourcentage (% de la classe)"
    )

  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption)
  }

  ft <- theme_analytique(ft, color = color)
  return(ft)
}
