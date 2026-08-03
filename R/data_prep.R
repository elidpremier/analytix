#' @title Détection des valeurs aberrantes (outliers)
#' @description Identifie les valeurs aberrantes d'une variable numérique via
#' la méthode de l'IQR (Tukey) et/ou du Z-score, et retourne un rapport formaté.
#'
#' @param data data.frame.
#' @param var Variable numérique à analyser.
#' @param var_name Libellé de la variable.
#' @param method Méthode: "iqr" (Tukey, défaut), "zscore", ou "both".
#' @param iqr_factor Multiplicateur de l'IQR pour Tukey (défaut: 1.5).
#' @param z_threshold Seuil du Z-score pour les valeurs aberrantes (défaut: 3).
#' @param color Couleur d'en-tête du flextable (défaut: "#D3D3D3").
#'
#' @return Une liste contenant `summary` (flextable) et `outlier_rows` (indices).
#'
#' @examples
#' df <- data.frame(age = c(23, 31, 45, 52, 18, 120, 29, 34, -5, 41))
#' detect_outliers(df, age, var_name = "Age")
#'
#' @export
detect_outliers <- function(data, var, var_name = NULL,
                             method = c("iqr", "zscore", "both"),
                             iqr_factor = 1.5, z_threshold = 3,
                             color = "#D3D3D3") {
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  method  <- match.arg(method)
  var_enq <- rlang::enquo(var)
  var_nm  <- rlang::as_name(var_enq)
  if (is.null(var_name)) var_name <- .get_label(data, var_nm, var_nm)

  vec   <- as.numeric(data[[var_nm]])
  n_all <- length(vec)
  valid <- !is.na(vec)
  x     <- vec[valid]
  idx_valid <- which(valid)

  q1  <- stats::quantile(x, 0.25)
  q3  <- stats::quantile(x, 0.75)
  iqr <- q3 - q1
  lower_iqr <- q1 - iqr_factor * iqr
  upper_iqr <- q3 + iqr_factor * iqr

  z_scores <- (x - mean(x)) / stats::sd(x)

  flag_iqr    <- x < lower_iqr | x > upper_iqr
  flag_z      <- abs(z_scores) > z_threshold
  flag_either <- flag_iqr | flag_z

  flag <- switch(method,
    iqr    = flag_iqr,
    zscore = flag_z,
    both   = flag_either
  )

  n_out <- sum(flag)
  outlier_global_idx <- idx_valid[flag]

  res_df <- data.frame(
    Indicateur = c(
      "Effectif total", "Valeurs manquantes", "Valeurs valides",
      "Méthode utilisée",
      paste0("Borne inférieure (IQR ×", iqr_factor, ")"),
      paste0("Borne supérieure (IQR ×", iqr_factor, ")"),
      paste0("Seuil Z-score (±", z_threshold, ")"),
      "Nombre de valeurs aberrantes détectées",
      "% valeurs aberrantes"
    ),
    Valeur = c(
      n_all, sum(!valid), sum(valid),
      method,
      format(round(lower_iqr, 2), decimal.mark = ","),
      format(round(upper_iqr, 2), decimal.mark = ","),
      paste0("±", z_threshold),
      n_out,
      paste0(format(round(100 * n_out / sum(valid), 1), decimal.mark = ","), "%")
    ),
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(res_df) %>%
    theme_analytique(color = color) %>%
    flextable::set_caption(paste("Rapport de valeurs aberrantes :", var_name)) %>%
    flextable::bold(i = 8, part = "body")

  list(summary = ft, outlier_rows = outlier_global_idx,
       outlier_values = x[flag], n_outliers = n_out)
}

#' @title Attacher des libellés à plusieurs variables d'un data.frame
#' @description Applique des attributs `label` (au sens `haven`/`labelled`)
#' à une liste de variables en une seule opération.
#'
#' @param data data.frame.
#' @param labels Vecteur nommé des libellés: `c(var1 = "Libellé 1", var2 = "Libellé 2")`.
#'
#' @return Le data.frame avec les attributs `label` attachés à chaque variable.
#'
#' @examples
#' df <- data.frame(age = c(25, 30, 45), sexe = c("H", "F", "H"))
#' df <- label_vars(df, c(age = "Âge en années", sexe = "Sexe du participant"))
#' attr(df$age, "label")
#'
#' @export
label_vars <- function(data, labels) {
  if (!is.data.frame(data)) stop("`data` doit être un data.frame.")
  if (!is.character(labels) || is.null(names(labels))) {
    stop("`labels` doit être un vecteur de caractères nommé.")
  }
  for (nm in names(labels)) {
    if (nm %in% names(data)) {
      attr(data[[nm]], "label") <- as.character(labels[[nm]])
    } else {
      warning(paste0("La variable '", nm, "' n'existe pas dans le data.frame."))
    }
  }
  data
}

#' @title Import et nettoyage automatique d'un fichier Excel ou CSV
#' @description Importe un fichier Excel (.xlsx / .xls) ou CSV et applique
#' automatiquement le nettoyage des noms de colonnes (`clean_names`),
#' l'encodage UTF-8 et l'uniformisation des types de base.
#'
#' @param path Chemin vers le fichier à importer.
#' @param sheet Nom ou numéro de l'onglet Excel (ignoré pour les CSV).
#' @param skip Nombre de lignes à sauter en début de fichier (défaut: 0).
#' @param sep Séparateur pour les CSV (défaut: ";").
#' @param clean Logique. Appliquer `clean_names()` sur les colonnes (défaut: TRUE).
#' @param ... Arguments supplémentaires passés à `readxl::read_excel()` ou `read.csv()`.
#'
#' @return Un data.frame nettoyé.
#'
#' @examples
#' \dontrun{
#'   df <- import_clean("data/enquete.xlsx", sheet = 1)
#'   df <- import_clean("data/donnees.csv", sep = ",")
#' }
#'
#' @export
import_clean <- function(path, sheet = 1, skip = 0, sep = ";", clean = TRUE, ...) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl requis pour les fichiers Excel.")
    df <- readxl::read_excel(path, sheet = sheet, skip = skip, ...)
  } else if (ext == "csv") {
    df <- utils::read.csv(path, sep = sep, encoding = "UTF-8",
                          stringsAsFactors = FALSE, skip = skip, ...)
  } else {
    stop(paste0("Format non supporté : '.", ext, "'. Utilisez .xlsx, .xls ou .csv."))
  }

  df <- as.data.frame(df)

  if (clean) {
    df <- clean_names(df)
  }

  df
}
