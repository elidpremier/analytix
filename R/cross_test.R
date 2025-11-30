#' @title Tableau croisé avec test d'association
#' @description Génère un tableau croisé lisible de deux variables catégorielles
#' avec effectifs, un type de pourcentage au choix, et un test d'association
#' (khi² ou Fisher) intégré. Format francophone, prêt pour rapport.
#'
#' @param data data.frame
#' @param var1 première variable (symbole non évalué)
#' @param var2 deuxième variable (symbole non évalué)
#' @param var1_name libellé de var1 (optionnel)
#' @param var2_name libellé de var2 (optionnel)
#' @param pct type de pourcentage à afficher : "row" (par défaut), "col", ou "total"
#' @param test méthode de test : "auto" (défaut), "chisq", ou "fisher"
#' @param digits nombre de décimales pour les pourcentages (défaut = 1)
#' @param show_test afficher le résultat du test dans la légende ? (défaut = TRUE)
#' @param color couleur de l'en-tête (défaut = "#D3D3D3")
#' @param include_na inclure les valeurs manquantes (NA) dans le tableau ? (défaut = FALSE)
#'
#' @return objet flextable, avec classe "analytix_table" (compatible avec export_to_word)
#'
#' @examples
#' cross_test(mtcars, am, cyl, var1_name = "Boîte automatique", var2_name = "Cylindres")
#'
#' @export
cross_test <- function(data, var1, var2,
                       var1_name = NULL, var2_name = NULL,
                       pct = c("row", "col", "total"),
                       test = c("auto", "chisq", "fisher"),
                       digits = 1, show_test = TRUE,
                       color = "#D3D3D3",
                       include_na = FALSE) {

  # --- Dépendances ---
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")

  pct <- match.arg(pct)
  test <- match.arg(test)

  # --- Extraction des variables avec rlang ---
  var1_enq <- rlang::enquo(var1)
  var2_enq <- rlang::enquo(var2)

  v1_name <- rlang::quo_name(var1_enq)
  v2_name <- rlang::quo_name(var2_enq)

  x <- dplyr::pull(data, !!var1_enq)
  y <- dplyr::pull(data, !!var2_enq)

  if (is.null(var1_name)) var1_name <- v1_name
  if (is.null(var2_name)) var2_name <- v2_name

  # --- Vérification implicite : trop de modalités ? ---
  if (is.numeric(x) && length(unique(x)) > 10) {
    warning("var1 semble numérique avec >10 modalités – résultat potentiellement illisible.")
  }
  if (is.numeric(y) && length(unique(y)) > 10) {
    warning("var2 semble numérique avec >10 modalités – résultat potentiellement illisible.")
  }

  # --- Définir useNA selon include_na ---
  use_na_arg <- if (include_na) "ifany" else "no"

  # --- Tableau de contingence ---
  tab <- table(x, y, useNA = use_na_arg, dnn = c("Ligne", "Colonne"))

  if (sum(tab) == 0) stop("Aucune observation valide.")

  # --- Convertir en matrice pour stabilité ---
  tab_mat <- as.matrix(tab)

  # --- Calcul du pourcentage choisi (basé sur le même jeu de données) ---
  pct_mat <- switch(pct,
                    "row"    = prop.table(tab_mat, margin = 1) * 100,
                    "col"    = prop.table(tab_mat, margin = 2) * 100,
                    "total"  = prop.table(tab_mat) * 100
  )

  # --- Création de data.frames compatibles ---
  df_n <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(df_n) <- c("Ligne", "Colonne", "n")

  df_pct <- as.data.frame(pct_mat, stringsAsFactors = FALSE)
  names(df_pct) <- c("Ligne", "Colonne", "pct")

  # Fusionner
  df <- merge(df_n, df_pct, by = c("Ligne", "Colonne"), all.x = TRUE)
  df$pct[is.na(df$pct)] <- 0

  # --- Formatage de la cellule : n (p%) ---
  df$cellule <- sprintf(
    "%s (%s%%)",
    df$n,
    format(round(df$pct, digits), nsmall = digits, decimal.mark = ",")
  )

  # --- Pivot en large ---
  cross_wide <- df %>%
    dplyr::select(Ligne, Colonne, cellule) %>%
    tidyr::pivot_wider(
      names_from = Colonne,
      values_from = cellule,
      values_fill = "0 (0%)"
    )

  # --- Test d'association (toujours sans NA) ---
  test_res <- test_association_internal(x, y, test)

  # --- Légende ---
  caption <- paste("Croisement :", var1_name, "×", var2_name)
  if (include_na) caption <- paste(caption, "(NA inclus)")

  if (show_test && !is.na(test_res$p.value)) {
    p_val <- test_res$p.value
    p_str <- if (p_val < 0.001) "<0,001" else format(round(p_val, 3), decimal.mark = ",")
    caption <- paste(caption, "—", test_res$test, "p =", p_str)
    if (!is.null(test_res$warning)) {
      caption <- paste(caption, "\n⚠️", test_res$warning)
    }
  }

  # --- Génération du flextable ---
  ft <- flextable::flextable(cross_wide) %>%
    flextable::set_caption(caption) %>%
    theme_analytique(color = color)

  class(ft) <- c("analytix_table", class(ft))

  return(ft)
}


# --- Fonction interne : logique de test (non exportée) ---
# Toujours exclure les NA pour le test
test_association_internal <- function(x, y, test = "auto") {
  tab <- table(x, y, useNA = "no")  # NA exclus pour le test
  if (sum(tab) == 0) return(list(p.value = NA, test = "vide", warning = NULL))

  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  expected <- chi$expected
  low5 <- sum(expected < 5)
  prop_low <- low5 / length(expected)
  is_2x2 <- all(dim(tab) == c(2, 2))

  p_val <- NA_real_
  test_name <- ""
  warning_msg <- NULL

  if (test == "chisq") {
    p_val <- chi$p.value
    test_name <- "Khi²"
    if (prop_low > 0.20) warning_msg <- "Conditions du khi² non respectées"

  } else if (test == "fisher") {
    ft <- if (is_2x2) fisher.test(tab) else fisher.test(tab, simulate.p.value = TRUE, B = 2000)
    p_val <- ft$p.value
    test_name <- if (is_2x2) "Fisher exact" else "Fisher simulé"

  } else { # auto
    if (is_2x2) {
      if (sum(tab) < 20 || any(tab < 5)) {
        ft <- fisher.test(tab)
        p_val <- ft$p.value
        test_name <- "Fisher exact"
      } else {
        p_val <- chi$p.value
        test_name <- "Khi²"
      }
    } else {
      if (prop_low > 0.20) {
        ft <- fisher.test(tab, simulate.p.value = TRUE, B = 2000)
        p_val <- ft$p.value
        test_name <- "Fisher simulé"
        warning_msg <- "Conditions du khi² non respectées"
      } else {
        p_val <- chi$p.value
        test_name <- "Khi²"
      }
    }
  }

  list(p.value = p_val, test = test_name, warning = warning_msg)
}
