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
#'
#' @return objet flextable, avec classe "analytix_table" (compatible avec export_to_word)
#'
#' @examples
#' cross_test(mtcars, am, cyl, var1_name = "Boîte automatique", var2_name = "Cylindres")
#' cross_test(mtcars, am, cyl, pct = "col", test = "fisher")
#'
#' @export
cross_test <- function(data, var1, var2,
                       var1_name = NULL, var2_name = NULL,
                       pct = c("row", "col", "total"),
                       test = c("auto", "chisq", "fisher"),
                       digits = 1, show_test = TRUE,
                       color = "#D3D3D3") {

  # --- Dépendances (déjà dans Imports selon ton README) ---
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")

  pct <- match.arg(pct)
  test <- match.arg(test)

  # --- Extraction des variables ---
  var1_enq <- rlang::enquo(var1)
  var2_enq <- rlang::enquo(var2)

  v1_name <- rlang::quo_name(var1_enq)
  v2_name <- rlang::quo_name(var2_enq)

  x <- dplyr::pull(data, !!var1_enq)
  y <- dplyr::pull(data, !!var2_enq)

  if (is.null(var1_name)) var1_name <- v1_name
  if (is.null(var2_name)) var2_name <- v2_name

  # --- Vérification implicite : catégorielles ---
  if (is.numeric(x) && length(unique(x)) > 10) {
    warning("var1 semble numérique avec >10 modalités – résultat potentiellement illisible.")
  }
  if (is.numeric(y) && length(unique(y)) > 10) {
    warning("var2 semble numérique avec >10 modalités – résultat potentiellement illisible.")
  }

  # --- Tableau de contingence ---
  tab <- table(x, y, useNA = "ifany", dnn = c("Ligne", "Colonne"))

  if (sum(tab) == 0) stop("Aucune observation valide.")

  # --- Calcul du pourcentage choisi ---
  pct_mat <- switch(pct,
                    "row"    = prop.table(tab, margin = 1) * 100,
                    "col"    = prop.table(tab, margin = 2) * 100,
                    "total"  = prop.table(tab) * 100
  )

  # --- Préparer les données ---
  df <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(df) <- c("Ligne", "Colonne", "n")

  df$pct <- mapply(function(i, j) {
    if (i %in% rownames(pct_mat) && j %in% colnames(pct_mat)) {
      pct_mat[i, j]
    } else 0
  }, df$Ligne, df$Colonne)

  # --- Formatage de la cellule : n (p%) ---
  df$cellule <- sprintf(
    "%s (%s%%)",
    df$n,
    format(round(df$pct, digits), nsmall = digits, decimal.mark = ",")
  )

  # --- Pivot ---
  cross_wide <- df %>%
    dplyr::select(Ligne, Colonne, cellule) %>%
    tidyr::pivot_wider(
      names_from = Colonne,
      values_from = cellule,
      values_fill = "0 (0%)"
    )

  # --- Test d'association ---
  test_res <- test_association_internal(x, y, test)

  # --- Légende ---
  caption <- paste("Croisement :", var1_name, "×", var2_name)

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

  # --- Classe pour compatibilité avec export_to_word ---
  class(ft) <- c("analytix_table", class(ft))

  return(ft)
}


# --- Fonction interne : logique de test (non exportée) ---
test_association_internal <- function(x, y, test = "auto") {
  tab <- table(x, y)
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
