#' @title Statistiques descriptives par groupe
#' @description Tableau de statistiques par catégorie d'une variable de groupe.
#' Supporte les variables numériques (moyenne, médiane, écart-type, tests T-test/ANOVA/Mann-Whitney/Kruskal-Wallis)
#' et catégorielles (pourcentages, test Chi²).
#' 
#' @param data data.frame
#' @param var variable à décrire (numérique ou catégorielle)
#' @param by variable de groupement (catégorielle)
#' @param var_name libellé de la variable
#' @param by_name libellé de la variable de groupe
#' @param digits nombre de décimales
#' @param color couleur de l'en-tête
#' @param test_stat logique. Afficher le test statistique ? (défaut: TRUE)
#' @param test_type type de test pour les variables numériques: "auto", "parametric" (t-test/ANOVA), ou "nonparametric" (Mann-Whitney/Kruskal-Wallis).
#' 
#' @return un objet flextable
#' @examples
#' # Cas numérique
#' descr_by_group(mtcars, mpg, cyl, var_name = "Consommation", by_name = "Cylindres")
#' 
#' # Cas catégoriel
#' descr_by_group(mtcars, am, vs, var_name = "Transmission", by_name = "Moteur")
#'
#' @export
descr_by_group <- function(data, var, by, var_name = NULL, by_name = NULL,
                           digits = 1, color = "#D3D3D3", test_stat = TRUE,
                           test_type = c("auto", "parametric", "nonparametric")) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr requis")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("flextable requis")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("tidyr requis")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang requis")
  if (!requireNamespace("stats", quietly = TRUE)) stop("stats requis")

  test_type <- match.arg(test_type)
  var_enq <- rlang::enquo(var)
  by_enq <- rlang::enquo(by)
  var_nm <- rlang::as_name(var_enq)
  by_nm <- rlang::as_name(by_enq)

  if (is.null(var_name)) {
    attr_l <- attr(data[[var_nm]], "label")
    var_name <- if(!is.null(attr_l)) attr_l else var_nm
  }
  if (is.null(by_name)) {
    attr_l <- attr(data[[by_nm]], "label")
    by_name <- if(!is.null(attr_l)) attr_l else by_nm
  }

  x <- data[[var_nm]]
  g <- data[[by_nm]]

  if (is.numeric(x)) {
    # --- Cas numérique ---
    stats_df <- data %>%
      dplyr::group_by(!!by_enq) %>%
      dplyr::summarise(
        n = base::sum(!base::is.na(!!var_enq)),
        Moyenne = base::mean(!!var_enq, na.rm = TRUE),
        Mediane = stats::median(!!var_enq, na.rm = TRUE),
        Ecart_type = stats::sd(!!var_enq, na.rm = TRUE),
        Min_Max = paste0(min(!!var_enq, na.rm = TRUE), " - ", max(!!var_enq, na.rm = TRUE)),
        .groups = "drop"
      )

    # Pivot pour présentation
    final_df <- stats_df %>%
      dplyr::mutate(dplyr::across(-!!by_enq, as.character)) %>%
      tidyr::pivot_longer(cols = -!!by_enq, names_to = "Statistique", values_to = "Valeur")
    
    # Formatage sélectif pour éviter les warnings de conversion sur Min_Max et n
    idx_num <- !final_df$Statistique %in% c("n", "Min_Max")
    final_df$Valeur[idx_num] <- base::format(
      base::round(as.numeric(final_df$Valeur[idx_num]), digits),
      nsmall = digits, decimal.mark = ","
    )

    final_df <- final_df %>%
      tidyr::pivot_wider(names_from = !!by_enq, values_from = Valeur)

    caption <- paste("Distribution de", var_name, "par", by_name)
    ft <- flextable::flextable(final_df) %>%
      flextable::set_caption(caption) %>%
      theme_analytique(color = color)
    
    if (test_stat) {
      clean_g <- factor(g[!is.na(x) & !is.na(g)])
      clean_x <- x[!is.na(x) & !is.na(g)]
      n_grps <- length(unique(clean_g))
      
      if (n_grps >= 2) {
        test_label <- ""
        p_val <- NA
        
        if (n_grps == 2) {
          if (test_type %in% c("auto", "parametric")) {
            tres <- suppressWarnings(stats::t.test(clean_x ~ clean_g))
            test_label <- "Test t de Student"
            p_val <- tres$p.value
          } else {
            tres <- suppressWarnings(stats::wilcox.test(clean_x ~ clean_g))
            test_label <- "Test de Mann-Whitney (Wilcoxon)"
            p_val <- tres$p.value
          }
        } else { # > 2 groupes
          if (test_type %in% c("auto", "parametric")) {
            tres <- suppressWarnings(stats::oneway.test(clean_x ~ clean_g, var.equal = FALSE))
            test_label <- "ANOVA (Welch)"
            p_val <- tres$p.value
          } else {
            tres <- suppressWarnings(stats::kruskal.test(clean_x ~ clean_g))
            test_label <- "Test de Kruskal-Wallis"
            p_val <- tres$p.value
          }
        }
        
        p_str <- if (p_val < 0.001) "< 0,001" else format(round(p_val, 3), decimal.mark = ",")
        ft <- flextable::add_footer_lines(ft, paste0(test_label, " : p = ", p_str))
      }
    }
    
    return(ft)

  } else {
    # --- Cas catégoriel ---
    tab <- table(x, g, useNA = "no")
    pct_tab <- prop.table(tab, margin = 2) * 100
    
    res_list <- list()
    for (mod in rownames(tab)) {
      row_data <- data.frame(Statistique = mod, stringsAsFactors = FALSE)
      for (col_nm in colnames(tab)) {
        row_data[[col_nm]] <- sprintf("%s (%s%%)", tab[mod, col_nm], format(round(pct_tab[mod, col_nm], digits), nsmall = digits, decimal.mark = ","))
      }
      res_list[[mod]] <- row_data
    }
    
    final_df <- dplyr::bind_rows(res_list)
    
    # Ajout du test de Khi² si demandé
    footer_msg <- ""
    if (test_stat) {
      test_res <- suppressWarnings(stats::chisq.test(tab))
      p_val <- test_res$p.value
      p_str <- if(p_val < 0.001) "< 0,001" else format(round(p_val, 3), decimal.mark = ",")
      footer_msg <- paste0("Test de Khi² : p = ", p_str)
    }

    ft <- flextable::flextable(final_df) %>%
      flextable::set_caption(paste(var_name, "par", by_name)) %>%
      theme_analytique(color = color)
    
    if (footer_msg != "") ft <- flextable::add_footer_lines(ft, footer_msg)
    
    return(ft)
  }
}
