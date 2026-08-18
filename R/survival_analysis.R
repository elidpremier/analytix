#' @title Tableaux et courbes de survie Kaplan-Meier
#' @name survival_analysis
#' @description Fonctions pour réaliser une analyse de survie (méthode de Kaplan-Meier).
#' \code{km_table} retourne un tableau formaté des médianes de survie et du test du Log-Rank.
#' \code{km_plot} génère la courbe de survie.
#'
#' @param data data.frame.
#' @param time_var Nom de la variable de temps de suivi (numérique).
#' @param event_var Nom de la variable d'événement (binaire 0/1 ou deux modalités).
#' @param group_var Nom de la variable de stratification (optionnelle, défaut NULL).
#' @param color Couleur d'en-tête (défaut "#D3D3D3").
#' @param digits Nombre de décimales.
#'
#' @return
#' \code{km_table} retourne un \code{flextable}.
#' \code{km_plot} retourne un objet \code{ggplot} (via survminer).
#'
#' @examples
#' \dontrun{
#' df <- analytix_sample_data(n = 200)
#' km_table(df, time_var = "suivi_mois", event_var = "deces", group_var = "traitement")
#' km_plot(df, time_var = "suivi_mois", event_var = "deces", group_var = "traitement")
#' }
NULL

#' @rdname survival_analysis
#' @export
km_table <- function(data, time_var, event_var, group_var = NULL,
                     color = "#D3D3D3", digits = 1) {

  if (!requireNamespace("survival", quietly = TRUE))
    stop("Package 'survival' requis.")

  t_var <- data[[time_var]]
  e_var <- data[[event_var]]
  if (!is.numeric(t_var)) stop("time_var doit être numérique.")
  if (!is.numeric(e_var)) e_var <- as.numeric(as.factor(e_var)) - 1

  surv_obj <- survival::Surv(t_var, e_var)

  if (!is.null(group_var) && group_var %in% names(data)) {
    g_var <- data[[group_var]]
    km_fit <- survival::survfit(surv_obj ~ g_var)
    km_diff <- survival::survdiff(surv_obj ~ g_var)
    p_logrank <- 1 - stats::pchisq(km_diff$chisq, df = length(km_diff$n) - 1)

    km_sum <- summary(km_fit)$table
    med_df <- as.data.frame(km_sum[, c("records","events","median"), drop = FALSE])
    med_df <- data.frame(
      Groupe = gsub("^g_var=", "", rownames(med_df)),
      med_df,
      row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE
    )
    names(med_df) <- c("Groupe", "N", "Événements", "Médiane de survie")
    med_df$`Médiane de survie` <- round(med_df$`Médiane de survie`, digits)

    p_str <- if (p_logrank < 0.001) "< 0.001" else format(round(p_logrank, 3), nsmall = 3)

    ft <- flextable::flextable(med_df) |>
      theme_analytique(color = color) |>
      flextable::add_footer_lines(paste0("Test du Log-Rank : p = ", p_str)) |>
      flextable::set_caption(paste("Analyse de survie stratifiée par", .get_label(data, group_var, group_var)))
  } else {
    km_fit <- survival::survfit(surv_obj ~ 1)
    km_sum <- summary(km_fit)$table

    med_df <- data.frame(
      Indicateur = c("N", "Événements", "Médiane de survie", "Q1 (75%)", "Q3 (25%)"),
      Valeur = c(
        km_fit$n,
        sum(km_fit$n.event),
        round(km_sum["median"], digits),
        round(km_fit$time[which.min(abs(km_fit$surv - 0.75))], digits),
        round(km_fit$time[which.min(abs(km_fit$surv - 0.25))], digits)
      )
    )

    ft <- flextable::flextable(med_df) |>
      theme_analytique(color = color) |>
      flextable::set_caption("Résumé de la survie (cohorte globale)")
  }
  return(ft)
}

#' @rdname survival_analysis
#' @export
km_plot <- function(data, time_var, event_var, group_var = NULL) {
  if (!requireNamespace("survival", quietly = TRUE)) stop("Package 'survival' requis.")
  if (!requireNamespace("survminer", quietly = TRUE)) stop("Package 'survminer' requis.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' requis.")

  t_var <- data[[time_var]]
  e_var <- data[[event_var]]
  if (!is.numeric(e_var)) e_var <- as.numeric(as.factor(e_var)) - 1
  surv_obj <- survival::Surv(t_var, e_var)

  if (!is.null(group_var)) {
    g_var <- data[[group_var]]
    km_fit <- survival::survfit(surv_obj ~ g_var)
    p <- survminer::ggsurvplot(
      km_fit, data = data,
      pval = TRUE, pval.method = TRUE,
      conf.int = TRUE, risk.table = TRUE,
      ggtheme = ggplot2::theme_minimal(base_size = 12),
      palette = c("#0284c7","#dc2626","#059669","#d97706"),
      title = paste("Courbes de survie selon", .get_label(data, group_var, group_var)),
      xlab = paste("Temps (", .get_label(data, time_var, time_var), ")"),
      legend.title = .get_label(data, group_var, group_var),
      legend.labs = levels(as.factor(g_var))
    )
  } else {
    km_fit <- survival::survfit(surv_obj ~ 1)
    p <- survminer::ggsurvplot(
      km_fit, data = data,
      conf.int = TRUE, risk.table = TRUE,
      ggtheme = ggplot2::theme_minimal(base_size = 12),
      palette = "#0284c7",
      title = "Courbe de survie globale",
      xlab = paste("Temps (", .get_label(data, time_var, time_var), ")")
    )
  }
  return(p)
}
