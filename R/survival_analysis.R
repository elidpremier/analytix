#' @title Tableaux et courbes de survie Kaplan-Meier
#' @name survival_analysis
#' @description Fonctions pour réaliser une analyse de survie (méthode de Kaplan-Meier).
#' \code{tbl_km} retourne un objet `analytix_table` contenant les médianes de survie
#' et le test du Log-Rank. \code{plot_km} génère la courbe de survie.
#'
#' @param data data.frame.
#' @param time_var Nom de la variable de temps de suivi (numérique).
#' @param event_var Nom de la variable d'événement (binaire 0/1 ou deux modalités).
#' @param group_var Nom de la variable de stratification (optionnelle, défaut NULL).
#' @param color Couleur d'en-tête (défaut "transparent").
#' @param digits Nombre de décimales.
#'
#' @return
#' \code{tbl_km} retourne un objet de classe `analytix_table` avec les champs
#' \code{data} et \code{flextable}. \code{plot_km} retourne un objet \code{ggplot}.
#'
#' @examples
#' \dontrun{
#' df <- analytix_sample_data(n = 200)
#' tbl_km(df, time_var = "suivi_mois", event_var = "deces", group_var = "traitement")
#' plot_km(df, time_var = "suivi_mois", event_var = "deces", group_var = "traitement")
#' }
NULL

#' @rdname survival_analysis
#' @export
tbl_km <- function(data, time_var, event_var, group_var = NULL,
                     color = "transparent", digits = 1) {

  if (!requireNamespace("survival", quietly = TRUE))
    stop("Package 'survival' requis.")

  t_var <- data[[time_var]]
  e_var <- data[[event_var]]
  if (!is.numeric(t_var)) stop("time_var doit être numérique.")
  if (any(is.na(t_var)) || any(t_var < 0)) {
    stop("time_var ne doit contenir que des temps numériques positifs ou nuls.",
         call. = FALSE)
  }
  if (!is.numeric(e_var)) e_var <- as.numeric(as.factor(e_var)) - 1
  if (any(is.na(e_var)) || !all(e_var %in% c(0, 1))) {
    stop("event_var doit être binaire et ne contenir aucune valeur manquante.",
         call. = FALSE)
  }

  surv_obj <- survival::Surv(t_var, e_var)

  if (!is.null(group_var) && group_var %in% names(data)) {
    g_var <- data[[group_var]]
    km_fit <- survival::survfit(surv_obj ~ g_var)
    km_diff <- survival::survdiff(surv_obj ~ g_var)
    p_logrank <- 1 - stats::pchisq(km_diff$chisq, df = length(km_diff$n) - 1)

    km_sum <- summary(km_fit)$table
    if (is.null(dim(km_sum))) km_sum <- matrix(km_sum, nrow = 1,
                                                dimnames = list("Cohorte", names(km_sum)))
    med_df <- as.data.frame(km_sum[, c("records","events","median"), drop = FALSE])
    med_df <- data.frame(
      Groupe = gsub("^g_var=", "", rownames(med_df)),
      med_df,
      row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE
    )
    names(med_df) <- c("Groupe", "N", "Événements", "Médiane de survie")
    med_df$`Médiane de survie` <- .fmt_decimal(med_df$`Médiane de survie`, digits)

    p_str <- .fmt_pvalue(p_logrank, 3)

    ft <- flextable::flextable(med_df) |>
      theme_analytique(color = color) |>
      flextable::add_footer_lines(paste0("Test du Log-Rank : p = ", p_str)) |>
      flextable::set_caption(paste("Analyse de survie stratifiée par", .get_label(data, group_var, group_var)))
  } else {
    km_fit <- survival::survfit(surv_obj ~ 1)
    km_sum <- summary(km_fit)$table

    q_time <- function(target) {
      if (!length(km_fit$time) || !any(km_fit$surv <= target)) return(NA_real_)
      km_fit$time[which(km_fit$surv <= target)[1]]
    }
    med_df <- data.frame(
      Indicateur = c("N", "Événements", "Médiane de survie", "Q1 (75%)", "Q3 (25%)"),
      Valeur = c(
        km_fit$n,
        sum(km_fit$n.event),
        .fmt_decimal(km_sum["median"], digits),
        .fmt_decimal(q_time(0.75), digits),
        .fmt_decimal(q_time(0.25), digits)
      )
    )

    ft <- flextable::flextable(med_df) |>
      theme_analytique(color = color) |>
      flextable::set_caption("Résumé de la survie (cohorte globale)")
  }
  as_analytix_table(data = med_df, flextable = ft)
}

#' @rdname survival_analysis
#' @export
plot_km <- function(data, time_var, event_var, group_var = NULL) {
  if (!requireNamespace("survival", quietly = TRUE)) stop("Package 'survival' requis.")
  if (!requireNamespace("survminer", quietly = TRUE)) stop("Package 'survminer' requis.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' requis.")

  t_var <- data[[time_var]]
  e_var <- data[[event_var]]
  if (!is.numeric(t_var) || any(is.na(t_var)) || any(t_var < 0)) {
    stop("time_var ne doit contenir que des temps numériques positifs ou nuls.",
         call. = FALSE)
  }
  if (!is.numeric(e_var)) e_var <- as.numeric(as.factor(e_var)) - 1
  if (any(is.na(e_var)) || !all(e_var %in% c(0, 1))) {
    stop("event_var doit être binaire et ne contenir aucune valeur manquante.",
         call. = FALSE)
  }
  km_data <- data.frame(.analytix_time = t_var, .analytix_event = e_var)

  if (!is.null(group_var)) {
    if (!group_var %in% names(data)) stop("group_var n'existe pas dans les données.",
                                          call. = FALSE)
    g_var <- data[[group_var]]
    km_data$.analytix_group <- g_var
    km_fit <- survival::survfit(
      survival::Surv(.analytix_time, .analytix_event) ~ .analytix_group,
      data = km_data
    )
    p <- survminer::ggsurvplot(
      km_fit, data = km_data,
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
    km_fit <- survival::survfit(
      survival::Surv(.analytix_time, .analytix_event) ~ 1,
      data = km_data
    )
    p <- survminer::ggsurvplot(
      km_fit, data = km_data,
      conf.int = TRUE, risk.table = TRUE,
      ggtheme = ggplot2::theme_minimal(base_size = 12),
      palette = "#0284c7",
      title = "Courbe de survie globale",
      xlab = paste("Temps (", .get_label(data, time_var, time_var), ")")
    )
  }
  return(p)
}
