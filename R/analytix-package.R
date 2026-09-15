#' analytix: Outils d'Analyse Descriptive et Statistique pour Tableaux Professionnels
#'
#' Package R d'automatisation de l'analyse de données et de génération de rapports
#' francophones professionnels. Il offre des outils complets pour le nettoyage des données,
#' l'analyse descriptive univariée et bivariée, les statistiques épidémiologiques avancées,
#' la visualisation de données avec \code{ggplot2} et l'exportation de tableaux sous Word
#' via \code{flextable} et \code{officer}.
#'
#' @details
#' Le package \code{analytix} utilise une nomenclature standardisée organisée par préfixes de famille :
#'
#' \bold{1. Nettoyage et Préparation des Données (\code{clean_*}, \code{prep_*})}
#' \itemize{
#'   \item \code{\link{prep_import}} : Importation et nettoyage initial des données Excel/CSV.
#'   \item \code{\link{clean_names}}, \code{\link{clean_colnames}}, \code{\link{clean_text}}, \code{\link{clean_numeric}}, \code{\link{clean_binary}}, \code{\link{prep_recode_odk}} : Nettoyage et standardisation.
#'   \item \code{\link{prep_labels}} : Définition et gestion des étiquettes de variables.
#'   \item \code{\link{prep_outliers}} : Détection des valeurs aberrantes (IQR, Z-score).
#'   \item \code{\link{prep_quick_code}} : Recodage rapide d'une variable catégorielle.
#'   \item \code{\link{prep_collapse}} : Regroupement de modalités de variables catégorielles.
#'   \item \code{\link{prep_categorize}} : Discrétisation des variables numériques en classes.
#'   \item \code{\link{prep_impute_mode}}, \code{\link{prep_impute_mean}}, \code{\link{prep_impute_mice}} : Imputation des valeurs manquantes.
#' }
#'
#' \bold{2. Analyses Descriptives (\code{desc_*})}
#' \itemize{
#'   \item \code{\link{desc_auto}} : Génération automatique de descriptions adaptées au type de variable.
#'   \item \code{\link{desc_numeric}} : Description des variables numériques (moyenne, écart-type, médiane, IQR).
#'   \item \code{\link{desc_categorical}} : Description des variables catégorielles (effectifs et pourcentages).
#'   \item \code{\link{desc_grouped}} : Description des sous-catégories groupées par thématique.
#'   \item \code{\link{desc_binary}} : Description des variables binaires.
#'   \item \code{\link{desc_age}} : Analyse spécifique de l'âge et découpage en tranches.
#'   \item \code{\link{desc_likert}}, \code{\link{prep_recode_likert}}, \code{\link{tbl_likert_multi}} : Analyse des échelles de Likert.
#'   \item \code{\link{desc_multi_choice}} : Analyse des questions à choix multiples.
#'   \item \code{\link{desc_by_group}} : Analyse croisée d'une variable par un groupe avec tests.
#'   \item \code{\link{desc_prevalence}} : Calcul de prévalence avec intervalle de confiance à 95% (Wilson).
#'   \item \code{\link{desc_multiple}} : Analyse automatisée de plusieurs variables mixtes.
#' }
#'
#' \bold{3. Tableaux Statologiques et Modélisation (\code{tbl_*}, \code{stat_*}, \code{interp_*})}
#' \itemize{
#'   \item \code{\link{tbl_cross_unique}}, \code{\link{tbl_cross_multi}} : Tableaux croisés univariés et multi-prédicteurs (Chi2 / Fisher).
#'   \item \code{\link{tbl_bivariate_or}} : Analyse bivariée avec calcul d'Odds Ratios (OR) et p-values.
#'   \item \code{\link{tbl_logistic}} : Modélisation par régression logistique multivariée.
#'   \item \code{\link{tbl_anova}} : Analyse de variance (ANOVA à un facteur + post-hoc Tukey).
#'   \item \code{\link{tbl_correlation}} : Matrice de corrélations (Pearson/Spearman).
#'   \item \code{\link{tbl_roc}} : Analyse des courbes ROC et aire sous la courbe (AUC).
#'   \item \code{\link{tbl_km}} : Tableaux d'analyse de survie (Kaplan-Meier).
#'   \item \code{\link{stat_sens_spec}} : Indicateurs diagnostiques (Sensibilité, Spécificité, VPP, VPN).
#'   \item \code{\link{interp_pvalue}}, \code{\link{interp_or}}, \code{\link{interp_association}} : Interprétation textuelle automatique en français.
#' }
#'
#' \bold{4. Visualisation de Données (\code{plot_*})}
#' \itemize{
#'   \item \code{\link{plot_bar}}, \code{\link{plot_pie}}, \code{\link{plot_box}} : Graphiques univariés.
#'   \item \code{\link{plot_bar_grouped}}, \code{\link{plot_bar_stacked}} : Graphiques bivariés.
#'   \item \code{\link{plot_distribution}} : Histogramme / densité adaptatif.
#'   \item \code{\link{plot_likert}} : Graphique en barres divergentes pour échelles Likert.
#'   \item \code{\link{plot_correlation}}, \code{\link{plot_heatmap}} : Heatmaps de corrélations et matrices.
#'   \item \code{\link{plot_missing}} : Carte visuelle des valeurs manquantes.
#'   \item \code{\link{plot_km}} : Courbes de survie de Kaplan-Meier.
#' }
#'
#' \bold{5. Formatage, Rapports et Exports Word (\code{fmt_*}, \code{report_*}, \code{export_*})}
#' \itemize{
#'   \item \code{\link{theme_analytique}}, \code{\link{fmt_flextable}}, \code{\link{fmt_regression}}, \code{\link{fmt_apply_theme}} : Thèmes et mises en forme flextable et ggplot2.
#'   \item \code{\link{report_generate}}, \code{\link{report_magic}}, \code{\link{report_missing}}, \code{\link{report_compile}} : Rapports Word automatiques et modulaires.
#'   \item \code{\link{export_word}}, \code{\link{export_tables}} : Exportation des tableaux au format Word (.docx).
#' }
#'
#' \bold{6. Interface Graphique (GUI)}
#' \itemize{
#'   \item \code{\link{run_gui}} ou \code{\link{run_analytix_ui}} : Lancement de l'application interactive Shiny no-code.
#' }
#'
#' @docType package
#' @name analytix
#' @aliases analytix analytix-package
#' @keywords package
#' @examples
#' \dontrun{
#' library(analytix)
#'
#' # Charger des données d'exemple
#' data("analytix_sample_data")
#'
#' # Description automatique d'une variable numérique
#' desc_numeric(analytix_sample_data, "age")
#'
#' # Lancer l'interface graphique
#' run_gui()
#' }
"_PACKAGE"
