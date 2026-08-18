#' @title Générer un jeu de données de santé factice (exemple)
#' @description Génère un \code{data.frame} contenant des données cliniques fictives
#' (âge, sexe, IMC, tabac, traitement, délai de suivi, décès, etc.) pour
#' illustrer et tester les fonctions du package \code{analytix}.
#'
#' @param n Nombre d'observations à générer (défaut: 200).
#' @param seed Graine de génération aléatoire (défaut: 42).
#'
#' @return Un \code{data.frame} de \code{n} lignes avec des variables cliniques étiquetées.
#'
#' @examples
#' df_clinique <- analytix_sample_data(n = 300)
#' summary(df_clinique)
#'
#' @export
analytix_sample_data <- function(n = 200, seed = 42) {
  set.seed(seed)

  age <- round(stats::rnorm(n, mean = 65, sd = 12))
  age <- pmax(min(age), pmin(max(age), 95)) # Borner l'âge

  sexe <- sample(c("Homme", "Femme"), n, replace = TRUE, prob = c(0.55, 0.45))
  imc <- round(stats::rnorm(n, 26, 4.5), 1)

  # Créer des corrélations artificielles pour la survie et la ROC
  # Plus on est vieux et qu'on a un fort IMC, plus le risque est élevé
  score_risque <- (age - 65)/10 + (imc - 25)/5
  prob_deces <- 1 / (1 + exp(-(score_risque - 1)))

  deces <- stats::rbinom(n, 1, prob = prob_deces)

  tabac <- sample(c("Non fumeur", "Ancien fumeur", "Fumeur actif"), n,
                  replace = TRUE, prob = c(0.5, 0.3, 0.2))

  traitement <- sample(c("Standard", "Nouveau"), n, replace = TRUE, prob = c(0.5, 0.5))

  # Temps de suivi (censuré)
  # Le nouveau traitement prolonge légèrement la survie
  suivi_base <- stats::rexp(n, rate = 0.05)
  suivi_base[traitement == "Nouveau"] <- suivi_base[traitement == "Nouveau"] * 1.3
  suivi_base[deces == 1] <- suivi_base[deces == 1] * 0.5 # ceux qui meurent ont un suivi plus court

  suivi_mois <- round(pmin(suivi_base, 60), 1) # max 5 ans (60 mois)

  # Introduction de quelques valeurs manquantes pour le réalisme
  imc[sample(1:n, size = round(n * 0.05))] <- NA
  tabac[sample(1:n, size = round(n * 0.03))] <- NA

  df <- data.frame(
    id         = sprintf("PAT-%04d", 1:n),
    age        = age,
    sexe       = factor(sexe, levels = c("Homme", "Femme")),
    imc        = imc,
    tabac      = factor(tabac, levels = c("Non fumeur", "Ancien fumeur", "Fumeur actif")),
    traitement = factor(traitement, levels = c("Standard", "Nouveau")),
    suivi_mois = suivi_mois,
    deces      = deces,
    stringsAsFactors = FALSE
  )

  # Ajout de labels (supporté par analytix)
  attr(df$age, "label") <- "Âge du patient (années)"
  attr(df$sexe, "label") <- "Sexe du patient"
  attr(df$imc, "label") <- "Indice de Masse Corporelle (kg/m²)"
  attr(df$tabac, "label") <- "Statut tabagique"
  attr(df$traitement, "label") <- "Bras de traitement"
  attr(df$suivi_mois, "label") <- "Délai de suivi (mois)"
  attr(df$deces, "label") <- "Décès toutes causes"

  return(df)
}
