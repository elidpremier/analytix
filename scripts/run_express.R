#!/usr/bin/env Rscript
# ==============================================================================
# run_express.R — Script CLI pour générer un Rapport Express analytix
# ==============================================================================
# Usage :
#   Rscript scripts/run_express.R --input chemin/vers/fichier.xlsx
#   Rscript scripts/run_express.R --input donnees.csv --output Rapport.docx \
#           --title "Mon Analyse" --outcome statut_nutrition \
#           --context "Enquête nutritionnelle, Bénin 2025" \
#           --provider groq
#
# Clés API LLM (optionnel) :
#   export GROQ_API_KEY="votre_clé_groq"
#   export MISTRAL_API_KEY="votre_clé_mistral"
# ==============================================================================

# ─── Lecture des arguments de la ligne de commande ───────────────────────────
args <- commandArgs(trailingOnly = TRUE)

.parse_args <- function(args) {
  params <- list(
    input    = NULL,
    output   = "Rapport_Express.docx",
    title    = "Rapport Express",
    subtitle = NULL,
    author   = NULL,
    outcome  = NULL,
    context  = NULL,
    provider = "groq",
    model    = NULL,
    use_ai   = TRUE,
    exclude  = NULL,
    no_ai    = FALSE
  )
  i <- 1
  while (i <= length(args)) {
    switch(args[i],
      "--input"    = { params$input    <- args[i + 1]; i <- i + 2 },
      "--output"   = { params$output   <- args[i + 1]; i <- i + 2 },
      "--title"    = { params$title    <- args[i + 1]; i <- i + 2 },
      "--subtitle" = { params$subtitle <- args[i + 1]; i <- i + 2 },
      "--author"   = { params$author   <- args[i + 1]; i <- i + 2 },
      "--outcome"  = { params$outcome  <- args[i + 1]; i <- i + 2 },
      "--context"  = { params$context  <- args[i + 1]; i <- i + 2 },
      "--provider" = { params$provider <- args[i + 1]; i <- i + 2 },
      "--model"    = { params$model    <- args[i + 1]; i <- i + 2 },
      "--exclude"  = { params$exclude  <- strsplit(args[i + 1], ",")[[1]]; i <- i + 2 },
      "--no-ai"    = { params$use_ai   <- FALSE; i <- i + 1 },
      "--help"     = {
        cat("
Usage : Rscript scripts/run_express.R [options]

Options obligatoires :
  --input FICHIER       Chemin vers le fichier de données (Excel ou CSV)

Options facultatives :
  --output FICHIER      Chemin du rapport Word généré (défaut: Rapport_Express.docx)
  --title TITRE         Titre du rapport
  --subtitle TEXTE      Sous-titre du rapport
  --author NOM          Nom de l'auteur
  --outcome VARIABLE    Variable de résultat pour l'analyse bivariée
  --context TEXTE       Contexte de l'analyse transmis au LLM
  --provider FOURNISSEUR  'groq' (défaut) ou 'mistral'
  --exclude VAR1,VAR2   Variables à exclure (séparées par des virgules)
  --no-ai               Désactiver la rédaction automatique par IA
  --help                Afficher cette aide

Variables d'environnement (pour la rédaction IA) :
  GROQ_API_KEY          Clé API Groq (https://console.groq.com)
  MISTRAL_API_KEY       Clé API Mistral (https://console.mistral.ai)

Exemple :
  Rscript scripts/run_express.R \\
    --input data/enquete.xlsx \\
    --output livrables/Rapport_Client.docx \\
    --title 'Analyse — Projet XYZ' \\
    --author 'IDO Elisée' \\
    --outcome statut_nutrition \\
    --context 'Enquête nutritionnelle, 350 enfants, Bénin 2025' \\
    --provider groq
")
        quit(status = 0)
      },
      {
        cat("⚠️  Option inconnue :", args[i], "\n")
        i <- i + 1
      }
    )
  }
  params
}

params <- .parse_args(args)

# ─── Vérifications préalables ────────────────────────────────────────────────
if (is.null(params$input)) {
  cat("❌ Erreur : --input est obligatoire.\n")
  cat("   Utilisez --help pour voir l'aide.\n")
  quit(status = 1)
}

if (!file.exists(params$input)) {
  cat("❌ Erreur : fichier introuvable :", params$input, "\n")
  quit(status = 1)
}

# ─── Chargement du package ───────────────────────────────────────────────────
cat("📦 Chargement du package analytix...\n")
if (!requireNamespace("analytix", quietly = TRUE)) {
  # Tenter de charger depuis le répertoire courant (développement)
  pkg_path <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), mustWork = FALSE)
  tryCatch({
    devtools::load_all(pkg_path, quiet = TRUE)
    cat("   (chargé en mode développement depuis :", pkg_path, ")\n")
  }, error = function(e) {
    cat("❌ Erreur : impossible de charger analytix.\n")
    cat("   Installez-le avec : devtools::install('", pkg_path, "')\n", sep = "")
    quit(status = 1)
  })
} else {
  library(analytix)
}

# ─── Génération du rapport ───────────────────────────────────────────────────
cat("\n🚀 Démarrage de la génération du Rapport Express...\n")
cat("   Fichier d'entrée  :", params$input, "\n")
cat("   Fichier de sortie :", params$output, "\n")
if (!is.null(params$outcome)) cat("   Variable résultat :", params$outcome, "\n")
cat("   Rédaction IA      :", if (params$use_ai) params$provider else "désactivée", "\n\n")

tryCatch({
  output_path <- report_express(
    data         = params$input,
    output       = params$output,
    title        = params$title,
    subtitle     = params$subtitle,
    author       = params$author,
    outcome      = if (!is.null(params$outcome)) params$outcome else NULL,
    exclude_vars = params$exclude,
    context      = params$context,
    use_ai       = params$use_ai,
    provider     = params$provider,
    model        = params$model,
    open_doc     = FALSE,
    verbose      = TRUE
  )
  cat("\n✅ SUCCÈS !\n")
  cat("   Rapport généré :", output_path, "\n\n")
  cat("   → Ouvrez le fichier avec LibreOffice Writer ou Microsoft Word.\n")
}, error = function(e) {
  cat("\n❌ ERREUR lors de la génération :\n")
  cat("  ", conditionMessage(e), "\n")
  quit(status = 1)
})
