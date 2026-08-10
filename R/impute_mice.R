#' @title Imputation multiple robuste par la méthode MICE
#' @description
#' Wrapper robuste autour de `mice` gérant automatiquement tous les cas
#' problématiques courants : colonnes constantes, colonnes 100% manquantes,
#' jeux de données avec peu d'observations (n << p), classes spéciales
#' (`labelled`, `haven_labelled`), facteurs à un seul niveau, et colinéarité
#' quasi-parfaite.
#'
#' La stratégie d'imputation s'adapte selon les données :
#' - **Stratégie 1** (prioritaire) : `mice::mice()` standard sur les colonnes
#'   imputables, avec méthodes automatiques (pmm / logreg / polyreg).
#' - **Stratégie 2** (fallback robuste) : imputation colonne par colonne avec
#'   `predictorMatrix` épurée, puis fallback sur moyenne/mode si nécessaire.
#'
#' @param data data.frame à imputer.
#' @param m Nombre d'imputations (défaut : 5). Réduit automatiquement à 1 en
#'   mode fallback pour les petits jeux de données.
#' @param maxit Nombre maximum d'itérations de MICE (défaut : 5).
#' @param seed Graine aléatoire pour la reproductibilité (défaut : 123).
#' @param min_obs_strategy1 Nombre minimum d'observations pour tenter la
#'   Stratégie 1 (MICE standard). En dessous de ce seuil, la Stratégie 2
#'   est directement utilisée (défaut : 10).
#' @param verbose Afficher des messages de progression (défaut : TRUE).
#' @param ... Arguments supplémentaires passés à `mice::mice()` en Stratégie 1
#'   (ex : `defaultMethod`, `visitSequence`).
#'
#' @return Un `data.frame` imputé, de même structure et avec les mêmes
#'   attributs `label` que `data`. Les colonnes 100% manquantes sont laissées
#'   intactes (impossible à imputer).
#'
#' @examples
#' # Cas standard
#' df <- data.frame(
#'   age   = c(25, NA, 30, NA, 28, 32, NA, 22),
#'   sexe  = factor(c("M", NA, "F", "M", NA, "F", "M", NA)),
#'   score = c(NA, 2.5, 3.1, NA, 4.0, NA, 1.8, 2.2)
#' )
#' impute_mice(df)
#'
#' # Cas avec colonne constante et colonne 100% NA
#' df2 <- data.frame(
#'   a = c(1, NA, 3, NA),
#'   b = c(5, 5, 5, 5),       # constante -> sera ignorée pour l'imputation
#'   c = c(NA, NA, NA, NA)    # 100% NA   -> laissée intacte
#' )
#' impute_mice(df2)
#'
#' @export
impute_mice <- function(data,
                        m                 = 5,
                        maxit             = 5,
                        seed              = 123,
                        min_obs_strategy1 = 10,
                        verbose           = TRUE,
                        ...) {

  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Le package 'mice' est requis. Installez-le avec install.packages('mice')",
         call. = FALSE)
  }

  stopifnot(is.data.frame(data), nrow(data) >= 1, ncol(data) >= 1)

  # -----------------------------------------------------------------
  # 1. Nettoyage des classes spéciales incompatibles avec mice
  #    (labelled, haven_labelled, vctrs_vctr, etc.)
  # -----------------------------------------------------------------
  SPECIAL_CLASSES <- c("labelled", "haven_labelled", "vctrs_vctr",
                       "haven_labelled_spss")

  .strip_special <- function(df) {
    as.data.frame(lapply(df, function(x) {
      lbl <- attr(x, "label")
      cls <- class(x)
      bad <- cls %in% SPECIAL_CLASSES
      if (any(bad)) class(x) <- cls[!bad]
      if (!is.null(lbl)) attr(x, "label") <- lbl
      x
    }), stringsAsFactors = FALSE)
  }

  .restore_labels <- function(imputed, original) {
    for (cn in intersect(names(original), names(imputed))) {
      lbl <- attr(original[[cn]], "label")
      if (!is.null(lbl)) attr(imputed[[cn]], "label") <- lbl
    }
    imputed
  }

  original <- data
  df       <- .strip_special(data)

  # -----------------------------------------------------------------
  # 2. Vérification initiale : y a-t-il des NA à imputer ?
  # -----------------------------------------------------------------
  has_any_na <- any(sapply(df, function(x) any(is.na(x))))
  if (!has_any_na) {
    if (verbose) message("impute_mice : aucune valeur manquante détectée — données retournées inchangées.")
    return(data)
  }

  n <- nrow(df)
  p <- ncol(df)

  # -----------------------------------------------------------------
  # 3. Catégorisation des colonnes
  # -----------------------------------------------------------------
  .col_info <- function(x, nm) {
    x_valid  <- x[!is.na(x)]
    n_na     <- sum(is.na(x))
    n_valid  <- length(x_valid)
    n_unique <- length(unique(x_valid))
    list(
      name        = nm,
      n_na        = n_na,
      n_valid     = n_valid,
      pct_na      = n_na / length(x),
      all_na      = n_valid == 0,
      constant    = n_unique <= 1,
      n_unique    = n_unique,
      needs_impute = n_na > 0
    )
  }

  col_infos     <- Map(.col_info, df, names(df))
  is_all_na     <- sapply(col_infos, `[[`, "all_na")
  is_constant   <- sapply(col_infos, `[[`, "constant")
  # Exclure les colonnes non-imputables (pas utilisées comme prédicteurs)
  is_excluded   <- is_all_na | is_constant
  needs_impute  <- sapply(col_infos, `[[`, "needs_impute") & !is_excluded

  if (verbose) {
    n_excl <- sum(is_excluded)
    n_imp  <- sum(needs_impute)
    if (n_excl > 0)
      message(sprintf("impute_mice : %d colonne(s) exclues (constantes ou 100%% NA) : %s",
                      n_excl, paste(names(df)[is_excluded], collapse = ", ")))
    message(sprintf("impute_mice : %d colonne(s) à imputer.", n_imp))
  }

  if (!any(needs_impute)) {
    if (verbose) message("impute_mice : aucune colonne imputable identifiée — données retournées inchangées.")
    return(data)
  }

  # -----------------------------------------------------------------
  # 4. Détermination de la méthode MICE par colonne
  # -----------------------------------------------------------------
  .mice_method <- function(x) {
    x_valid <- x[!is.na(x)]
    if (is.numeric(x))                                    return("pmm")
    if (is.logical(x))                                    return("logreg")
    x_f <- if (is.factor(x)) x else factor(x)
    nk  <- length(levels(droplevels(x_f[!is.na(x_f)])))
    if (nk <= 2)                                          return("logreg")
    if (nk <= 20)                                         return("polyreg")
    return("pmm")  # très nombreux niveaux -> PMM approché
  }

  method_vec <- rep("", p)
  names(method_vec) <- names(df)
  for (cn in names(df)[needs_impute]) {
    method_vec[cn] <- .mice_method(df[[cn]])
  }

  # -----------------------------------------------------------------
  # 5. Stratégie 1 : mice standard (si assez d'observations)
  # -----------------------------------------------------------------
  .try_mice_standard <- function(df_sub, method_sub, m, maxit, seed, ...) {
    tryCatch({
      # Construire la matrice de prédicteurs propre
      pred_mat <- tryCatch(
        mice::quickpred(df_sub, mincor = 0.1),
        error = function(e) {
          pm <- matrix(1L, nrow = ncol(df_sub), ncol = ncol(df_sub),
                       dimnames = list(names(df_sub), names(df_sub)))
          diag(pm) <- 0L
          pm
        }
      )
      suppressWarnings(
        mice::mice(df_sub,
                   m                = m,
                   maxit            = maxit,
                   seed             = seed,
                   method           = method_sub,
                   predictorMatrix  = pred_mat,
                   printFlag        = FALSE,
                   ...)
      )
    }, error = function(e) NULL)
  }

  result <- NULL

  if (n >= min_obs_strategy1 && sum(!is_excluded) >= 2) {
    imputable_cols <- names(df)[!is_excluded]
    df_sub     <- df[, imputable_cols, drop = FALSE]
    method_sub <- method_vec[imputable_cols]

    imp <- .try_mice_standard(df_sub, method_sub, m, maxit, seed, ...)

    if (!is.null(imp)) {
      completed <- mice::complete(imp, 1)
      df_out    <- df
      df_out[, imputable_cols] <- completed
      result    <- df_out
      if (verbose) message("impute_mice : imputation MICE standard réussie.")
    }
  }

  # -----------------------------------------------------------------
  # 6. Stratégie 2 (fallback) : imputation colonne par colonne
  # -----------------------------------------------------------------
  if (is.null(result)) {
    if (verbose) message("impute_mice : bascule en mode robuste (imputation marginale colonne par colonne).")

    df_imp   <- df
    max_pass <- 3  # plusieurs passes pour propager les valeurs imputées

    for (pass in seq_len(max_pass)) {
      remaining_na <- any(sapply(df_imp[, needs_impute, drop = FALSE], function(x) any(is.na(x))))
      if (!remaining_na) break

      for (cn in names(df_imp)[needs_impute]) {
        x <- df_imp[[cn]]
        if (!any(is.na(x))) next

        x_valid <- x[!is.na(x)]
        if (length(x_valid) == 0) next  # 100% NA -> impossible

        meth <- method_vec[cn]

        # Identifier les prédicteurs valides pour cette colonne
        pred_candidates <- setdiff(names(df_imp)[!is_excluded], cn)
        valid_preds <- Filter(function(other_cn) {
          oth <- df_imp[[other_cn]]
          n_valid_oth <- sum(!is.na(oth))
          n_valid_oth >= max(3, ceiling(n / 5))
        }, pred_candidates)

        # Construire la predictorMatrix pour cette colonne uniquement
        all_cols <- names(df_imp)
        pm_full  <- matrix(0L, nrow = p, ncol = p,
                           dimnames = list(all_cols, all_cols))
        if (length(valid_preds) > 0) {
          pm_full[cn, valid_preds] <- 1L
        }

        method_full      <- rep("", p)
        names(method_full) <- all_cols
        method_full[cn]  <- meth

        imp_col <- tryCatch(
          suppressWarnings(
            mice::mice(df_imp,
                       m               = 1,
                       maxit           = 2,
                       seed            = seed,
                       method          = method_full,
                       predictorMatrix = pm_full,
                       printFlag       = FALSE)
          ),
          error = function(e) NULL
        )

        if (!is.null(imp_col)) {
          df_imp[[cn]] <- mice::complete(imp_col, 1)[[cn]]
        } else {
          # Fallback ultime : moyenne (numérique) ou mode (catégoriel)
          if (is.numeric(x)) {
            df_imp[[cn]][is.na(df_imp[[cn]])] <- mean(x_valid, na.rm = TRUE)
          } else {
            mode_val <- names(sort(table(x_valid), decreasing = TRUE))[1]
            df_imp[[cn]][is.na(df_imp[[cn]])] <- mode_val
          }
        }
      }
    }

    result <- df_imp
    if (verbose) message("impute_mice : imputation robuste terminée.")
  }

  # -----------------------------------------------------------------
  # 7. Restauration des attributs label + retour
  # -----------------------------------------------------------------
  .restore_labels(result, original)
}
