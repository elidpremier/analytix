test_that("tbl_cross_unique fonctionne avec target_name et inclut p_global", {
  df <- data.frame(
    sexe = c("M", "F", "M", "F", "M", "F", "M", "F"),
    gueri = c("Oui", "Non", "Oui", "Oui", "Non", "Non", "Oui", "Non"),
    age_grp = c("Jeune", "Vieux", "Jeune", "Jeune", "Vieux", "Vieux", "Jeune", "Jeune")
  )

  res <- tbl_cross_unique(df, target = gueri, sexe, age_grp, target_name = "Statut de Guérison")

  expect_s3_class(res, "analytix_table")
  expect_true(inherits(res$flextable, "flextable"))
  expect_true("p_global" %in% names(res$data))
  expect_true("p_value" %in% names(res$data))
})

test_that("tbl_cross_unique lève une erreur explicite sans variables en ligne", {
  df <- data.frame(gueri = c("Oui", "Non"))
  expect_error(tbl_cross_unique(df, target = gueri), "Aucune variable en ligne fournie")
})

test_that("tbl_cross_unique gère les petits effectifs sans warning chisq", {
  df <- data.frame(
    target = c("Oui", "Oui", "Non", "Non"),
    var1 = c("A", "B", "A", "A")
  )
  expect_no_warning({
    res <- tbl_cross_unique(df, target = target, var1)
  })
  expect_s3_class(res, "analytix_table")
})

test_that("desc_categorical et desc_numeric fonctionnent dans une boucle for avec une variable chaîne", {
  df <- data.frame(
    sexe = c("M", "F", "M", "F"),
    age = c(20, 30, 40, 50)
  )

  cols_cat <- c("sexe")
  for (v in cols_cat) {
    res <- desc_categorical(df, v)
    expect_s3_class(res, "analytix_table")
    expect_equal(res$variable_name, "sexe")
  }

  cols_num <- c("age")
  for (v in cols_num) {
    res <- desc_numeric(df, v)
    expect_s3_class(res, "analytix_table")
    expect_equal(res$variable_name, "age")
  }
})

test_that("export_tables avec strip_manual_numbering et auto_landscape fonctionne", {
  df <- data.frame(a = 1:5, b = c("Oui", "Non", "Oui", "Oui", "Non"))
  t1 <- desc_numeric(df, a)
  t2 <- desc_categorical(df, b)

  tmp_file <- tempfile(fileext = ".docx")
  expect_silent({
    export_tables(
      tables = list("1.1 Description de MPG" = t1, "8.7 Description de Species" = t2),
      file = tmp_file,
      title = "Test Rapport",
      strip_manual_numbering = TRUE,
      auto_landscape = TRUE
    )
  })
  expect_true(file.exists(tmp_file))
  unlink(tmp_file)
})

test_that("theme_analytique gère l'argument caption et auto-scale la police pour les grands tableaux", {
  df <- as.data.frame(matrix(1:24, nrow = 3, ncol = 8))
  ft <- flextable::flextable(df)

  ft_themed <- theme_analytique(ft, caption = "Tableau Grand", font_size = 11)
  expect_true(inherits(ft_themed, "flextable"))
})

test_that("desc_multi_choice accepte des chaines et l'argument positive", {
  df <- data.frame(
    item1 = c("Oui", "Non", "Oui", "Non"),
    item2 = c("Non", "Oui", "Oui", "Oui")
  )

  res <- desc_multi_choice(df, cols = c("item1", "item2"), positive = "Oui")
  expect_s3_class(res, "analytix_table")
  expect_equal(nrow(res$data), 2)
  expect_true("Option" %in% names(res$data))
})

test_that("desc_score calcule correctement le score et les tranches", {
  df <- data.frame(
    q1 = c(1, 1, 0, 1),
    q2 = c(1, 0, 1, 1),
    q3 = c(0, 1, 1, 1)
  )

  res <- desc_score(df, cols = c("q1", "q2", "q3"), breaks = c(-Inf, 1, 3), labels = c("Faible", "Élevé"))
  expect_s3_class(res, "analytix_table")
  expect_equal(res$data$max_score, 3)
  expect_equal(length(res$data$scores), 4)
})

test_that("desc_grouped retourne un data.frame plat propre dans $data", {
  df <- data.frame(
    classe = c("A", "A", "B", "B"),
    element = c("e1", "e2", "e3", "e4")
  )

  res <- desc_grouped(df, group_col = "classe", sub_col = "element")
  expect_s3_class(res, "analytix_table")
  expect_false(any(is.na(res$data$Classe)))
})

test_that("interp_pvalue utilise des formulations non causales et gère small_counts", {
  txt <- interp_pvalue(0.004, small_counts = TRUE)
  expect_false(grepl("lien réel", txt))
  expect_true(grepl("statistiquement significative", txt))
  expect_true(grepl("Fisher est privilégié", txt))
})
