test_that("plot_barplot generates ggplot object", {
  p1 <- plot_barplot(iris, Species, title = "Espèces")
  expect_s3_class(p1, "ggplot")
  
  p2 <- plot_barplot(table(iris$Species), horiz = TRUE)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_pie_chart generates ggplot object", {
  p <- plot_pie_chart(iris, Species, title = "Camembert")
  expect_s3_class(p, "ggplot")
})

test_that("plot_stacked_bar_100 generates ggplot object", {
  p <- plot_stacked_bar_100(mtcars, x = cyl, fill = am, title = "Empilé 100%")
  expect_s3_class(p, "ggplot")
})

test_that("plot_grouped_bar generates ggplot object", {
  p <- plot_grouped_bar(mtcars, x = cyl, fill = am, show_pct = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_boxplot generates ggplot object", {
  p <- plot_boxplot(mtcars, x = cyl, y = mpg, title = "Boxplot")
  expect_s3_class(p, "ggplot")
})
