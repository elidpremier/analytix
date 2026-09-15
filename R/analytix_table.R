#' @title Constructeur pour les objets de résultats de tableaux analytix
#' @description Encapsule un jeu de données tidy ($data) et son objet flextable ($flextable)
#' dans une structure standardisée de classe `analytix_table`.
#'
#' @param data data.frame des résultats.
#' @param flextable objet flextable associé.
#' @param ... Métadonnées additionnelles.
#' @return Un objet de classe `analytix_table`.
#' @export
as_analytix_table <- function(data, flextable, ...) {
  res <- list(
    data = data,
    flextable = flextable,
    ...
  )
  class(res) <- c("analytix_table", "list")
  res
}

#' @export
print.analytix_table <- function(x, ...) {
  if (!is.null(x$flextable) && inherits(x$flextable, "flextable")) {
    print(x$flextable, ...)
  } else {
    print.default(x, ...)
  }
}

#' @export
as_flextable.analytix_table <- function(x, ...) {
  x$flextable
}
