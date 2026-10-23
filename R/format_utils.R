# Helpers internes de formatage des résultats statistiques.

.fmt_decimal <- function(x, digits = 1L) {
  if (length(x) == 0L) return(character())
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != as.integer(digits)) {
    stop("`digits` doit être un entier positif ou nul.", call. = FALSE)
  }
  base::format(
    round(as.numeric(x), as.integer(digits)),
    nsmall = as.integer(digits),
    decimal.mark = ",",
    trim = TRUE,
    scientific = FALSE
  )
}

.fmt_count <- function(x) {
  if (length(x) == 0L) return(character())
  as.character(as.integer(round(as.numeric(x), 0L)))
}

.fmt_percent <- function(x, digits = 1L, suffix = FALSE) {
  value <- .fmt_decimal(x, digits)
  if (isTRUE(suffix)) paste0(value, " %") else value
}

.fmt_pvalue <- function(x, digits = 3L) {
  value <- as.numeric(x)
  out <- .fmt_decimal(value, digits)
  out[!is.na(value) & value < 10^(-digits)] <- paste0("< ", .fmt_decimal(10^(-digits), digits))
  out
}

.fmt_ci_percent <- function(lower, upper, digits = 1L, level = 95L) {
  paste0(
    "[IC", as.integer(level), " % : ",
    .fmt_percent(lower, digits),
    " - ",
    .fmt_percent(upper, digits),
    " %]"
  )
}
