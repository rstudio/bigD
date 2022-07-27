
cldr_wkdays <- function() {
  c(
    "sun", "mon", "tue", "wed", "thu", "fri", "sat"
  )
}

cldr_months <- function() {
  c(
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec"
  )
}

braced <- function(value) {
  paste0("{", value, "}")
}

substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
