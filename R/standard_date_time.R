#' Obtain a standard datetime format that works across locales
#'
#' @export
standard_date_time <- function(
    type = c("full", "long", "medium", "short")
) {
  type <- match.arg(type)
  x <- "sdt"
  class(x) <- c("date_time_pattern", "standard", "date_time", type)
  x
}

#' Obtain a standard date format that works across locales
#'
#' @export
standard_date <- function(
    type = c("full", "long", "medium", "short")
) {
  type <- match.arg(type)
  x <- "sdt"
  class(x) <- c("date_time_pattern", "standard", "date", type)
  x
}

#' Obtain a standard time format that works across locales
#'
#' @export
standard_time <- function(
    type = c("full", "long", "medium", "short")
) {
  type <- match.arg(type)
  x <- "sdt"
  class(x) <- c("date_time_pattern", "standard", "time", type)
  x
}
