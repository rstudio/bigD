#' Obtain a standard datetime format that works across locales
#'
#' @param type One of four standardized types for the resulting datetime that
#'   range in precision and verbosity. These are `"short"` (the default),
#'   `"medium"`, `"long"`, and `"full"`.
#'
#' @export
standard_date_time <- function(
    type = c("short", "medium", "long", "full")
) {
  type <- match.arg(type)
  x <- "sdt"
  class(x) <- c("date_time_pattern", "standard", "date_time", type)
  x
}

#' Obtain a standard date format that works across locales
#'
#' @param type One of four standardized types for the resulting date that range
#'   in precision and verbosity. These are `"short"` (the default), `"medium"`,
#'   `"long"`, and `"full"`.
#'
#' @export
standard_date <- function(
    type = c("short", "medium", "long", "full")
) {
  type <- match.arg(type)
  x <- "sdt"
  class(x) <- c("date_time_pattern", "standard", "date", type)
  x
}

#' Obtain a standard time format that works across locales
#'
#' @param type One of four standardized types for the resulting time that range
#'   in precision and verbosity. These are `"short"` (the default), `"medium"`,
#'   `"long"`, and `"full"`.
#'
#' @export
standard_time <- function(
    type = c("short", "medium", "long", "full")
) {
  type <- match.arg(type)
  x <- "sdt"
  class(x) <- c("date_time_pattern", "standard", "time", type)
  x
}
