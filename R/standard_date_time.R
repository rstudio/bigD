#' Obtain a standard datetime format that works across locales
#'
#' The `standard_date_time()` function can be invoked in the `format` argument
#' of the `fdt()` function to help generate a locale-specific formatting string
#' of a certain 'type' of formatted datetime. The `type` value is a keyword that
#' represents precision and verbosity; the available keywords are `"short"` (the
#' default), `"medium"`, `"long"`, and `"full"`.
#'
#' @param type One of four standardized types for the resulting datetime that
#'   range in precision and verbosity. These are `"short"` (the default),
#'   `"medium"`, `"long"`, and `"full"`.
#'
#' @return A vector of class `date_time_pattern`.
#'
#' @section Examples:
#'
#' With an input datetime of `"2018-07-04 22:05(America/Vancouver)"`, we can
#' format the date and time in a standardized way with `standard_date_time()`
#' providing the correct formatting string. This function is invoked in the
#' `format` argument of `fdt()`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time(type = "full")
#' )
#' ```
#' ```
#' #> [1] "Wednesday, July 4, 2018 at 10:05:00 PM Pacific Daylight Time"
#' ```
#'
#' The locale can be changed and we don't have to worry about the particulars
#' of the formatting string (they are standardized across locales).
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time(type = "full"),
#'   locale = fdt_locales_lst$nl
#' )
#' ```
#' ```
#' #> [1] "woensdag 4 juli 2018 om 22:05:00 Pacific-zomertijd"
#' ```
#'
#' We can use different `type` values to control the output datetime string. The
#' default is `"short"`.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time()
#' )
#' ```
#' ```
#' #> [1] "7/4/18, 10:05 PM"
#' ```
#'
#' After that, it's `"medium"`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time(type = "medium")
#' )
#' ```
#' ```
#' #> [1] "Jul 4, 2018, 10:05:00 PM"
#' ```
#'
#' The `"short"` and `"medium"` types don't display time zone information in the
#' output. Beginning with `"long"`, the tz is shown.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time(type = "long")
#' )
#' ```
#' ```
#' #> [1] "July 4, 2018 at 10:05:00 PM PDT"
#' ```
#'
#' If you don't include time zone information in the input, the `"UTC"` time
#' zone will be assumed:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = standard_date_time(type = "full")
#' )
#' ```
#' ```
#' #> [1] "Wednesday, July 4, 2018 at 10:05:00 PM GMT+00:00"
#' ```
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
#' The `standard_date()` function can be invoked in the `format` argument of the
#' `fdt()` function to help generate a locale-specific formatting string of a
#' certain 'type' of formatted date. The `type` value is a keyword that
#' represents precision and verbosity; the available keywords are `"short"` (the
#' default), `"medium"`, `"long"`, and `"full"`.
#'
#' @param type One of four standardized types for the resulting date that range
#'   in precision and verbosity. These are `"short"` (the default), `"medium"`,
#'   `"long"`, and `"full"`.
#'
#' @return A vector of class `date_time_pattern`.
#'
#' @section Examples:
#'
#' With an input datetime of `"2018-07-04 22:05(America/Vancouver)"`, we can
#' format as a date in a standardized way with `standard_date()` providing the
#' correct formatting string. This function is invoked in the `format` argument
#' of `fdt()`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date(type = "full")
#' )
#' ```
#' ```
#' #> [1] "Wednesday, July 4, 2018"
#' ```
#'
#' The locale can be changed and we don't have to worry about the particulars
#' of the formatting string (they are standardized across locales).
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date(type = "full"),
#'   locale = fdt_locales_lst$nl
#' )
#' ```
#' ```
#' #> [1] "woensdag 4 juli 2018"
#' ```
#'
#' We can use different `type` values to control the output date string. The
#' default is `"short"`.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date()
#' )
#' ```
#' ```
#' #> [1] "7/4/18"
#' ```
#'
#' After that, it's `"medium"`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date(type = "medium")
#' )
#' ```
#' ```
#' #> [1] "Jul 4, 2018"
#' ```
#'
#' Then, `"long"`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date(type = "long")
#' )
#' ```
#' ```
#' #> [1] "July 4, 2018"
#' ```
#'
#' And finally up to `"full"`, which was demonstrated in the first example.
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
#' The `standard_time()` function can be invoked in the `format` argument of the
#' `fdt()` function to help generate a locale-specific formatting string of a
#' certain 'type' of formatted time. The `type` value is a keyword that
#' represents precision and verbosity; the available keywords are `"short"` (the
#' default), `"medium"`, `"long"`, and `"full"`.
#'
#' @param type One of four standardized types for the resulting time that range
#'   in precision and verbosity. These are `"short"` (the default), `"medium"`,
#'   `"long"`, and `"full"`.
#'
#' @return A vector of class `date_time_pattern`.
#'
#' @section Examples:
#'
#' With an input datetime of `"2018-07-04 22:05(America/Vancouver)"`, we can
#' format as a time in a standardized way with `standard_time()` providing the
#' correct formatting string. This function is invoked in the `format` argument
#' of `fdt()`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_time(type = "full")
#' )
#' ```
#' ```
#' #> [1] "10:05:00 PM Pacific Daylight Time"
#' ```
#'
#' The locale can be changed and we don't have to worry about the particulars
#' of the formatting string (they are standardized across locales).
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_time(type = "full"),
#'   locale = fdt_locales_lst$nl
#' )
#' ```
#' ```
#' #> [1] "22:05:00 Pacific-zomertijd"
#' ```
#'
#' We can use different `type` values to control the output date string. The
#' default is `"short"`.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_time()
#' )
#' ```
#' ```
#' #> [1] "10:05 PM"
#' ```
#'
#' After that, it's `"medium"`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_time(type = "medium")
#' )
#' ```
#' ```
#' #> [1] "10:05:00 PM"
#' ```
#'
#' Then, `"long"`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_time(type = "long")
#' )
#' ```
#' ```
#' #> [1] "10:05:00 PM PDT"
#' ```
#'
#' And finally up to `"full"`, which was demonstrated in the first example.
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
