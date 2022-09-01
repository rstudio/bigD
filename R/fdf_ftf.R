#' Get a vector of all flexible date types
#'
#' @description
#' The `flex_d_vec()` function produces a vector of all supported flexible date
#' types in the **bigD** package. These types are essentially identifiers for
#' classes of cross-locale date formatting, so, none of these should be used
#' directly in the `format` argument of the `fdt()` function (use the
#' [flex_d_lst] object for that).
#'
#' @return
#' A character vector of supported flexible date types.
#'
#' @export
flex_d_vec <- function() {

  c(
    "yMd",
    "yMEd",
    "yMMM",
    "yMMMM",
    "yMMMd",
    "yMMMEd",
    "GyMd",
    "GyMMMd",
    "GyMMMEd",
    "yM",
    "Md",
    "MEd",
    "MMMd",
    "MMMEd",
    "MMMMd",
    "GyMMM",
    "yQQQ",
    "yQQQQ",
    "Gy",
    "y",
    "M",
    "MMM",
    "d",
    "Ed",
    #"E",    # TODO: need to determine local day of week for this to work
    "MMMMW", # as "MMMMW-count-*" `one`, `other`, `few`, `many`, `two`, `zero`
    "yw"     # as "yw-*"          `one`, `other`, `few`, `many`, `two`, `zero`
  )
}

#' Get a vector of all 12-hour flexible time types
#'
#' @description
#' The `flex_t12_vec()` function produces a vector of all supported flexible
#' 12-hour time types in the **bigD** package. These types are essentially
#' identifiers for classes of cross-locale time formatting, so, none of these
#' should be used directly in the `format` argument of the `fdt()` function (use
#' the [flex_t12_lst] object for that).
#'
#' @return
#' A character vector of supported 12-hour flexible time types.
#'
#' @export
flex_t12_vec <- function() {

  c(
    "hms",
    "hm",
    "h",
    "Ehm",
    "Ehms",
    "EBhms",
    "Bhms",
    "EBhm",
    "Bhm",
    "Bh",
    "hmsv",
    "hmv"
  )
}

#' Get a vector of all 24-hour flexible time types
#'
#' @description
#' The `flex_t24_vec()` function produces a vector of all supported flexible
#' 24-hour time types in the **bigD** package. These types are essentially
#' identifiers for classes of cross-locale time formatting, so, none of these
#' should be used directly in the `format` argument of the `fdt()` function (use
#' the [flex_t24_lst] object for that).
#'
#' @return
#' A character vector of supported 24-hour flexible time types.
#'
#' @export
flex_t24_vec <- function() {

  c(
    "Hms",
    "Hm",
    "H",
    "EHm",
    "EHms",
    "Hmsv",
    "Hmv",
    "ms"
  )
}

#' A list of all flexible date types
#'
#' @description
#' The `flex_d_lst` object is a list of widely supported flexible date types.
#' Flexible date types are classes of date formatting which can be translated
#' across locales. There are `r length(flex_d_lst)` flexible date types in
#' `flex_d_lst`.
#'
#' @return A list where each element corresponds to a classifier for a flexible
#' date type.
#'
#' @section Examples:
#'
#' The `flex_d_lst` object can be incredibly useful when you need to get a
#' format for date formatting that works across all locales. You can avoid
#' typing errors by using this list and every flexible date type from this list
#' is guaranteed to work across all supported locales. In this example, we'll
#' use the `"yMMMEd"` flexible date type by accessing it from the `flex_d_lst`
#' object.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = flex_d_lst$yMMMEd,
#'   locale = "en"
#' )
#' ```
#' ```
#' #> [1] "Wed, Jul 4, 2018"
#' ```
#'
#' If we wanted this in a different locale, the locale-specific `format` pattern
#' behind the flexible date identifier would ensure consistency while moving to
#' that locale. Let's use the `fdt_locales_lst` object in the same spirit to
#' specify the French (Canada) locale.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = flex_d_lst$yMMMEd,
#'   locale = fdt_locales_lst$fr_CA
#' )
#' ```
#' ```
#' #> [1] "mer. 4 juill. 2018"
#' ```
#'
#' @export
flex_d_lst <-
  lapply(
    stats::setNames(as.list(flex_d_vec()), as.list(flex_d_vec())),
    FUN = function(x) {
      class(x) <- c("date_time_pattern", "flex_d")
      x
    }
  )

#' A list of all 12-hour flexible time types
#'
#' @description
#' The `flex_t12_lst` object is a list of the 12-hour flexible time types which
#' are widely supported. Flexible time types are classes of time formatting
#' which can be translated across locales. There are `r length(flex_t12_lst)`
#' flexible time types of the 12-hour variety in `flex_t12_lst`.
#'
#' @return A list where each element corresponds to a classifier for a 12-hour
#' flexible time type.
#'
#' @section Examples:
#'
#' The `flex_t12_lst` object can be incredibly useful when you need to get a
#' format for 12-hour time formatting that works across all locales. You can
#' avoid typing errors by using this list and every flexible time type from this
#' list is guaranteed to work across all supported locales. In this example,
#' we'll use the `"Ehms"` flexible time type by accessing it from the
#' `flex_t12_lst` object.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = flex_t12_lst$Bhms,
#'   locale = "en"
#' )
#' ```
#' ```
#' #> [1] "10:05:00 at night"
#' ```
#'
#' If we wanted this in a different locale, the locale-specific `format` pattern
#' behind the flexible date identifier would ensure consistency while moving to
#' that locale. Let's use the `fdt_locales_lst` object in the same spirit to
#' specify the German (Austria) locale.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = flex_t12_lst$Bhms,
#'   locale = fdt_locales_lst$de_AT
#' )
#' ```
#' ```
#' #> [1] "10:05:00 abends"
#' ```
#'
#' @export
flex_t12_lst <-
  lapply(
    stats::setNames(as.list(flex_t12_vec()), as.list(flex_t12_vec())),
    FUN = function(x) {
      class(x) <- c("date_time_pattern", "flex_t12")
      x
    }
  )

#' A list of all 24-hour flexible time types
#'
#' @description
#' The `flex_t24_lst` object is a list of the 24-hour flexible time types which
#' are widely supported. Flexible time types are classes of time formatting
#' which can be translated across locales. There are `r length(flex_t24_lst)`
#' flexible time types of the 24-hour variety in `flex_t24_lst`.
#'
#' @return A list where each element corresponds to a classifier for a 24-hour
#' flexible time type.
#'
#' @section Examples:
#'
#' The `flex_t24_lst` object can be incredibly useful when you need to get a
#' format for 24-hour time formatting that works across all locales. You can
#' avoid typing errors by using this list and every flexible time type from this
#' list is guaranteed to work across all supported locales. In this example,
#' we'll use the `"EHms"` flexible time type by accessing it from the
#' `flex_t24_lst` object.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = flex_t24_lst$EHms,
#'   locale = "en"
#' )
#' ```
#' ```
#' #> [1] "Wed 22:05:00"
#' ```
#'
#' If we wanted this in a different locale, the locale-specific `format` pattern
#' behind the flexible date identifier would ensure consistency while moving to
#' that locale. Let's use the `fdt_locales_lst` object in the same spirit to
#' specify the German locale.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = flex_t24_lst$EHms,
#'   locale = fdt_locales_lst$de
#' )
#' ```
#' ```
#' #> [1] "Mi, 22:05:00"
#' ```
#'
#' @export
flex_t24_lst <-
  lapply(
    stats::setNames(as.list(flex_t24_vec()), as.list(flex_t24_vec())),
    FUN = function(x) {
      class(x) <- c("date_time_pattern", "flex_t24")
      x
    }
  )
