#' Get the locale-specific decimal pattern
#' @inheritParams fmt_date_time
#' @export
get_dec_pattern <- function(locale) {

  get_localized_value(
    type = "numbers_number_formatting_patterns",
    section = "standard_patterns_standard-decimal",
    locale = locale)
}

#' Get the locale-specific currency pattern
#' @inheritParams fmt_date_time
#' @export
get_currency_pattern <- function(locale) {

  get_localized_value(
    type = "numbers_number_formatting_patterns",
    section = "standard_patterns_standard-currency",
    locale = locale)
}

#' Get the locale-specific accounting currency pattern
#' @inheritParams fmt_date_time
#' @export
get_acct_currency_pattern <- function(locale) {

  get_localized_value(
    type = "numbers_number_formatting_patterns",
    section = "standard_patterns_accounting-currency",
    locale = locale)
}

#' Get the locale-specific standard date pattern
#' @inheritParams fmt_date_time
#' @param width one of four standardized widths for the output representation.
#'   These are either of \code{"short"}, \code{"medium"}, \code{"long"}, or
#'   \code{"full"}. Some locales will provide unique values for each of these
#'   options but it is often the case that the \code{"short"} and
#'   \code{"medium"} forms will be equivalent (as is the case with the
#'   \code{"long"} and the \code{"full"} forms) due to a lack of variation.
#' @export
get_standard_date_pattern <- function(locale,
                                      width = NULL) {

  if (is.null(width)) {
    width <- "full"
  }

  get_localized_value(
    type = "date_and_time_generic",
    section = paste0("formats_standard_date_formats_", width),
    locale = locale)
}

#' Get the locale-specific standard time pattern
#' @inheritParams fmt_date_time
#' @inheritParams get_standard_date_pattern
#' @export
get_standard_time_pattern <- function(locale,
                                      width = NULL) {

  if (is.null(width)) {
    width <- "full"
  }

  get_localized_value(
    type = "date_and_time_gregorian",
    section = paste0("formats_standard_time_formats_", width),
    locale = locale)
}

#' Get the locale-specific standard date-time combining pattern
#' @inheritParams fmt_date_time
#' @inheritParams get_standard_date_pattern
#' @export
get_standard_date_time_combining_pattern <- function(locale,
                                                     width = NULL) {

  if (is.null(width)) {
    width <- "full"
  }

  get_localized_value(
    type = "date_and_time_gregorian",
    section = paste0("formats_standard_date_time_combination_formats_", width),
    locale = locale)
}

#' Get the locale-specific form of a currency symbol
#' @inheritParams fmt_date_time
#' @param name the ISO 4217:2015 alphabetic code (three-letter) that defines a
#'   currency. Examples include \code{"USD"} for U.S. Dollars and \code{"JPY"}
#'   for Japanese Yen.
#' @param type the output representation of the currency symbol in the provided
#'   \code{locale}. This can be \code{"symbol"}, \code{"narrow"}, or
#'   \code{"variant"}.
#' @export
get_currency_symbol <- function(name,
                                locale,
                                type = NULL) {

  if (is.null(type)) {
    type <- "symbol"
  }

  name <- tolower(name)
  type_ <- type
  locale_ <- locale

  if (grepl("_", locale_)) {
    base_lang <- strsplit(locale_, "_")[[1]][1]
    if (grepl("_[A-Z][A-Z]", locale_)) {
      region <- gsub(".*?_([A-Z][A-Z])", "\\1", locale_)
    } else {
      region <- NA_character_
    }
  } else {
    base_lang <- locale_
  }

  if (!(name %in% currency_symbols$symbol)) {

    stop("The provided currency `name` is not in the list of currency codes.",
         call. = FALSE)
  }

  # Get a filtered table corresponding to the
  # currency `name` and representation `type`
  subset_tbl_f <-
    subset(currency_symbols, symbol == name & type == type_)

  # If the `variant` form of the currency symbol doesn't exist, then
  # switch to the `narrow` form
  if (type_ == "variant" & nrow(subset_tbl_f) == 0) {

    type_ <- "narrow"

    subset_tbl_f <-
      subset(currency_symbols, symbol == name & type == type_)
  }

  # If the `narrow` form of the currency symbol doesn't exist, then
  # switch to the `symbol` form
  if (type_ == "narrow" & nrow(subset_tbl_f) == 0) {

    type_ <- "symbol"

    subset_tbl_f <-
      subset(currency_symbols, symbol == name & type == type_)
  }

  if (locale_ %in% subset_tbl_f$locale) {
    subset_tbl_f <-
      subset(subset_tbl_f, locale == locale_)
  } else if (base_lang %in% subset_tbl_f$locale) {
    subset_tbl_f <-
      subset(subset_tbl_f, locale == base_lang)
  } else if ("all_others" %in% subset_tbl_f$locale) {
    subset_tbl_f <-
      subset(subset_tbl_f, locale == "all_others")
  } else {
    return(NA_character_)
  }

  # Get the value
  subset_tbl_f[["value"]]
}

#' Get the locale-specific percent pattern
#' @inheritParams fmt_date_time
#' @export
get_percent_pattern <- function(locale) {

  get_localized_value(
    type = "numbers_number_formatting_patterns",
    section = "standard_patterns_standard-percent",
    locale = locale)
}

#' Get the locale-specific decimal mark symbol
#' @inheritParams fmt_date_time
#' @export
get_dec_mark <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_decimal",
    locale = locale)
}

#' Get the locale-specific grouping separator symbol
#' @inheritParams fmt_date_time
#' @export
get_sep_mark <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_group",
    locale = locale)
}

#' Get the locale-specific plus sign symbol
#' @inheritParams fmt_date_time
#' @export
get_plus_sign <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_plussign",
    locale = locale)
}

#' Get the locale-specific minus sign symbol
#' @inheritParams fmt_date_time
#' @export
get_minus_sign <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_minussign",
    locale = locale)
}

#' Get the locale-specific percent sign symbol
#' @inheritParams fmt_date_time
#' @export
get_percent_sign <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_percentsign",
    locale = locale)
}

#' Get the locale-specific per mille sign symbol
#' @inheritParams fmt_date_time
#' @export
get_permille_sign <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_permille",
    locale = locale)
}

#' Get the locale-specific time separator symbol
#' @inheritParams fmt_date_time
#' @export
get_time_sep <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "time_symbols_time separator",
    locale = locale)
}
