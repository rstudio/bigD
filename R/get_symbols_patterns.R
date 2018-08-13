#' Get the locale-specific decimal pattern
#' @noRd
get_dec_pattern <- function(locale) {

  get_localized_value(
    type = "numbers_number_formatting_patterns",
    section = "standard_patterns_standard-decimal",
    locale = locale)
}

#' Get the locale-specific currency pattern
#' @noRd
get_currency_pattern <- function(locale) {

  get_localized_value(
    type = "numbers_number_formatting_patterns",
    section = "standard_patterns_standard-currency",
    locale = locale)
}

#' Get the locale-specific accounting currency pattern
#' @noRd
get_acct_currency_pattern <- function(locale) {

  get_localized_value(
    type = "numbers_number_formatting_patterns",
    section = "standard_patterns_accounting-currency",
    locale = locale)
}

#' Get the locale-specific standard date pattern
#' @noRd
get_standard_date_pattern <- function(locale,
                                      width = c("short", "medium", "long", "full")) {

  get_localized_value(
    type = "date_and_time_generic",
    section = paste0("formats_standard_date_formats_", width),
    locale = locale)
}

#' Get the locale-specific standard time pattern
#' @noRd
get_standard_time_pattern <- function(locale,
                                      width = c("short", "medium", "long", "full")) {

  get_localized_value(
    type = "date_and_time_gregorian",
    section = paste0("formats_standard_time_formats_", width),
    locale = locale)
}

#' Get the locale-specific standard date-time combining pattern
#' @noRd
get_standard_date_time_combining_pattern <- function(locale,
                                                     width = c("short", "medium", "long", "full")) {

  get_localized_value(
    type = "date_and_time_gregorian",
    section = paste0("formats_standard_date_time_combination_formats_", width),
    locale = locale)
}

# currency_name_table <- function() {
#
# }
#
# get_currency_symbol <- function(name, locale, type = c("symbol", "narrow")) {
#
# }

#' Get the locale-specific percent pattern
#' @noRd
get_percent_pattern <- function(locale) {

  get_localized_value(
    type = "numbers_number_formatting_patterns",
    section = "standard_patterns_standard-percent",
    locale = locale)
}

#' Get the locale-specific decimal mark symbol
#' @noRd
get_dec_mark <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_decimal",
    locale = locale)
}

#' Get the locale-specific grouping separator symbol
#' @noRd
get_sep_mark <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_group",
    locale = locale)
}

#' Get the locale-specific plus sign symbol
#' @noRd
get_plus_sign <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_plussign",
    locale = locale)
}

#' Get the locale-specific minus sign symbol
#' @noRd
get_minus_sign <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_minussign",
    locale = locale)
}

#' Get the locale-specific percent sign symbol
#' @noRd
get_percent_sign <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_percentsign",
    locale = locale)
}

#' Get the locale-specific per mille sign symbol
#' @noRd
get_permille_sign <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "symbols_permille",
    locale = locale)
}

#' Get the locale-specific time separator symbol
#' @noRd
get_time_sep <- function(locale) {

  get_localized_value(
    type = "numbers_symbols",
    section = "time_symbols_time separator",
    locale = locale)
}
