#' Get a vector of all the short weekday names
#'
#' @description
#' The `names_wkdays()` function produces a vector of all short weekday names
#' used by the **bigD** package.
#'
#' @return
#' A character vector of short weekday names.
#'
#' @examples
#' # Let's get all the short weekday names with
#' # the `names_wkdays()` function
#' names_wkdays()
#'
#' @export
names_wkdays <- function() {
  c(
    "sun", "mon", "tue", "wed", "thu", "fri", "sat"
  )
}

#' Get a vector of all the short month names
#'
#' @description
#' The `names_months()` function produces a vector of all short month names
#' used by the **bigD** package.
#'
#' @return
#' A character vector of short month names.
#'
#' @examples
#' # Let's get all the short month names with
#' # the `names_months()` function
#' names_months()
#'
#' @export
names_months <- function() {
  c(
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec"
  )
}

#' Get a named vector of all first-day-of-the-week names for different regions
#'
#' @description
#' The `names_months()` function produces a vector of all short month names
#' used by the **bigD** package.
#'
#' @return
#' A character vector of short month names.
#'
#' @examples
#' # Let's get a vector of regions where the
#' # first day of the week is Saturday
#' names(first_day_of_week()[first_day_of_week() == "sat"])
#'
#' @export
first_day_of_week <- function() {
  c(
    "001" = "mon",
    "AD" = "mon",
    "AE" = "sat",
    "AF" = "sat",
    "AG" = "sun",
    "AI" = "mon",
    "AL" = "mon",
    "AM" = "mon",
    "AN" = "mon",
    "AR" = "mon",
    "AS" = "sun",
    "AT" = "mon",
    "AU" = "mon",
    "AX" = "mon",
    "AZ" = "mon",
    "BA" = "mon",
    "BD" = "sun",
    "BE" = "mon",
    "BG" = "mon",
    "BH" = "sat",
    "BM" = "mon",
    "BN" = "mon",
    "BR" = "sun",
    "BS" = "sun",
    "BT" = "sun",
    "BW" = "sun",
    "BY" = "mon",
    "BZ" = "sun",
    "CA" = "sun",
    "CH" = "mon",
    "CL" = "mon",
    "CM" = "mon",
    "CN" = "sun",
    "CO" = "sun",
    "CR" = "mon",
    "CY" = "mon",
    "CZ" = "mon",
    "DE" = "mon",
    "DJ" = "sat",
    "DK" = "mon",
    "DM" = "sun",
    "DO" = "sun",
    "DZ" = "sat",
    "EC" = "mon",
    "EE" = "mon",
    "EG" = "sat",
    "ES" = "mon",
    "ET" = "sun",
    "FI" = "mon",
    "FJ" = "mon",
    "FO" = "mon",
    "FR" = "mon",
    "GB" = "mon",
    "GE" = "mon",
    "GF" = "mon",
    "GP" = "mon",
    "GR" = "mon",
    "GT" = "sun",
    "GU" = "sun",
    "HK" = "sun",
    "HN" = "sun",
    "HR" = "mon",
    "HU" = "mon",
    "ID" = "sun",
    "IE" = "mon",
    "IL" = "sun",
    "IN" = "sun",
    "IQ" = "sat",
    "IR" = "sat",
    "IS" = "mon",
    "IT" = "mon",
    "JM" = "sun",
    "JO" = "sat",
    "JP" = "sun",
    "KE" = "sun",
    "KG" = "mon",
    "KH" = "sun",
    "KR" = "sun",
    "KW" = "sat",
    "KZ" = "mon",
    "LA" = "sun",
    "LB" = "mon",
    "LI" = "mon",
    "LK" = "mon",
    "LT" = "mon",
    "LU" = "mon",
    "LV" = "mon",
    "LY" = "sat",
    "MC" = "mon",
    "MD" = "mon",
    "ME" = "mon",
    "MH" = "sun",
    "MK" = "mon",
    "MM" = "sun",
    "MN" = "mon",
    "MO" = "sun",
    "MQ" = "mon",
    "MT" = "sun",
    "MV" = "fri",
    "MX" = "sun",
    "MY" = "mon",
    "MZ" = "sun",
    "NI" = "sun",
    "NL" = "mon",
    "NO" = "mon",
    "NP" = "sun",
    "NZ" = "mon",
    "OM" = "sat",
    "PA" = "sun",
    "PE" = "sun",
    "PH" = "sun",
    "PK" = "sun",
    "PL" = "mon",
    "PR" = "sun",
    "PT" = "sun",
    "PY" = "sun",
    "QA" = "sat",
    "RE" = "mon",
    "RO" = "mon",
    "RS" = "mon",
    "RU" = "mon",
    "SA" = "sun",
    "SD" = "sat",
    "SE" = "mon",
    "SG" = "sun",
    "SI" = "mon",
    "SK" = "mon",
    "SM" = "mon",
    "SV" = "sun",
    "SY" = "sat",
    "TH" = "sun",
    "TJ" = "mon",
    "TM" = "mon",
    "TR" = "mon",
    "TT" = "sun",
    "TW" = "sun",
    "UA" = "mon",
    "UM" = "sun",
    "US" = "sun",
    "UY" = "mon",
    "UZ" = "mon",
    "VA" = "mon",
    "VE" = "sun",
    "VI" = "sun",
    "VN" = "mon",
    "WS" = "sun",
    "XK" = "mon",
    "YE" = "sun",
    "ZA" = "sun",
    "ZW" = "sun"
  )
}
