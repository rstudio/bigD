
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

first_day_of_week <-
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

braced <- function(value) {
  paste0("{", value, "}")
}

substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

#
# Taken from the i18n package
#
dates_elements_bigd <-
  list(
    months_format_abbrev = "months_format_abbrev",
    months_format_narrow = "months_format_narrow",
    months_format_wide = "months_format_wide",
    months_standalone_abbrev = "months_standalone_abbrev",
    months_standalone_narrow = "months_standalone_narrow",
    months_standalone_wide = "months_standalone_wide",
    days_format_abbrev = "days_format_abbrev",
    days_format_narrow = "days_format_narrow",
    days_format_short = "days_format_short",
    days_format_wide = "days_format_wide",
    days_standalone_abbrev = "days_standalone_abbrev",
    days_standalone_narrow = "days_standalone_narrow",
    days_standalone_short = "days_standalone_short",
    days_standalone_wide = "days_standalone_wide",
    quarters_format_abbrev = "quarters_format_abbrev",
    quarters_format_narrow = "quarters_format_narrow",
    quarters_format_wide = "quarters_format_wide",
    quarters_standalone_abbrev = "quarters_standalone_abbrev",
    quarters_standalone_narrow = "quarters_standalone_narrow",
    quarters_standalone_wide = "quarters_standalone_wide",
    dayperiods_format_abbrev = "dayperiods_format_abbrev",
    dayperiods_format_narrow = "dayperiods_format_narrow",
    dayperiods_format_wide = "dayperiods_format_wide",
    dayperiods_standalone_abbrev = "dayperiods_standalone_abbrev",
    dayperiods_standalone_narrow = "dayperiods_standalone_narrow",
    dayperiods_standalone_wide = "dayperiods_standalone_wide",
    eras_abbrev = "eras_abbrev",
    eras_names = "eras_names",
    eras_narrow = "eras_narrow",
    date_formats = "date_formats",
    date_skeletons = "date_skeletons",
    time_formats = "time_formats",
    time_skeletons = "time_skeletons",
    date_time_patterns = "date_time_patterns",
    date_time_available_formats = "date_time_available_formats",
    date_time_append_items = "date_time_append_items",
    date_time_interval_formats = "date_time_interval_formats"
  )

#
# Taken from the i18n package
#
cldr_dates_bigd <- function(locale = "en", element) {

  values <- dates[dates$locale == locale, ][[element]]
  values <- unlist(values, use.names = TRUE)

  names(values) <- gsub("^value\\.", "", names(values))

  as.list(values)
}
