
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

  rows <- which(dates$locale == locale)
  values <- dates[[rows, element]]
  values <- unlist(values, use.names = TRUE)

  names(values) <- sub("^value\\.", "", names(values))

  as.list(values)
}

# TODO Remove this when depending on R 4.4
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
