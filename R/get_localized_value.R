#' Get a localized value from the l18n tables.
#' @param type the localization category.
#' @param sections the section name within the localization category.
#' @param locale a locale ID. Examples include \code{"en_US"} for English
#' (United States) and \code{"fr_FR"} for French (France).
#' @return a localized value.
#' @noRd
get_localized_value <- function(type,
                                sections,
                                locale) {

  sections_ <- sections
  locale_ <- locale

  l18n_tbl <-
    switch(
      type,
      date_and_time_fields = date_and_time_fields,
      date_and_time_gregorian = date_and_time_gregorian,
      date_and_time_generic = date_and_time_generic,
      numbers_symbols = numbers_symbols,
      numbers_minimal_pairs = numbers_minimal_pairs,
      numbers_number_formatting_patterns = numbers_number_formatting_patterns,
      numbers_compact_decimal_formatting = numbers_compact_decimal_formatting,
      timezones_africa = timezones_africa,
      timezones_antarctica = timezones_antarctica,
      timezones_australasia = timezones_australasia,
      timezones_central_asia = timezones_central_asia,
      timezones_eastern_asia = timezones_eastern_asia,
      timezones_europe = timezones_europe,
      timezones_north_america = timezones_north_america,
      timezones_oceania = timezones_oceania,
      timezones_overrides = timezones_overrides,
      timezones_russia = timezones_russia,
      timezones_south_america = timezones_south_america,
      timezones_southeast_asia = timezones_southeast_asia,
      timezones_southern_asia = timezones_southern_asia,
      timezones_western_asia = timezones_western_asia)

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

  # Get a filtered table corresponding to the
  # specified section
  l18n_tbl_f <-
    subset(l18n_tbl, section %in% sections_)


  if (locale_ %in% l18n_tbl_f$locale) {
    l18n_tbl_f <-
      subset(l18n_tbl_f, locale == locale_)
  } else if (base_lang %in% l18n_tbl_f$locale) {
    l18n_tbl_f <-
      subset(l18n_tbl_f, locale == base_lang)
  } else if ("all_others" %in% l18n_tbl_f$locale) {
    l18n_tbl_f <-
      subset(l18n_tbl_f, locale == "all_others")
  } else {
    return(NA_character_)
  }

  # Get the values
  l18n_tbl_f[["value"]]
}
