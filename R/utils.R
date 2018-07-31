
cldr_wkdays <- function() {
  c("sun", "mon", "tue", "wed", "thu", "fri", "sat")
}

cldr_months <- function() {
  c("jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec")
}

braced <- function(value) {
  paste0("{", value, "}")
}

substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

fdf_types <- function() {

  c(
    "yMd",
    "yMEd",
    "yMMM",
    "GyMMMd",
    "GyMMMEd",
    "yM",
    "Md",
    "MEd",
    "MMMd",
    "MMMMd",
    "GyMMM",
    "Gy",
    "y",
    "M",
    "MMM",
    "d",
    "Ed",
    "E")
}


l_fmt_EEEEEE <- function(locale) {

  EEEEEE <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "days_short_formatting_",
        cldr_wkdays()),
      locale = locale)

  if (all(is.na(EEEEEE))) {

    EEEEEE <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "days_short_standalone_",
          cldr_wkdays()),
        locale = locale)
  }

  EEEEEE
}

l_fmt_EEEEE <- function(locale) {

  EEEEE <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "days_narrow_formatting_",
        cldr_wkdays()),
      locale = locale)

  if (all(is.na(EEEEE))) {

    EEEEE <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "days_narrow_standalone_",
          cldr_wkdays()),
        locale = locale)
  }

  EEEEE
}

l_fmt_EEEE <- function(locale) {

  EEEE <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "days_wide_formatting_",
        cldr_wkdays()),
      locale = locale)

  if (all(is.na(EEEE))) {

    EEEE <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "days_wide_standalone_",
          cldr_wkdays()),
        locale = locale)

  }

  EEEE
}

l_fmt_EEE <- function(locale) {

  EEE <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "days_abbreviated_formatting_",
        cldr_wkdays()),
      locale = locale)

  if (all(is.na(EEE))) {

    EEE <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "days_abbreviated_standalone_",
          cldr_wkdays()),
        locale = locale)

  }

  EEE
}

l_fmt_cccccc <- function(locale) {

  cccccc <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "days_short_standalone_",
        cldr_wkdays()),
      locale = locale)

  if (all(is.na(cccccc))) {

    cccccc <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "days_short_formatting_",
          cldr_wkdays()),
        locale = locale)
  }

  cccccc
}

l_fmt_ccccc <- function(locale) {

  ccccc <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "days_narrow_standalone_",
        cldr_wkdays()),
      locale = locale)

  if (all(is.na(ccccc))) {

    ccccc <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "days_narrow_formatting_",
          cldr_wkdays()),
        locale = locale)
  }

  ccccc
}

l_fmt_cccc <- function(locale) {

  cccc <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "days_wide_standalone_",
        cldr_wkdays()),
      locale = locale)

  if (all(is.na(cccc))) {

    cccc <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "days_wide_formatting_",
          cldr_wkdays()),
        locale = locale)
  }

  cccc
}

l_fmt_ccc <- function(locale) {

  ccc <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "days_abbreviated_standalone_",
        cldr_wkdays()),
      locale = locale)

  if (all(is.na(ccc))) {

    ccc <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "days_abbreviated_formatting_",
          cldr_wkdays()),
        locale = locale)
  }

  ccc
}

l_fmt_GGGGG <- function(locale) {

  GGGGG <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = c(
        "eras_narrow_0", "eras_narrow_1"),
      locale = locale)

  if (all(is.na(GGGGG))) {

    GGGGG <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = c(
          "eras_abbreviated_0", "eras_abbreviated_1"),
        locale = locale)
  }

  GGGGG
}

l_fmt_GGGG <- function(locale) {

  get_localized_value(
    type = "date_and_time_gregorian",
    sections = c(
      "eras_wide_0", "eras_wide_1"),
    locale = locale)
}

l_fmt_GGG <- function(locale) {

  get_localized_value(
    type = "date_and_time_gregorian",
    sections = c(
      "eras_abbreviated_0", "eras_abbreviated_1"),
    locale = locale)
}

l_fmt_MMMMM <- function(locale) {

  MMMMM <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "months_narrow_formatting_",
        cldr_months()),
      locale = locale)

  if (all(is.na(MMMMM))) {

    MMMMM <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "months_narrow_standalone_",
          cldr_months()),
        locale = locale)
  }

  MMMMM
}

l_fmt_MMMM <- function(locale) {

  MMMM <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "months_wide_formatting_",
        cldr_months()),
      locale = locale)

  if (all(is.na(MMMM))) {

    MMMM <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "months_wide_standalone_",
          cldr_months()),
        locale = locale)
  }

  MMMM
}

l_fmt_MMM <- function(locale) {

  MMM <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "months_abbreviated_formatting_",
        cldr_months()),
      locale = locale)

  if (all(is.na(MMM))) {

    MMM <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "months_abbreviated_standalone_",
          cldr_months()),
        locale = locale)
  }

  MMM
}

l_fmt_LLLLL <- function(locale) {

  LLLLL <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "months_narrow_standalone_",
        cldr_months()),
      locale = locale)

  if (all(is.na(LLLLL))) {

    LLLLL <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "months_narrow_formatting_",
          cldr_months()),
        locale = locale)
  }

  LLLLL
}

l_fmt_LLLL <- function(locale) {

  LLLL <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "months_wide_standalone_",
        cldr_months()),
      locale = locale)

  if (all(is.na(LLLL))) {

    LLLL <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "months_wide_formatting_",
          cldr_months()),
        locale = locale)
  }

  LLLL
}

l_fmt_LLL <- function(locale) {

  LLL <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = paste0(
        "months_abbreviated_standalone_",
        cldr_months()),
      locale = locale)

  if (all(is.na(LLL))) {

    LLL <-
      get_localized_value(
        type = "date_and_time_gregorian",
        sections = paste0(
          "months_abbreviated_formatting_",
          cldr_months()),
        locale = locale)
  }

  LLL
}

l_fmt_a <- function(locale) {

  am <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = "day_periods_abbreviated_formatting_am",
      locale = locale)

  pm <-
    get_localized_value(
      type = "date_and_time_gregorian",
      sections = "day_periods_abbreviated_formatting_pm",
      locale = locale)

  c(am, pm)
}

