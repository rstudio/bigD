
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
    "yMMMM",
    "yMMMd",
    "yMMMEd",
    "GyMMMd",
    "GyMMMEd",
    "yM",
    "Md",
    "MEd",
    "MMMd",
    "MMMEd",
    "MMMMd",
    "GyMMM",
    #"yQQQ",
    #"yQQQQ",
    "Gy",
    "y",
    "M",
    "MMM",
    "d",
    "Ed",
    "E")
}

ftf_12_types <- function() {

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
    "hmv")
}

ftf_24_types <- function() {

  c(
    "Hms",
    "Hm",
    "H",
    "EHm",
    "EHms",
    "Hmsv",
    "Hmv",
    "ms")
}

date_time_combine_types <- function() {

  c(
    "short",
    "medium",
    "long",
    "full")
}

str_rev <- function(x) {

  nc <- nchar(x)
  paste(substring(x, nc:1, nc:1), collapse = "")
}

split_str_by_index <- function(target, index) {

  index <- sort(index)

  substr(
    rep(target, length(index) + 1),
    start = c(1, index),
    stop = c(index -1, nchar(target)))
}

interleave <- function(v1, v2) {

  ord_1 <- 2 * (1:length(v1)) - 1
  ord_2 <- 2 * (1:length(v2))

  c(v1, v2)[order(c(ord_1, ord_2))]
}

insert_str <- function(target, insert, index) {

  insert <- insert[order(index)]
  index <- sort(index)

  paste(
    interleave(
      split_str_by_index(target, index), insert),
    collapse = "")
}

get_insertion_sequence <- function(number, grouping) {

  number <- strsplit(as.character(number), "\\.")[[1]][1]

  nchar_n <- nchar(number)

  x <- 1

  repeat {

    pattern <- rep(grouping, x) %>% cumsum() + 1

    if (max(pattern) >= nchar_n) {

      pattern <- pattern[pattern <= nchar_n]

      break
    } else {
      x <- x + 1
    }
  }

  pattern
}

# Date-time formatting

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

