
# Era // abbreviated (G..GGG)
dt_G <- function(input, locale = NULL) {
  "G"
}

# Era // wide
dt_GGGG <- function(input, locale = NULL) {
  "GGGG"
}

# Era // narrow
dt_GGGGG <- function(input, locale = NULL) {
  "GGGGG"
}

# Full year
dt_y <- function(input) {
  as.character(lubridate::year(input))
}

# Abbreviated year (2 digit)
dt_yy <- function(input) {
  substring(as.character(lubridate::year(input)), 3, 4)
}

# Full year with minimum zero padding (yyy -> 2, yyyy -> 3, etc.)
dt_yyy_plus <- function(input, length) {
  "yyy_plus"
}

# Full year (week in year calendar)
dt_Y <- function(input) {
  "Y"
}

# Abbreviated year (2 digit) (week in year calendar)
dt_YY <- function(input) {
  "YY"
}

# Full year with minimum zero padding (yyy -> 2, yyyy -> 3, etc.)
# (week in year calendar)
dt_YYY_plus <- function(input, length) {
  "YYY_plus"
}

# Extended year (numeric)
dt_u <- function(input) {
  "u"
}

# Cyclic year name (abbreviated) U..UUU
dt_U <- function(input) {
  "U"
}

# Cyclic year name (wide)
dt_UUUU <- function(input, locale = NULL) {
  "UUUU"
}

# Cyclic year name (narrow)
dt_UUUUU <- function(input, locale = NULL) {
  "UUUUU"
}

# Related Gregorian year (numeric)
dt_r_plus <- function(input, length) {
  "r_plus"
}

# Quarter (formatting), numeric (1 digit-wide)
dt_Q <- function(input) {
  "Q"
}

# Quarter (formatting), numeric (2 digit-wide, zero padded)
dt_QQ <- function(input) {
  "QQ"
}

# Quarter (formatting), abbreviated
dt_QQQ <- function(input, locale = NULL) {
  "QQQ"
}

# Quarter (formatting), wide
dt_QQQQ <- function(input, locale = NULL) {
  "QQQQ"
}

# Quarter (formatting), narrow
dt_QQQQQ <- function(input, locale = NULL) {
  "QQQQQ"
}

# Quarter (standalone), numeric (1 digit)
dt_q <- function(input) {
  "q"
}

# Quarter (standalone), numeric (2 digit, zero padded)
dt_qq <- function(input) {
  "qq"
}

# Quarter (standalone), abbreviated
dt_qqq <- function(input, locale = NULL) {
  "qqq"
}

# Quarter (standalone), wide
dt_qqqq <- function(input, locale = NULL) {
  "qqqq"
}

# Quarter (standalone), narrow
dt_qqqqq <- function(input, locale = NULL) {
  "qqqqq"
}

# Month (format), numeric form (1 digit) (e.g. "9")
dt_M <- function(input) {
  as.character(lubridate::month(input))
}

# Month (format), numeric form (2 digit, zero padded) (e.g. "09")
dt_MM <- function(input) {
  out <- lubridate::month(input)
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Month (format), abbreviated (e.g. "Sep")
dt_MMM <- function(input, locale = NULL) {

  i18n::cldr_dates(
    locale = locale,
    element = i18n::dates_elements$months_format_abbrev
  )[[lubridate::month(input)]]
}

# Month (format), full (e.g. "September")
dt_MMMM <- function(input, locale = NULL) {
  i18n::cldr_dates(
    locale = locale,
    element = i18n::dates_elements$months_format_wide
  )[[lubridate::month(input)]]
}

# Month (format), narrow (e.g. "S")
dt_MMMMM <- function(input, locale = NULL) {

  substr(
    i18n::cldr_dates(
      locale = locale,
      element = i18n::dates_elements$months_format_abbrev
    )[[lubridate::month(input)]],
    1, 1
  )
}

# Month (standalone), numeric form (1 digit) (e.g. "9")
dt_L <- function(input) {
  dt_M(input)
}

# Month (standalone), numeric form (2 digit, zero padded) (e.g. "09")
dt_LL <- function(input) {
  dt_MM(input)
}

# Month (standalone), abbreviated (e.g. "Sep")
dt_LLL <- function(input, locale = NULL) {
  "LLL"
}

# Month (standalone), full (e.g. "September")
dt_LLLL <- function(input, locale = NULL) {
  "LLLL"
}

# Month (standalone), narrow (e.g. "S")
dt_LLLLL <- function(input, locale = NULL) {
  "LLLLL"
}

# Week of year, numeric, 1-2 digits
dt_w <- function(input) {
  "w"
}

# Week of year, numeric, 2 digits zero-padded
dt_ww <- function(input) {
  "ww"
}

# Week of month, numeric, 1 digit
dt_W <- function(input) {
  "W"
}

# Day of month, numeric, 1-2 digits
dt_d <- function(input) {
  as.character(lubridate::day(input))
}

# Day of month, numeric, 2 digits zero padded
dt_dd <- function(input) {
  out <- lubridate::day(input)
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Day of year, numeric, 1-3 digits
dt_D <- function(input) {

  as.character(lubridate::yday(input))
}

# Day of year, numeric, 2-3 digits zero-padded
dt_DD <- function(input) {

  out <- lubridate::yday(input)
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Day of year, numeric, 3 digits zero-padded
dt_DDD <- function(input) {

  out <- lubridate::yday(input)
  if (out < 10) {
    out <- paste0("00", out)
  } else if (out < 100) {
    out <- paste0("0", out)
  }
  as.character(out)
}

# Day of week in month, numeric, 1 digit
dt_F <- function(input) {
  "F"
}

# Modified Julian Day, numeric
dt_g_plus <- function(input, length) {
  "g_plus"
}

# Day of Week Name // abbreviated (E..EEE) (e.g., "Tue")
dt_E <- function(input, locale = NULL) {

  # FIXME: should be abbreviated and not short (provides "Tu")
  i18n::cldr_dates(
    locale = locale,
    element = dates_elements$days_standalone_short
  )[[cldr_wkdays()[lubridate::wday(input, abbr = TRUE)]]]
}

# Day of Week Name // wide (e.g., "Tuesday")
dt_EEEE <- function(input, locale = NULL) {

  i18n::cldr_dates(
    locale = locale,
    element = dates_elements$days_standalone_wide
  )[[cldr_wkdays()[lubridate::wday(input, abbr = TRUE)]]]
}

# Day of Week Name // narrow (e.g., "T")
dt_EEEEE <- function(input, locale = NULL) {

  i18n::cldr_dates(
    locale = locale,
    element = dates_elements$days_standalone_narrow
  )[[cldr_wkdays()[lubridate::wday(input, abbr = TRUE)]]]
}

# Day of Week Name // short (e.g., "Tu")
dt_EEEEEE <- function(input, locale = NULL) {

  i18n::cldr_dates(
    locale = locale,
    element = dates_elements$days_standalone_short
  )[[cldr_wkdays()[lubridate::wday(input, abbr = TRUE)]]]
}

# Local Day of Week Name/Number // 1 digit
dt_e <- function(input, locale = NULL) {
  "e"
}

# Local Day of Week Name/Number // 2 digit, zero padded
dt_ee <- function(input, locale = NULL) {
  "ee"
}

# Local Day of Week Name/Number // abbreviated (e.g., "Tue")
dt_eee <- function(input, locale = NULL) {
  dt_E(input, locale)
}

# Local Day of Week Name/Number // wide (e.g., "Tuesday")
dt_eeee <- function(input, locale = NULL) {
  dt_EEEE(input, locale)
}

# Local Day of Week Name/Number // narrow (e.g., "T")
dt_eeeee <- function(input, locale = NULL) {
  dt_EEEEE(input, locale)
}

# Local Day of Week Name/Number // short (e.g., "Tu")
dt_eeeeee <- function(input, locale = NULL) {
  dt_EEEEEE(input, locale)
}

# Standalone Local Day of Week Name/Number // 1 digit
dt_c <- function(input, locale = NULL) {
  dt_e(input, locale)
}

# Standalone Local Day of Week Name/Number // 2 digit, zero padded
dt_cc <- function(input, locale = NULL) {
  dt_ee(input_locale)
}

# Standalone Local Day of Week Name/Number // abbreviated (e.g., "Tue")
dt_ccc <- function(input, locale = NULL) {
  "ccc"
}

# Standalone Local Day of Week Name/Number // wide (e.g., "Tuesday")
dt_cccc <- function(input, locale = NULL) {
  "cccc"
}

# Standalone Local Day of Week Name/Number // narrow (e.g., "T")
dt_ccccc <- function(input, locale = NULL) {
  "ccccc"
}

# Standalone Local Day of Week Name/Number // short (e.g., "Tu")
dt_cccccc <- function(input, locale = NULL) {
  "cccccc"
}

# Period: am, pm // abbreviated (a..aaa)
dt_a <- function(input, locale = NULL) {

  i18n::cldr_dates(
    locale = locale,
    element = dates_elements$dayperiods_format_abbrev
  )[[ifelse(lubridate::am(input), "am", "pm")]]
}

# Period: am, pm // wide
dt_aaaa <- function(input, locale = NULL) {

  i18n::cldr_dates(
    locale = locale,
    element = dates_elements$dayperiods_format_wide
  )[[ifelse(lubridate::am(input), "am", "pm")]]
}

# Period: am, pm // narrow
dt_aaaaa <- function(input, locale = NULL) {

  i18n::cldr_dates(
    locale = locale,
    element = dates_elements$dayperiods_format_narrow
  )[[ifelse(lubridate::am(input), "am", "pm")]]
}

# Period: am, pm, noon, midnight // abbreviated (b..bbb)
dt_b <- function(input, locale = NULL) {
  "b"
}

# Period: am, pm, noon, midnight // wide
dt_bbbb <- function(input, locale = NULL) {
  "bbbb"
}

# Period: am, pm, noon, midnight // narrow
dt_bbbbb <- function(input, locale = NULL) {
  "bbbbb"
}

# Flexible day periods // abbreviated (B..BBB)
dt_B <- function(input, locale = NULL) {
  "B"
}

# Flexible day periods // wide
dt_BBBB <- function(input, locale = NULL) {
  "BBBB"
}

# Flexible day periods // narrow
dt_BBBBB <- function(input, locale = NULL) {
  "BBBBB"
}

# Hour [1-12] // numeric, 1-2 digits
dt_h <- function(input) {
  out <- lubridate::hour(input)
  if (out > 12) out <- out - 12
  out <- out + 1
  as.character(out)
}

# Hour [1-12] // numeric, 2 digits, zero padded
dt_hh <- function(input) {

  out <- lubridate::hour(input)
  if (out > 12) out <- out - 12
  out <- out + 1
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Hour [0-23] // numeric, 1-2 digits
dt_H <- function(input) {
  as.character(lubridate::hour(input))
}

# Hour [0-23] // numeric, 2 digits, zero padded
dt_HH <- function(input) {
  out <- lubridate::hour(input)
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Hour [0-11] // numeric, 1-2 digits
dt_K <- function(input) {

  out <- lubridate::hour(input)
  if (out > 12) out <- out - 12
  as.character(out)
}

# Hour [0-11] // numeric, 2 digits, zero padded
dt_KK <- function(input) {

  out <- lubridate::hour(input)
  if (out > 12) out <- out - 12
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Hour [1-24] // numeric, 1-2 digits
dt_k <- function(input) {

  as.character(lubridate::hour(input) + 1L)
}

# Hour [1-24] // numeric, 2 digits, zero padded
dt_kk <- function(input) {

  out <- lubridate::hour(input) + 1L
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Hour (Input Skeleton Symbol) // numeric, 1-2 digits + abbrev day period
dt_j <- function(input) {
  "j"
}

# Hour (Input Skeleton Symbol) // numeric, 2 digits, zero padded + abbrev day period
dt_jj <- function(input) {
  "jj"
}

# Hour (Input Skeleton Symbol) // numeric, 1-2 digits + wide day period
dt_jjj <- function(input) {
  "jjj"
}

# Hour (Input Skeleton Symbol) // numeric, 2 digits, zero padded + wide day period
dt_jjjj <- function(input) {
  "jjjj"
}

# Hour (Input Skeleton Symbol) // numeric, 1-2 digits + narrow day period
dt_jjjjj <- function(input) {
  "jjjjj"
}

# Hour (Input Skeleton Symbol) // numeric, 2 digits, zero padded + narrow day period
dt_jjjjjj <- function(input) {
  "jjjjjj"
}

# Hour (Input Skeleton Symbol) // numeric, 2 digits, zero padded (no AM/PM)
dt_J <- function(input) {
  "J"
}

# Hour (Input Skeleton Symbol) // numeric, 1-2 digits (no AM/PM)
dt_JJ <- function(input) {
  "JJ"
}

# Hour (Input Skeleton Symbol) // numeric, 1-2 digits + abbrev day period (no AM/PM)
dt_C <- function(input) {
  "C"
}

# Hour (Input Skeleton Symbol) // numeric, 2 digits, zero padded + abbrev day period (no AM/PM)
dt_CC <- function(input) {
  "CC"
}

# Hour (Input Skeleton Symbol) // numeric, 1-2 digits + wide day period (no AM/PM)
dt_CCC <- function(input) {
  "CCC"
}

# Hour (Input Skeleton Symbol) // numeric, 2 digits, zero padded + wide day period (no AM/PM)
dt_CCCC <- function(input) {
  "CCCC"
}

# Hour (Input Skeleton Symbol) // numeric, 1-2 digits + narrow day period (no AM/PM)
dt_CCCCC <- function(input) {
  "CCCCC"
}

# Hour (Input Skeleton Symbol) // numeric, 2 digits, zero padded + narrow day period (no AM/PM)
dt_CCCCCC <- function(input) {
  "CCCCCC"
}

# Minute // numeric, 1-2 digits
dt_m <- function(input) {

  as.character(lubridate::minute(input))
}

# Minute // numeric, 2 digits, zero padded
dt_mm <- function(input) {

  out <- lubridate::minute(input)
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Second // numeric, 1-2 digits
dt_s <- function(input) {

  as.character(trunc(lubridate::second(input)))
}

# Second // numeric, 2 digits, zero padded
dt_ss <- function(input) {

  out <- lubridate::second(input)
  if (out < 10) out <- paste0("0", out)
  as.character(out)
}

# Fractional Second
dt_S_plus <- function(input, length) {
  "S_plus"
}

# Milliseconds in Day
dt_A_plus <- function(input, length) {
  "A_plus"
}

# TZ // short specific non-location format (e.g., "PDT") (z..zzz)
# Fallback to "O"
dt_z <- function(
    input,
    locale = NULL,
    tz_short_specific = NULL,
    tz_offset = NULL
) {

  if (is.null(tz_short_specific) & !is.null(tz_offset)) {
    out <-
      dt_O(input = input, locale = locale, tz_offset = tz_offset)
  }

  lubridate::tz(input)
}

# TZ // long specific non-location format (e.g., "Pacific Daylight Time")
# Fallback to "OOOO"
dt_zzzz <- function(input, locale = NULL) {
  "zzzz"
}

# TZ // short specific non-location format (e.g., "-0800") (Z..ZZZ)
# Equivalent to "xxxx"
dt_Z <- function(input, locale = NULL) {
  "Z"
}

# TZ // long localized GMT format (e.g., "GMT-8:00")
# Equivalent to "OOOO"
dt_ZZZZ <- function(input, locale = NULL) {
  "ZZZZ"
}

# TZ // ISO8601 extended format (e.g., "-08:00")
# Equivalent to "XXXXX"
dt_ZZZZZ <- function(input, locale = NULL) {
  "ZZZZZ"
}

# TZ // short localized GMT format (e.g., "GMT-8")
dt_O <- function(input, locale = NULL, tz_offset = NULL) {
  "O"
}

# TZ // long localized GMT format (e.g., "GMT-8:00")
dt_OOOO <- function(input, locale = NULL, tz_offset = NULL) {
  "OOOO"
}

# TZ // short generic non-location format (e.g., "PT")
dt_v <- function(input, locale = NULL) {
  "v"
}

# TZ // long generic non-location format (e.g., "Pacific Time")
dt_vvvv <- function(input, locale = NULL) {
  "vvvv"
}

# TZ // short time zone ID (e.g., "uslax")
dt_V <- function(input, locale = NULL) {
  "V"
}

# TZ // long time zone ID (e.g., "America/Los_Angeles")
dt_VV <- function(input, locale = NULL) {
  "VV"
}

# TZ // long time zone ID (e.g., "America/Los_Angeles")
dt_VVV <- function(input, locale = NULL) {
  "VVV"
}

# TZ // generic location format (e.g., "Los Angeles Time")
dt_VVVV <- function(input, locale = NULL) {
  "VVVV"
}

# TZ // ISO 8601 basic format (e.g., "-08"), optional minutes, uses "Z"
dt_X <- function(input, locale = NULL) {
  "X"
}

# TZ // ISO 8601 basic format (e.g., "-0800"), uses minutes, uses "Z"
dt_XX <- function(input, locale = NULL) {
  "XX"
}

# TZ // ISO 8601 extended format (e.g., "-08:00"), uses minutes, uses "Z"
dt_XXX <- function(input, locale = NULL) {
  "XXX"
}

# TZ // ISO 8601 basic format (e.g., "-0800"), uses minutes, optional seconds,
# uses "Z"
dt_XXXX <- function(input, locale = NULL) {
  "XXXX"
}

# TZ // ISO 8601 extended format (e.g., "-08:00"), uses minutes, optional seconds,
# uses "Z"
dt_XXXXX <- function(input, locale = NULL) {
  "XXXXX"
}

# TZ // ISO 8601 basic format (e.g., "-08"), optional minutes,
# doesn't use "Z"
dt_x <- function(input, locale = NULL) {
  "x"
}

# TZ // ISO 8601 basic format (e.g., "-0800"), uses minutes,
# doesn't use "Z"
dt_xx <- function(input, locale = NULL) {
  "xx"
}

# TZ // ISO 8601 extended format (e.g., "-08:00"), uses minutes,
# doesn't use "Z"
dt_xxx <- function(input, locale = NULL) {
  "xxx"
}

# TZ // ISO 8601 basic format (e.g., "-0800"), uses minutes, optional seconds,
# doesn't use "Z"
dt_xxxx <- function(input, locale = NULL) {
  "xxxx"
}

# TZ // ISO 8601 extended format (e.g., "-08:00"), uses minutes, optional seconds,
# doesn't use "Z"
dt_xxxxx <- function(input, locale = NULL) {
  "xxxxx"
}
