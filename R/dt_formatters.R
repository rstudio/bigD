
zero_pad_to_width <- function(value, width) {
  formatC(value, width = width, flag = "0", format = "d")
}

format_tz_offset_min_sec <- function(
    tz_offset,
    use_colon = TRUE,
    optional_min = TRUE,
    use_z = FALSE,
    hours_width = 2,
    prepend_with = NULL
) {

  if (tz_offset == 0 && use_z) {
    return("Z")
  }

  hours_val <- zero_pad_to_width(abs(trunc(tz_offset)), hours_width)
  minutes_val <- zero_pad_to_width(abs((tz_offset - trunc(tz_offset))) * 60, 2)

  paste0(
    if (!is.null(prepend_with)) prepend_with,
    ifelse(tz_offset >= 0, "+", "-"),
    hours_val,
    if ((optional_min && minutes_val == "00")) {
      ""
    } else {
      paste0(ifelse(use_colon, ":", ""), minutes_val)
    }
  )
}

format_yearweek <- function(input) {

  date_input <- as.Date(input, format = "%Y-%m-%d")
  day_input <- as.integer(format(date_input, format = "%d"))
  month_input <- as.integer(format(date_input, format = "%m"))

  yearweek_int <- as.integer(strftime(date_input, format = "%Y%V"))
  week_int <- as.integer(strftime(date_input, format = "%V"))

  yearweek_int_res <-
    ifelse(
      day_input > 7 & week_int == 1,
      yearweek_int + 100,
      ifelse(
        as.integer(format(date_input, format = "%m")) == 1 & week_int > 51,
        yearweek_int - 100,
        yearweek_int
      )
    )

  yearweek_int_res_char <- as.character(yearweek_int_res)

  week_part <- substr(yearweek_int_res_char, nchar(yearweek_int_res_char) - 1, nchar(yearweek_int_res_char))
  year_part <- substr(yearweek_int_res_char, 1, nchar(yearweek_int_res_char) - 2)

  paste0(year_part, "-W", week_part)
}

get_week_in_month <- function(input) {

  date_input <- as.Date(input, format = "%Y-%m-%d")

  week_number <- as.integer(format(date_input, format = "%U"))
  min_date_in_month <- as.Date(paste0(format(date_input, "%Y-%m"), "-01"))
  week_num_mininmum <- as.integer(format(min_date_in_month, format = "%U"))

  week_number - (week_num_mininmum - 1)
}

get_modified_julian_day <- function(input) {

  date_input <- as.Date(input, format = "%Y-%m-%d")
  day_input <- as.integer(format(date_input, format = "%d"))
  month_input <- as.integer(format(date_input, format = "%m"))
  year_input <- as.integer(format(date_input, format = "%Y"))

  if (month_input <= 2) {
    m_x <- 1
  } else {
    m_x <- 0
  }

  m_m <- (12 * m_x) + month_input
  y_x <- year_input - m_x
  y_y <- floor(0.01 * y_x)

  floor(365.25 * y_x) +
    floor(30.6001 * (1 + m_m)) +
    day_input +
    1720995 +
    (2 - (y_y - floor(0.25 * y_y))) -
    2400001
}

format_quarter <- function(input) {

  date_input <- as.Date(input, format = "%Y-%m-%d")

  month_input <- as.integer(format(date_input, format = "%m"))
  year_input <- as.integer(format(date_input, format = "%Y"))

  if (month_input < 4) {
    quarter <- 1
  } else if (month_input > 3 & month_input < 7) {
    quarter <- 2
  } else if (month_input > 6 & month_input < 10) {
    quarter <- 3
  } else if (month_input > 9) {
    quarter <- 4
  }

  paste0(year_input, "-Q", quarter)
}

# Era // abbreviated (G..GGG) (AD, variant: CE)
dt_G <- function(input, locale = NULL) {

  year <- as.integer(dt_Y(input = input))

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$eras_abbrev
  )[[if (year < 0) "0" else "1"]]
}

# Era // wide (Anno Domini, variant: Common Era)
dt_GGGG <- function(input, locale = NULL) {

  year <- as.integer(dt_Y(input = input))

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$eras_names
  )[[if (year < 0) "0" else "1"]]
}

# Era // narrow (A)
dt_GGGGG <- function(input, locale = NULL) {

  year <- as.integer(dt_Y(input = input))

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$eras_narrow
  )[[if (year < 0) "0" else "1"]]
}

# Calendar year
dt_y <- function(input) {
  format(input, format = "%Y")
}

# Abbreviated year (final 2 digits)
dt_yy <- function(input) {
  format(input, format = "%y")
}

# Full year with minimum zero padding (yyy -> 2, yyyy -> 3, etc.)
dt_yyy_plus <- function(input, length) {
  zero_pad_to_width(
    value = format(input, format = "%Y"),
    width = length
  )
}

# Calendar year (min 3-9 digits wide)
dt_yyy <- function(input) dt_yyy_plus(input = input, length = 3)
dt_yyyy <- function(input) dt_yyy_plus(input = input, length = 4)
dt_yyyyy <- function(input) dt_yyy_plus(input = input, length = 5)
dt_yyyyyy <- function(input) dt_yyy_plus(input = input, length = 6)
dt_yyyyyyy <- function(input) dt_yyy_plus(input = input, length = 7)
dt_yyyyyyyy <- function(input) dt_yyy_plus(input = input, length = 8)
dt_yyyyyyyyy <- function(input) dt_yyy_plus(input = input, length = 9)

# Full year (week in year calendar)
dt_Y <- function(input) {
  yearweek <- format_yearweek(input = input)
  unlist(strsplit(yearweek, "-W"))[[1]]
}

# Abbreviated year (2 digit) (week in year calendar)
dt_YY <- function(input) {
  year_str <- dt_Y(input = input)
  substr(year_str, nchar(year_str) - 1, nchar(year_str))
}

# Full year with minimum zero padding (yyy -> 2, yyyy -> 3, etc.)
# (week in year calendar)
dt_YYY_plus <- function(input, length) {
  zero_pad_to_width(
    value = as.integer(dt_Y(input = input)),
    width = length
  )
}

# Calendar year (week in year calendar, min 3-9 digits wide)
dt_YYY <- function(input) dt_YYY_plus(input = input, length = 3)
dt_YYYY <- function(input) dt_YYY_plus(input = input, length = 4)
dt_YYYYY <- function(input) dt_YYY_plus(input = input, length = 5)
dt_YYYYYY <- function(input) dt_YYY_plus(input = input, length = 6)
dt_YYYYYYY <- function(input) dt_YYY_plus(input = input, length = 7)
dt_YYYYYYYY <- function(input) dt_YYY_plus(input = input, length = 8)
dt_YYYYYYYYY <- function(input) dt_YYY_plus(input = input, length = 9)

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
  yearquarter <- format_quarter(input = input)
  unlist(strsplit(yearquarter, "-Q"))[[2]]
}

# Quarter (formatting), numeric (2 digit-wide, zero padded)
dt_QQ <- function(input) {

  yearquarter <- format_quarter(input = input)

  zero_pad_to_width(
    value = as.integer(unlist(strsplit(yearquarter, "-Q"))[[2]]),
    width = 2
  )
}

# Quarter (formatting), abbreviated
dt_QQQ <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q"))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_format_abbrev
  )[[quarter]]
}

# Quarter (formatting), wide
dt_QQQQ <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q"))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_format_wide
  )[[quarter]]
}

# Quarter (formatting), narrow
dt_QQQQQ <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q"))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_format_narrow
  )[[quarter]]
}

# Quarter (standalone), numeric (1 digit)
dt_q <- function(input) {
  dt_Q(input = input)
}

# Quarter (standalone), numeric (2 digit, zero padded)
dt_qq <- function(input) {
  dt_QQ(input = input)
}

# Quarter (standalone), abbreviated
dt_qqq <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q"))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_standalone_abbrev
  )[[quarter]]
}

# Quarter (standalone), wide
dt_qqqq <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q"))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_standalone_wide
  )[[quarter]]
}

# Quarter (standalone), narrow
dt_qqqqq <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q"))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_standalone_narrow
  )[[quarter]]
}

# Month (format), numeric form (1 digit) ("9")
dt_M <- function(input) {
  as.character(as.integer(format(input, format = "%m")))
}

# Month (format), numeric form (2 digit, zero padded) ("09")
dt_MM <- function(input) {
  format(input, format = "%m")
}

# Month (format), abbreviated ("Sep")
dt_MMM <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$months_format_abbrev
  )[[as.integer(format(input, format = "%m"))]]
}

# Month (format), full ("September")
dt_MMMM <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$months_format_wide
  )[[as.integer(format(input, format = "%m"))]]
}

# Month (format), narrow ("S")
dt_MMMMM <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$months_format_narrow
  )[[as.integer(format(input, format = "%m"))]]
}

# Month (standalone), numeric form (1 digit) ("9")
dt_L <- function(input) {
  dt_M(input)
}

# Month (standalone), numeric form (2 digit, zero padded) ("09")
dt_LL <- function(input) {
  dt_MM(input)
}

# Month (standalone), abbreviated ("Sep")
dt_LLL <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$months_standalone_abbrev
  )[[as.integer(format(input, format = "%m"))]]
}

# Month (standalone), full ("September")
dt_LLLL <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$months_standalone_wide
  )[[as.integer(format(input, format = "%m"))]]
}

# Month (standalone), narrow ("S")
dt_LLLLL <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$months_standalone_narrow
  )[[as.integer(format(input, format = "%m"))]]
}

# Week of year, numeric, 1-2 digits
dt_w <- function(input) {
  yearweek <- format_yearweek(input = input)
  as.character(as.integer(unlist(strsplit(yearweek, "-W"))[[2]]))
}

# Week of year, numeric, 2 digits zero-padded
dt_ww <- function(input) {

  yearweek <- format_yearweek(input = input)

  zero_pad_to_width(
    value = as.integer(unlist(strsplit(yearweek, "-W"))[[2]]),
    width = 2
  )
}

# Week of month, numeric, 1 digit
dt_W <- function(input) {
  as.character(get_week_in_month(input))
}

# Day of month, numeric, 1-2 digits
dt_d <- function(input) {
  as.character(as.integer(format(input, format = "%d")))
}

# Day of month, numeric, 2 digits zero padded
dt_dd <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%d")), width = 2)
}

# Day of year, numeric, 1-3 digits
dt_D <- function(input) {
  j_day <- as.integer(format(input, format = "%j"))
  zero_pad_to_width(value = j_day, width = 1)
}

# Day of year, numeric, 2-3 digits zero-padded
dt_DD <- function(input) {
  j_day <- as.integer(format(input, format = "%j"))
  zero_pad_to_width(value = j_day, width = 2)
}

# Day of year, numeric, 3 digits zero-padded
dt_DDD <- function(input) {
  j_day <- as.integer(format(input, format = "%j"))
  zero_pad_to_width(value = j_day, width = 3)
}

# Day of week in month, numeric, 1 digit ("2", as in the 2nd Wed in July")
dt_F <- function(input) {
  "F"
}

# Modified Julian Day, numeric ("2451334")
dt_g_plus <- function(input, length) {
  zero_pad_to_width(
    value = get_modified_julian_day(input = input),
    width = length
  )
}

# Calendar year (week in year calendar, min 3-9 digits wide)
dt_g <- function(input) dt_g_plus(input = input, length = 1)
dt_gg <- function(input) dt_g_plus(input = input, length = 2)
dt_ggg <- function(input) dt_g_plus(input = input, length = 3)
dt_gggg <- function(input) dt_g_plus(input = input, length = 4)
dt_ggggg <- function(input) dt_g_plus(input = input, length = 5)
dt_gggggg <- function(input) dt_g_plus(input = input, length = 6)
dt_ggggggg <- function(input) dt_g_plus(input = input, length = 7)
dt_gggggggg <- function(input) dt_g_plus(input = input, length = 8)
dt_ggggggggg <- function(input) dt_g_plus(input = input, length = 9)

# Day of Week Name // abbreviated (E..EEE) ("Tue")
dt_E <- function(input, locale = NULL) {

  dow <- as.integer(format(input, format = "%w")) + 1L

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$days_standalone_abbrev
  )[[cldr_wkdays()[dow]]]
}

# Day of Week Name // wide ("Tuesday")
dt_EEEE <- function(input, locale = NULL) {

  dow <- as.integer(format(input, format = "%w")) + 1L

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$days_standalone_wide
  )[[cldr_wkdays()[dow]]]
}

# Day of Week Name // narrow ("T")
dt_EEEEE <- function(input, locale = NULL) {

  dow <- as.integer(format(input, format = "%w")) + 1L

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$days_standalone_narrow
  )[[cldr_wkdays()[dow]]]
}

# Day of Week Name // short ("Tu")
dt_EEEEEE <- function(input, locale = NULL) {

  dow <- as.integer(format(input, format = "%w")) + 1L

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$days_standalone_short
  )[[cldr_wkdays()[dow]]]
}

# Local Day of Week Name/Number // 1 digit
dt_e <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "e"
}

# Local Day of Week Name/Number // 2 digit, zero padded
dt_ee <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "ee"
}

# Local Day of Week Name/Number // abbreviated ("Tue")
dt_eee <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "eee"
}

# Local Day of Week Name/Number // wide ("Tuesday")
dt_eeee <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "eeee"
}

# Local Day of Week Name/Number // narrow ("T")
dt_eeeee <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "eeeee"
}

# Local Day of Week Name/Number // short ("Tu")
dt_eeeeee <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "eeeeee"
}

# Standalone Local Day of Week Name/Number // 1 digit
dt_c <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "c"
}

# Standalone Local Day of Week Name/Number // 2 digit, zero padded
dt_cc <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "cc"
}

# Standalone Local Day of Week Name/Number // abbreviated ("Tue")
dt_ccc <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "ccc"
}

# Standalone Local Day of Week Name/Number // wide ("Tuesday")
dt_cccc <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "cccc"
}

# Standalone Local Day of Week Name/Number // narrow ("T")
dt_ccccc <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "ccccc"
}

# Standalone Local Day of Week Name/Number // short ("Tu")
dt_cccccc <- function(input, locale = NULL) {

  # TODO: get starting day of the week for locale

  "cccccc"
}

# Period: am, pm // abbreviated (a..aaa)
dt_a <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$dayperiods_format_abbrev
  )[[format(input, format = "%p")]]
}

# Period: am, pm // wide
dt_aaaa <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$dayperiods_format_wide
  )[[format(input, format = "%p")]]
}

# Period: am, pm // narrow
dt_aaaaa <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$dayperiods_format_narrow
  )[[format(input, format = "%p")]]
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
  zero_pad_to_width(value = as.integer(format(input, format = "%I")), width = 1)
}

# Hour [1-12] // numeric, 2 digits, zero padded
dt_hh <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%I")), width = 2)
}

# Hour [0-23] // numeric, 1-2 digits
dt_H <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%H")), width = 1)
}

# Hour [0-23] // numeric, 2 digits, zero padded
dt_HH <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%H")), width = 2)
}

# Hour [0-11] // numeric, 1-2 digits
dt_K <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%I")) - 1, width = 1)
}

# Hour [0-11] // numeric, 2 digits, zero padded
dt_KK <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%I")) - 1, width = 2)
}

# Hour [1-24] // numeric, 1-2 digits
dt_k <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%H")) + 1, width = 1)
}

# Hour [1-24] // numeric, 2 digits, zero padded
dt_kk <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%H")) + 1, width = 2)
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
  zero_pad_to_width(value = as.integer(format(input, format = "%M")), width = 1)
}

# Minute // numeric, 2 digits, zero padded
dt_mm <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%M")), width = 2)
}

# Second // numeric, 1-2 digits
dt_s <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%S")), width = 1)
}

# Second // numeric, 2 digits, zero padded
dt_ss <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%S")), width = 2)
}

# Fractional Second
dt_S_plus <- function(input, length) {
  "S_plus"
}

# Milliseconds in Day
dt_A_plus <- function(input, length) {
  "A_plus"
}

# TZ // short specific non-location format ("PDT") (z..zzz)
# Fallback to "O"
dt_z <- function(input, tz_info, locale = NULL) {

  tz_short_specific <- tz_info$tz_short_specific
  tz_offset <- tz_info$tz_offset

  # If the short specific non-location format tz is present, return that
  if (!is.na(tz_short_specific)) {
    return(tz_short_specific)
  }

  # If we don't have the former tz type but have an offset, use the short
  # localized GMT format
  if (is.na(tz_short_specific) && !is.na(tz_offset)) {
    out <-
      dt_O(
        input = input,
        tz_info = tz_info,
        locale = locale
      )
    return(out)
  }

  # If we don't have either, return an empty string`
  ""
}

# TZ // long specific non-location format ("Pacific Daylight Time") (Z..ZZZ)
# Fallback to "OOOO"
dt_zzzz <- function(input, tz_info, locale = NULL) {

  tz_long_specific <- tz_info$tz_long_specific

  if (is.na(tz_long_specific)) {
    return(dt_OOOO(input = input, tz_info = tz_info, locale = locale))
  }

  tz_long_specific
}

# TZ // short specific non-location format ("-0800") (Z..ZZZ)
# Equivalent to "xxxx"
dt_Z <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = FALSE,
    optional_min = FALSE,
    use_z = FALSE
  )
}

# TZ // long localized GMT format ("GMT-8:00")
# Equivalent to "OOOO"
dt_ZZZZ <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = TRUE,
    optional_min = FALSE,
    use_z = FALSE,
    hours_width = 1,
    prepend_with = "GMT"
  )
}

# TZ // ISO8601 extended format ("-08:00")
# Equivalent to "XXXXX"
dt_ZZZZZ <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = TRUE,
    optional_min = FALSE,
    use_z = TRUE
  )
}

# TZ // short localized GMT format ("GMT-8")
dt_O <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = TRUE,
    optional_min = TRUE,
    use_z = FALSE,
    hours_width = 1,
    prepend_with = "GMT"
  )
}

# TZ // long localized GMT format ("GMT-08:00")
dt_OOOO <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = TRUE,
    optional_min = FALSE,
    use_z = FALSE,
    hours_width = 2,
    prepend_with = "GMT"
  )
}

# TZ // short generic non-location format ("PT")
#
# Where that is unavailable, falls back to the generic location
# format ("VVVV"), then the short localized GMT format as the
# final fallback.
dt_v <- function(input, tz_info, locale = NULL) {

  long_tzid <- tz_info$long_tzid

  tz_generic_non_location_short <-
    get_tz_non_location(
      long_tzid = long_tzid,
      locale = locale,
      short_long = "short",
      type = "generic"
    )

  if (is.na(tz_generic_non_location_short)) {
    return(dt_VVVV(input = input, tz_info = tz_info, locale = locale))
  }

  tz_generic_non_location_short
}

# TZ // long generic non-location format ("Pacific Time")
#
# Where that is unavailable, falls back to generic location
# format ("VVVV").
dt_vvvv <- function(input, tz_info, locale = NULL) {

  long_tzid <- tz_info$long_tzid

  tz_generic_non_location_long <-
    get_tz_non_location(
      long_tzid = long_tzid,
      locale = locale,
      short_long = "long",
      type = "generic"
    )

  if (is.na(tz_generic_non_location_long)) {
    return(dt_VVVV(input = input, tz_info = tz_info, locale = locale))
  }

  tz_generic_non_location_long
}

# TZ // short time zone ID ("uslax")
dt_V <- function(input, tz_info, locale = NULL) {

  long_tzid <- tz_info$long_tzid

  bcp_id <- get_tz_bcp_id(long_tzid = long_tzid)

  if (is.na(bcp_id)) {
    return("unk")
  }

  bcp_id
}

# TZ // long time zone ID ("America/Los_Angeles")
dt_VV <- function(input, tz_info, locale = NULL) {

  long_tzid <- tz_info$long_tzid

  # If a long time zone ID is available, return it
  if (!is.na(long_tzid)) {
    return(long_tzid)
  }

  # Otherwise return an empty string
  ""
}

# TZ // exemplar city locaation ("Los_Angeles")
#
# The exemplar city (location) for the time zone. Where that is
# unavailable, the localized exemplar city name for the special
# zone `Etc/Unknown` is used as the fallback (for example, "Unknown City").
dt_VVV <- function(input, tz_info, locale = NULL) {

  long_tzid <- tz_info$long_tzid

  # TODO: Validate `long_tzid`

  # create the `get_localized_examplar_city()` function
  examplar_city <-
    get_localized_exemplar_city(long_tzid = long_tzid, locale = locale)

  examplar_city
}

# TZ // generic location format ("Los Angeles Time")
#
# Where that is unavailable, falls back to the long localized
# GMT format ("OOOO"; Note: Fallback is only necessary with a
# GMT-style Time Zone ID, like Etc/GMT-830.)
# This is especially useful when presenting possible timezone
# choices for user selection, since the naming is more uniform
# than the "v" format.
dt_VVVV <- function(input, tz_info, locale = NULL) {

  long_tzid <- tz_info$long_tzid

  # TODO: Validate `long_tzid`

  # Get the localized variant of the exemplar city
  exemplar_city_localized <-
    get_localized_exemplar_city(
      long_tzid = long_tzid,
      locale = locale,
      yield_unknown = FALSE
    )

  if (is.na(exemplar_city_localized)) {
    return(dt_OOOO(input = input, tz_info = tz_info, locale = locale))
  }

  tz_generic_location_pattern <-
    tz_formats[tz_formats$locale == locale, ][["region_format"]]

  gsub("{0}", exemplar_city_localized, tz_generic_location_pattern, fixed = TRUE)
}

# TZ // ISO 8601 basic format ("-08"), optional minutes, uses "Z"
dt_X <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = FALSE,
    optional_min = TRUE,
    use_z = TRUE
  )
}

# TZ // ISO 8601 basic format ("-0800"), uses minutes, uses "Z"
dt_XX <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = FALSE,
    optional_min = FALSE,
    use_z = TRUE
  )
}

# TZ // ISO 8601 extended format ("-08:00"), uses minutes, uses "Z"
dt_XXX <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = TRUE,
    optional_min = FALSE,
    use_z = TRUE
  )
}

# TZ // ISO 8601 basic format ("-0800"), uses minutes, optional seconds,
# uses "Z"
dt_XXXX <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = FALSE,
    optional_min = FALSE,
    use_z = TRUE
  )
}

# TZ // ISO 8601 extended format ("-08:00"), uses minutes, optional seconds,
# uses "Z"
dt_XXXXX <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = TRUE,
    optional_min = FALSE,
    use_z = TRUE
  )
}

# TZ // ISO 8601 basic format ("-08"), optional minutes,
# doesn't use "Z"
dt_x <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = FALSE,
    optional_min = TRUE,
    use_z = FALSE
  )
}

# TZ // ISO 8601 basic format ("-0800"), uses minutes,
# doesn't use "Z"
dt_xx <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = FALSE,
    optional_min = FALSE,
    use_z = FALSE
  )
}

# TZ // ISO 8601 extended format ("-08:00"), uses minutes,
# doesn't use "Z"
dt_xxx <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = TRUE,
    optional_min = FALSE,
    use_z = FALSE
  )
}

# TZ // ISO 8601 basic format ("-0800"), uses minutes, optional seconds,
# doesn't use "Z"
dt_xxxx <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = FALSE,
    optional_min = FALSE,
    use_z = FALSE
  )
}

# TZ // ISO 8601 extended format ("-08:00"), uses minutes, optional seconds,
# doesn't use "Z"
dt_xxxxx <- function(input, tz_info, locale = NULL) {

  tz_offset <- tz_info$tz_offset
  if (is.na(tz_offset)) tz_offset <- 0

  format_tz_offset_min_sec(
    tz_offset = tz_offset,
    use_colon = TRUE,
    optional_min = FALSE,
    use_z = FALSE
  )
}
