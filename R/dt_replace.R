dt_replace <- function(dt, input_dt, dt_lett, locale, tz_info) {


  # Verify that there is no duplication in dt_lett
  stopifnot(anyDuplicated(dt_lett) == 0)

  # This means we can use sub() instead of gsub()

  if ("G" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{G}", dt_G(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{GG}", dt_G(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{GGG}", dt_G(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{GGGG}", dt_GGGG(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{GGGGG}", dt_GGGGG(input_dt, locale), dt, fixed = TRUE)
  }


  if ("y" %in% dt_lett) {
    # Like stringr::str_extract()
    pattern <- regmatches(dt, m = regexpr("\\{y+\\}", dt))
    # Pattern will be of shape "{y+}" and will call the appropriate function
    # For replacing the year in the final value
    # Will call dt_year$`{yy}`(input_dt) and replace the resulting value
    dt <- sub(pattern, dt_year[[pattern]](input_dt), dt, fixed = TRUE)
  }

  if ("Y" %in% dt_lett) {
    pattern <- regmatches(dt, m = regexpr("\\{Y+\\}", dt))
    dt <- sub(pattern, dt_year_full[[pattern]](input_dt), dt, fixed = TRUE)
  }

  if ("u" %in% dt_lett) {
    dt <- sub("{u}", dt_u(input_dt), dt, fixed = TRUE)
  }

  if ("U" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{U}", dt_U(input_dt), dt, fixed = TRUE)
    dt <- sub("{UU}", dt_U(input_dt), dt, fixed = TRUE)
    dt <- sub("{UUU}", dt_U(input_dt), dt, fixed = TRUE)
    dt <- sub("{UUUU}", dt_UUUU(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{UUUUU}", dt_UUUUU(input_dt, locale), dt, fixed = TRUE)
  }

  if ("Q" %in% dt_lett) {
    pattern <- regmatches(dt, m = regexpr("\\{Q+\\}", dt))
    dt <- sub(pattern, dt_quarter_full[[pattern]](input_dt, locale), dt, fixed = TRUE)
  }

  if ("q" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{q}", dt_q(input_dt), dt, fixed = TRUE)
    dt <- sub("{qq}", dt_qq(input_dt), dt, fixed = TRUE)
    dt <- sub("{qqq}", dt_qqq(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{qqqq}", dt_qqqq(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{qqqqq}", dt_qqqqq(input_dt, locale), dt, fixed = TRUE)
  }

  if ("M" %in% dt_lett) {
    # Like stringr::str_extract()
    pattern <- regmatches(dt, m = regexpr("\\{M+\\}", dt))
    # Pattern will be of shape "{y+}" and will call the appropriate function
    # For replacing the year in the final value
    # Will call dt_year$`{MM}`(input_dt) and replace the resulting value
    dt <- sub(pattern, dt_Month[[pattern]](input_dt, locale), dt, fixed = TRUE)
  }

  if ("L" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{L}", dt_L(input_dt), dt, fixed = TRUE)
    dt <- sub("{LL}", dt_LL(input_dt), dt, fixed = TRUE)
    dt <- sub("{LLL}", dt_LLL(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{LLLL}", dt_LLLL(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{LLLLL}", dt_LLLLL(input_dt, locale), dt, fixed = TRUE)
  }

  if ("w" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{w}", dt_w(input_dt), dt, fixed = TRUE)
    dt <- sub("{ww}", dt_ww(input_dt), dt, fixed = TRUE)
  }

  if ("W" %in% dt_lett) {
    dt <- sub("{W}", dt_W(input_dt, locale), dt, fixed = TRUE)
  }

  if ("d" %in% dt_lett) {
    pattern <- regmatches(dt, m = regexpr("\\{d+\\}", dt))
    dt <- sub(pattern, dt_day[[pattern]](input_dt), dt, fixed = TRUE)
  }

  if ("D" %in% dt_lett) {
    pattern <- regmatches(dt, m = regexpr("\\{D+\\}", dt))
    dt <- sub(pattern, dt_day_full[[pattern]](input_dt), dt, fixed = TRUE)
  }

  if ("F" %in% dt_lett) {
    dt <- sub("{F}", dt_F(input_dt), dt, fixed = TRUE)
  }

  if ("E" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{E}", dt_E(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{EE}", dt_E(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{EEE}", dt_E(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{EEEE}", dt_EEEE(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{EEEEE}", dt_EEEEE(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{EEEEEE}", dt_EEEEEE(input_dt, locale), dt, fixed = TRUE)
  }

  if ("e" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{e}", dt_e(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{ee}", dt_ee(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{eee}", dt_eee(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{eeee}", dt_eeee(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{eeeee}", dt_eeeee(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{eeeeee}", dt_eeeeee(input_dt, locale), dt, fixed = TRUE)
  }

  if ("c" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{c}", dt_c(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{cc}", dt_cc(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{ccc}", dt_ccc(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{cccc}", dt_cccc(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{ccccc}", dt_ccccc(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{cccccc}", dt_cccccc(input_dt, locale), dt, fixed = TRUE)
  }

  if ("a" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{a}", dt_a(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{aa}", dt_a(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{aaa}", dt_a(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{aaaa}", dt_aaaa(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{aaaaa}", dt_aaaaa(input_dt, locale), dt, fixed = TRUE)
  }

  if ("b" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{b}", dt_b(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{bb}", dt_b(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{bbb}", dt_b(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{bbbb}", dt_bbbb(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{bbbbb}", dt_bbbbb(input_dt, locale), dt, fixed = TRUE)
  }

  if ("B" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{B}", dt_B(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{BB}", dt_B(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{BBB}", dt_B(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{BBBB}", dt_BBBB(input_dt, locale), dt, fixed = TRUE)
    dt <- sub("{BBBBB}", dt_BBBBB(input_dt, locale), dt, fixed = TRUE)
  }

  if ("h" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{h}", dt_h(input_dt), dt, fixed = TRUE)
    dt <- sub("{hh}", dt_hh(input_dt), dt, fixed = TRUE)
  }

  if ("H" %in% dt_lett) {
    pattern <- regmatches(dt, m = regexpr("\\{H+\\}", dt))
    # Pattern will be of shape "{y+}" and will call the appropriate function
    # For replacing the year in the final value
    # Will call dt_year$`{MM}`(input_dt) and replace the resulting value
    dt <- sub(pattern, dt_hour_full[[pattern]](input_dt), dt, fixed = TRUE)
  }

  if ("K" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{K}", dt_K(input_dt), dt, fixed = TRUE)
    dt <- sub("{KK}", dt_KK(input_dt), dt, fixed = TRUE)
  }

  if ("k" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{k}", dt_k(input_dt), dt, fixed = TRUE)
    dt <- sub("{kk}", dt_kk(input_dt), dt, fixed = TRUE)
  }

  if ("m" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{m}", dt_m(input_dt), dt, fixed = TRUE)
    dt <- sub("{mm}", dt_mm(input_dt), dt, fixed = TRUE)
  }

  if ("s" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{s}", dt_s(input_dt), dt, fixed = TRUE)
    dt <- sub("{ss}", dt_ss(input_dt), dt, fixed = TRUE)
  }

  if ("S" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{S}", dt_S(input_dt), dt, fixed = TRUE)
    dt <- sub("{SS}", dt_SS(input_dt), dt, fixed = TRUE)
    dt <- sub("{SSS}", dt_SSS(input_dt), dt, fixed = TRUE)
    dt <- sub("{SSSS}", dt_SSSS(input_dt), dt, fixed = TRUE)
    dt <- sub("{SSSSS}", dt_SSSSS(input_dt), dt, fixed = TRUE)
    dt <- sub("{SSSSSS}", dt_SSSSSS(input_dt), dt, fixed = TRUE)
    dt <- sub("{SSSSSSS}", dt_SSSSSSS(input_dt), dt, fixed = TRUE)
    dt <- sub("{SSSSSSSS}", dt_SSSSSSSS(input_dt), dt, fixed = TRUE)
    dt <- sub("{SSSSSSSSS}", dt_SSSSSSSSS(input_dt), dt, fixed = TRUE)
  }

  if ("A" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{A}", dt_A(input_dt), dt, fixed = TRUE)
    dt <- sub("{AA}", dt_AA(input_dt), dt, fixed = TRUE)
    dt <- sub("{AAA}", dt_AAA(input_dt), dt, fixed = TRUE)
    dt <- sub("{AAAA}", dt_AAAA(input_dt), dt, fixed = TRUE)
    dt <- sub("{AAAAA}", dt_AAAAA(input_dt), dt, fixed = TRUE)
    dt <- sub("{AAAAAA}", dt_AAAAAA(input_dt), dt, fixed = TRUE)
    dt <- sub("{AAAAAAA}", dt_AAAAAAA(input_dt), dt, fixed = TRUE)
    dt <- sub("{AAAAAAAA}", dt_AAAAAAAA(input_dt), dt, fixed = TRUE)
    dt <- sub("{AAAAAAAAA}", dt_AAAAAAAAA(input_dt), dt, fixed = TRUE)
  }

  if ("z" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{z}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{zz}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{zzz}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{zzzz}", dt_zzzz(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("Z" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{Z}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{ZZ}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{ZZZ}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{ZZZZ}", dt_ZZZZ(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{ZZZZZ}", dt_ZZZZZ(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("O" %in% dt_lett) {
    dt <- sub("{O}", dt_O(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{OOOO}", dt_OOOO(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("v" %in% dt_lett) {
    dt <- sub("{v}", dt_v(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{vvvv}", dt_vvvv(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("V" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{V}", dt_V(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{VV}", dt_VV(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{VVV}", dt_VVV(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{VVVV}", dt_VVVV(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("X" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{X}", dt_X(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{XX}", dt_XX(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{XXX}", dt_XXX(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{XXXX}", dt_XXXX(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{XXXXX}", dt_XXXXX(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("x" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{x}", dt_x(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{xx}", dt_xx(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{xxx}", dt_xxx(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{xxxx}", dt_xxxx(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- sub("{xxxxx}", dt_xxxxx(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("g" %in% dt_lett) {
    # TODO extract the exact substitution to do to avoid problematic
    dt <- sub("{g}", dt_g(input_dt), dt, fixed = TRUE)
    dt <- sub("{gg}", dt_gg(input_dt), dt, fixed = TRUE)
    dt <- sub("{ggg}", dt_ggg(input_dt), dt, fixed = TRUE)
    dt <- sub("{gggg}", dt_gggg(input_dt), dt, fixed = TRUE)
    dt <- sub("{ggggg}", dt_ggggg(input_dt), dt, fixed = TRUE)
    dt <- sub("{gggggg}", dt_gggggg(input_dt), dt, fixed = TRUE)
    dt <- sub("{ggggggg}", dt_ggggggg(input_dt), dt, fixed = TRUE)
    dt <- sub("{gggggggg}", dt_gggggggg(input_dt), dt, fixed = TRUE)
    dt <- sub("{ggggggggg}", dt_ggggggggg(input_dt), dt, fixed = TRUE)
  }

  # Return modified output
  dt
}


# Era // abbreviated (G..GGG) (AD, variant: CE)
dt_G <- function(input, locale = NULL) {

  year <- as.integer(format(input, format = "%Y"))

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$eras_abbrev
  )[[if (year < 0) "0" else "1"]]
}

# Era // wide (Anno Domini, variant: Common Era)
dt_GGGG <- function(input, locale = NULL) {

  year <- as.integer(format(input, format = "%Y"))

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$eras_names
  )[[if (year < 0) "0" else "1"]]
}

# Era // narrow (A)
dt_GGGGG <- function(input, locale = NULL) {

  year <- as.integer(format(input, format = "%Y"))

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$eras_narrow
  )[[if (year < 0) "0" else "1"]]
}


dt_year <- list(
  # Calendar year
  `{y}` = function(input) {
    as.character(abs(as.integer(format(input, format = "%Y"))))
  },
  # Abbreviated year (final 2 digits)
  `{yy}` = function(input) {
    format(input, format = "%y")
  },
  # Calendar year (min 3-9 digits wide)
  `{yyy}` = function(input) dt_yyy_plus(input = input, length = 3),
  `{yyyy}` = function(input) dt_yyy_plus(input = input, length = 4),
  `{yyyyy}` = function(input) dt_yyy_plus(input = input, length = 5),
  `{yyyyyy}` = function(input) dt_yyy_plus(input = input, length = 6),
  `{yyyyyyy}` = function(input) dt_yyy_plus(input = input, length = 7),
  `{yyyyyyyy}` = function(input) dt_yyy_plus(input = input, length = 8),
  `{yyyyyyyyy}` = function(input) dt_yyy_plus(input = input, length = 9)
)

# Full year with minimum zero padding (yyy -> 2, yyyy -> 3, etc.)
dt_yyy_plus <- function(input, length) {
  zero_pad_to_width(
    value = abs(as.integer(format(input, format = "%Y"))),
    width = length
  )
}

dt_year_full <- list(
  # Full year (week in year calendar)
  `{Y}` = function(input) {
    yearweek <- format_yearweek(input = input)
    unlist(strsplit(yearweek, "-W", fixed = TRUE))[[1]]
  },
  # Abbreviated year (2 digit) (week in year calendar)
  `{YY}` = function(input) {
    year_str <- dt_Y(input = input)
    substr(year_str, nchar(year_str) - 1, nchar(year_str))
  },
  # Calendar year (week in year calendar, min 3-9 digits wide)
  # appear unused?
  `{YYY}` = function(input) dt_YYY_plus(input = input, length = 3),
  `{YYYY}` = function(input) dt_YYY_plus(input = input, length = 4),
  `{YYYYY}` = function(input) dt_YYY_plus(input = input, length = 5),
  `{YYYYYY}` = function(input) dt_YYY_plus(input = input, length = 6),
  `{YYYYYYY}` = function(input) dt_YYY_plus(input = input, length = 7),
  `{YYYYYYYY}` = function(input) dt_YYY_plus(input = input, length = 8),
  `{YYYYYYYYY}` = function(input) dt_YYY_plus(input = input, length = 9)
)

# Full year with minimum zero padding (yyy -> 2, yyyy -> 3, etc.)
# (week in year calendar)
dt_YYY_plus <- function(input, length) {
  zero_pad_to_width(
    value = as.integer(dt_year_full$`{Y}`(input = input)),
    width = length
  )
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


dt_quarter_full <- list(
  `{Q}` = function(input, ...) {
    # Quarter (formatting), numeric (1 digit-wide)
    yearquarter <- format_quarter(input = input)
    unlist(strsplit(yearquarter, "-Q", fixed = TRUE))[[2]]
  },
  # Quarter (formatting), numeric (2 digit-wide, zero padded)
  `{QQ}` = function(input, ...) {
    yearquarter <- format_quarter(input = input)

    zero_pad_to_width(
      value = as.integer(unlist(strsplit(yearquarter, "-Q", fixed = TRUE))[[2]]),
      width = 2
    )
  },
  # Quarter (formatting), abbreviated
  `{QQQ}` = function(input, locale = NULL) {
    yearquarter <- format_quarter(input = input)
    quarter <- as.integer(unlist(strsplit(yearquarter, "-Q", fixed = TRUE))[[2]])

    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$quarters_format_abbrev
    )[[quarter]]
  },
  # Quarter (formatting), wide
  `{QQQQ}` = function(input, locale = NULL) {
    yearquarter <- format_quarter(input = input)
    quarter <- as.integer(unlist(strsplit(yearquarter, "-Q", fixed = TRUE))[[2]])

    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$quarters_format_wide
    )[[quarter]]
  },
  # Quarter (formatting), narrow
  `{QQQQQ}` = function(input, locale = NULL) {
    yearquarter <- format_quarter(input = input)
    quarter <- as.integer(unlist(strsplit(yearquarter, "-Q", fixed = TRUE))[[2]])

    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$quarters_format_narrow
    )[[quarter]]
  }
)

# Quarter (standalone), numeric (1 digit)
dt_q <- function(input) {
  dt_quarter_full$`{Q}`(input = input)
}

# Quarter (standalone), numeric (2 digit, zero padded)
dt_qq <- function(input) {
  dt_quarter_full$`{QQ}`(input = input)
}

# Quarter (standalone), abbreviated
dt_qqq <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q", fixed = TRUE))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_standalone_abbrev
  )[[quarter]]
}

# Quarter (standalone), wide
dt_qqqq <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q", fixed = TRUE))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_standalone_wide
  )[[quarter]]
}

# Quarter (standalone), narrow
dt_qqqqq <- function(input, locale = NULL) {

  yearquarter <- format_quarter(input = input)
  quarter <- as.integer(unlist(strsplit(yearquarter, "-Q", fixed = TRUE))[[2]])

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$quarters_standalone_narrow
  )[[quarter]]
}

# List of function to transform "{MM}" to the correct output
dt_Month <- list(
  # Month (format), numeric form (1 digit) ("9")
  `{M}` = function(input, ...) {
    as.character(as.integer(format(input, format = "%m")))
  },
  # Month (format), numeric form (2 digit, zero padded) ("09")
  `{MM}` = function(input, ...) {
    format(input, format = "%m")
  },
  # Month (format), abbreviated ("Sep")
  `{MMM}` = function(input, locale = NULL) {
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$months_format_abbrev
    )[[as.integer(format(input, format = "%m"))]]
  },
  # Month (format), full ("September")
  `{MMMM}` = function(input, locale = NULL) {
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$months_format_wide
    )[[as.integer(format(input, format = "%m"))]]
  },
  # Month (format), narrow ("S")
  `{MMMMM}` = function(input, locale = NULL) {
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$months_format_narrow
    )[[as.integer(format(input, format = "%m"))]]
  }
)

# Month (standalone), numeric form (1 digit) ("9")
dt_L <- function(input) {
  dt_Month$`{M}`(input)
}

# Month (standalone), numeric form (2 digit, zero padded) ("09")
dt_LL <- function(input) {
  dt_Month$`{MM}`(input)
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
  as.character(as.integer(unlist(strsplit(yearweek, "-W", fixed = TRUE))[[2]]))
}

# Week of year, numeric, 2 digits zero-padded
dt_ww <- function(input) {

  yearweek <- format_yearweek(input = input)

  zero_pad_to_width(
    value = as.integer(unlist(strsplit(yearweek, "-W", fixed = TRUE))[[2]]),
    width = 2
  )
}

# Week of month, numeric, 1 digit
dt_W <- function(input, locale) {
  as.character(get_week_in_month(input = input, locale = locale))
}


dt_day <- list(
  # Day of month, numeric, 1-2 digits
  `{d}` = function(input) {
    as.character(as.integer(format(input, format = "%d")))
  },
  # Day of month, numeric, 2 digits zero padded
  `{dd}` =  function(input) {
    zero_pad_to_width(value = as.integer(format(input, format = "%d")), width = 2)
  }
)

dt_day_full <- list(
  # Day of year, numeric, 1-3 digits
  `{D}` = function(input) {
    j_day <- as.integer(format(input, format = "%j"))
    zero_pad_to_width(value = j_day, width = 1)
  },

  # Day of year, numeric, 2-3 digits zero-padded
  `{DD}` =  function(input) {
    j_day <- as.integer(format(input, format = "%j"))
    zero_pad_to_width(value = j_day, width = 2)
  },

  # Day of year, numeric, 3 digits zero-padded
  `{DDD}` = function(input) {
    j_day <- as.integer(format(input, format = "%j"))
    zero_pad_to_width(value = j_day, width = 3)
  }
)

# Day of week in month, numeric, 1 digit ("2", as in the 2nd Wed in July")
dt_F <- function(input) {
  as.character(get_dow_n_in_month(input = input))
}

# Modified Julian day, numeric ("2451334")
dt_g_plus <- function(input, length) {
  zero_pad_to_width(
    value = get_modified_julian_day(input = input),
    width = length
  )
}

# Modified Julian day (min 1-9 digits wide)
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
  )[[names_wkdays()[dow]]]
}

# Day of Week Name // wide ("Tuesday")
dt_EEEE <- function(input, locale = NULL) {

  dow <- as.integer(format(input, format = "%w")) + 1L

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$days_standalone_wide
  )[[names_wkdays()[dow]]]
}

# Day of Week Name // narrow ("T")
dt_EEEEE <- function(input, locale = NULL) {

  dow <- as.integer(format(input, format = "%w")) + 1L

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$days_standalone_narrow
  )[[names_wkdays()[dow]]]
}

# Day of Week Name // short ("Tu")
dt_EEEEEE <- function(input, locale = NULL) {

  dow <- as.integer(format(input, format = "%w")) + 1L

  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$days_standalone_short
  )[[names_wkdays()[dow]]]
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
  )[[tolower(format(input, format = "%p"))]]
}

# Period: am, pm // wide
dt_aaaa <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$dayperiods_format_wide
  )[[tolower(format(input, format = "%p"))]]
}

# Period: am, pm // narrow
dt_aaaaa <- function(input, locale = NULL) {
  cldr_dates_bigd(
    locale = locale,
    element = dates_elements_bigd$dayperiods_format_narrow
  )[[tolower(format(input, format = "%p"))]]
}

# Period: am, pm, noon, midnight // abbreviated (b..bbb)
dt_b <- function(input, locale = NULL) {

  period <- get_noon_midnight_period(input = input, locale = locale)

  if (is.na(period)) {
    return(dt_a(input = input, locale = locale))
  }

  day_periods_locale <-
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$dayperiods_format_abbrev
    )

  if (!(period %in% names(day_periods_locale))) {
    return(dt_a(input = input, locale = locale))
  }

  day_periods_locale[[period]]
}

# Period: am, pm, noon, midnight // wide
dt_bbbb <- function(input, locale = NULL) {

  period <- get_noon_midnight_period(input = input, locale = locale)

  if (is.na(period)) {
    return(dt_aaaa(input = input, locale = locale))
  }

  day_periods_locale <-
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$dayperiods_format_wide
    )

  if (!(period %in% names(day_periods_locale))) {
    return(dt_aaaa(input = input, locale = locale))
  }

  day_periods_locale[[period]]
}

# Period: am, pm, noon, midnight // narrow
dt_bbbbb <- function(input, locale = NULL) {

  period <- get_noon_midnight_period(input = input, locale = locale)

  if (is.na(period)) {
    return(dt_aaaaa(input = input, locale = locale))
  }

  day_periods_locale <-
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$dayperiods_format_narrow
    )

  if (!(period %in% names(day_periods_locale))) {
    return(dt_aaaaa(input = input, locale = locale))
  }

  day_periods_locale[[period]]
}

# Flexible day periods // abbreviated (B..BBB)
dt_B <- function(input, locale = NULL) {

  period <- get_flexible_day_period(input = input, locale = locale)

  if (is.na(period)) {
    return(dt_a(input = input, locale = locale))
  }

  day_periods_locale <-
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$dayperiods_format_abbrev
    )

  if (!(period %in% names(day_periods_locale))) {
    return(dt_a(input = input, locale = locale))
  }

  day_periods_locale[[period]]
}

# Flexible day periods // wide
dt_BBBB <- function(input, locale = NULL) {

  period <- get_flexible_day_period(input = input, locale = locale)

  if (is.na(period)) {
    return(dt_aaaa(input = input, locale = locale))
  }

  day_periods_locale <-
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$dayperiods_format_wide
    )

  if (!(period %in% names(day_periods_locale))) {
    return(dt_aaaa(input = input, locale = locale))
  }

  day_periods_locale[[period]]
}

# Flexible day periods // narrow
dt_BBBBB <- function(input, locale = NULL) {

  period <- get_flexible_day_period(input = input, locale = locale)

  if (is.na(period)) {
    return(dt_aaaaa(input = input, locale = locale))
  }

  day_periods_locale <-
    cldr_dates_bigd(
      locale = locale,
      element = dates_elements_bigd$dayperiods_format_narrow
    )

  if (!(period %in% names(day_periods_locale))) {
    return(dt_aaaaa(input = input, locale = locale))
  }

  day_periods_locale[[period]]
}

# Hour [1-12] // numeric, 1-2 digits
dt_h <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%I")), width = 1)
}

# Hour [1-12] // numeric, 2 digits, zero padded
dt_hh <- function(input) {
  zero_pad_to_width(value = as.integer(format(input, format = "%I")), width = 2)
}


dt_hour_full <- list(
  # Hour [0-23] // numeric, 1-2 digits
  `{H}` = function(input) {
    zero_pad_to_width(value = as.integer(format(input, format = "%H")), width = 1)
  },
  # Hour [0-23] // numeric, 2 digits, zero padded
  `{HH}` = function(input) {
    zero_pad_to_width(value = as.integer(format(input, format = "%H")), width = 2)
  }
)


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

# Fractional second
dt_S_plus <- function(input, length) {
  format_fractional_seconds(input = input, digits = length)
}

# Fractional second (min 1-9 digits wide)
dt_S <- function(input) dt_S_plus(input = input, length = 1)
dt_SS <- function(input) dt_S_plus(input = input, length = 2)
dt_SSS <- function(input) dt_S_plus(input = input, length = 3)
dt_SSSS <- function(input) dt_S_plus(input = input, length = 4)
dt_SSSSS <- function(input) dt_S_plus(input = input, length = 5)
dt_SSSSSS <- function(input) dt_S_plus(input = input, length = 6)
dt_SSSSSSS <- function(input) dt_S_plus(input = input, length = 7)
dt_SSSSSSSS <- function(input) dt_S_plus(input = input, length = 8)
dt_SSSSSSSSS <- function(input) dt_S_plus(input = input, length = 9)

# Milliseconds in day
dt_A_plus <- function(input, length) {
  zero_pad_to_width(
    value = get_milliseconds_in_day(input = input),
    width = length
  )
}

# Milliseconds in day (min 1-9 digits wide)
dt_A <- function(input) dt_A_plus(input = input, length = 1)
dt_AA <- function(input) dt_A_plus(input = input, length = 2)
dt_AAA <- function(input) dt_A_plus(input = input, length = 3)
dt_AAAA <- function(input) dt_A_plus(input = input, length = 4)
dt_AAAAA <- function(input) dt_A_plus(input = input, length = 5)
dt_AAAAAA <- function(input) dt_A_plus(input = input, length = 6)
dt_AAAAAAA <- function(input) dt_A_plus(input = input, length = 7)
dt_AAAAAAAA <- function(input) dt_A_plus(input = input, length = 8)
dt_AAAAAAAAA <- function(input) dt_A_plus(input = input, length = 9)

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

  if (is.na(tz_long_specific) || length(tz_long_specific) != 1) {
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

  tz_formats_i <- tz_formats[tz_formats$locale == locale, ]

  tz_offset <- tz_info$tz_offset

  if (is.na(tz_offset)) tz_offset <- 0

  if (tz_offset == 0) {
    return(tz_formats_i[["gmt_zero_format"]])
  }

  gmt_format <- tz_formats_i[["gmt_format"]]
  hour_format_combined <- tz_formats_i[["hour_format"]]

  if (tz_offset < 0) {
    hour_format <- unlist(strsplit(hour_format_combined, split = ";", fixed = TRUE))[2]
  } else {
    hour_format <- unlist(strsplit(hour_format_combined, split = ";", fixed = TRUE))[1]
  }

  hours_val <- zero_pad_to_width(abs(trunc(tz_offset)), 1)
  minutes_val <- zero_pad_to_width(abs((tz_offset - trunc(tz_offset))) * 60, 2)

  if (minutes_val == "00") {
    hour_format <- gsub("mm", "", hour_format, fixed = TRUE)
    hour_format <- gsub(":", "", hour_format, fixed = TRUE)
  }

  hours_minutes <- gsub("(H|HH)", hours_val, hour_format)
  hours_minutes <- gsub("mm", minutes_val, hours_minutes, fixed = TRUE)

  gsub("{0}", hours_minutes, gmt_format, fixed = TRUE)
}

# TZ // long localized GMT format ("GMT-08:00")
dt_OOOO <- function(input, tz_info, locale = NULL) {

  tz_formats_i <- tz_formats[tz_formats$locale == locale, ]

  tz_offset <- tz_info$tz_offset

  if (is.na(tz_offset)) tz_offset <- 0

  if (tz_offset == 0) {
    return(tz_formats_i[["gmt_zero_format"]])
  }

  gmt_format <- tz_formats_i[["gmt_format"]]
  hour_format_combined <- tz_formats_i[["hour_format"]]

  if (tz_offset < 0) {
    hour_format <- unlist(strsplit(hour_format_combined, split = ";", fixed = TRUE))[2]
  } else {
    hour_format <- unlist(strsplit(hour_format_combined, split = ";", fixed = TRUE))[1]
  }

  if (grepl("HH", hour_format, fixed = TRUE)) {
    hours_val <- zero_pad_to_width(abs(trunc(tz_offset)), 2)
  } else {
    hours_val <- zero_pad_to_width(abs(trunc(tz_offset)), 1)
  }

  minutes_val <- zero_pad_to_width(abs((tz_offset - trunc(tz_offset))) * 60, 2)

  hours_minutes <- gsub("(H|HH)", hours_val, hour_format)
  hours_minutes <- gsub("mm", minutes_val, hours_minutes, fixed = TRUE)

  gsub("{0}", hours_minutes, gmt_format, fixed = TRUE)
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

# TZ // exemplar city location ("Los_Angeles")
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
    tz_formats[tz_formats$locale == locale, "region_format"]

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
