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

  if (use_z && tz_offset == 0) {
    return("Z")
  }

  hours_val <- zero_pad_to_width(abs(trunc(tz_offset)), hours_width)
  minutes_val <- zero_pad_to_width(abs((tz_offset - trunc(tz_offset))) * 60, 2)

  paste0(
    prepend_with, # can be NULL (will not print)
    ifelse(tz_offset >= 0, "+", "-"),
    hours_val,
    if (optional_min && minutes_val == "00") {
      ""
    } else if (use_colon) {
      paste0(":", minutes_val)
    } else {
      minutes_val
    }
  )
}

get_iso_week <- function(input) {
  as.integer(strftime(as.Date(input, format = "%Y-%m-%d"), format = "%V"))
}

format_yearweek <- function(input) {

  date_input <- as.Date(input, format = "%Y-%m-%d")
  day_input <- as.integer(format(date_input, format = "%d"))
  month_input <- as.integer(format(date_input, format = "%m"))

  yearweek_int <- as.integer(strftime(date_input, format = "%Y%V"))
  week_int <- get_iso_week(input = date_input)

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

format_fractional_seconds <- function(input, digits) {

  frac_s <- gsub("0.", "", as.character(as.POSIXlt(input)$sec %% 1), fixed = TRUE)

  if (frac_s == "0") {
    return(strrep("0", digits))
  }

  n_char <- nchar(frac_s)

  if (n_char > 3) {
    frac_s <- substr(frac_s, 1, 3)
  }

  if (digits < 3) {
    frac_s <- substr(frac_s, 1, digits)
  }

  if (digits > 3) {
    frac_s <- paste0(frac_s, strrep("0", digits - 3))
  }

  frac_s
}

get_milliseconds_in_day <- function(input) {

  input_lt <- as.POSIXlt(input)

  # Get the number of milliseconds elapsed in day
  as.integer(
    (
      (
        (input_lt$hour * 3600) +
          (input_lt$min * 60)
      ) * 1000
    ) +
      trunc(input_lt$sec * 1000, digits = 0)
  )
}

get_locale_territory <- function(locale) {

  locale_has_territory <- grepl("^[a-zA-Z_-]+?(-|_)?([A-Z]{2}|001|419|150)", locale)

  if (locale_has_territory) {

    territory <- gsub(".*([A-Z]{2}|001|419|150).*", "\\1", locale)
    return(territory)
  }

  default_locale_name <-
    default_locales[default_locales$base_locale == gsub("_", "-", locale, fixed = TRUE), "default_locale"]

  if (length(default_locale_name) == 0L) {

    default_locale_name <-
      default_locales[startsWith(default_locales$base_locale, locale), "default_locale"][1]
  }

  territory <- gsub(".*([A-Z]{2}|001|419|150).*", "\\1", default_locale_name)

  if (is.na(territory)) {
    territory <- "001"
  }

  territory
}

get_week_in_month <- function(input, locale) {

  date_input <- as.Date(input, format = "%Y-%m-%d")

  territory <- get_locale_territory(locale = locale)

  start_of_week_territory <- start_of_week[start_of_week$territory == territory, "day_of_week"]

  if (length(start_of_week_territory) == 0L) {
    start_of_week <- "mon"
  } else {
    start_of_week <- start_of_week_territory[[1]]
  }

  if (start_of_week == "mon") {
    week_number <- get_iso_week(input = input)
  } else {
    week_number <- as.integer(format(date_input, format = "%U"))
  }

  min_date_in_month <- as.Date(paste0(format(date_input, "%Y-%m"), "-01"))
  week_num_mininmum <- as.integer(format(min_date_in_month, format = "%U"))

  week_number - (week_num_mininmum - 1)
}

get_dow_n_in_month <- function(input) {

  date_input <- as.Date(input, format = "%Y-%m-%d")
  day_input <- as.integer(format(date_input, format = "%d"))

  length(seq(from = day_input, to = 1, by = -7))
}

zero_weekday <- function(input) {
  as.integer((as.integer(format(input, "%w")) + 6) %% 7 + 1L)
}

zero_thursday <- function(date) {
  date - zero_weekday(input = as.Date(date)) + 3L
}

week_date_as_datetime <- function(input_str) {

  year <- as.integer(substr(input_str, 1, 4))
  week <- as.integer(substr(input_str, 6, 7))

  if (nchar(input_str) == 8) {
    weekday <- as.integer(substr(input_str, 8, 8))
  } else {
    weekday <- 1L
  }

  january_fourth <- as.Date(paste(year, "01", "04", sep = "-"), tz = "UTC")
  first_thursday <- zero_thursday(date = january_fourth)
  nearest_thursday <- first_thursday + 7 * (week - 1)
  final_date <- nearest_thursday - 4 + weekday + 1
  as.POSIXct(as.character(final_date), tz = "UTC")
}

month_day_as_datetime <- function(input_str) {

  month_str <- substr(input_str, 1, 2)
  day_str <- substr(input_str, 3, 4)

  date_now <- as.character(Sys.Date())
  year_str <- substr(date_now, 1, 4)

  as.POSIXct(paste(year_str, month_str, day_str, sep = "-"), tz = "UTC")
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

get_flexible_day_period <- function(input, locale) {

  locale_in_day_periods_tbl <- locale %in% day_periods[["locale"]]

  # Verify that the supplied locale is defined within the `day_periods`
  # table (use the base locale if necessary); return NA if not found
  if (!locale_in_day_periods_tbl) {

    locale <- sub("^([a-z]*).*", "\\1", locale)

    if (!locale %in% day_periods[["locale"]]) {
      return(NA_character_)
    }
  }

  input_lt <- as.POSIXlt(input)

  time_str <-
    paste0(
      zero_pad_to_width(
        value = input_lt$hour,
        width = 2
      ),
      ":",
      zero_pad_to_width(
        value = input_lt$min,
        width = 2
      )
    )

  day_periods_locale <- day_periods[day_periods$locale == locale, ]

  if (time_str == "00:00" && "00:00" %in% day_periods_locale$at) {

    period <-
      day_periods_locale[
        !is.na(day_periods_locale$at) &
          day_periods_locale$at == "00:00",
        "period",
        drop = TRUE
      ]

  } else if (time_str == "12:00" && "12:00" %in% day_periods_locale$at) {

    period <-
      day_periods_locale[
        !is.na(day_periods_locale$at) &
          day_periods_locale$at == "12:00", "period", drop = TRUE]

  } else {

    day_periods_locale_from_to <-
      day_periods_locale[
        !is.na(day_periods_locale$from), c("period", "from", "to")
      ]

    to_next_day_idx <-
      which(day_periods_locale_from_to$to < day_periods_locale_from_to$from)

    if (length(to_next_day_idx) == 1) {

      row_1 <- row_2 <- day_periods_locale_from_to[to_next_day_idx, ]

      row_1$to <- "24:00"
      row_2$from <- "00:00"

      day_periods_locale_from_to <-
        rbind(
          day_periods_locale_from_to[-to_next_day_idx, ],
          row_1,
          row_2
        )
    }

    period <-
      day_periods_locale_from_to[
        day_periods_locale_from_to$from <= time_str &
          time_str < day_periods_locale_from_to$to, "period", drop = TRUE]
  }

  period
}

get_noon_midnight_period <- function(input, locale) {

  locale_in_day_periods_tbl <- locale %in% day_periods[["locale"]]

  # Verify that the supplied locale is defined within the `day_periods`
  # table (use the base locale if necessary); return NA if not found
  if (!locale_in_day_periods_tbl) {

    # Modify locale
    locale <- sub("^([a-z]*).*", "\\1", locale)

    if (!locale %in% day_periods[["locale"]]) {
      return(NA_character_)
    }
  }

  input_lt <- as.POSIXlt(input, tz = "UTC")

  time_str <-
    paste0(
      zero_pad_to_width(
        value = input_lt$hour,
        width = 2
      ),
      ":",
      zero_pad_to_width(
        value = input_lt$min,
        width = 2
      )
    )

  day_periods_locale <- day_periods[day_periods$locale == locale, ]

  if (time_str == "00:00" && "00:00" %in% day_periods_locale$at) {

    period <-
      day_periods_locale[
        !is.na(day_periods_locale$at) &
          day_periods_locale$at == "00:00", "period", drop = TRUE]

  } else if (time_str == "12:00" && "12:00" %in% day_periods_locale$at) {

    period <-
      day_periods_locale[
        !is.na(day_periods_locale$at) &
          day_periods_locale$at == "12:00", "period", drop = TRUE]

  } else {

    period <- NA_character_
  }

  period
}

format_quarter <- function(input) {

  date_input <- as.Date(input, format = "%Y-%m-%d")

  month_input <- as.integer(format(date_input, format = "%m"))
  year_input <- as.integer(format(date_input, format = "%Y"))

  if (month_input < 4L) {
    quarter <- 1
  } else if (month_input >= 4L && month_input < 7L) {
    quarter <- 2
  } else if (month_input >= 7L && month_input < 10L) {
    quarter <- 3
  } else if (month_input >= 10L) {
    quarter <- 4
  }

  paste0(year_input, "-Q", quarter)
}
