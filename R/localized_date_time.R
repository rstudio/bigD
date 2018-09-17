#' Augment a pattern string with date and time parts.
#' @param date_input the date component of the input date-time.
#' @param time_input the time component of the input date-time.
#' @param subst_pattern a partial pattern string.
#' @param locale a locale ID. Examples include \code{"en_US"} for English
#'   (United States) and \code{"fr_FR"} for French (France).
#' @return an augmented pattern string.
#' @noRd
localized_date_time <- function(date_input = NULL,
                                time_input = NULL,
                                tz_offset = NULL,
                                iana_tz_name = NULL,
                                subst_pattern,
                                locale = NULL) {

  idx <- 1L
  replace_stack <- c()

  if (is.null(locale)) {
    locale <- "en"
  }

  if (!is.null(date_input)) {

    # Extract year, month, and day from the ISO input date
    year <- as.numeric(strsplit(date_input, "-")[[1]][[1]])
    month <- as.numeric(strsplit(date_input, "-")[[1]][[2]])
    day <- as.numeric(strsplit(date_input, "-")[[1]][[3]])
    dow <- as.numeric(strftime(date_input, "%u"))
    woy <- as.numeric(strftime(date_input, "%U"))

    # Padded variants of `month` and `day`
    month_p <- ifelse(month < 10, paste0("0", month), as.character(month))
    day_p <- ifelse(day < 10, paste0("0", day), as.character(day))
    woy_p <- ifelse(woy < 10, paste0("0", woy), as.character(woy))

    #
    # Replacement of date parts
    #

    # Replacement of `yyyy`
    if (grepl("yyyy", subst_pattern)) {
      subst_pattern <- gsub("yyyy", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(year))
      idx <- idx + 1L
    }

    # Replacement of `yy`
    if (grepl("yy", subst_pattern)) {
      subst_pattern <- gsub("yy", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, substr(as.character(year), 3, 4))
      idx <- idx + 1L
    }

    # Replacement of `y`
    if (grepl("y", subst_pattern)) {
      subst_pattern <- gsub("y", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(year))
      idx <- idx + 1L
    }

    # Replacement of `MMMMM`
    if (grepl("MMMMM", subst_pattern)) {
      MMMMM <- l_fmt_MMMMM(locale = locale)
      subst_pattern <- gsub("MMMMM", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, MMMMM[month])
      idx <- idx + 1L
    }

    # Replacement of `MMMM`
    if (grepl("MMMM", subst_pattern)) {
      MMMM <- l_fmt_MMMM(locale = locale)
      subst_pattern <- gsub("MMMM", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, MMMM[month])
      idx <- idx + 1L
    }

    # Replacement of `MMM`
    if (grepl("MMM", subst_pattern)) {
      MMM <- l_fmt_MMM(locale = locale)
      subst_pattern <- gsub("MMM", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, MMM[month])
      idx <- idx + 1L
    }

    # Replacement of `MM`
    if (grepl("MM", subst_pattern)) {
      subst_pattern <- gsub("MM", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, month_p)
      idx <- idx + 1L
    }

    # Replacement of `M`
    if (grepl("M", subst_pattern)) {
      subst_pattern <- gsub("M", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(month))
      idx <- idx + 1L
    }

    # Replacement of `dd`
    if (grepl("dd", subst_pattern)) {
      subst_pattern <- gsub("dd", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, day_p)
      idx <- idx + 1L
    }

    # Replacement of `d`
    if (grepl("d", subst_pattern)) {
      subst_pattern <- gsub("d", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(day))
      idx <- idx + 1L
    }

    # Replacement of `EEEEEE`
    if (grepl("EEEEEE", subst_pattern)) {
      EEEEEE <- l_fmt_EEEEEE(locale = locale)
      subst_pattern <- gsub("EEEEEE", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, EEEEEE[dow])
      idx <- idx + 1L
    }

    # Replacement of `EEEEE`
    if (grepl("EEEEE", subst_pattern)) {
      EEEEE <- l_fmt_EEEEE(locale = locale)
      subst_pattern <- gsub("EEEEE", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, EEEEE[dow])
      idx <- idx + 1L
    }

    # Replacement of `EEEE`
    if (grepl("EEEE", subst_pattern)) {
      EEEE <- l_fmt_EEEE(locale = locale)
      subst_pattern <- gsub("EEEE", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, EEEE[dow])
      idx <- idx + 1L
    }

    # Replacement of `EEE`
    if (grepl("EEE", subst_pattern)) {
      EEE <- l_fmt_EEE(locale = locale)
      subst_pattern <- gsub("EEE", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, EEE[dow])
      idx <- idx + 1L
    }

    # Replacement of `EE`
    if (grepl("EE", subst_pattern)) {
      EEE <- l_fmt_EEE(locale = locale)
      subst_pattern <- gsub("EE", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, EEE[dow])
      idx <- idx + 1L
    }

    # Replacement of `E`
    if (grepl("E", subst_pattern)) {
      EEE <- l_fmt_EEE(locale = locale)
      subst_pattern <- gsub("E", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, EEE[dow])
      idx <- idx + 1L
    }

    # Replacement of `cccccc`
    if (grepl("cccccc", subst_pattern)) {
      cccccc <- l_fmt_cccccc(locale = locale)
      subst_pattern <- gsub("cccccc", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, cccccc[dow])
      idx <- idx + 1L
    }

    # Replacement of `ccccc`
    if (grepl("ccccc", subst_pattern)) {
      ccccc <- l_fmt_ccccc(locale = locale)
      subst_pattern <- gsub("ccccc", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, ccccc[dow])
      idx <- idx + 1L
    }

    # Replacement of `cccc`
    if (grepl("cccc", subst_pattern)) {
      cccc <- l_fmt_cccc(locale = locale)
      subst_pattern <- gsub("cccc", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, cccc[dow])
      idx <- idx + 1L
    }

    # Replacement of `ccc`
    if (grepl("ccc", subst_pattern)) {
      ccc <- l_fmt_ccc(locale = locale)
      subst_pattern <- gsub("ccc", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, ccc[dow])
      idx <- idx + 1L
    }

    # Replacement of `cc`
    if (grepl("cc", subst_pattern)) {
      ccc <- l_fmt_ccc(locale = locale)
      subst_pattern <- gsub("cc", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, ccc[dow])
      idx <- idx + 1L
    }

    # Replacement of `c`
    if (grepl("c", subst_pattern)) {
      ccc <- l_fmt_ccc(locale = locale)
      subst_pattern <- gsub("c", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, ccc[dow])
      idx <- idx + 1L
    }

    # Replacement of `GGGGG`
    if (grepl("GGGGG", subst_pattern)) {
      GGGGG <- l_fmt_GGGGG(locale = locale)
      GGGGG <- ifelse(year >= 0, GGGGG[2], GGGGG[1])
      subst_pattern <- gsub("GGGGG", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, GGGGG)
      idx <- idx + 1L
    }

    # Replacement of `GGGG`
    if (grepl("GGGG", subst_pattern)) {
      GGGG <- l_fmt_GGGG(locale = locale)
      GGGG <- ifelse(year >= 0, GGGG[2], GGGG[1])
      subst_pattern <- gsub("GGGG", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, GGGG)
      idx <- idx + 1L
    }

    # Replacement of `GGG`
    if (grepl("GGG", subst_pattern)) {
      GGG <- l_fmt_GGG(locale = locale)
      GGG <- ifelse(year >= 0, GGG[2], GGG[1])
      subst_pattern <- gsub("GGG", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, GGG)
      idx <- idx + 1L
    }

    # Replacement of `GG`
    if (grepl("GG", subst_pattern)) {
      GGG <- l_fmt_GGG(locale = locale)
      GGG <- ifelse(year >= 0, GGG[2], GGG[1])
      subst_pattern <- gsub("GG", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, GGG)
      idx <- idx + 1L
    }

    # Replacement of `G`
    if (grepl("G", subst_pattern)) {
      GGG <- l_fmt_GGG(locale = locale)
      GGG <- ifelse(year >= 0, GGG[2], GGG[1])
      subst_pattern <- gsub("G", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, GGG)
      idx <- idx + 1L
    }

    # Replacement of `LLLLL`
    if (grepl("LLLLL", subst_pattern)) {
      LLLLL <- l_fmt_LLLLL(locale = locale)
      subst_pattern <- gsub("LLLLL", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, LLLLL[month])
      idx <- idx + 1L
    }

    # Replacement of `LLLL`
    if (grepl("LLLL", subst_pattern)) {
      LLLL <- l_fmt_LLLL(locale = locale)
      subst_pattern <- gsub("LLLL", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, LLLL[month])
      idx <- idx + 1L
    }

    # Replacement of `LLL`
    if (grepl("LLL", subst_pattern)) {
      LLL <- l_fmt_LLL(locale = locale)
      subst_pattern <- gsub("LLL", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, LLL[month])
      idx <- idx + 1L
    }

    # Replacement of `LL`
    if (grepl("LL", subst_pattern)) {
      subst_pattern <- gsub("LL", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, month_p)
      idx <- idx + 1L
    }

    # Replacement of `L`
    if (grepl("L", subst_pattern)) {
      subst_pattern <- gsub("L", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(month))
      idx <- idx + 1L
    }

    # Replacement of `ww`
    if (grepl("ww", subst_pattern)) {
      subst_pattern <- gsub("ww", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, woy_p)
      idx <- idx + 1L
    }

    # Replacement of `w`
    if (grepl("w", subst_pattern)) {
      subst_pattern <- gsub("w", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(woy))
      idx <- idx + 1L
    }

    # NOTE the missing components:
    #  - Y (year of week of year)
    #  - u (extended year)
    #  - U (cyclic year name)
    #  - r (related Georgian year)
    #  - Q (quarter - formatting)
    #  - q (quarter - standalone)
    #  - W (week of month)
    #  - D (day of year)
    #  - F (day of week in month)
    #  - g (modified Julian day)
    #  - e (local day of week - formatting)
  }

  if (!is.null(time_input)) {

    # Extract hour and minute from the input time
    hour <- as.numeric(strsplit(time_input, ":")[[1]][[1]])
    minute <- as.numeric(strsplit(time_input, ":")[[1]][[2]])

    # If there is a seconds component, obtain that
    if (length(strsplit(time_input, ":")[[1]]) == 3) {
      seconds <- as.numeric(strsplit(time_input, ":")[[1]][[3]])
    } else {
      seconds <- "0"
    }

    # Get the 12-hour (`hour_12`) hour value
    hour_12 <- ifelse(hour > 12, hour - 12, hour)

    # Padded variants of `month` and `day`
    hour_p <- ifelse(hour < 10, paste0("0", hour), as.character(hour))
    minute_p <- ifelse(minute < 10, paste0("0", minute), as.character(minute))

    seconds_full <-
      strsplit(as.character(seconds), "\\.")[[1]][[1]]

    seconds_p <-
      ifelse(as.numeric(seconds_full) < 10, paste0("0", seconds_full), seconds_full)

    if (grepl("\\.", as.character(seconds))) {

      f_sec <- strsplit(as.character(seconds), "\\.")[[1]][[2]]
      f_sec_digits <- nchar(f_sec)

      f_sec_1 <-
        ifelse(f_sec_digits >= 1, substr(f_sec, 1, 1), "0")

      f_sec_2 <-
        ifelse(
          f_sec_digits >= 2,
          substr(f_sec, 1, 2),
          paste0(f_sec, paste(rep("0", 2 - f_sec_digits), collapse = "")))

      f_sec_3 <-
        ifelse(
          f_sec_digits >= 3,
          substr(f_sec, 1, 3),
          paste0(f_sec, paste(rep("0", 3 - f_sec_digits), collapse = "")))

      f_sec_4 <-
        ifelse(
          f_sec_digits >= 3,
          paste0(substr(f_sec, 1, 3), "0"),
          paste0(f_sec, paste(rep("0", 4 - f_sec_digits), collapse = "")))

      f_sec_v <- c(f_sec_1, f_sec_2, f_sec_3, f_sec_4)
    }

    # Time zone information
    if (!is.null(iana_tz_name)) {

      specific_nonloc_tz <-
        subset(
          tz_tbl_dst,
          zone_name == iana_tz_name & date_start < as.Date(date_input))

      if (nrow(specific_nonloc_tz) > 0) {

        specific_nonloc_tz <- specific_nonloc_tz[nrow(specific_nonloc_tz), ]

        full_name <- specific_nonloc_tz$full_name
        abbrev <- specific_nonloc_tz$abbrev
        country_code <- specific_nonloc_tz$country_code
        country_name <- specific_nonloc_tz$country_name

      } else {

        full_name <- NA_character_
        abbrev <- NA_character_
        country_code <- NA_character_
        country_name <- NA_character_
      }
    } else {

      full_name <- NA_character_
      abbrev <- NA_character_
      country_code <- NA_character_
      country_name <- NA_character_
    }

    if (!is.null(tz_offset)) {

      long_local_gmt <- get_long_local_gmt(tz_offset)
      short_local_gmt <- get_short_local_gmt(tz_offset)

    } else {

      long_local_gmt <- NA_character_
      short_local_gmt <- NA_character_
    }

    # Replacement of `a`
    if (grepl("a", subst_pattern)) {
      a <- l_fmt_a(locale = locale)
      a <- ifelse(hour >= 12, a[2], a[1])
      subst_pattern <- gsub("a", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, a)
      idx <- idx + 1L
    }

    # Replacement of `HH`
    if (grepl("HH", subst_pattern)) {
      subst_pattern <- gsub("HH", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, hour_p)
      idx <- idx + 1L
    }

    # Replacement of `H`
    if (grepl("H", subst_pattern)) {
      subst_pattern <- gsub("H", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(hour))
      idx <- idx + 1L
    }

    # Replacement of `h`
    if (grepl("h", subst_pattern)) {
      subst_pattern <- gsub("h", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(hour_12))
      idx <- idx + 1L
    }

    # Replacement of `mm`
    if (grepl("mm", subst_pattern)) {
      subst_pattern <- gsub("mm", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, minute_p)
      idx <- idx + 1L
    }

    # Replacement of `m`
    if (grepl("m", subst_pattern)) {
      subst_pattern <- gsub(subst_pattern, "m", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, as.character(minute))
      idx <- idx + 1L
    }

    # Replacement of `ss`
    if (grepl("ss", subst_pattern)) {
      subst_pattern <- gsub("ss", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, seconds_full)
      idx <- idx + 1L
    }

    # Replacement of `s`
    if (grepl("s", subst_pattern)) {
      subst_pattern <- gsub("s", braced(idx), subst_pattern)
      replace_stack <- c(replace_stack, seconds_p)
      idx <- idx + 1L
    }

    # Replacement of `zzzz`
    if (grepl("zzzz", subst_pattern)) {

      if (!is.na(full_name)) {
        subst_pattern <- gsub("zzzz", braced(idx), subst_pattern)
        replace_stack <- c(replace_stack, full_name)
        idx <- idx + 1L

      } else {

        subst_pattern <- gsub("zzzz", braced(idx), subst_pattern)
        replace_stack <- c(replace_stack, long_local_gmt)
        idx <- idx + 1L
      }
    }

    # Replacement of `zzz`
    if (grepl("zzz", subst_pattern)) {

      if (!is.na(full_name)) {
        subst_pattern <- gsub("zzz", braced(idx), subst_pattern)
        replace_stack <- c(replace_stack, full_name)
        idx <- idx + 1L

      } else {

        subst_pattern <- gsub("zzz", braced(idx), subst_pattern)
        replace_stack <- c(replace_stack, long_local_gmt)
        idx <- idx + 1L
      }
    }

    # Replacement of `zz`
    if (grepl("zz", subst_pattern)) {

      if (!is.na(full_name)) {
        subst_pattern <- gsub("zz", braced(idx), subst_pattern)
        replace_stack <- c(replace_stack, full_name)
        idx <- idx + 1L

      } else {

        subst_pattern <- gsub("zz", braced(idx), subst_pattern)
        replace_stack <- c(replace_stack, long_local_gmt)
        idx <- idx + 1L
      }
    }

    # Replacement of `z`
    if (grepl("z", subst_pattern)) {

      if (!is.na(abbrev)) {
        subst_pattern <- gsub("z", braced(idx), subst_pattern)
        replace_stack <- c(replace_stack, abbrev)
        idx <- idx + 1L

      } else {

        subst_pattern <- gsub("z", braced(idx), subst_pattern)
        replace_stack <- c(replace_stack, short_local_gmt)
        idx <- idx + 1L
      }
    }

    # NOTE the missing components:
    #  - k (hour in day: 1-24)
    #  - K (hour in am/pm: 0-11)
    #  - S (fractional second S, SS, SSS, SSSS)
    #  - A (milliseconds in day)
    #  - Z (TZ)
    #  - O (TZ)
    #  - v (TZ generic non-location)
    #  - V (TZ)
    #  - X (TZ ISO-8601)
    #  - x (TZ ISO-8601)
  }

  # Add in replacements from the `replace_stack`
  if (!is.null(replace_stack)) {
    for (i in seq_along(replace_stack)) {
      subst_pattern <-
        gsub(paste0("\\{", i, "\\}"), replace_stack[i], subst_pattern)
    }
  }

  subst_pattern
}
