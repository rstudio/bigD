library(tidyverse)

tzdb <-
  readr::read_csv(
    file = "data-raw/time_zone.csv",
    col_names = c(
      "zone_name", "country_code", "abbreviation",
      "time_start", "gmt_offset", "dst"
    ),
    col_types = cols(
      zone_name = col_character(),
      country_code = col_character(),
      abbreviation = col_character(),
      time_start = col_double(),
      gmt_offset = col_double(),
      dst = col_double()
    )
  ) %>%
  dplyr::mutate(dst = as.logical(dst)) %>%
  dplyr::rename(
    abbrev = abbreviation,
    gmt_offset_s = gmt_offset
  ) %>%
  dplyr::mutate(
    gmt_offset_h = gmt_offset_s / 3600,
    date_start = as.Date("1970-01-01") + (time_start / 86400)
  ) %>%
  dplyr::select(
    zone_name, country_code, abbrev, time_start, date_start,
    gmt_offset_s, gmt_offset_h, dst
  )
