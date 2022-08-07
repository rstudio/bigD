library(dplyr)
library(readr)

tz_name_resolution <-
  readr::read_csv(
    file = "data-raw/tz_name_resolution.csv",
    col_types = cols(
      tz_canonical = col_character(),
      tz_alt = col_character()
    )
  ) %>%
  dplyr::arrange(tz_canonical)
