library(usethis)

source("data-raw/X01-tzdb.R")
source("data-raw/X02-dates.R")
source("data-raw/X03-tz_exemplar.R")
source("data-raw/X04-tz_map.R")
source("data-raw/X05-tz_formats.R")
source("data-raw/X06-tz_bcp_id.R")

# Create external datasets
usethis::use_data(
  tzdb, dates, tz_exemplar, tz_map, tz_formats, tz_bcp_id,
  internal = TRUE, overwrite = TRUE
)
