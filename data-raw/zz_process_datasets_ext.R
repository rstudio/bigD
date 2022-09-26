library(usethis)

source("data-raw/X01-tzdb.R")
source("data-raw/X02-dates.R")
source("data-raw/X03-day_periods.R")
source("data-raw/X04-start_of_week.R")
source("data-raw/X05-tz_exemplar.R")
source("data-raw/X06-tz_metazone_names.R")
source("data-raw/X07-tz_metazone_users.R")
source("data-raw/X08-tz_map.R")
source("data-raw/X09-tz_formats.R")
source("data-raw/X10-tz_bcp_id.R")
source("data-raw/X11-tz_name_resolution.R")
source("data-raw/X12-default_locales.R")

# Create external datasets
usethis::use_data(
  tzdb, dates, day_periods, start_of_week,
  tz_exemplar, tz_metazone_names, tz_metazone_users,
  tz_map, tz_formats, tz_bcp_id, tz_name_resolution, default_locales,
  internal = TRUE, overwrite = TRUE
)
