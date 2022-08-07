library(usethis)

source("data-raw/X01-tzdb.R")
source("data-raw/X02-dates.R")
source("data-raw/X03-tz_exemplar.R")
source("data-raw/X04-tz_metazone_names.R")
source("data-raw/X05-tz_metazone_users.R")
source("data-raw/X06-tz_map.R")
source("data-raw/X07-tz_formats.R")
source("data-raw/X08-tz_bcp_id.R")
source("data-raw/X09-tz_name_resolution.R")

# Create external datasets
usethis::use_data(
  tzdb, dates, tz_exemplar,
  tz_metazone_names, tz_metazone_users,
  tz_map, tz_formats, tz_bcp_id, tz_name_resolution,
  internal = TRUE, overwrite = TRUE
)
