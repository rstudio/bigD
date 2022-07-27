library(usethis)

source("data-raw/X01-tzdb.R")

# Create external datasets
usethis::use_data(
  tzdb,
  internal = TRUE, overwrite = TRUE
)
