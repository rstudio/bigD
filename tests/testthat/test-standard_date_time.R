test_that("The `standard_*()` functions produce very specific objects", {

  expect_vector(standard_date_time())
  expect_type(standard_date_time(), "character")
  expect_length(standard_date_time(), 1)
  expect_s3_class(
    standard_date_time(),
    c("date_time_pattern", "standard", "date_time", "short")
  )
  expect_s3_class(standard_date_time(type = "medium"), "medium")
  expect_s3_class(standard_date_time(type = "long"), "long")
  expect_s3_class(standard_date_time(type = "full"), "full")

  expect_vector(standard_date())
  expect_type(standard_date(), "character")
  expect_length(standard_date(), 1)
  expect_s3_class(
    standard_date(),
    c("date_time_pattern", "standard", "date", "short")
  )
  expect_s3_class(standard_date(type = "medium"), "medium")
  expect_s3_class(standard_date(type = "long"), "long")
  expect_s3_class(standard_date(type = "full"), "full")

  expect_vector(standard_time())
  expect_type(standard_time(), "character")
  expect_length(standard_time(), 1)
  expect_s3_class(
    standard_time(),
    c("date_time_pattern", "standard", "time", "short")
  )
  expect_s3_class(standard_time(type = "medium"), "medium")
  expect_s3_class(standard_time(type = "long"), "long")
  expect_s3_class(standard_time(type = "full"), "full")
})

test_that("The `standard_*()` functions can be used in `fdt()`", {

  expect_equal(
    vapply(
      c("short", "medium", "long", "full"),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        fdt(
          input = "2018-07-04 22:05 (America/Los_Angeles)",
          format = standard_date_time(type = x)
        )
      }
    ),
    c(
      "7/4/18, 10:05 PM",
      "Jul 4, 2018, 10:05:00 PM",
      "July 4, 2018 at 10:05:00 PM PDT",
      "Wednesday, July 4, 2018 at 10:05:00 PM Pacific Daylight Time"
    )
  )

  expect_equal(
    vapply(
      c("short", "medium", "long", "full"),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        fdt(
          input = "2018-07-04 22:05 (America/Los_Angeles)",
          format = standard_date(type = x)
        )
      }
    ),
    c(
      "7/4/18",
      "Jul 4, 2018",
      "July 4, 2018",
      "Wednesday, July 4, 2018"
    )
  )

  expect_equal(
    vapply(
      c("short", "medium", "long", "full"),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        fdt(
          input = "2018-07-04 22:05 (America/Los_Angeles)",
          format = standard_time(type = x)
        )
      }
    ),
    c(
      "10:05 PM",
      "10:05:00 PM",
      "10:05:00 PM PDT",
      "10:05:00 PM Pacific Daylight Time"
    )
  )

  select_locales <- c("pt", "fr", "en")

  for (i in seq_along(select_locales)) {
    for (j in c("short", "medium", "long", "full")) {

      fmt_val_dt <-
        fdt(
          input = "2018-07-04 22:05 (America/Los_Angeles)",
          format = standard_time(type = j),
          locale = select_locales[i]
        )

      expect_vector(fmt_val_dt)
      expect_length(fmt_val_dt, 1)
      expect_type(fmt_val_dt, "character")
    }
  }
})
