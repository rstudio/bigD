test_that("Vectors can be obtained from the `*_vec()` functions", {

  expect_vector(flex_d_vec())
  expect_vector(flex_t12_vec())
  expect_vector(flex_t24_vec())

  expect_type(flex_d_vec(), "character")
  expect_type(flex_t12_vec(), "character")
  expect_type(flex_t24_vec(), "character")

  expect_length(flex_d_vec(), 26)
  expect_length(flex_t12_vec(), 12)
  expect_length(flex_t24_vec(), 8)
})

test_that("The `*_list` objects have a particular structure", {

  expect_type(flex_d_lst, "list")
  expect_type(flex_t12_lst, "list")
  expect_type(flex_t24_lst, "list")

  expect_named(flex_d_lst, flex_d_vec())
  expect_named(flex_t12_lst, flex_t12_vec())
  expect_named(flex_t24_lst, flex_t24_vec())

  expect_equal(unlist(flex_d_lst, use.names = FALSE), flex_d_vec())
  expect_equal(unlist(flex_t12_lst, use.names = FALSE), flex_t12_vec())
  expect_equal(unlist(flex_t24_lst, use.names = FALSE), flex_t24_vec())

  lapply(
    flex_d_lst, FUN = function(x) {
      expect_s3_class(x, c("date_time_pattern", "flex_d"))
      expect_type(x, "character")
    }
  )

  lapply(
    flex_t12_lst, FUN = function(x) {
      expect_s3_class(x, c("date_time_pattern", "flex_t12"))
      expect_type(x, "character")
    }
  )

  lapply(
    flex_t24_lst, FUN = function(x) {
      expect_s3_class(x, c("date_time_pattern", "flex_t24"))
      expect_type(x, "character")
    }
  )
})

test_that("`flex_*_lst` can be used in `fdt()`", {

  expect_equal(
    fdt(
      input = "2018-07-04 22:05",
      format = flex_d_lst$yMd
    ),
    "7/4/2018"
  )
  expect_equal(
    fdt(
      input = "2018-07-04 22:05",
      format = flex_d_lst[[1]]
    ),
    "7/4/2018"
  )
  expect_equal(
    fdt(
      input = "2018-07-04 22:05",
      format = flex_d_lst[["yMd"]]
    ),
    "7/4/2018"
  )

  expect_equal(
    vapply(
      flex_d_vec(),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        fdt(
          input = "2018-07-04 22:05",
          format = flex_d_lst[[x]]
        )
      }
    ),
    c(
      "7/4/2018", "Wed, 7/4/2018", "Jul 2018", "July 2018", "Jul 4, 2018",
      "Wed, Jul 4, 2018", "7/4/2018 A", "Jul 4, 2018 AD", "Wed, Jul 4, 2018 AD",
      "7/2018", "7/4", "Wed, 7/4", "Jul 4", "Wed, Jul 4", "July 4",
      "Jul 2018 AD", "Q3 2018", "3rd quarter 2018", "2018 AD", "2018",
      "7", "Jul", "4", "4 Wed", "week 1 of July", "week 27 of 2018"
    )
  )

  expect_equal(
    vapply(
      flex_d_vec(),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        fdt(
          input = "2014-01-03 08:09:06 (America/New_York)",
          format = flex_d_lst[[x]]
        )
      }
    ),
    c(
      "1/3/2014", "Fri, 1/3/2014", "Jan 2014", "January 2014", "Jan 3, 2014",
      "Fri, Jan 3, 2014", "1/3/2014 A", "Jan 3, 2014 AD", "Fri, Jan 3, 2014 AD",
      "1/2014", "1/3", "Fri, 1/3", "Jan 3", "Fri, Jan 3", "January 3",
      "Jan 2014 AD", "Q1 2014", "1st quarter 2014", "2014 AD", "2014",
      "1", "Jan", "3", "3 Fri", "week 1 of January", "week 1 of 2014"
    )
  )

  select_locales <- c("pt", "fr", "en")

  for (i in seq_along(select_locales)) {
    for (j in seq_along(flex_d_vec())) {

      fmt_val_d <-
        fdt(
          input = "2018-07-04 22:05",
          format = flex_d_lst[[j]],
          locale = select_locales[i]
        )

      expect_vector(fmt_val_d)
      expect_length(fmt_val_d, 1)
      expect_type(fmt_val_d, "character")
    }
  }

  for (i in seq_along(select_locales)) {
    for (j in seq_along(flex_t12_vec())) {

      fmt_val_t12 <-
        fdt(
          input = "2018-07-04 22:05",
          format = flex_t12_lst[[j]],
          locale = select_locales[i]
        )

      expect_vector(fmt_val_t12)
      expect_length(fmt_val_t12, 1)
      expect_type(fmt_val_t12, "character")
    }
  }

  for (i in seq_along(select_locales)) {
    for (j in seq_along(flex_t24_vec())) {

      fmt_val_t24 <-
        fdt(
          input = "2018-07-04 22:05",
          format = flex_t24_lst[[j]],
          locale = select_locales[i]
        )

      expect_vector(fmt_val_t24)
      expect_length(fmt_val_t24, 1)
      expect_type(fmt_val_t24, "character")
    }
  }

  expect_equal(
    vapply(
      flex_d_vec(),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        fdt(
          input = "2018-07-04 22:05",
          format = list(flex_d_lst[[x]], flex_t12_lst[[1]])
        )
      }
    ),
    c(
      "7/4/2018, 10:05:00 PM", "Wed, 7/4/2018, 10:05:00 PM",
      "Jul 2018, 10:05:00 PM",  "July 2018, 10:05:00 PM",
      "Jul 4, 2018, 10:05:00 PM", "Wed, Jul 4, 2018, 10:05:00 PM",
      "7/4/2018 A, 10:05:00 PM", "Jul 4, 2018 AD, 10:05:00 PM",
      "Wed, Jul 4, 2018 AD, 10:05:00 PM", "7/2018, 10:05:00 PM",
      "7/4, 10:05:00 PM", "Wed, 7/4, 10:05:00 PM", "Jul 4, 10:05:00 PM",
      "Wed, Jul 4, 10:05:00 PM", "July 4, 10:05:00 PM",
      "Jul 2018 AD, 10:05:00 PM", "Q3 2018, 10:05:00 PM",
      "3rd quarter 2018, 10:05:00 PM", "2018 AD, 10:05:00 PM",
      "2018, 10:05:00 PM", "7, 10:05:00 PM", "Jul, 10:05:00 PM",
      "4, 10:05:00 PM", "4 Wed, 10:05:00 PM", "week 1 of July, 10:05:00 PM",
      "week 27 of 2018, 10:05:00 PM"
    )
  )
})

test_that("fdt() works in all contexts", {
  expect_equal(
    bigD::fdt("2024-03-01", format = "GyMMMEd", use_tz = "America/Toronto"),
    "AD2024MarFri1"
  )

})
