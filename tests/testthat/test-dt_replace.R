# Just testing internal functions to show examples of how they work
test_that("dt_replace() works", {
  skip_on_cran() # not sure how robust this is.
  result <- dt_replace(
    dt = "{yyyy}-{MM}-{dd}'1'{HH}:{mm}:{ss}{XXX}",
    input_dt =as.POSIXct("2015-06-28 20:49:46", tz = "UTC"),
    dt_lett = c("y", "M", "d", "H", "m", "s", "X"),
    locale = "en",
    tz_info = list(
      tz_str = "GMT",
      tz_offset = 0L,
      long_tzid = "UTC",
      tz_short_specific = NA_character_,
      tz_long_specific = NA_character_
    )
  )
  expect_equal(
    result,
    "2015-06-28'1'20:49:46Z"
  )
})

test_that("The dt_* helpers work as expected (#16)", {
  expect_equal(dt_year[["{y}"]](2016), "2016")

})
