test_that("ISO dates can be parsed from a string and formatted", {

  iso_date_1 <- "2018-07-04"
  iso_date_2 <- "2020-01-12"
  iso_date_3 <- "2020-01-04"

  #
  # Checks with `y`/`yy`, `M`/`MM`/`MMM`/`MMMM`/`MMMMM`, `d`/`dd`
  #

  fmt_dt(input = iso_date_1, dt_format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_date_1, dt_format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_date_1, dt_format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_date_1, dt_format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_date_1, dt_format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")

  #
  # Checks with `L` and `LL`
  #

  fmt_dt(input = iso_date_1, dt_format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_date_1, dt_format = "y-LL-dd") %>%
    expect_equal("2018-07-04")

  #
  # Checks with `D`/`DD`/`DDD`
  #

  fmt_dt(input = iso_date_1, dt_format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_date_1, dt_format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_date_1, dt_format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_date_2, dt_format = "y/M/D") %>%
    expect_equal("2020/1/12")
  fmt_dt(input = iso_date_2, dt_format = "y/M/DD") %>%
    expect_equal("2020/1/12")
  fmt_dt(input = iso_date_2, dt_format = "y/M/DDD") %>%
    expect_equal("2020/1/012")
  fmt_dt(input = iso_date_3, dt_format = "y/M/D") %>%
    expect_equal("2020/1/4")
  fmt_dt(input = iso_date_3, dt_format = "y/M/DD") %>%
    expect_equal("2020/1/04")
  fmt_dt(input = iso_date_3, dt_format = "y/M/DDD") %>%
    expect_equal("2020/1/004")
})

test_that("ISO dates can be formatted even with string literals", {

  iso_date_1 <- "2018-07-04"
  iso_date_2 <- "2020-01-12"
  iso_date_3 <- "2020-01-04"

  fmt_dt(input = iso_date_1, dt_format = "'Day': dd, 'Month': MM (yy)") %>%
    expect_equal("Day: 04, Month: 07 (18)")

  fmt_dt(input = iso_date_1, dt_format = "d'th' MMMM y (EEEE).") %>%
    expect_equal("4th July 2018 (Wednesday).")

  fmt_dt(input = iso_date_1, dt_format = "'T:' yyyy-MM-dd") %>%
    expect_equal("T: 2018-07-04")

  fmt_dt(input = iso_date_1, dt_format = "'T: 'yyyy-MM-dd") %>%
    expect_equal("T: 2018-07-04")

  fmt_dt(input = iso_date_1, dt_format = "'' 'T: 'yyyy-MM-dd") %>%
    expect_equal("' T: 2018-07-04")

  fmt_dt(input = iso_date_1, dt_format = "'''''''H' 'T: 'yyyy-MM-dd''''''") %>%
    expect_equal("'''H T: 2018-07-04'''")
})

test_that("ISO datetimes can be parsed from a string and formatted", {

  iso_datetime_1 <- "2018-07-04T22:05"
  iso_datetime_2 <- "2018-07-04 22:05"
  iso_datetime_3 <- "2018-07-04 22:05:23"
  iso_datetime_4 <- "2018-07-04 22:05:00"
  iso_datetime_5 <- "2018-10-04T22:05"
  iso_datetime_6 <- "2018-10-04 22:05"
  iso_datetime_7 <- "2018-10-04 22:05:23"
  iso_datetime_8 <- "2018-10-04 22:05:00"
  iso_datetime_9  <- "2018-01-04T22:05"
  iso_datetime_10 <- "2018-01-04 22:05"
  iso_datetime_11 <- "2018-01-04 22:05:23"
  iso_datetime_12 <- "2018-01-04 22:05:00"
  iso_datetime_13 <- "2018-01-13T22:05"
  iso_datetime_14 <- "2018-01-13 22:05"
  iso_datetime_15 <- "2018-01-13 22:05:23"
  iso_datetime_16 <- "2018-01-13 22:05:00"

  #
  # Checks with `y`/`yy`, `M`/`MM`/`MMM`/`MMMM`/`MMMMM`, `d`/`dd`
  #

  fmt_dt(input = iso_datetime_1, dt_format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_datetime_1, dt_format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_datetime_1, dt_format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_datetime_1, dt_format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_datetime_1, dt_format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fmt_dt(input = iso_datetime_2, dt_format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_datetime_2, dt_format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_datetime_2, dt_format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_datetime_2, dt_format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_datetime_2, dt_format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fmt_dt(input = iso_datetime_3, dt_format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_datetime_3, dt_format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_datetime_3, dt_format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_datetime_3, dt_format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_datetime_3, dt_format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fmt_dt(input = iso_datetime_4, dt_format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_datetime_4, dt_format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_datetime_4, dt_format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_datetime_4, dt_format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_datetime_4, dt_format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")

  #
  # Checks with `L`
  #

  fmt_dt(input = iso_datetime_1, dt_format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_datetime_2, dt_format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_datetime_3, dt_format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_datetime_4, dt_format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_datetime_5, dt_format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_6, dt_format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_7, dt_format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_8, dt_format = "y-L-dd") %>%
    expect_equal("2018-10-04")

  #
  # Checks with `LL`
  #

  fmt_dt(input = iso_datetime_1, dt_format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fmt_dt(input = iso_datetime_2, dt_format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fmt_dt(input = iso_datetime_3, dt_format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fmt_dt(input = iso_datetime_4, dt_format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fmt_dt(input = iso_datetime_5, dt_format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_6, dt_format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_7, dt_format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_8, dt_format = "y-LL-dd") %>%
    expect_equal("2018-10-04")

  #
  # Checks with `D`
  #

  fmt_dt(input = iso_datetime_1, dt_format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_2, dt_format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_3, dt_format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_4, dt_format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_5, dt_format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_6, dt_format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_7, dt_format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_8, dt_format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_9, dt_format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fmt_dt(input = iso_datetime_10, dt_format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fmt_dt(input = iso_datetime_11, dt_format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fmt_dt(input = iso_datetime_12, dt_format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fmt_dt(input = iso_datetime_13, dt_format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_14, dt_format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_15, dt_format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_16, dt_format = "y/M/D") %>%
    expect_equal("2018/1/13")

  #
  # Checks with `DD`
  #

  fmt_dt(input = iso_datetime_1, dt_format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_2, dt_format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_3, dt_format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_4, dt_format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_5, dt_format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_6, dt_format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_7, dt_format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_8, dt_format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_9, dt_format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fmt_dt(input = iso_datetime_10, dt_format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fmt_dt(input = iso_datetime_11, dt_format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fmt_dt(input = iso_datetime_12, dt_format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fmt_dt(input = iso_datetime_13, dt_format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_14, dt_format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_15, dt_format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_16, dt_format = "y/M/DD") %>%
    expect_equal("2018/1/13")

  #
  # Checks with `DDD`
  #

  fmt_dt(input = iso_datetime_1, dt_format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_2, dt_format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_3, dt_format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_4, dt_format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_5, dt_format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_6, dt_format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_7, dt_format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_8, dt_format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_9, dt_format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fmt_dt(input = iso_datetime_10, dt_format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fmt_dt(input = iso_datetime_11, dt_format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fmt_dt(input = iso_datetime_12, dt_format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fmt_dt(input = iso_datetime_13, dt_format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fmt_dt(input = iso_datetime_14, dt_format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fmt_dt(input = iso_datetime_15, dt_format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fmt_dt(input = iso_datetime_16, dt_format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
})

test_that("ISO datetimes with tz info can be parsed and formatted", {

  dt_list <-
    list(
      iso_datetime_tz_1  = "2018-07-04T22:05-0800",
      iso_datetime_tz_2  = "2018-07-04 22:05-0800",
      iso_datetime_tz_3  = "2018-07-04T22:05:38-0800",
      iso_datetime_tz_4  = "2018-07-04 22:05:38-0800",
      iso_datetime_tz_5  = "2018-07-04T22:05(America/Vancouver)",
      iso_datetime_tz_6  = "2018-07-04 22:05(America/Vancouver)",
      iso_datetime_tz_7  = "2018-07-04T22:05 (America/Vancouver)",
      iso_datetime_tz_8  = "2018-07-04 22:05 (America/Vancouver)",
      iso_datetime_tz_9  = "2018-07-04T22:05-0800(America/Vancouver)",
      iso_datetime_tz_10 = "2018-07-04 22:05-0800(America/Vancouver)",
      iso_datetime_tz_11 = "2018-09-06 08:00(Africa/Accra)",
      iso_datetime_tz_12 = "2012-11-26 08:00(Africa/Johannesburg)",
      iso_datetime_tz_13 = "2013-01-01 08:00(Asia/Kolkata)"
    )

  #
  # Checks with `zzz`: short specific non-location format (e.g., 'PDT'); falls
  # back to the short localized GMT format `O`
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-09-06 08:00:00 GMT")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2012-11-26 08:00:00 SAST")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2013-01-01 08:00:00 IST")

  #
  # Checks with `z` and `zz` which should be equivalent to `zzz`
  #

  lapply(
    dt_list,
    FUN = function(x) {
      expect_equal(
        fmt_dt(input = x, dt_format = "y-MM-dd HH:mm:ss z"),
        fmt_dt(input = x, dt_format = "y-MM-dd HH:mm:ss zzz")
      )
    }
  )
  lapply(
    dt_list,
    FUN = function(x) {
      expect_equal(
        fmt_dt(input = x, dt_format = "y-MM-dd HH:mm:ss zz"),
        fmt_dt(input = x, dt_format = "y-MM-dd HH:mm:ss zzz")
      )
    }
  )

  #
  # Checks with `zzzz`: long specific non-location format
  # (e.g., 'Pacific Daylight Time'); falls back to the long localized GMT
  # format `OOOO`
  #
  # Not yet implemented
  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss zzzz")

  #
  # Checks with `ZZZ`: ISO8601 basic format
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +0530")

  #
  # Checks with `Z` and `ZZ` which should be equivalent to `ZZZ`
  #

  lapply(
    dt_list,
    FUN = function(x) {
      expect_equal(
        fmt_dt(input = x, dt_format = "y-MM-dd HH:mm:ss Z"),
        fmt_dt(input = x, dt_format = "y-MM-dd HH:mm:ss ZZZ")
      )
    }
  )
  lapply(
    dt_list,
    FUN = function(x) {
      expect_equal(
        fmt_dt(input = x, dt_format = "y-MM-dd HH:mm:ss ZZ"),
        fmt_dt(input = x, dt_format = "y-MM-dd HH:mm:ss ZZZ")
      )
    }
  )

  #
  # Checks with `ZZZZ`: long localized GMT format (e.g., 'GMT-8:00')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-09-06 08:00:00 GMT+0:00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2012-11-26 08:00:00 GMT+2:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")

  #
  # Checks with `ZZZZZ`: ISO8601 extended format (e.g., '-08:00')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")

  #
  # Checks with `O`: the short localized GMT format (e.g., 'GMT-8')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-09-06 08:00:00 GMT+0")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2012-11-26 08:00:00 GMT+2")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")

  #
  # Checks with `OOOO`: the long localized GMT format (e.g., 'GMT-08:00')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-09-06 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2012-11-26 08:00:00 GMT+02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2013-01-01 08:00:00 GMT+05:30")

  #
  # Expect an error if using the undefined `OO`, `OOO`, and `OOOOO` formats
  #

  expect_error(
    fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss OO")
  )
  expect_error(
    fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss OOO")
  )
  expect_error(
    fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss OOOOO")
  )

  #
  # Checks with `v`: the short generic non-location format (e.g., 'PT'); falls
  # back to `VVVV` and then to `OOOO`
  #
  # Not yet implemented
  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss v")

  #
  # Expect an error if using the undefined `vv`, `vvv`, and `vvvvv` formats
  #

  expect_error(
    fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss vv")
  )
  expect_error(
    fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss vvv")
  )
  expect_error(
    fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss vvvvv")
  )

  #
  # Checks with `vvvv`: the long generic non-location format (e.g.,
  # 'Pacific Time'); falls back to `VVVV` and then to `OOOO`
  #
  # Not yet implemented
  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss vvvv")

  #
  # Checks with `V`: the short time zone ID
  #
  # Not yet implemented
  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss V")

  #
  # Checks with `VV`: the long time zone ID
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 ")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 ")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:38 ")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:38 ")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-09-06 08:00:00 Africa/Accra")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2012-11-26 08:00:00 Africa/Johannesburg")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2013-01-01 08:00:00 Asia/Kolkata")

  #
  # Checks with `VVV`: the localized exemplar city name; if not available then
  # use the Etc/Unknown fallback ('Unknown City')
  #
  # Not yet implemented

  #
  # Checks with `VVVV`: the generic location format (e.g., 'Los Angeles Time');
  # if not available then long localized GMT format (`OOOO`)
  #
  # Not yet implemented

  #
  # Checks with `X`: ISO8601 basic format with hours and optional minutes field;
  # 'Z' used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2012-11-26 08:00:00 +02")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2013-01-01 08:00:00 +0530")

  #
  # Checks with `XX`: ISO8601 basic format with hours and minutes fields; 'Z' used
  # when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")

  #
  # Checks with `XXX`: ISO8601 extended format with hours and minutes fields;
  # 'Z' used  when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")

  #
  # Checks with `XXXX`: ISO8601 basic format with hours, minutes, and
  # optional seconds fields; 'Z' used  when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")

  #
  # Checks with `XXXXX`: ISO8601 extended format with hours, minutes, and
  # optional seconds fields; 'Z' used  when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")

  #
  # Checks with `X`: ISO8601 basic format with hours and optional minutes field;
  # 'Z' *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-09-06 08:00:00 +00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2012-11-26 08:00:00 +02")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2013-01-01 08:00:00 +0530")

  #
  # Checks with `XX`: ISO8601 basic format with hours and minutes fields; 'Z'
  # *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")

  #
  # Checks with `XXX`: ISO8601 extended format with hours and minutes fields;
  # 'Z' *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-09-06 08:00:00 +00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")

  #
  # Checks with `XXXX`: ISO8601 basic format with hours, minutes, and
  # optional seconds fields; 'Z' *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")

  #
  # Checks with `XXXXX`: ISO8601 extended format with hours, minutes, and
  # optional seconds fields; 'Z' *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_1, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_2, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_3, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_4, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_5, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_6, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_7, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_8, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_9, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-09-06 08:00:00 +00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, dt_format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
})
