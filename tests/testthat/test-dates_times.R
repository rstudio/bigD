test_that("ISO dates can be parsed from a string and formatted", {

  iso_date_1 <- "2018-07-04"
  iso_date_2 <- "2020-01-12"
  iso_date_3 <- "2020-01-04"

  #
  # Checks with `y`/`yy`, `M`/`MM`/`MMM`/`MMMM`/`MMMMM`, `d`/`dd`
  #

  fmt_dt(input = iso_date_1, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_date_1, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_date_1, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_date_1, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_date_1, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")

  #
  # Checks with `L` and `LL`
  #

  fmt_dt(input = iso_date_1, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_date_1, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")

  #
  # Checks with `D`/`DD`/`DDD`
  #

  fmt_dt(input = iso_date_1, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_date_1, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_date_1, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_date_2, format = "y/M/D") %>%
    expect_equal("2020/1/12")
  fmt_dt(input = iso_date_2, format = "y/M/DD") %>%
    expect_equal("2020/1/12")
  fmt_dt(input = iso_date_2, format = "y/M/DDD") %>%
    expect_equal("2020/1/012")
  fmt_dt(input = iso_date_3, format = "y/M/D") %>%
    expect_equal("2020/1/4")
  fmt_dt(input = iso_date_3, format = "y/M/DD") %>%
    expect_equal("2020/1/04")
  fmt_dt(input = iso_date_3, format = "y/M/DDD") %>%
    expect_equal("2020/1/004")
})

test_that("ISO dates can be formatted even with string literals", {

  iso_date_1 <- "2018-07-04"
  iso_date_2 <- "2020-01-12"
  iso_date_3 <- "2020-01-04"

  fmt_dt(input = iso_date_1, format = "'Day': dd, 'Month': MM (yy)") %>%
    expect_equal("Day: 04, Month: 07 (18)")

  fmt_dt(input = iso_date_1, format = "d'th' MMMM y (EEEE).") %>%
    expect_equal("4th July 2018 (Wednesday).")

  fmt_dt(input = iso_date_1, format = "'T:' yyyy-MM-dd") %>%
    expect_equal("T: 2018-07-04")

  fmt_dt(input = iso_date_1, format = "'T: 'yyyy-MM-dd") %>%
    expect_equal("T: 2018-07-04")

  fmt_dt(input = iso_date_1, format = "'' 'T: 'yyyy-MM-dd") %>%
    expect_equal("' T: 2018-07-04")

  fmt_dt(input = iso_date_1, format = "'''''''H' 'T: 'yyyy-MM-dd''''''") %>%
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

  fmt_dt(input = iso_datetime_1, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_datetime_1, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_datetime_1, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_datetime_1, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_datetime_1, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fmt_dt(input = iso_datetime_2, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_datetime_2, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_datetime_2, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_datetime_2, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_datetime_2, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fmt_dt(input = iso_datetime_3, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_datetime_3, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_datetime_3, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_datetime_3, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_datetime_3, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fmt_dt(input = iso_datetime_4, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fmt_dt(input = iso_datetime_4, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fmt_dt(input = iso_datetime_4, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fmt_dt(input = iso_datetime_4, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fmt_dt(input = iso_datetime_4, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")

  #
  # Checks with `L`
  #

  fmt_dt(input = iso_datetime_1, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_datetime_2, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_datetime_3, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_datetime_4, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_datetime_5, format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_6, format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_7, format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_8, format = "y-L-dd") %>%
    expect_equal("2018-10-04")

  #
  # Checks with `LL`
  #

  fmt_dt(input = iso_datetime_1, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fmt_dt(input = iso_datetime_2, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fmt_dt(input = iso_datetime_3, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fmt_dt(input = iso_datetime_4, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fmt_dt(input = iso_datetime_5, format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_6, format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_7, format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fmt_dt(input = iso_datetime_8, format = "y-LL-dd") %>%
    expect_equal("2018-10-04")

  #
  # Checks with `D`
  #

  fmt_dt(input = iso_datetime_1, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_2, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_3, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_4, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_5, format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_6, format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_7, format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_8, format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_9, format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fmt_dt(input = iso_datetime_10, format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fmt_dt(input = iso_datetime_11, format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fmt_dt(input = iso_datetime_12, format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fmt_dt(input = iso_datetime_13, format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_14, format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_15, format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_16, format = "y/M/D") %>%
    expect_equal("2018/1/13")

  #
  # Checks with `DD`
  #

  fmt_dt(input = iso_datetime_1, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_2, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_3, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_4, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_5, format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_6, format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_7, format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_8, format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_9, format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fmt_dt(input = iso_datetime_10, format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fmt_dt(input = iso_datetime_11, format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fmt_dt(input = iso_datetime_12, format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fmt_dt(input = iso_datetime_13, format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_14, format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_15, format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fmt_dt(input = iso_datetime_16, format = "y/M/DD") %>%
    expect_equal("2018/1/13")

  #
  # Checks with `DDD`
  #

  fmt_dt(input = iso_datetime_1, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_2, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_3, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_4, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fmt_dt(input = iso_datetime_5, format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_6, format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_7, format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_8, format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fmt_dt(input = iso_datetime_9, format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fmt_dt(input = iso_datetime_10, format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fmt_dt(input = iso_datetime_11, format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fmt_dt(input = iso_datetime_12, format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fmt_dt(input = iso_datetime_13, format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fmt_dt(input = iso_datetime_14, format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fmt_dt(input = iso_datetime_15, format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fmt_dt(input = iso_datetime_16, format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
})

test_that("ISO datetimes with tz info can be parsed and formatted", {

  dt_list <-
    list(
      iso_datetime_tz_01 = "2018-07-04T22:05-0800",
      iso_datetime_tz_02 = "2018-07-04 22:05-0800",
      iso_datetime_tz_03 = "2018-07-04T22:05:38-0800",
      iso_datetime_tz_04 = "2018-07-04 22:05:38-0800",
      iso_datetime_tz_05 = "2018-07-04T22:05(America/Vancouver)",
      iso_datetime_tz_06 = "2018-07-04 22:05(America/Vancouver)",
      iso_datetime_tz_07 = "2018-07-04T22:05 (America/Vancouver)",
      iso_datetime_tz_08 = "2018-07-04 22:05 (America/Vancouver)",
      iso_datetime_tz_09 = "2018-07-04T22:05-0800(America/Vancouver)",
      iso_datetime_tz_10 = "2018-07-04 22:05-0800(America/Vancouver)",
      iso_datetime_tz_11 = "2018-09-06 08:00(Africa/Accra)",
      iso_datetime_tz_12 = "2012-11-26 08:00(Africa/Johannesburg)",
      iso_datetime_tz_13 = "2013-01-01 08:00(Asia/Kolkata)",
      iso_datetime_tz_14 = "2013-01-01 08:00(Asia/Calcutta)",
      iso_datetime_tz_15 = "2014-05-15 08:00(ROC)",
      iso_datetime_tz_16 = "2014-06-16 08:00(US/Indiana-Starke)",
      iso_datetime_tz_17 = "2014-07-17 08:00(GB-Eire)",
      iso_datetime_tz_18 = "2015-08-18 08:00(Etc/GMT)",
      iso_datetime_tz_19 = "2015-09-19 08:00(GMT)",
      iso_datetime_tz_20 = "2018-07-04T22:05"
    )

  #
  # Checks with `zzz`: short specific non-location format (e.g., 'PDT'); falls
  # back to the short localized GMT format `O`
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-09-06 08:00:00 GMT")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2012-11-26 08:00:00 SAST")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2013-01-01 08:00:00 IST")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2013-01-01 08:00:00 IST")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2014-05-15 08:00:00 CST")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2014-06-16 08:00:00 CDT")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2014-07-17 08:00:00 BST")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2015-08-18 08:00:00 GMT+0")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2015-09-19 08:00:00 GMT+0")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT+0")

  #
  # Checks with `z` and `zz` which should be equivalent to `zzz`
  #

  lapply(
    dt_list[1:13],
    FUN = function(x) {
      expect_equal(
        fmt_dt(input = x, format = "y-MM-dd HH:mm:ss z"),
        fmt_dt(input = x, format = "y-MM-dd HH:mm:ss zzz")
      )
    }
  )
  lapply(
    dt_list[1:13],
    FUN = function(x) {
      expect_equal(
        fmt_dt(input = x, format = "y-MM-dd HH:mm:ss zz"),
        fmt_dt(input = x, format = "y-MM-dd HH:mm:ss zzz")
      )
    }
  )

  #
  # Checks with `zzzz`: long specific non-location format
  # (e.g., 'Pacific Daylight Time'); falls back to the long localized GMT
  # format `OOOO`
  #
  # Not yet implemented
  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Daylight Time")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Daylight Time")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Daylight Time")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Daylight Time")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Daylight Time")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Daylight Time")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-09-06 08:00:00 Abidjan Time")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2012-11-26 08:00:00 Johannesburg Time")
  # fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss zzzz") %>%
  #   expect_equal("2013-01-01 08:00:00 Unknown City Time") # FIXME: this is incorrect
  # fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss zzzz") %>%
  #   expect_equal("2013-01-01 08:00:00 Unknown City Time") # FIXME: this is incorrect
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2014-05-15 08:00:00 Taipei Time")
  # fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss zzzz") %>%
  #   expect_equal("2014-06-16 08:00:00 Unknown City Daylight Time") # FIXME: this is incorrect
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2014-07-17 08:00:00 London Daylight Time")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2015-08-18 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2015-09-19 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT+00:00")


  #
  # Checks with `ZZZ`: ISO8601 basic format
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2015-08-18 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2015-09-19 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 +0000")

  #
  # Checks with `Z` and `ZZ` which should be equivalent to `ZZZ`
  #

  lapply(
    dt_list[1:13],
    FUN = function(x) {
      expect_equal(
        fmt_dt(input = x, format = "y-MM-dd HH:mm:ss Z"),
        fmt_dt(input = x, format = "y-MM-dd HH:mm:ss ZZZ")
      )
    }
  )
  lapply(
    dt_list[1:13],
    FUN = function(x) {
      expect_equal(
        fmt_dt(input = x, format = "y-MM-dd HH:mm:ss ZZ"),
        fmt_dt(input = x, format = "y-MM-dd HH:mm:ss ZZZ")
      )
    }
  )

  #
  # Checks with `ZZZZ`: long localized GMT format (e.g., 'GMT-8:00')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-09-06 08:00:00 GMT+0:00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2012-11-26 08:00:00 GMT+2:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2014-05-15 08:00:00 GMT+8:00")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2014-06-16 08:00:00 GMT-5:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2014-07-17 08:00:00 GMT+1:00")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2015-08-18 08:00:00 GMT+0:00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2015-09-19 08:00:00 GMT+0:00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT+0:00")

  #
  # Checks with `ZZZZZ`: ISO8601 extended format (e.g., '-08:00')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `O`: the short localized GMT format (e.g., 'GMT-8')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-09-06 08:00:00 GMT+0")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2012-11-26 08:00:00 GMT+2")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2014-05-15 08:00:00 GMT+8")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2014-06-16 08:00:00 GMT-5")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2014-07-17 08:00:00 GMT+1")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2015-08-18 08:00:00 GMT+0")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2015-09-19 08:00:00 GMT+0")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT+0")

  #
  # Checks with `OOOO`: the long localized GMT format (e.g., 'GMT-08:00')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-09-06 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2012-11-26 08:00:00 GMT+02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2013-01-01 08:00:00 GMT+05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2013-01-01 08:00:00 GMT+05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2014-05-15 08:00:00 GMT+08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2014-07-17 08:00:00 GMT+01:00")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2015-08-18 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2015-09-19 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT+00:00")

  #
  # Checks with `v`: the short generic non-location format (e.g., 'PT'); falls
  # back to `VVVV` and then to `OOOO`
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-09-06 08:00:00 Greenwich Mean Time")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2012-11-26 08:00:00 South Africa Standard Time")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2014-05-15 08:00:00 Taipei Time")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2014-07-17 08:00:00 Greenwich Mean Time")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2015-08-18 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2015-09-19 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 GMT+00:00")

  #
  # Checks with `vvvv`: the long generic non-location format (e.g.,
  # 'Pacific Time'); falls back to `VVVV` and then to `OOOO`
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-09-06 08:00:00 Greenwich Mean Time")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2012-11-26 08:00:00 South Africa Standard Time")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2014-05-15 08:00:00 Taipei Time")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2014-07-17 08:00:00 Greenwich Mean Time")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2015-08-18 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2015-09-19 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 GMT+00:00")

  #
  # Checks with `V`: the short time zone ID The short time zone ID. Where that
  # is unavailable, the special short time zone ID unk (Unknown Zone) is used.
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 unk")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 unk")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:38 unk")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:38 unk")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-09-06 08:00:00 ciabj")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2012-11-26 08:00:00 zajnb")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2013-01-01 08:00:00 inccu")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2013-01-01 08:00:00 inccu")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2014-05-15 08:00:00 twtpe")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2014-06-16 08:00:00 usknx")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2014-07-17 08:00:00 gblon")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2015-08-18 08:00:00 gmt")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2015-09-19 08:00:00 gmt")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 utc")

  #
  # Checks with `VV`: the long time zone ID (e.g., America/Los_Angeles)
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 ")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 ")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:38 ")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:38 ")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-09-06 08:00:00 Africa/Abidjan")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2012-11-26 08:00:00 Africa/Johannesburg")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2013-01-01 08:00:00 Asia/Kolkata")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2013-01-01 08:00:00 Asia/Kolkata")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2014-05-15 08:00:00 Asia/Taipei")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2014-06-16 08:00:00 America/Indiana/Knox")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2014-07-17 08:00:00 Europe/London")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2015-08-18 08:00:00 Etc/GMT")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2015-09-19 08:00:00 Etc/GMT")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 UTC")

  #
  # Checks with `VVV`: the localized exemplar city name; if not available then
  # use the Etc/Unknown fallback ('Unknown City')
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Unknown City")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Unknown City")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:38 Unknown City")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:38 Unknown City")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-09-06 08:00:00 Abidjan")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2012-11-26 08:00:00 Johannesburg")
  # fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss VVV")
  # fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss VVV")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2014-05-15 08:00:00 Taipei")
  # fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss VVV")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2014-07-17 08:00:00 London")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2015-08-18 08:00:00 Unknown City")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2015-09-19 08:00:00 Unknown City")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Unknown City")

  #
  # Checks with `VVVV`: the generic location format (e.g., 'Los Angeles Time');
  # if not available then long localized GMT format (`OOOO`)
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-09-06 08:00:00 Abidjan Time")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2012-11-26 08:00:00 Johannesburg Time")
  # fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss VVVV") %>%
  #   expect_equal("2013-01-01 08:00:00 Calcutta Time")
  # fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss VVVV") %>%
  #   expect_equal("2013-01-01 08:00:00 GMT+05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2014-05-15 08:00:00 Taipei Time")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2014-07-17 08:00:00 London Time")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2015-08-18 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2015-09-19 08:00:00 GMT+00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 GMT+00:00")

  #
  # Checks with `X`: ISO8601 basic format with hours and optional minutes field;
  # 'Z' used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2012-11-26 08:00:00 +02")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2014-05-15 08:00:00 +08")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2014-06-16 08:00:00 -05")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2014-07-17 08:00:00 +01")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `XX`: ISO8601 basic format with hours and minutes fields; 'Z' used
  # when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `XXX`: ISO8601 extended format with hours and minutes fields;
  # 'Z' used  when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `XXXX`: ISO8601 basic format with hours, minutes, and
  # optional seconds fields; 'Z' used  when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `XXXXX`: ISO8601 extended format with hours, minutes, and
  # optional seconds fields; 'Z' used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `X`: ISO8601 basic format with hours and optional minutes field;
  # 'Z' *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-09-06 08:00:00 +00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2012-11-26 08:00:00 +02")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2014-05-15 08:00:00 +08")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2014-06-16 08:00:00 -05")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2014-07-17 08:00:00 +01")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2015-08-18 08:00:00 +00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2015-09-19 08:00:00 +00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 +00")

  #
  # Checks with `XX`: ISO8601 basic format with hours and minutes fields; 'Z'
  # *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2015-08-18 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2015-09-19 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 +0000")

  #
  # Checks with `XXX`: ISO8601 extended format with hours and minutes fields;
  # 'Z' *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-09-06 08:00:00 +00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2015-08-18 08:00:00 +00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2015-09-19 08:00:00 +00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 +00:00")

  #
  # Checks with `XXXX`: ISO8601 basic format with hours, minutes, and
  # optional seconds fields; 'Z' *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2015-08-18 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2015-09-19 08:00:00 +0000")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 +0000")

  #
  # Checks with `XXXXX`: ISO8601 extended format with hours, minutes, and
  # optional seconds fields; 'Z' *is not* used when local time offset is 0
  #

  fmt_dt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fmt_dt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-09-06 08:00:00 +00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fmt_dt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fmt_dt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fmt_dt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fmt_dt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fmt_dt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2015-08-18 08:00:00 +00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2015-09-19 08:00:00 +00:00")
  fmt_dt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 +00:00")
})
