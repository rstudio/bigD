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

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 PDT")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-09-06 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2012-11-26 08:00:00 SAST")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2013-01-01 08:00:00 IST")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2013-01-01 08:00:00 IST")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2014-05-15 08:00:00 CST")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2014-06-16 08:00:00 CDT")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2014-07-17 08:00:00 BST")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2015-08-18 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2015-09-19 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss zzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT")

  #
  # Checks with `z` and `zz` which should be equivalent to `zzz`
  #

  lapply(
    dt_list[1:13],
    FUN = function(x) {
      expect_equal(
        fdt(input = x, format = "y-MM-dd HH:mm:ss z"),
        fdt(input = x, format = "y-MM-dd HH:mm:ss zzz")
      )
    }
  )
  lapply(
    dt_list[1:13],
    FUN = function(x) {
      expect_equal(
        fdt(input = x, format = "y-MM-dd HH:mm:ss zz"),
        fdt(input = x, format = "y-MM-dd HH:mm:ss zzz")
      )
    }
  )

  #
  # Checks with `zzzz`: long specific non-location format
  # (e.g., 'Pacific Daylight Time'); falls back to the long localized GMT
  # format `OOOO`
  #
  # Not yet implemented
  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Daylight Time")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Daylight Time")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Daylight Time")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Daylight Time")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Daylight Time")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Daylight Time")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-09-06 08:00:00 Greenwich Mean Time")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2012-11-26 08:00:00 South Africa Standard Time")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2014-05-15 08:00:00 Taipei Standard Time")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2014-07-17 08:00:00 GMT+01:00")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2015-08-18 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2015-09-19 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss zzzz") %>%
    expect_equal("2018-07-04 22:05:00 GMT")


  #
  # Checks with `ZZZ`: ISO8601 basic format
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2015-08-18 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2015-09-19 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss ZZZ") %>%
    expect_equal("2018-07-04 22:05:00 +0000")

  #
  # Checks with `Z` and `ZZ` which should be equivalent to `ZZZ`
  #

  lapply(
    dt_list[1:13],
    FUN = function(x) {
      expect_equal(
        fdt(input = x, format = "y-MM-dd HH:mm:ss Z"),
        fdt(input = x, format = "y-MM-dd HH:mm:ss ZZZ")
      )
    }
  )
  lapply(
    dt_list[1:13],
    FUN = function(x) {
      expect_equal(
        fdt(input = x, format = "y-MM-dd HH:mm:ss ZZ"),
        fdt(input = x, format = "y-MM-dd HH:mm:ss ZZZ")
      )
    }
  )

  #
  # Checks with `ZZZZ`: long localized GMT format (e.g., 'GMT-8:00')
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7:00")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-09-06 08:00:00 GMT+0:00")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2012-11-26 08:00:00 GMT+2:00")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2014-05-15 08:00:00 GMT+8:00")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2014-06-16 08:00:00 GMT-5:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2014-07-17 08:00:00 GMT+1:00")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2015-08-18 08:00:00 GMT+0:00")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2015-09-19 08:00:00 GMT+0:00")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss ZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 GMT+0:00")

  #
  # Checks with `ZZZZZ`: ISO8601 extended format (e.g., '-08:00')
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss ZZZZZ") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `O`: the short localized GMT format (e.g., 'GMT-8')
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-8")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:38 GMT-8")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT-7")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-09-06 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2012-11-26 08:00:00 GMT+2")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2013-01-01 08:00:00 GMT+5:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2014-05-15 08:00:00 GMT+8")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2014-06-16 08:00:00 GMT-5")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2014-07-17 08:00:00 GMT+1")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2015-08-18 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2015-09-19 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss O") %>%
    expect_equal("2018-07-04 22:05:00 GMT")

  #
  # Checks with `OOOO`: the long localized GMT format (e.g., 'GMT-08:00')
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT-07:00")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-09-06 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2012-11-26 08:00:00 GMT+02:00")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2013-01-01 08:00:00 GMT+05:30")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2013-01-01 08:00:00 GMT+05:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2014-05-15 08:00:00 GMT+08:00")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2014-07-17 08:00:00 GMT+01:00")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2015-08-18 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2015-09-19 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss OOOO") %>%
    expect_equal("2018-07-04 22:05:00 GMT")

  #
  # Checks with `v`: the short generic non-location format (e.g., 'PT'); falls
  # back to `VVVV` and then to `OOOO`
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 PT")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-09-06 08:00:00 Greenwich Mean Time")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2012-11-26 08:00:00 South Africa Standard Time")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2014-05-15 08:00:00 Taipei Time")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2014-07-17 08:00:00 Greenwich Mean Time")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2015-08-18 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2015-09-19 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss v") %>%
    expect_equal("2018-07-04 22:05:00 GMT")

  #
  # Checks with `vvvv`: the long generic non-location format (e.g.,
  # 'Pacific Time'); falls back to `VVVV` and then to `OOOO`
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 Pacific Time")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-09-06 08:00:00 Greenwich Mean Time")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2012-11-26 08:00:00 South Africa Standard Time")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2013-01-01 08:00:00 India Standard Time")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2014-05-15 08:00:00 Taipei Time")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2014-07-17 08:00:00 Greenwich Mean Time")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2015-08-18 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2015-09-19 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss vvvv") %>%
    expect_equal("2018-07-04 22:05:00 GMT")

  #
  # Checks with `V`: the short time zone ID The short time zone ID. Where that
  # is unavailable, the special short time zone ID unk (Unknown Zone) is used.
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 unk")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 unk")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:38 unk")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:38 unk")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 cavan")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-09-06 08:00:00 ciabj")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2012-11-26 08:00:00 zajnb")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2013-01-01 08:00:00 inccu")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2013-01-01 08:00:00 inccu")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2014-05-15 08:00:00 twtpe")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2014-06-16 08:00:00 usknx")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2014-07-17 08:00:00 gblon")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2015-08-18 08:00:00 gmt")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2015-09-19 08:00:00 gmt")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss V") %>%
    expect_equal("2018-07-04 22:05:00 utc")

  #
  # Checks with `VV`: the long time zone ID (e.g., America/Los_Angeles)
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 ")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 ")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:38 ")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:38 ")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 America/Vancouver")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-09-06 08:00:00 Africa/Abidjan")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2012-11-26 08:00:00 Africa/Johannesburg")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2013-01-01 08:00:00 Asia/Kolkata")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2013-01-01 08:00:00 Asia/Kolkata")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2014-05-15 08:00:00 Asia/Taipei")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2014-06-16 08:00:00 America/Indiana/Knox")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2014-07-17 08:00:00 Europe/London")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2015-08-18 08:00:00 Etc/GMT")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2015-09-19 08:00:00 Etc/GMT")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss VV") %>%
    expect_equal("2018-07-04 22:05:00 UTC")

  #
  # Checks with `VVV`: the localized exemplar city name; if not available then
  # use the Etc/Unknown fallback ('Unknown City')
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Unknown City")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Unknown City")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:38 Unknown City")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:38 Unknown City")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-09-06 08:00:00 Abidjan")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2012-11-26 08:00:00 Johannesburg")
  # fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss VVV")
  # fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss VVV")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2014-05-15 08:00:00 Taipei")
  # fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss VVV")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2014-07-17 08:00:00 London")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2015-08-18 08:00:00 Unknown City")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2015-09-19 08:00:00 Unknown City")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss VVV") %>%
    expect_equal("2018-07-04 22:05:00 Unknown City")

  #
  # Checks with `VVVV`: the generic location format (e.g., 'Los Angeles Time');
  # if not available then long localized GMT format (`OOOO`)
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:38 GMT-08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 Vancouver Time")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-09-06 08:00:00 Abidjan Time")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2012-11-26 08:00:00 Johannesburg Time")
  # fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss VVVV") %>%
  #   expect_equal("2013-01-01 08:00:00 Calcutta Time")
  # fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss VVVV") %>%
  #   expect_equal("2013-01-01 08:00:00 GMT+05:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2014-05-15 08:00:00 Taipei Time")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2014-06-16 08:00:00 GMT-05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2014-07-17 08:00:00 London Time")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2015-08-18 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2015-09-19 08:00:00 GMT")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss VVVV") %>%
    expect_equal("2018-07-04 22:05:00 GMT")

  #
  # Checks with `X`: ISO8601 basic format with hours and optional minutes field;
  # 'Z' used when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2012-11-26 08:00:00 +02")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2014-05-15 08:00:00 +08")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2014-06-16 08:00:00 -05")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2014-07-17 08:00:00 +01")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss X") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `XX`: ISO8601 basic format with hours and minutes fields; 'Z' used
  # when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss XX") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `XXX`: ISO8601 extended format with hours and minutes fields;
  # 'Z' used  when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss XXX") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `XXXX`: ISO8601 basic format with hours, minutes, and
  # optional seconds fields; 'Z' used  when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss XXXX") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `XXXXX`: ISO8601 extended format with hours, minutes, and
  # optional seconds fields; 'Z' used when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-09-06 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2015-08-18 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2015-09-19 08:00:00 Z")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss XXXXX") %>%
    expect_equal("2018-07-04 22:05:00 Z")

  #
  # Checks with `X`: ISO8601 basic format with hours and optional minutes field;
  # 'Z' *is not* used when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -08")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:38 -08")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 -07")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-09-06 08:00:00 +00")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2012-11-26 08:00:00 +02")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2014-05-15 08:00:00 +08")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2014-06-16 08:00:00 -05")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2014-07-17 08:00:00 +01")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2015-08-18 08:00:00 +00")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2015-09-19 08:00:00 +00")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss x") %>%
    expect_equal("2018-07-04 22:05:00 +00")

  #
  # Checks with `XX`: ISO8601 basic format with hours and minutes fields; 'Z'
  # *is not* used when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2015-08-18 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2015-09-19 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss xx") %>%
    expect_equal("2018-07-04 22:05:00 +0000")

  #
  # Checks with `XXX`: ISO8601 extended format with hours and minutes fields;
  # 'Z' *is not* used when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-09-06 08:00:00 +00:00")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2015-08-18 08:00:00 +00:00")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2015-09-19 08:00:00 +00:00")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss xxx") %>%
    expect_equal("2018-07-04 22:05:00 +00:00")

  #
  # Checks with `XXXX`: ISO8601 basic format with hours, minutes, and
  # optional seconds fields; 'Z' *is not* used when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0800")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:38 -0800")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 -0700")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-09-06 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2012-11-26 08:00:00 +0200")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2013-01-01 08:00:00 +0530")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2014-05-15 08:00:00 +0800")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2014-06-16 08:00:00 -0500")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2014-07-17 08:00:00 +0100")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2015-08-18 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2015-09-19 08:00:00 +0000")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss xxxx") %>%
    expect_equal("2018-07-04 22:05:00 +0000")

  #
  # Checks with `XXXXX`: ISO8601 extended format with hours, minutes, and
  # optional seconds fields; 'Z' *is not* used when local time offset is 0
  #

  fdt(input = dt_list$iso_datetime_tz_01, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_02, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -08:00")
  fdt(input = dt_list$iso_datetime_tz_03, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_04, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:38 -08:00")
  fdt(input = dt_list$iso_datetime_tz_05, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_06, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_07, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_08, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_09, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_10, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 -07:00")
  fdt(input = dt_list$iso_datetime_tz_11, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-09-06 08:00:00 +00:00")
  fdt(input = dt_list$iso_datetime_tz_12, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2012-11-26 08:00:00 +02:00")
  fdt(input = dt_list$iso_datetime_tz_13, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_14, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2013-01-01 08:00:00 +05:30")
  fdt(input = dt_list$iso_datetime_tz_15, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2014-05-15 08:00:00 +08:00")
  fdt(input = dt_list$iso_datetime_tz_16, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2014-06-16 08:00:00 -05:00")
  fdt(input = dt_list$iso_datetime_tz_17, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2014-07-17 08:00:00 +01:00")
  fdt(input = dt_list$iso_datetime_tz_18, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2015-08-18 08:00:00 +00:00")
  fdt(input = dt_list$iso_datetime_tz_19, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2015-09-19 08:00:00 +00:00")
  fdt(input = dt_list$iso_datetime_tz_20, format = "y-MM-dd HH:mm:ss xxxxx") %>%
    expect_equal("2018-07-04 22:05:00 +00:00")

  #
  # Test that the time zone can be overridden with the `use_tz` argument
  #

  fdt(
    input = dt_list$iso_datetime_tz_20,
    format = "y-MM-dd HH:mm:ss xx",
    use_tz = "America/Toronto"
  ) %>%
    expect_equal("2018-07-04 22:05:00 -0400")

  fdt(
    input = dt_list$iso_datetime_tz_14,
    format = "y-MM-dd HH:mm:ss xx",
    use_tz = "America/Toronto"
  ) %>%
    expect_equal("2013-01-01 08:00:00 -0500")

  fdt(
    input = dt_list$iso_datetime_tz_14,
    format = "y-MM-dd HH:mm:ss VVVV",
    use_tz = "America/Toronto"
  ) %>%
    expect_equal("2013-01-01 08:00:00 Toronto Time")

  fdt(
    input = dt_list$iso_datetime_tz_14,
    format = "y-MM-dd HH:mm:ss V",
    use_tz = "America/Toronto"
  ) %>%
    expect_equal("2013-01-01 08:00:00 cator")

  fdt(
    input = dt_list$iso_datetime_tz_20,
    format = "y-MM-dd HH:mm:ss xx",
    use_tz = "Australia/NSW"
  ) %>%
    expect_equal("2018-07-04 22:05:00 +1000")

  fdt(
    input = dt_list$iso_datetime_tz_14,
    format = "y-MM-dd HH:mm:ss xx",
    use_tz = "Australia/NSW"
  ) %>%
    expect_equal("2013-01-01 08:00:00 +1100")

  fdt(
    input = dt_list$iso_datetime_tz_14,
    format = "y-MM-dd HH:mm:ss VVVV",
    use_tz = "Australia/NSW"
  ) %>%
    expect_equal("2013-01-01 08:00:00 Sydney Time")

  fdt(
    input = dt_list$iso_datetime_tz_14,
    format = "y-MM-dd HH:mm:ss V",
    use_tz = "Australia/NSW"
  ) %>%
    expect_equal("2013-01-01 08:00:00 ausyd")
})
