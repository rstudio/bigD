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
  iso_datetime_17 <- "2019-01-15 20:15:24.45678"
  iso_datetime_18 <- "2019-01-15 20:15:24.00160"
  iso_datetime_19 <- "2019-01-15 00:00:00"

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

  #
  # Checks with 'S+'
  #

  fmt_dt(input = iso_datetime_16, format = "y/MM/DD HH:mm:ss.S") %>%
    expect_equal("2018/01/13 22:05:00.0")
  fmt_dt(input = iso_datetime_16, format = "y/MM/DD HH:mm:ss.SS") %>%
    expect_equal("2018/01/13 22:05:00.00")
  fmt_dt(input = iso_datetime_16, format = "y/MM/DD HH:mm:ss.SSS") %>%
    expect_equal("2018/01/13 22:05:00.000")

  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.S") %>%
    expect_equal("2019/01/15 20:15:24.4")
  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SS") %>%
    expect_equal("2019/01/15 20:15:24.45")
  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSS") %>%
    expect_equal("2019/01/15 20:15:24.456")
  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSS") %>%
    expect_equal("2019/01/15 20:15:24.4560")
  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSS") %>%
    expect_equal("2019/01/15 20:15:24.45600")
  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.456000")
  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.4560000")
  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.45600000")
  fmt_dt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.456000000")

  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.S") %>%
    expect_equal("2019/01/15 20:15:24.0")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SS") %>%
    expect_equal("2019/01/15 20:15:24.00")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSS") %>%
    expect_equal("2019/01/15 20:15:24.001")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSS") %>%
    expect_equal("2019/01/15 20:15:24.0010")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSS") %>%
    expect_equal("2019/01/15 20:15:24.00100")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.001000")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.0010000")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.00100000")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.001000000")

  #
  # Checks with 'A+'
  #

  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (A)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fmt_dt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAAAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (072924001)")

  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (A)") %>%
    expect_equal("2019/01/15 00:00:00 (0)")
  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AA)") %>%
    expect_equal("2019/01/15 00:00:00 (00)")
  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAA)") %>%
    expect_equal("2019/01/15 00:00:00 (000)")
  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (0000)")
  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (00000)")
  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (000000)")
  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (0000000)")
  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (00000000)")
  fmt_dt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAAAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (000000000)")
})
