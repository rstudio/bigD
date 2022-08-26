test_that("ISO dates can be parsed from a string and formatted", {

  iso_date_1 <- "2018-07-04"
  iso_date_2 <- "2020-01-12"
  iso_date_3 <- "2020-01-04"

  #
  # Checks with `y`/`yy`, `M`/`MM`/`MMM`/`MMMM`/`MMMMM`, `d`/`dd`
  #

  fdt(input = iso_date_1, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fdt(input = iso_date_1, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fdt(input = iso_date_1, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fdt(input = iso_date_1, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fdt(input = iso_date_1, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")

  #
  # Checks with `L` and `LL`
  #

  fdt(input = iso_date_1, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fdt(input = iso_date_1, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")

  #
  # Checks with `D`/`DD`/`DDD`
  #

  fdt(input = iso_date_1, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_date_1, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_date_1, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_date_2, format = "y/M/D") %>%
    expect_equal("2020/1/12")
  fdt(input = iso_date_2, format = "y/M/DD") %>%
    expect_equal("2020/1/12")
  fdt(input = iso_date_2, format = "y/M/DDD") %>%
    expect_equal("2020/1/012")
  fdt(input = iso_date_3, format = "y/M/D") %>%
    expect_equal("2020/1/4")
  fdt(input = iso_date_3, format = "y/M/DD") %>%
    expect_equal("2020/1/04")
  fdt(input = iso_date_3, format = "y/M/DDD") %>%
    expect_equal("2020/1/004")
})

test_that("Distant years can be accepted", {

  fdt(input = "1001-01-01") %>%
    expect_equal("1001-01-01T00:00:00Z")
  fdt(input = "0900-01-01") %>%
    expect_equal("0900-01-01T00:00:00Z")
  fdt(input = "0090-01-01") %>%
    expect_equal("0090-01-01T00:00:00Z")
  fdt(input = "0009-01-01") %>%
    expect_equal("0009-01-01T00:00:00Z")
  fdt(input = "0000-01-01") %>%
    expect_equal("0000-01-01T00:00:00Z")
  fdt(input = "9001-01-01") %>%
    expect_equal("9001-01-01T00:00:00Z")

  fdt(input = "1001-01-01", format = "y") %>%
    expect_equal("1001")
  fdt(input = "0900-01-01", format = "y") %>%
    expect_equal("900")
  fdt(input = "0090-01-01", format = "y") %>%
    expect_equal("90")
  fdt(input = "0009-01-01", format = "y") %>%
    expect_equal("9")
  fdt(input = "0000-01-01", format = "y") %>%
    expect_equal("0")
  fdt(input = "9001-01-01", format = "y") %>%
    expect_equal("9001")

  fdt(input = "1001-01-01", format = "yy") %>%
    expect_equal("01")
  fdt(input = "0900-01-01", format = "yy") %>%
    expect_equal("00")
  fdt(input = "0090-01-01", format = "yy") %>%
    expect_equal("90")
  fdt(input = "0009-01-01", format = "yy") %>%
    expect_equal("09")
  fdt(input = "0000-01-01", format = "yy") %>%
    expect_equal("00")
  fdt(input = "9001-01-01", format = "yy") %>%
    expect_equal("01")

  fdt(input = "1001-01-01", format = "yyy") %>%
    expect_equal("1001")
  fdt(input = "0900-01-01", format = "yyy") %>%
    expect_equal("900")
  fdt(input = "0090-01-01", format = "yyy") %>%
    expect_equal("090")
  fdt(input = "0009-01-01", format = "yyy") %>%
    expect_equal("009")
  fdt(input = "0000-01-01", format = "yyy") %>%
    expect_equal("000")
  fdt(input = "9001-01-01", format = "yyy") %>%
    expect_equal("9001")
})

test_that("Years by themselves are accepted", {

  fdt(input = "1001") %>%
    expect_equal("1001-01-01T00:00:00Z")
  fdt(input = "0900") %>%
    expect_equal("0900-01-01T00:00:00Z")
  fdt(input = "0090") %>%
    expect_equal("0090-01-01T00:00:00Z")
  fdt(input = "0009") %>%
    expect_equal("0009-01-01T00:00:00Z")
  fdt(input = "0000") %>%
    expect_equal("0000-01-01T00:00:00Z")
  fdt(input = "9001") %>%
    expect_equal("9001-01-01T00:00:00Z")

  fdt(input = "1001", format = "y") %>%
    expect_equal("1001")
  fdt(input = "0900", format = "y") %>%
    expect_equal("900")
  fdt(input = "0090", format = "y") %>%
    expect_equal("90")
  fdt(input = "0009", format = "y") %>%
    expect_equal("9")
  fdt(input = "0000", format = "y") %>%
    expect_equal("0")
  fdt(input = "9001", format = "y") %>%
    expect_equal("9001")

  fdt(input = "1001", format = "yy") %>%
    expect_equal("01")
  fdt(input = "0900", format = "yy") %>%
    expect_equal("00")
  fdt(input = "0090", format = "yy") %>%
    expect_equal("90")
  fdt(input = "0009", format = "yy") %>%
    expect_equal("09")
  fdt(input = "0000", format = "yy") %>%
    expect_equal("00")
  fdt(input = "9001", format = "yy") %>%
    expect_equal("01")

  fdt(input = "1001", format = "yyy") %>%
    expect_equal("1001")
  fdt(input = "0900", format = "yyy") %>%
    expect_equal("900")
  fdt(input = "0090", format = "yyy") %>%
    expect_equal("090")
  fdt(input = "0009", format = "yyy") %>%
    expect_equal("009")
  fdt(input = "0000", format = "yyy") %>%
    expect_equal("000")
  fdt(input = "9001", format = "yyy") %>%
    expect_equal("9001")
})

test_that("The various week date formats are accepted", {

  expect_equal(fdt("2023-W52"), "2023-12-25T00:00:00Z")
  expect_equal(fdt("2023-W52"), fdt("2023W52"))
  expect_equal(fdt("2023-W52"), fdt("2023-W52-1"))
  expect_equal(fdt("2023-W52"), fdt("2023W521"))
  expect_equal(fdt("2023-W01"), "2023-01-02T00:00:00Z")
  expect_equal(fdt("2023-W01-1"), "2023-01-02T00:00:00Z")
  expect_equal(fdt("2023-W01-2"), "2023-01-03T00:00:00Z")

  expect_equal(fdt("2023-W01-3"), "2023-01-04T00:00:00Z")
  expect_equal(fdt("2023-W01-4"), "2023-01-05T00:00:00Z")
  expect_equal(fdt("2023-W01-5"), "2023-01-06T00:00:00Z")
  expect_equal(fdt("2023-W01-6"), "2023-01-07T00:00:00Z")
  expect_equal(fdt("2023-W01-7"), "2023-01-08T00:00:00Z")

  expect_equal(fdt("1922-W01"), "1922-01-02T00:00:00Z")
  expect_equal(fdt("1963-W01"), "1962-12-31T00:00:00Z")
  expect_equal(fdt("1963-W53"), "1963-12-30T00:00:00Z")
})

test_that("The --MM-DD and --MMDD formats are accepted", {

  expect_equal(fdt(input = "--03-05", format = "MM-dd"), "03-05")
  expect_equal(fdt(input = "--0305", format = "MM-dd"), "03-05")

  expect_equal(fdt(input = "--12-05", format = "MM-dd"), "12-05")
  expect_equal(fdt(input = "--1205", format = "MM-dd"), "12-05")

  expect_equal(fdt(input = "--06-30", format = "MM-dd"), "06-30")
  expect_equal(fdt(input = "--0630", format = "MM-dd"), "06-30")
})

test_that("Years with BC or AD (or variations of these) can be parsed", {

  expect_equal(fdt(input = "5000 BC", format = "y G"), "5000 BC")
  expect_equal(fdt(input = "5000 BC", format = "y GGGG"), "5000 Before Christ")
  expect_equal(fdt(input = "5000 BC", format = "y GGGGG"), "5000 B")

  expect_equal(fdt(input = "101010 BC", format = "y G"), "101010 BC")
  expect_equal(fdt(input = "5101010 AD", format = "y G"), "5101010 AD")

  expect_equal(fdt(input = "500 BCE", format = "y G"), "500 BC")
  expect_equal(fdt(input = "500 B.C.E.", format = "y G"), "500 BC")
  expect_equal(fdt(input = "20 AD", format = "y G"), "20 AD")
  expect_equal(fdt(input = "20 CE", format = "y G"), "20 AD")
  expect_equal(fdt(input = "20 C.E.", format = "y G"), "20 AD")
  expect_equal(fdt(input = "20 EV", format = "y G"), "20 AD")

  expect_equal(fdt(input = "500 BCE", format = "y G", locale = "es"), "500 a. C.")
  expect_equal(fdt(input = "500 AD", format = "y G", locale = "es"), "500 d. C.")
})

test_that("The YYYY-MM format is accepted", {

  fdt(input = "1001-02") %>%
    expect_equal("1001-02-01T00:00:00Z")
  fdt(input = "0900-03") %>%
    expect_equal("0900-03-01T00:00:00Z")
  fdt(input = "0090-04") %>%
    expect_equal("0090-04-01T00:00:00Z")
  fdt(input = "0009-05") %>%
    expect_equal("0009-05-01T00:00:00Z")
  fdt(input = "0000-06") %>%
    expect_equal("0000-06-01T00:00:00Z")
  fdt(input = "9001-11") %>%
    expect_equal("9001-11-01T00:00:00Z")

  fdt(input = "1001-05", format = "y/MM") %>%
    expect_equal("1001/05")
  fdt(input = "0900-10", format = "y/MM") %>%
    expect_equal("900/10")
  fdt(input = "0090-05", format = "y/MM") %>%
    expect_equal("90/05")
  fdt(input = "0009-10", format = "y/MM") %>%
    expect_equal("9/10")
  fdt(input = "0000-05", format = "y/MM") %>%
    expect_equal("0/05")
  fdt(input = "9001-10", format = "y/MM") %>%
    expect_equal("9001/10")
})

test_that("ISO dates can be formatted even with string literals", {

  iso_date_1 <- "2018-07-04"
  iso_date_2 <- "2020-01-12"
  iso_date_3 <- "2020-01-04"

  fdt(input = iso_date_1, format = "'Day': dd, 'Month': MM (yy)") %>%
    expect_equal("Day: 04, Month: 07 (18)")

  fdt(input = iso_date_1, format = "'dd': dd, 'MM': MM ('yy': yy)") %>%
    expect_equal("dd: 04, MM: 07 (yy: 18)")

  fdt(input = iso_date_1, format = "'d': dd, 'M': MM ('y': yy)") %>%
    expect_equal("d: 04, M: 07 (y: 18)")

  fdt(input = iso_date_1, format = "h 'h' mm 'min' ss 's' B", locale = "fr_CA") %>%
    expect_equal("12 h 00 min 00 s minuit")

  fdt(input = iso_date_1, format = "d'th' MMMM y (EEEE).") %>%
    expect_equal("4th July 2018 (Wednesday).")

  fdt(input = iso_date_1, format = "'T:' yyyy-MM-dd") %>%
    expect_equal("T: 2018-07-04")

  fdt(input = iso_date_1, format = "'T: 'yyyy-MM-dd") %>%
    expect_equal("T: 2018-07-04")

  fdt(input = iso_date_1, format = "'' 'T: 'yyyy-MM-dd") %>%
    expect_equal("' T: 2018-07-04")

  fdt(input = iso_date_1, format = "'''''''H' 'T: 'yyyy-MM-dd''''''") %>%
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

  fdt(input = iso_datetime_1, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fdt(input = iso_datetime_1, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fdt(input = iso_datetime_1, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fdt(input = iso_datetime_1, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fdt(input = iso_datetime_1, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fdt(input = iso_datetime_2, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fdt(input = iso_datetime_2, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fdt(input = iso_datetime_2, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fdt(input = iso_datetime_2, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fdt(input = iso_datetime_2, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fdt(input = iso_datetime_3, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fdt(input = iso_datetime_3, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fdt(input = iso_datetime_3, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fdt(input = iso_datetime_3, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fdt(input = iso_datetime_3, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")
  fdt(input = iso_datetime_4, format = "y/M/d") %>%
    expect_equal("2018/7/4")
  fdt(input = iso_datetime_4, format = "yy-MM-dd") %>%
    expect_equal("18-07-04")
  fdt(input = iso_datetime_4, format = "y-MMM-dd") %>%
    expect_equal("2018-Jul-04")
  fdt(input = iso_datetime_4, format = "y-MMMM-dd") %>%
    expect_equal("2018-July-04")
  fdt(input = iso_datetime_4, format = "y-MMMMM-dd") %>%
    expect_equal("2018-J-04")

  #
  # Checks with `L`
  #

  fdt(input = iso_datetime_1, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fdt(input = iso_datetime_2, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fdt(input = iso_datetime_3, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fdt(input = iso_datetime_4, format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fdt(input = iso_datetime_5, format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fdt(input = iso_datetime_6, format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fdt(input = iso_datetime_7, format = "y-L-dd") %>%
    expect_equal("2018-10-04")
  fdt(input = iso_datetime_8, format = "y-L-dd") %>%
    expect_equal("2018-10-04")

  #
  # Checks with `LL`
  #

  fdt(input = iso_datetime_1, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fdt(input = iso_datetime_2, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fdt(input = iso_datetime_3, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fdt(input = iso_datetime_4, format = "y-LL-dd") %>%
    expect_equal("2018-07-04")
  fdt(input = iso_datetime_5, format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fdt(input = iso_datetime_6, format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fdt(input = iso_datetime_7, format = "y-LL-dd") %>%
    expect_equal("2018-10-04")
  fdt(input = iso_datetime_8, format = "y-LL-dd") %>%
    expect_equal("2018-10-04")

  #
  # Checks with `D`
  #

  fdt(input = iso_datetime_1, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_2, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_3, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_4, format = "y/M/D") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_5, format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_6, format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_7, format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_8, format = "y/M/D") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_9, format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fdt(input = iso_datetime_10, format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fdt(input = iso_datetime_11, format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fdt(input = iso_datetime_12, format = "y/M/D") %>%
    expect_equal("2018/1/4")
  fdt(input = iso_datetime_13, format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fdt(input = iso_datetime_14, format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fdt(input = iso_datetime_15, format = "y/M/D") %>%
    expect_equal("2018/1/13")
  fdt(input = iso_datetime_16, format = "y/M/D") %>%
    expect_equal("2018/1/13")

  #
  # Checks with `DD`
  #

  fdt(input = iso_datetime_1, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_2, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_3, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_4, format = "y/M/DD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_5, format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_6, format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_7, format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_8, format = "y/M/DD") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_9, format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fdt(input = iso_datetime_10, format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fdt(input = iso_datetime_11, format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fdt(input = iso_datetime_12, format = "y/M/DD") %>%
    expect_equal("2018/1/04")
  fdt(input = iso_datetime_13, format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fdt(input = iso_datetime_14, format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fdt(input = iso_datetime_15, format = "y/M/DD") %>%
    expect_equal("2018/1/13")
  fdt(input = iso_datetime_16, format = "y/M/DD") %>%
    expect_equal("2018/1/13")

  #
  # Checks with `DDD`
  #

  fdt(input = iso_datetime_1, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_2, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_3, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_4, format = "y/M/DDD") %>%
    expect_equal("2018/7/185")
  fdt(input = iso_datetime_5, format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_6, format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_7, format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_8, format = "y/M/DDD") %>%
    expect_equal("2018/10/277")
  fdt(input = iso_datetime_9, format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fdt(input = iso_datetime_10, format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fdt(input = iso_datetime_11, format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fdt(input = iso_datetime_12, format = "y/M/DDD") %>%
    expect_equal("2018/1/004")
  fdt(input = iso_datetime_13, format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fdt(input = iso_datetime_14, format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fdt(input = iso_datetime_15, format = "y/M/DDD") %>%
    expect_equal("2018/1/013")
  fdt(input = iso_datetime_16, format = "y/M/DDD") %>%
    expect_equal("2018/1/013")

  #
  # Checks with 'S+'
  #

  fdt(input = iso_datetime_16, format = "y/MM/DD HH:mm:ss.S") %>%
    expect_equal("2018/01/13 22:05:00.0")
  fdt(input = iso_datetime_16, format = "y/MM/DD HH:mm:ss.SS") %>%
    expect_equal("2018/01/13 22:05:00.00")
  fdt(input = iso_datetime_16, format = "y/MM/DD HH:mm:ss.SSS") %>%
    expect_equal("2018/01/13 22:05:00.000")

  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.S") %>%
    expect_equal("2019/01/15 20:15:24.4")
  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SS") %>%
    expect_equal("2019/01/15 20:15:24.45")
  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSS") %>%
    expect_equal("2019/01/15 20:15:24.456")
  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSS") %>%
    expect_equal("2019/01/15 20:15:24.4560")
  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSS") %>%
    expect_equal("2019/01/15 20:15:24.45600")
  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.456000")
  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.4560000")
  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.45600000")
  fdt(input = iso_datetime_17, format = "y/MM/DD HH:mm:ss.SSSSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.456000000")

  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.S") %>%
    expect_equal("2019/01/15 20:15:24.0")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SS") %>%
    expect_equal("2019/01/15 20:15:24.00")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSS") %>%
    expect_equal("2019/01/15 20:15:24.001")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSS") %>%
    expect_equal("2019/01/15 20:15:24.0010")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSS") %>%
    expect_equal("2019/01/15 20:15:24.00100")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.001000")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.0010000")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.00100000")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss.SSSSSSSSS") %>%
    expect_equal("2019/01/15 20:15:24.001000000")

  #
  # Checks with 'A+'
  #

  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (A)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (72924001)")
  fdt(input = iso_datetime_18, format = "y/MM/DD HH:mm:ss (AAAAAAAAA)") %>%
    expect_equal("2019/01/15 20:15:24 (072924001)")

  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (A)") %>%
    expect_equal("2019/01/15 00:00:00 (0)")
  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AA)") %>%
    expect_equal("2019/01/15 00:00:00 (00)")
  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAA)") %>%
    expect_equal("2019/01/15 00:00:00 (000)")
  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (0000)")
  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (00000)")
  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (000000)")
  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (0000000)")
  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (00000000)")
  fdt(input = iso_datetime_19, format = "y/MM/DD HH:mm:ss (AAAAAAAAA)") %>%
    expect_equal("2019/01/15 00:00:00 (000000000)")
})
