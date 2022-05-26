test_that("ISO dates can be parsed from a string and formatted", {

  iso_date_1 <- "2018-07-04"
  iso_date_2 <- "2020-01-12"
  iso_date_3 <- "2020-01-04"

  # y/yy, M/MM/MMM/MMMM/MMMMM, d/dd

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

  # L and LL

  fmt_dt(input = iso_date_1, dt_format = "y-L-dd") %>%
    expect_equal("2018-7-04")
  fmt_dt(input = iso_date_1, dt_format = "y-LL-dd") %>%
    expect_equal("2018-07-04")

  # Variations of D/DD/DDD

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

  # String literals in date patterns
  fmt_dt(input = iso_date_1, dt_format = "dd 'Day' dd 'Month' MM (yy)") %>%
    expect_equal("04 'Day' 04 'Month' 07 (18)")


  fmt_dt(input = iso_date_1, dt_format = "yyyy-MM-dd'T'HH:mm:ss") %>%
    expect_equal("2018-07-04'T'00:00:00")

  fmt_dt(input = iso_date_1, dt_format = "MMM dd HH:mm:ss ZZZZ yyyy") %>%
    expect_equal("Jul 04 00:00:00 ZZZZ 2018")

  #MMM dd HH:mm:ss ZZZZ yyyy	Jan 21 18:20:11 +0000 2017


  #yyyy MMM dd HH:mm:ss.SSS zzz	2017 Mar 03 05:12:41.211 PDT
  #dd/MMM/yyyy:HH:mm:ss ZZZZ	19/Apr/2017:06:36:15 -0700
  #MMM dd, yyyy hh:mm:ss a	Dec 2, 2017 2:39:58 AM
  #MMM dd yyyy HH:mm:ss	Jun 09 2018 15:28:14
  #MMM dd HH:mm:ss yyyy	Apr 20 00:00:35 2010
  #MMM dd HH:mm:ss ZZZZ	Sep 28 19:00:00 +0000
  #MMM dd HH:mm:ss	Mar 16 08:12:04
  #yyyy-MM-dd'T'HH:mm:ssZZZZ	2017-10-14T22:11:20+0000
  #yyyy-MM-dd'T'HH:mm:ss.SSS'Z'	2017-07-01T14:59:55.711'+0000' 2017-07-01T14:59:55.711Z
  #yyyy-MM-dd HH:mm:ss ZZZZ	2017-08-19 12:17:55 -0400
  #yyyy-MM-dd HH:mm:ssZZZZ	2017-08-19 12:17:55-0400
  #yyyy-MM-dd HH:mm:ss,SSS	2017-06-26 02:31:29,573
  #yyyy/MM/dd*HH:mm:ss	2017/04/12*19:37:50
  #yyyy MMM dd HH:mm:ss.SSS*zzz	2018 Apr 13 22:08:13.211*PDT
  #yyyy MMM dd HH:mm:ss.SSS	2017 Mar 10 01:44:20.392
  #yyyy-MM-dd HH:mm:ss,SSSZZZZ	2017-03-10 14:30:12,655+0000
  #yyyy-MM-dd HH:mm:ss.SSS	2018-02-27 15:35:20.311
  #yyyy-MM-dd HH:mm:ss.SSSZZZZ	2017-03-12 13:11:34.222-0700
  #yyyy-MM-dd'T'HH:mm:ss.SSS	2017-07-22'T'16:28:55.444
  #yyyy-MM-dd'T'HH:mm:ss	2017-09-08'T'03:13:10
  #yyyy-MM-dd'T'HH:mm:ss'Z'	2017-03-12'T'17:56:22'-0700'
  #yyyy-MM-dd'T'HH:mm:ss.SSS	2017-11-22'T'10:10:15.455
  #yyyy-MM-dd'T'HH:mm:ss	2017-02-11'T'18:31:44
  #yyyy-MM-dd*HH:mm:ss:SSS	2017-10-30*02:47:33:899
  #yyyy-MM-dd*HH:mm:ss	2017-07-04*13:23:55
  #yy-MM-dd HH:mm:ss,SSS ZZZZ	11-02-11 16:47:35,985 +0000
  #yy-MM-dd HH:mm:ss,SSS	10-06-26 02:31:29,573
  #yy-MM-dd HH:mm:ss	10-04-19 12:00:17
  #yy/MM/dd HH:mm:ss	06/01/22 04:11:05
  #yyMMdd HH:mm:ss	150423 11:42:35
  #yyyyMMdd HH:mm:ss.SSS	20150423 11:42:35.173
  #MM/dd/yy*HH:mm:ss	08/10/11*13:33:56
  #MM/dd/yyyy*HH:mm:ss	11/22/2017*05:13:11
  #MM/dd/yyyy*HH:mm:ss*SSS	05/09/2017*08:22:14*612
  #MM/dd/yy HH:mm:ss ZZZZ	04/23/17 04:34:22 +0000
  #MM/dd/yyyy HH:mm:ss ZZZZ 	10/03/2017 07:29:46 -0700
  #HH:mm:ss	11:42:35
  #HH:mm:ss.SSS	11:42:35.173
  #HH:mm:ss,SSS	11:42:35,173
  #dd/MMM HH:mm:ss,SSS	23/Apr 11:42:35,173
  #dd/MMM/yyyy:HH:mm:ss	23/Apr/2017:11:42:35
  #dd/MMM/yyyy HH:mm:ss	23/Apr/2017 11:42:35
  #dd-MMM-yyyy HH:mm:ss	23-Apr-2017 11:42:35
  #dd-MMM-yyyy HH:mm:ss.SSS	23-Apr-2017 11:42:35.883
  #dd MMM yyyy HH:mm:ss	23 Apr 2017 11:42:35
  #dd MMM yyyy HH:mm:ss*SSS	23 Apr 2017 10:32:35*311
  #MMdd_HH:mm:ss	0423_11:42:35
  #MMdd_HH:mm:ss.SSS	0423_11:42:35.883
  #MM/dd/yyyy hh:mm:ss a:SSS	8/5/2011 3:31:18 AM:234
  #MM/dd/yyyy hh:mm:ss a	9/28/2011 2:23:15 PM
})
