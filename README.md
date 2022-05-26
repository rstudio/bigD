
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bigD

The goal of **bigD** is to flexibly format dates and times to a given
locale.

Given the ISO-8601 date string `"2018-07-04"` we can use formatting
strings to precisely get the date in the form we need.

``` r
fmt_dt(input = "2018-07-04", dt_format = "y/M/d")
```

    2018/7/4

``` r
fmt_dt(input = "2018-07-04", dt_format = "MMMM d, y.")
```

    July 4, 2018.

With the `locale` option, we can localize the date.

``` r
fmt_dt(input = "2018-07-04", dt_format = "d. MMMM y (EEEE).", locale = "de")
```

    4. Juli 2018 (Mittwoch).

With a datetime string, we have more possibilities.

``` r
fmt_dt(
  input = "2018-07-24T14:44:22.234343-0800",
  dt_format = "MMM dd HH:mm:ss ZZZZ yyyy",
  locale = "fr"
)
```

    juil. 24 14:44:22 GMT-8:00 2018

``` r
fmt_dt(
  input = "2018-07-24T14:44:22.234343-0800",
  dt_format = "MMMM dd HH:mm:ss 'yy XX",
  locale = "fi"
)
```

    heinäkuuta 24 14:44:22 '18 -0800

### Installation

Want to try this out? The **bigD** package can be installed from CRAN:

``` r
install.packages("bigD")
```

Also, you can install the development version of **bigD** from
**GitHub**:

``` r
devtools::install_github("rich-iannone/bigD")
```

If you encounter a bug, have usage questions, or want to share ideas to
make this package better, feel free to file an
[issue](https://github.com/rich-iannone/bigD/issues).

##### Code of Conduct

Please note that the `rich-iannone/bigD` project is released with a
[contributor code of
conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).<br>By
participating in this project you agree to abide by its terms.

##### 🏛️ Governance

This project is maintained by [Rich
Iannone](https://github.com/rich-iannone).
