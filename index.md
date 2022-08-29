# bigD <img src="man/figures/logo.svg" style="text-align:right;" alt="" width="120" />

<!-- badges: start -->
<a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/License-MIT-yellow.svg" alt="License: MIT" /></a>
<a href="https://github.com/rich-iannone/bigD/actions"><img src="https://github.com/rich-iannone/bigD/workflows/R-CMD-check/badge.svg" alt="R build status" /></a>
[![Codecov test coverage](https://codecov.io/gh/rich-iannone/bigD/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rich-iannone/bigD?branch=main)
<a href="https://www.contributor-covenant.org/version/2/0/code_of_conduct/"><img src="https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg" alt="Contributor Covenant" /></a>
<!-- badges: end -->

<br />

The goal of **bigD** is to give everyone an easy yet flexible solution for formatting dates and times. The main function, `fdt()`, can take dates, times, and datetimes in various formats (including just strings) and it provides a means to format the output in different locales. The formatting syntax is much more powerful than `strptime`-based formatting (plus no need for `%` everywhere). Time zones in the input can be expressed in multiple ways and there's a ton of options for formatting time zones in the output too.

### Examples

Given the ISO-8601 date string `"2018-07-04"`, let's adjust the `format` string to precisely get the date in the form we need. With `"y/M/d"` a nice year/month/day date is returned to us.

```r
fdt(
  input = "2018-07-04",
  format = "y/M/d"
)
```
```
#> [1] 2018/7/4
```

With variations of the same time parts, it's possible to get a friendlier version of the same date.

```r
fdt(
  input = "2018-07-04",
  format = "MMMM d, y."
)
```
```
#> [1] July 4, 2018.
```

With the `locale` option, we can localize the date. Let's change up the format string and use the German locale (`"de"`).

```r
fdt(
  input = "2018-07-04",
  format = "d. MMMM y (EEEE).",
  locale = "de"
)
```
```
#> [1] 4. Juli 2018 (Mittwoch).
```

With a datetime string like `"2018-07-24T14:44:22.234343-0800"`, we have more possibilities. This follows the ISO 8601 spec pretty closely and notice that the UTC offset value is added at the end (where it ought to be) to express some time zone information. Let's see a different datetime in French.

```r
fdt(
  input = "2018-07-24T14:44:22.234343-0800",
  format = "MMM dd HH:mm:ss ZZZZ yyyy",
  locale = "fr"
)
```
```
#> [1] juil. 24 14:44:22 GMT-8:00 2018
```

Next, let's take a look at a slight variation in Finnish. In the above the tz offset was formatted with `"ZZZZ"`. Below, let's use `"XX"` for that.

```r
fdt(
  input = "2018-07-24T14:44:22.234343-0800",
  format = "MMMM dd HH:mm:ss 'yy XX",
  locale = "fi"
)
```
```
#> [1] heinäkuuta 24 14:44:22 '18 -0800
```

Time zone support is super comprehensive. We can attach a time zone ID, like `"America/Vancouver"` (and there are many others), to a datetime string. We just got to make sure it's wrapped up in parens.

``` r
fdt(
  input = "2014-06-23T13:24:09.84(America/Vancouver)",
  format = "yyyy.MM.dd G, HH:mm:ss zzzz",
  locale = "es"
)
```
```
#> [1] 2014.06.23 d. C., 13:24:09 hora de verano del Pacífico
```

Just so you know, the time zone ID can alternatively be set to the `use_tz` argument of `fdt()`. Also, POSIXct/POSIXlt/Date times can be used as inputs. Plus, this function is vectorized.

The formatting syntax has a lot to it but you can learn all about it in the fairly comprehensive documentation.

### Installation

Want to try this out? Install the development version of **bigD** from **GitHub**:

```r
devtools::install_github("rich-iannone/bigD")
```

If you encounter a bug, have usage questions, or want to share ideas to make this package better, feel free to file an [issue](https://github.com/rich-iannone/bigD/issues).

##### Code of Conduct

Please note that the `rich-iannone/bigD` project is released with a [contributor code of conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).<br>By participating in this project you agree to abide by its terms.

##### 🏛️ Governance

This project is maintained by [Rich Iannone](https://github.com/rich-iannone).
