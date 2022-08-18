# bigD

The goal of **bigD** is to flexibly format dates and times to a given locale.

Given the ISO-8601 date string `"2018-07-04"` we can use formatting strings to precisely get the date in the form we need.

``` r
fdt(input = "2018-07-04", format = "y/M/d")
```
```
2018/7/4
```


``` r
fdt(input = "2018-07-04", format = "MMMM d, y.")
```
```
July 4, 2018.
```

With the `locale` option, we can localize the date.

``` r
fdt(
  input = "2018-07-04",
  format = "d. MMMM y (EEEE).",
  locale = "de"
)
```
```
4. Juli 2018 (Mittwoch).
```

With a datetime string, we have more possibilities.

``` r
fdt(
  input = "2018-07-24T14:44:22.234343-0800",
  format = "MMM dd HH:mm:ss ZZZZ yyyy",
  locale = "fr"
)
```
```
juil. 24 14:44:22 GMT-8:00 2018
```

``` r
fdt(
  input = "2018-07-24T14:44:22.234343-0800",
  format = "MMMM dd HH:mm:ss 'yy XX",
  locale = "fi"
)
```
```
heinäkuuta 24 14:44:22 '18 -0800
```

Time zone support is super comprehensive. We can attach a time zone ID to a datetime string or provide it with the `use_tz` argument.

``` r
fdt(
  input = "2014-06-23T13:24:09.84(America/Vancouver)",
  format = "yyyy.MM.dd G, HH:mm:ss zzzz",
  locale = "es"
)
```
```
2014.06.23 d. C., 13:24:09 hora de verano del Pacífico
```

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
