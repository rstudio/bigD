
<!-- README.md is generated from README.Rmd. Please edit that file -->

# i18n

The goal of i18n is to simply get localized variants of dates, times,
currencies, and other stuff that can be localized with some degree of
precision.

## Some Examples

Here is a date-time string that’s all ISO 8601:2004:
`"2018-07-24T14:44:22.234343-0800(America/Vancouver)"`. Let’s take that
as our input going forward.

``` r
input <- "2018-07-24T14:44:22.234343-0800(America/Vancouver)"
```

There are standardized date forms across all locales. They come in 4
flavors: `short`, `medium`, `long`, or `full`. Let’s just get that
date-time into the first two, with random locales:

``` r
input %>%
  fmt_date_time_standard(
    locale = "en",
    width = "short")
#> [1] "7/24/2018 A, 2:44 PM"
```

``` r
input %>%
  fmt_date_time_standard(
    locale = "de",
    width = "medium")
#> [1] "24.07.2018 n. Chr., 14:44:22"
```

We can also apply a smattering of different simple date format presets
to this input. The `format_w_pattern()` function is useful for this. We
can choose to keep the date portion, the time portion, or both. The
`fdf()` helper function is useful for choosing a preset date format from
the flexible date format (FDF).

``` r
input %>%
  fmt_date_time(
    date_format = fdf("yMd"),
    time_format = NULL,
    locale = "es")
#> [1] "24/7/2018"
```

Time components have presets under the flexible time format (FTF), which
is split into 12- and 24-hour variants. There are `ftf_12()` and
`ftf_24()` helper functions enable selections of presets within those.

``` r
input %>%
  fmt_date_time(
    date_format = fdf("yMd"),
    time_format = ftf_24("EHms"),
    locale = "es_MX")
#> [1] "24/7/2018, lun. 14:44:22"
```

It’s hard to know what the format names are for `fdf()`, `ftf_12()`, and
`ftf_24()`, so, there are information functions that provide the names
and yield previews. These info functions are:

  - `info_fdf_types()`
  - `info_ftf_12_types()`
  - `info_ftf_24_types()`

Here, we can specify a combining pattern by using a locale specific
version with the `date_time_combine()` helper function. In this case, we
select the `"full"` combining pattern for the `"de_AT"` locale.

``` r
input %>%
  fmt_date_time(
    date_format = fdf("yMd"),
    time_format = ftf_24("Hms"),
    combination = date_time_combine("full"),
    locale = "de_AT")
#> [1] "24.7.2018 um 14:44:22"
```

There is indeed an info function available to help determine which
combining pattern to use, it’s `info_date_time_combine()`.

Here is a comparison table of localized dates for all the presets
available in the flexible date format:

<img src="man/figures/i18n_tbl.png">

## Installation

You can install i18n if you dare from GitHub. Use:

``` r
devtools::install_github("rich-iannone/i18n")
```

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

MIT © Richard Iannone
