#' Format a datetime with a formatting string
#'
#' With `fdt()`, we can format datetime values with the greatest of ease, and,
#' with great power. There is a lot of leniency in what types of input
#' date/time/datetime values can be passed in. The formatting string allows for
#' a huge array of possibilities when formatting. Not only that, we can set a
#' `locale` value and get the formatted values localized in the language/region
#' of choice. There's plenty of ways to represent time zone information, and
#' this goes along with the option to enrich the input values with a precise
#' time zone identifier (like `"America/Los_Angeles"`). The choices are ample
#' here, with the goal being a comprehensiveness and ease-of-use in date/time
#' formatting.
#'
#' @section Valid Input Values:
#'
#' The `input` argument of the `fdt()` function allows for some flexibility on
#' what can be passed in. This section describes the kinds of inputs that are
#' understandable by `fdt()`. A vector of strings is allowed, as are vectors of
#' `Date` or `POSIXct` values.
#'
#' If using strings, a good option is to use those that adhere to the ISO
#' 8601:2004 standard. For a datetime this can be of the form
#' `YYYY-MM-DDThh:mm:ss.s<TZD>`. With this, `YYYY-MM-DD` corresponds to the
#' date, the literal `"T"` is optional, `hh:mm:ss` is the time (where seconds,
#' `ss`, is optional as are `.s` for fractional seconds), and `<TZD>` refers to
#' an optional time-zone designation (more on time zones in the next paragraph).
#' You can provide just the date part, and this assumes midnight as an implicit
#' time. It's also possible to provide just the time part, and this internally
#' assembles a datetime that uses the current date. When formatting standalone
#' dates or times, you'll probably just format the explicit parts but `fdt()`
#' won't error if you format the complementary parts.
#'
#' The time zone designation on string-based datetimes is completely optional.
#' If not provided then `"UTC"` is assumed. If you do want to supply time zone
#' information, it can be given as an offset value with the following
#' constructions:
#'
#' - `<time>Z`
#' - `<time>(+/-)hh:mm`
#' - `<time>(+/-)hhmm`
#' - `<time>(+/-)hh`
#'
#' The first, `<time>Z`, is zone designator for the zero UTC offset; it's
#' equivalent to `"+00:00"`. The next two are formats for providing the time
#' offsets from UTC with hours and minutes fields. Examples are `"-05:00"` (New
#' York, standard time), `"+0200"` (Cairo), and `"+05:30"` (Mumbai). Note that
#' the colon is optional but leading zeros to maintain two-digit widths are
#' essential. The final format, `<time>(+/-)hh`, omits the minutes field and as
#' so many offsets have `"00"` minutes values, this can be convenient.
#'
#' We can also supply an Olson/IANA-style time zone identifier (tzid) in
#' parentheses within the string, or, as a value supplied to `use_tz` (should a
#' tzid apply to all date/time/datetime values in the `input` vector). By
#' extension, this would use the form: `YYYY-MM-DDThh:mm:ss.s<TZD>(<tzid>)`.
#' Both a `<TZD>` (UTC offset value) and a `<tzid>` shouldn't really be used
#' together but if that occurs the `<tzid>` overrides the UTC offset. Here are
#' some examples:
#'
#' - `"2018-07-04T22:05 (America/Vancouver)"` (preferable)
#' - `"2018-07-04T22:05-0800(America/Vancouver)"` (redundant, but still okay)
#'
#' A tzid contains much more information about the time zone than a UTC offset
#' value since it is tied to some geographical location and the timing of
#' Standard Time (STD) and Daylight Saving Time (DST) is known. In essence we
#' can derive UTC offset values from a tzid and also a host of other identifiers
#' (time zone names, their abbreviations, etc.). Here are some examples of valid
#' tzid values that can be used:
#'
#' - `"America/Jamaica"` (the official time in Jamaica, or, `"Jamaica Time"`)
#' - `"Australia/Perth"` (`"+08:00"` year round in Western Australia)
#' - `"Europe/Dublin"` (IST/GMT time: `"+01:00"`/`"+00:00"`)
#'
#' The tz database (a compilation of information about the world's time zones)
#' consists of canonical zone names (those that are primary and preferred) and
#' alternative names (less preferred in modern usage, and was either discarded
#' or more commonly replaced by a canonical zone name). The `fdt()` function can
#' handle both types and what occurs is that non-canonical tzid values are
#' internally mapped onto canonical zone names. Here's a few examples:
#'
#' - `"Africa/Luanda"` (in Angola) maps to `"Africa/Lagos"`
#' - `"America/Indianapolis"` maps to `"America/Indiana/Indianapolis"`
#' - `"Asia/Calcutta"` maps to `"Asia/Kolkata"`
#' - `"Pacific/Midway"` maps to `"Pacific/Pago_Pago"`
#' - `"Egypt"` maps to `"Africa/Cairo"`
#'
#' For the most part, the Olson-format tzid follows the form `"{region}/{city}"`
#' where the region is usually a continent, the city is considered an 'exemplar
#' city', and the exemplar city itself belongs in a country.
#'
#' @section Date/Time Format Syntax:
#'
#' A formatting pattern as used in **bigD** consists of a string of characters,
#' where certain strings are replaced with date and time data that are derived
#' from the parsed input.
#'
#' The characters used in patterns are tabulated below to show which specific
#' strings produce which outputs (e.g., `"y"` for the year). A common pattern is
#' characters that are used consecutively to produce variations on a date, time,
#' or timezone output. Say that the year in the input is 2015. If using `"yy"`
#' you'll get `"15"` but with `"yyyy"` the output becomes `"1999"`. There's a
#' whole lot of this, so the following subsections try to illustrate as best as
#' possible what each string will produce. All of the examples will use this
#' string-based datetime input unless otherwise indicated:
#'
#' `"2018-07-04T22:05:09.2358(America/Vancouver)"`
#'
#' ## The Era Designator (big G)
#'
#' | Formatting String              | Output                                 |
#' |------------------------------- |----------------------------------------|
#' | `"G"`, `"GG"`, or `"GGG"`      | `"AD"`                                 |
#' | `"GGGG"`                       | `"Anno Domini"`                        |
#' | `"GGGGG"`                      | `"A"`                                  |
#'
#'
#' ## Year (little y)
#'
#' | Formatting String              | Output                                 |
#' |------------------------------- |----------------------------------------|
#' | `"y"`                          | `"2018"`                               |
#' | `"yy"`                         | `"18"`                                 |
#' | `"yyy"`                        | `"2018"`                               |
#' | `"yyyy"`                       | `"2018"`                               |
#' | `"yyyyy"`                      | `"02018"`                              |
#' | `"yyyyyy"`                     | `"002018"`                             |
#' | `"yyyyyyy"`                    | `"0002018"`                            |
#' | `"yyyyyyyy"`                   | `"00002018"`                           |
#' | `"yyyyyyyyy"`                  | `"000002018"`                          |
#'
#'
#' ## Year in the Week in Year Calendar (big Y)
#'
#' This is the year in 'Week of Year' based calendars in which the year
#' transition occurs on a week boundary. This may differ from calendar year 'y'
#' near a year transition. This numeric year designation is used in conjunction
#' with pattern character 'w' in the ISO year-week calendar as defined by ISO
#' 8601.
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"Y"`                          | `"2018"`                               |
#' | `"YY"`                         | `"18"`                                 |
#' | `"YYY"`                        | `"2018"`                               |
#' | `"YYYY"`                       | `"2018"`                               |
#' | `"YYYYY"`                      | `"02018"`                              |
#' | `"YYYYYY"`                     | `"002018"`                             |
#' | `"YYYYYYY"`                    | `"0002018"`                            |
#' | `"YYYYYYYY"`                   | `"00002018"`                           |
#' | `"YYYYYYYYY"`                  | `"000002018"`                          |
#'
#'
#' ## Quarter of the Year: formatting ver. (big Q)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"Q"`                          | `"3"`                                  |
#' | `"QQ"`                         | `"03"`                                 |
#' | `"QQQ"`                        | `"Q3"`                                 |
#' | `"QQQQ"`                       | `"3rd quarter"`                        |
#' | `"QQQQQ"`                      | `"3"`                                  |
#'
#'
#' ## Quarter of the Year: standalone ver. (little q)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"q"`                          | `"3"`                                  |
#' | `"qq"`                         | `"03"`                                 |
#' | `"qqq"`                        | `"Q3"`                                 |
#' | `"qqqq"`                       | `"3rd quarter"`                        |
#' | `"qqqqq"`                      | `"3"`                                  |
#'
#'
#' ## Month: formatting ver. (big M)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"M"`                          | `"7"`                                  |
#' | `"MM"`                         | `"07"`                                 |
#' | `"MMM"`                        | `"Jul"`                                |
#' | `"MMMM"`                       | `"July"`                               |
#' | `"MMMMM"`                      | `"J"`                                  |
#'
#'
#' ## Month: standalone ver. (big L)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"L"`                          | `"7"`                                  |
#' | `"LL"`                         | `"07"`                                 |
#' | `"LLL"`                        | `"Jul"`                                |
#' | `"LLLL"`                       | `"July"`                               |
#' | `"LLLLL"`                      | `"J"`                                  |
#'
#'
#' ## Week of Year (little w)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"w"`                          | `"27"` (minimal digits)                |
#' | `"ww"`                         | `"27"` (two digits, zero padded)       |
#'
#'
#' ## Week of Month (big W)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"W"`                          | `"1"`                                  |
#'
#'
#' ## Day of Month (little d)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"d"`                          | `"4"` (minimal digits)                 |
#' | `"dd"`                         | `"04"` (two digits, zero padded)       |
#'
#'
#' ## Day of Year (big D)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"D"`                          | `"185"`                                |
#' | `"DD"`                         | `"185"` (zero padded to min-width of 2)|
#' | `"DDD"`                        | `"185"` (zero padded to min-width of 3)|
#'
#'
#' ## Day of Week in Month (big F)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"F"`                          | `"1"`                                  |
#'
#'
#' ## Modified Julian Day (little g)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"g"`                          | `"58303"`                              |
#' | `"gg"`                         | `"58303"`                              |
#' | `"ggg"`                        | `"58303"`                              |
#' | `"gggg"`                       | `"58303"`                              |
#' | `"ggggg"`                      | `"58303"`                              |
#' | `"gggggg"`                     | `"058303"`                             |
#' | `"ggggggg"`                    | `"0058303"`                            |
#' | `"gggggggg"`                   | `"00058303"`                           |
#' | `"ggggggggg"`                  | `"000058303"`                          |
#'
#'
#' ## Day of Week Name (big E)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"E"`                          | `"Wed"`                                |
#' | `"EE"`                         | `"Wed"`                                |
#' | `"EEE"`                        | `"Wed"`                                |
#' | `"EEEE"`                       | `"Wednesday"`                          |
#' | `"EEEEE"`                      | `"W"`                                  |
#' | `"EEEEEE"`                     | `"We"`                                 |
#'
#'
#' ## AM/PM Period of Day (little a)
#'
#' | Formatting String              | Output   | Note                        |
#' |--------------------------------|----------|-----------------------------|
#' | `"a"`, `"aa"`, or `"aaa"`      | `"PM"`   | Abbreviated                 |
#' | `"aaaa"`                       | `"PM"`   | Wide                        |
#' | `"aaaaa"`                      | `"p"`    | Narrow                      |
#'
#'
#' ## AM/PM Period of Day Plus Noon and Midnight (little b)
#'
#' (a) `input_midnight`: `"2020-05-05T00:00:00"`
#'
#' (b) `input_noon`: `"2020-05-05T12:00:00"`
#'
#' | Formatting String              | Output             | Note              |
#' |--------------------------------|--------------------|-------------------|
#' | `"b"`, `"bb"`, or `"bbb"`      | (a) `"midnight"`   | Abbreviated       |
#' | `"" "" ""`                     | (b) `"noon"`       | `"" "" ""`        |
#' | `"bbbb"`                       | (a) `"midnight"`   | Wide              |
#' | `"" "" ""`                     | (b) `"noon"`       | `"" "" ""`        |
#' | `"bbbbb"`                      | (a) `"mi"`         | Narrow            |
#' | `"" "" ""`                     | (b) `"n"`          | `"" "" ""`        |
#'
#'
#' ## Flexible Day Periods (big B)
#'
#' (a) `input_morning`: `"2020-05-05T00:08:30"`
#'
#' (b) `input_afternoon`: `"2020-05-05T14:00:00"`
#'
#' | Formatting String          | Output                   | Note            |
#' |----------------------------|--------------------------|-----------------|
#' | `"B"`, `"BB"`, or `"BBB"`  | (a) `"in the morning"`   | Abbreviated     |
#' | `"" "" ""`                 | (b) `"in the afternoon"` | `"" "" ""`      |
#' | `"BBBB"`                   | (a) `"in the morning"`   | Wide            |
#' | `"" "" ""`                 | (b) `"in the afternoon"` | `"" "" ""`      |
#' | `"BBBBB"`                  | (a) `"in the morning"`   | Narrow          |
#' | `"" "" ""`                 | (b) `"in the afternoon"` | `"" "" ""`      |
#'
#'
#' ## Hour 1-12 (little h)
#'
#' Using: `"2015-08-01T08:35:09"`
#'
#' | Formatting String      | Output  | Note                             |
#' |------------------------|---------|----------------------------------|
#' | `"h"`                  | `"8"`   | Numeric, minimum digits          |
#' | `"hh"`                 | `"08"`  | Numeric, 2 digits (zero padded)  |
#'
#'
#' ## Hour 0-23 (big H)
#'
#' Using: `"2015-08-01T08:35:09"`
#'
#' | Formatting String      | Output  | Note                             |
#' |------------------------|---------|----------------------------------|
#' | `"H"`                  | `"8"`   | Numeric, minimum digits          |
#' | `"HH"`                 | `"08"`  | Numeric, 2 digits (zero padded)  |
#'
#'
#' ## Hour 0-11 (big K)
#'
#' Using: `"2015-08-01T08:35:09"`
#'
#' | Formatting String      | Output  | Note                             |
#' |------------------------|---------|----------------------------------|
#' | `"K"`                  | `"7"`   | Numeric, minimum digits          |
#' | `"KK"`                 | `"07"`  | Numeric, 2 digits (zero padded)  |
#'
#'
#' ## Hour 1-24 (little k)
#'
#' Using: `"2015-08-01T08:35:09"`
#'
#' | Formatting String      | Output  | Note                             |
#' |------------------------|---------|----------------------------------|
#' | `"k"`                  | `"9"`   | Numeric, minimum digits          |
#' | `"kk"`                 | `"09"`  | Numeric, 2 digits (zero padded)  |
#'
#'
#' ## Minute (little m)
#'
#' | Formatting String      | Output  | Note                             |
#' |------------------------|---------|----------------------------------|
#' | `"m"`                  | `"5"`   | Numeric, minimum digits          |
#' | `"mm"`                 | `"06"`  | Numeric, 2 digits (zero padded)  |
#'
#'
#' ## Second (little s)
#'
#' | Formatting String      | Output  | Note                             |
#' |------------------------|---------|----------------------------------|
#' | `"s"`                  | `"9"`   | Numeric, minimum digits          |
#' | `"ss"`                 | `"09"`  | Numeric, 2 digits (zero padded)  |
#'
#'
#' ## Fractional Second (big S)
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"S"`                          | `"2"`                                  |
#' | `"SS"`                         | `"23"`                                 |
#' | `"SSS"`                        | `"235"`                                |
#' | `"SSSS"`                       | `"2350"`                               |
#' | `"SSSSS"`                      | `"23500"`                              |
#' | `"SSSSSS"`                     | `"235000"`                             |
#' | `"SSSSSSS"`                    | `"2350000"`                            |
#' | `"SSSSSSSS"`                   | `"23500000"`                           |
#' | `"SSSSSSSSS"`                  | `"235000000"`                          |
#'
#'
#' ## Milliseconds Elapsed in Day (big A)
#'
#' Using: `"2011-07-27T00:07:19.7223"`
#'
#' | Formatting String              | Output                                 |
#' |--------------------------------|----------------------------------------|
#' | `"A"`                          | `"439722"`                             |
#' | `"AA"`                         | `"439722"`                             |
#' | `"AAA"`                        | `"439722"`                             |
#' | `"AAAA"`                       | `"439722"`                             |
#' | `"AAAAA"`                      | `"439722"`                             |
#' | `"AAAAAA"`                     | `"439722"`                             |
#' | `"AAAAAAA"`                    | `"0439722"`                            |
#' | `"AAAAAAAA"`                   | `"00439722"`                           |
#' | `"AAAAAAAAA"`                  | `"000439722"`                          |
#'
#'
#' ## TZ // Short and Long Specific non-Location Format (little z)
#'
#' | Formatting String          | Output                    | Note           |
#' |----------------------------|---------------------------|----------------|
#' | `"z"`, `"zz"`, or `"zzz"`  | `"PDT"`                   | Short Specific |
#' | `"zzzz"`                   | `"Pacific Daylight Time"` | Long Specific  |
#'
#'
#' ## TZ // Short and Long Specific non-Location Formats (big Z)
#'
#' | Formatting String          | Output       | Note                      |
#' |----------------------------|--------------|---------------------------|
#' | `"Z"`, `"ZZ"`, or `"ZZZ"`  | `"-0700"`    | ISO 8601 basic format     |
#' | `"ZZZZ"`                   | `"GMT-7:00"` | Long localized GMT format |
#' | `"ZZZZZ"`                  | `"-07:00"`   | ISO 8601 extended format  |
#'
#'
#' ## TZ // Short and Long Localized GMT Formats (big O)
#'
#' | Formatting String       | Output        | Note                        |
#' |-------------------------|---------------|-----------------------------|
#' | `"O"`                   | `"GMT-7"`     | Short localized GMT format  |
#' | `"OOOO"`                | `"GMT-07:00"` | Long localized GMT format   |
#'
#'
#' ## TZ // Short and Long Localized GMT Formats (little v)
#'
#' | Formatting String  | Output           | Note                              |
#' |--------------------|------------------|-----------------------------------|
#' | `"v"`              | `"PT"`           | Short generic non-location format |
#' | `"vvvv"`           | `"Pacific Time"` | Long generic non-location format  |
#'
#'
#' ## TZ // Short Time Zone IDs and Exemplar City Formats (big V)
#'
#' | Formatting String  | Output                | Note                     |
#' |--------------------|-----------------------|--------------------------|
#' | `"V"`              | `"cavan"`             | Short time zone ID       |
#' | `"VV"`             | `"America/Vancouver"` | Long time zone ID        |
#' | `"VVV"`            | `"Vancouver"`         | The tz exemplar city     |
#' | `"VVVV"`           | `"Vancouver Time"`    | Generic location format  |
#'
#'
#' ## TZ // ISO 8601 Formats with Z for +0000 (big X)
#'
#' | Formatting String | Output     | Note                                      |
#' |-------------------|------------|-------------------------------------------|
#' | `"X"`             | `"-07"`    | ISO 8601 basic format (h; optional m)     |
#' | `"XX"`            | `"-0700"`  | ISO 8601 basic format (h & m)             |
#' | `"XXX"`           | `"-07:00"` | ISO 8601 extended format (h & m)          |
#' | `"XXXX"`          | `"-0700"`  | ISO 8601 basic format (h & m, optional s) |
#' | `"XXXXX"`         | `"-07:00"` | ISO 8601 extended format (h & m, optional s) |
#'
#'
#' ## TZ // ISO 8601 Formats (no use of Z for +0000) (little x)
#'
#' | Formatting String | Output     | Note                                      |
#' |-------------------|------------|-------------------------------------------|
#' | `"x"`             | `"-07"`    | ISO 8601 basic format (h; optional m)     |
#' | `"xx"`            | `"-0700"`  | ISO 8601 basic format (h & m)             |
#' | `"xxx"`           | `"-07:00"` | ISO 8601 extended format (h & m)          |
#' | `"xxxx"`          | `"-0700"`  | ISO 8601 basic format (h & m, optional s) |
#' | `"xxxxx"`         | `"-07:00"` | ISO 8601 extended format (h & m, optional s) |
#'
#' @param input A vector of date, time, or datetime values. Several
#'   representations are acceptable here including strings, `Date` objects, or
#'   `POSIXct` objects. Refer to the *Valid Input Values* section for more
#'   information.
#' @param format The formatting string to apply to all `input` values. If not
#'   provided, the inputs will be formatted to ISO 8601 datetime strings. The
#'   *Date/Time Format Syntax* section has detailed information on how to create
#'   a formatting string.
#' @param use_tz A tzid (e.g., `"America/New_York"`) time-zone designation for
#'   precise formatting of related outputs. This overrides any time zone
#'   information available in `character`-based input values and is applied to
#'   all vector components.
#' @param locale The output locale to use for formatting the input value
#'   according to the specified locale's rules. Example locale names include
#'   `"en"` for English (United States) and `"es-EC"` for Spanish (Ecuador). If
#'   a locale isn't provided the `"en"` locale will be used. The
#'   [fdt_locales_vec] vector contains the valid locales and [fdt_locales_lst]
#'   list provides an easy way to obtain a valid locale.
#'
#' @return A character vector of formatted dates, times, or datetimes.
#'
#' @section Examples:
#'
#' ## Basics with `input` datetimes, formatting strings, and localization
#'
#' With an input datetime of `"2018-07-04 22:05"` supplied as a string, we can
#' format to get just a date with the full year first, the month abbreviation
#' second, and the day of the month last (separated by hyphens):
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = "y-MMM-dd"
#' )
#' ```
#' ```
#' #> [1] "2018-Jul-04"
#' ```
#'
#' There are sometimes many options for each time part. Instead of using
#' `"y-MMM-dd"`, let's try a variation on that with `"yy-MMMM-d"`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = "yy-MMMM-d"
#' )
#' ```
#' ```
#' #> [1] "18-July-4"
#' ```
#'
#' The output is localizable and so elements will be translated when supplying
#' the appropriate locale code. Let's use `locale = es` to get the month written
#' in Spanish:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = "yy-MMMM-d",
#'   locale = "es"
#' )
#' ```
#' ```
#' #> [1] "18-julio-4"
#' ```
#'
#' `POSIXct` or `POSIXlt` datetimes can serve as an `input` to `fdt()`. Let's
#' create a single datetime value where the timezone is set as `"Asia/Tokyo"`.
#'
#' ```r
#' fdt(
#'   input = lubridate::ymd_hms("2020-03-15 19:09:12", tz = "Asia/Tokyo"),
#'   format = "EEEE, MMMM d, y 'at' h:mm:ss B (VVVV)"
#' )
#' ```
#' ```
#' #> [1] "Sunday, March 15, 2020 at 7:09:12 in the evening (Tokyo Time)"
#' ```
#'
#' If you're going minimal, it's possible to supply an input datetime string
#' without a `format` directive. What this gives us is formatted datetime
#' output that conforms to ISO 8601. Note that the implied time zone is UTC.
#'
#' ```r
#' fdt(input = "2018-07-04 22:05")
#' ```
#' ```
#' #> [1] "2018-07-04T22:05:00Z"
#' ````
#'
#' ## Using locales stored in the [fdt_locales_lst] list
#'
#' The [fdt_locales_lst] object is provided in **bigD** to make it easier to
#' choose one of supported locales. You can avoid typing errors and every
#' element of the list is meant to work. For example, we can use the `"it"`
#' locale by accessing it from [fdt_locales_lst] (autocomplete makes this
#' relatively simple).
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = "yy-MMMM-d",
#'   locale = fdt_locales_lst$it
#' )
#' ```
#' ```
#' #> [1] "18-luglio-4"
#' ````
#'
#' ## Omission of date or time in `input`
#'
#' You don't have to supply a full datetime to `input`. Just supplying the date
#' portion implies midnight (and is just fine if you're only going to present
#' the date anyway).
#'
#' ```r
#' fdt(input = "2018-07-04")
#' ```
#' ```
#' #> [1] "2018-07-04T00:00:00Z"
#' ````
#'
#' If you omit the date and just supply a time, `fdt()` will correctly parse
#' this. The current date on the user system will be used because we need to
#' create some sort of datetime value internally. Again, this is alright if
#' you just intend to present a formatted time value.
#'
#' ```r
#' fdt(input = "22:05")
#' ```
#' ```
#' #> [1] "2022-08-16T22:05:00Z"
#' ````
#'
#' To see all of the supported locales, we can look at the vector supplied by
#' the [fdt_locales_vec()] function.
#'
#' ## Using standardized forms with the `standard_*()` helper functions
#'
#' With an input datetime of `"2018-07-04 22:05(America/Vancouver)"`, we can
#' format the date and time in a standardized way with `standard_date_time()`
#' providing the correct formatting string. This function is invoked in the
#' `format` argument of `fdt()`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time(type = "full")
#' )
#' ```
#' ```
#' #> [1] "Wednesday, July 4, 2018 at 10:05:00 PM Pacific Daylight Time"
#' ```
#'
#' The locale can be changed and we don't have to worry about the particulars
#' of the formatting string (they are standardized across locales).
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time(type = "full"),
#'   locale = fdt_locales_lst$nl
#' )
#' ```
#' ```
#' #> [1] "woensdag 4 juli 2018 om 22:05:00 Pacific-zomertijd"
#' ```
#'
#' We can use different `type` values to control the output datetime string. The
#' default is `"short"`.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time()
#' )
#' ```
#' ```
#' #> [1] "7/4/18, 10:05 PM"
#' ```
#'
#' After that, it's `"medium"`:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time(type = "medium")
#' )
#' ```
#' ```
#' #> [1] "Jul 4, 2018, 10:05:00 PM"
#' ```
#'
#' The `"short"` and `"medium"` types don't display time zone information in the
#' output. Beginning with `"long"`, the tz is shown.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05(America/Vancouver)",
#'   format = standard_date_time(type = "long")
#' )
#' ```
#' ```
#' #> [1] "July 4, 2018 at 10:05:00 PM PDT"
#' ```
#'
#' If you don't include time zone information in the input, the `"UTC"` time
#' zone will be assumed:
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = standard_date_time(type = "full")
#' )
#' ```
#' ```
#' #> [1] "Wednesday, July 4, 2018 at 10:05:00 PM GMT+00:00"
#' ```
#'
#' ## Using flexible date and time (12- and 24-hour) formatting
#'
#' The **bigD** package supplies a set of lists to allow for flexible date and
#' time formatting ([flex_d_lst], [flex_t24_lst], and [flex_t12_lst]). These
#' are useful when you need a particular format that works across all locales.
#' Here's an example that uses the `"yMMMEd"` flexible date type by accessing it
#' from the `flex_d_lst` object, yielding a formatted date.
#'
#' ```r
#' fdt(
#'   input = "2021-01-09 16:32(America/Toronto)",
#'   format = flex_d_lst$yMMMEd,
#' )
#' ```
#' ```
#' #> [1] "Sat, Jan 9, 2021"
#' ```
#'
#' If we wanted this in a different locale, the locale-specific `format` pattern
#' behind the flexible date identifier would ensure consistency while moving to
#' that locale.
#'
#' ```r
#' fdt(
#'   input = "2021-01-09 16:32(America/Toronto)",
#'   format = flex_d_lst$yMMMEd,
#'   locale = "fr_CA"
#' )
#' ```
#' ```
#' #> [1] "sam. 9 janv. 2021"
#' ```
#'
#' Formatting as a 12-hour time with the [flex_t12_lst] list and using the
#' `"hms"` flexible type:
#'
#' ```r
#' fdt(
#'   input = "2021-01-09 16:32(America/Toronto)",
#'   format = flex_t12_lst$hms
#' )
#' ```
#' ```
#' #> [1] "4:32:00 PM"
#' ```
#'
#' The 24-hour variant, [flex_t24_lst], has a similar `"Hms"` flexible type that
#' will give us a 24-hour version of the same clock time:
#'
#' ```r
#' fdt(
#'   input = "2021-01-09 16:32(America/Toronto)",
#'   format = flex_t24_lst$Hms
#' )
#' ```
#' ```
#' #> [1] "16:32:00"
#' ```
#'
#' A flexible date and time can be used together by enveloping the two in a
#' list (**bigD** will handle putting the date and time together in a sensible
#' manner).
#'
#' ```r
#' fdt(
#'   input = "2021-01-09 16:32(America/Toronto)",
#'   format = list(flex_d_lst$yMMMEd, flex_t24_lst$Hmv)
#' )
#' ```
#' ```
#' #> "Sat, Jan 9, 2021, 16:32 ET"
#' ```
#'
#' @export
fdt <- function(
    input,
    format = NULL,
    use_tz = NULL,
    locale = NULL
) {

  # TODO: Validate locale

  if (!is.null(locale)) {
    locale <- gsub("_", "-", locale)
  }

  if (is.null(locale)) {
    locale <- "en"
  }

  if (inherits(input, "POSIXlt")) {
    input <- as.POSIXct(input)
  }

  if (inherits(input, "Date")) {
    input <- as.POSIXct(as.POSIXlt(input))
  }


  if (is.null(format)) {
    format <- "yyyy-MM-dd'T'HH:mm:ssXXX"
  } else if (
    length(format) == 1 &&
    is.character(format) &&
    !inherits(format, "date_time_pattern")
  ) {
    NULL
  } else if (
    length(format) == 1 &&
    is.character(format) &&
    inherits(format, "date_time_pattern") &&
    !inherits(format, "standard")
  ) {

    format <- amend_week_pattern(format = format, input = input)

    format <- dates[dates$locale == locale, ][["date_time_available_formats"]][["value"]][[format]]

  } else if (
    length(format) == 1 &&
    is.character(format) &&
    inherits(format, "date_time_pattern") &&
    inherits(format, "standard")
  ) {

    dt_types <- base::setdiff(class(format), c("date_time_pattern", "standard"))
    type_date_time <- dt_types[1]
    type_size <- dt_types[2]

    date_format <-
      dates[dates$locale == locale, ][["date_formats"]][["value"]][[type_size]]

    time_format <-
      dates[dates$locale == locale, ][["time_formats"]][["value"]][[type_size]]

    combining_format <-
      dates[dates$locale == locale, ][["date_time_patterns"]][["value"]][[type_size]]

    if (type_date_time == "date_time") {
      format <- gsub("{1}", date_format, combining_format, fixed = TRUE)
      format <- gsub("{0}", time_format, format, fixed = TRUE)
    } else if (type_date_time == "date") {
      format <- date_format
    } else {
      format <- time_format
    }

  } else if (
    is.list(format) &&
    length(format) == 2 &&
    inherits(format[[1]], "date_time_pattern") &&
    inherits(format[[2]], "date_time_pattern")
  ) {
    if (inherits(format[[1]], "flex_d")) {
      format_1 <- "date"
    } else {
      format_1 <- "time"
    }

    if (inherits(format[[2]], "flex_d")) {
      format_2 <- "date"
    } else {
      format_2 <- "time"
    }

    if (identical(format_1, format_2)) {
      stop(
        "When supplying two flexible formats, one must represent a date and ",
        "the other a time",
        call. = FALSE
      )
    }

    date_format <- if (format_1 == "date") format[[1]] else format[[2]]
    time_format <- if (format_2 == "time") format[[2]] else format[[1]]

    date_format <- amend_week_pattern(format = date_format, input = input)

    combine_length <- "medium"

    date_format <-
      dates[dates$locale == locale, ][["date_time_available_formats"]][["value"]][[date_format]]

    time_format <-
      dates[dates$locale == locale, ][["date_time_available_formats"]][["value"]][[time_format]]

    combining_format <-
      dates[dates$locale == locale, ][["date_time_patterns"]][["value"]][[combine_length]]

    format <- gsub("{1}", date_format, combining_format, fixed = TRUE)
    format <- gsub("{0}", time_format, format, fixed = TRUE)
  }

  dt_out <- rep(NA_character_, length(input))

  for (i in seq_along(input)) {

    input_i <- input[i]

    # Initialize an empty `tz_info` list
    tz_info <-
      list(
        tz_str = NA_character_,
        tz_offset = NA_real_,
        long_tzid = NA_character_,
        tz_short_specific = NA_character_,
        tz_long_specific = NA_character_
      )

    # Modify the `format` string so it can be more precisely formatted
    pattern_list <- dt_format_pattern(format = format)

    if (is.character(input_i)) {

      date_present <- is_date_present(input = input_i)
      time_present <- is_time_present(input = input_i)

      # Determine if tz information is present, either as:
      # [1] a tz offset in hours from GMT
      # [2] a long tz identifier (either canonical or an alias)

      tz_present <- is_tz_present(input = input_i)
      long_tzid_present <- is_long_tzid_present(input = input_i)

      # Strip away tz information from the input and return as `input_str`
      if (tz_present) {
        input_str <- strip_tz(input = input_i)
      } else if (long_tzid_present) {
        input_str <- strip_long_tzid(input = input_i)
      } else {
        input_str <- input_i
      }

      # Obtain the date and time
      if (date_present) {

        input_dt <- as.POSIXct(gsub("T", " ", input_str), tz = "UTC")

      } else if (!date_present && time_present) {

        date_now <- as.character(Sys.Date())
        input_str <- paste0(date_now, "T", input_str)
        input_dt <- as.POSIXct(gsub("T", " ", input_str), tz = "UTC")

      } else if (!date_present && !time_present) {

        if (nchar(input_str) == 4 && grepl("^[0-9]{4}$", input_str)) {

          # Case where only a four-digit C.E. year is provided
          input_str <- paste0(input_str, "-01-01")
          input_dt <- as.POSIXct(input_str, tz = "UTC")
        }

        if (nchar(input_str) == 7 && grepl("^[0-9]{4}-[0-9]{2}$", input_str)) {

          # Case where the year and month are provided in the YYYY-MM format
          input_str <- paste0(input_str, "-01")
          input_dt <- as.POSIXct(input_str, tz = "UTC")
        }

        if (grepl("W", input_str)) {

          # Case where week dates are possibly provided in one of
          # several permitted ISO 8601 formats

          # Perform check of `input_str` to confirm that week dates
          # conform to one of four ISO 8601 specifications:
          # (1) YYYY-Www
          # (2) YYYYWww
          # (3) YYYY-Www-D
          # (4) YYYYWwwD
          if (!grepl("^([0-9]{4}-?W[0-9]{2}|[0-9]{4}-?W[0-9]{2}-?[1-7])$", input_str)) {
            stop(
              "The provided input '", input_str, "' does not conform to a week ",
              "date representation.",
              call. = FALSE
            )
          }

          # Remove hyphens if any are present
          input_str <- gsub("-", "", input_str)

          input_dt <- week_date_as_datetime(input_str = input_str)
        }

        if (grepl("^--", input_str)) {

          if (!grepl("^--[0-9]{2}-?[0-9]{2}$", input_str)) {
            stop(
              "The provided input '", input_str, "' does not conform to a ",
              "month-day representation.",
              call. = FALSE
            )
          }

          # Remove all hyphens from `input_str`
          input_str <- gsub("-", "", input_str)

          input_dt <- month_day_as_datetime(input_str = input_str)
        }

        if (grepl("[0-9]+\\s?(BC|AD|BCE|CE|B\\.C\\.E\\.|C\\.E\\.|EV)$", input_str)) {

          # Remove any spaces or periods from `input_str`
          input_str <- gsub(" ", "", input_str)
          input_str <- gsub("\\.", "", input_str)

          year <- as.integer(gsub("([0-9]+).*", "\\1", input_str))
          era <- gsub("[0-9]+", "", input_str)

          if (any(c("BC", "BCE") %in% era)) {
            year <- year * (-1)
          }

          adjustment <- -1900L

          posixlt_dt <- as.POSIXlt("0000-01-01", tz = "UTC")
          posixlt_dt$year <- year + adjustment
          input_dt <- as.POSIXct(posixlt_dt)
        }

        # TODO: Add parsers for different date/time cases
      }

      # Derive more detailed time zone information from the `long_tzid` value
      if (!is.null(use_tz)) {

        # Validate and then normalize the provided time zone
        validate_long_tzid(long_tzid = use_tz)
        long_tzid <- normalize_long_tzid(long_tzid = use_tz)

        tz_info$long_tzid <- long_tzid
        tz_info$tz_str <- long_tzid_to_tz_str(long_tzid = tz_info$long_tzid, input_dt = input_dt)
        tz_info$tz_offset <- get_tz_offset_val_from_tz_str(tz_str = tz_info$tz_str)
        tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
        tz_info$tz_long_specific <- get_tz_long_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt, locale = locale)

      } else if (long_tzid_present) {

        tz_info$long_tzid <- get_long_tzid_str(input = input_i)
        tz_info$tz_str <- long_tzid_to_tz_str(long_tzid = tz_info$long_tzid, input_dt = input_dt)
        tz_info$tz_offset <- get_tz_offset_val_from_tz_str(tz_str = tz_info$tz_str)
        tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
        tz_info$tz_long_specific <- get_tz_long_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt, locale = locale)

      } else if (tz_present) {

        tz_info$tz_str <- get_tz_str(input = input_i)
        tz_info$tz_offset <- get_tz_offset_val(input = input_i)

      } else {

        tz_info$long_tzid <- "UTC"
        tz_info$tz_str <- "GMT"
        tz_info$tz_offset <- 0L
        tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
      }
    }

    if (inherits(input, "POSIXct")) {

      input_dt <- input_i

      # Extract the input time zone
      long_tzid <- attr(input_dt, which = "tzone", exact = TRUE)

      offset_val <- as.POSIXlt(input_dt)$gmtoff / 3600

      if (!is.null(long_tzid) && long_tzid == "UTC") {

        tz_info$long_tzid <- long_tzid
        tz_info$tz_str <- "GMT"
        tz_info$tz_offset <- 0L
        tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
        tz_info$tz_long_specific <- get_tz_long_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt, locale = locale)

      } else if (is.null(long_tzid)) {

        tz_info$long_tzid <- NA_character_
        tz_info$tz_str <- "GMT"
        tz_info$tz_offset <- offset_val

      } else {

        tz_info$long_tzid <- long_tzid
        tz_info$tz_str <- long_tzid_to_tz_str(long_tzid = tz_info$long_tzid, input_dt = input_dt)
        tz_info$tz_offset <- get_tz_offset_val_from_tz_str(tz_str = tz_info$tz_str)
        tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
        tz_info$tz_long_specific <- get_tz_long_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt, locale = locale)
      }
    }

    dt_lett <- pattern_list$dt_letters
    dt <- pattern_list$format

    if ("G" %in% dt_lett) {
      dt <- u_gsub("{G}", dt_G(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{GG}", dt_G(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{GGG}", dt_G(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{GGGG}", dt_GGGG(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{GGGGG}", dt_GGGGG(input_dt, locale), dt, fixed = TRUE)
    }

    if ("y" %in% dt_lett) {
      dt <- u_gsub("{y}", dt_y(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{yy}", dt_yy(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{yyy}", dt_yyy(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{yyyy}", dt_yyyy(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{yyyyy}", dt_yyyyy(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{yyyyyy}", dt_yyyyyy(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{yyyyyyy}", dt_yyyyyyy(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{yyyyyyyy}", dt_yyyyyyyy(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{yyyyyyyyy}", dt_yyyyyyyyy(input_dt), dt, fixed = TRUE)
    }

    if ("Y" %in% dt_lett) {
      dt <- u_gsub("{Y}", dt_Y(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{YY}", dt_YY(input_dt), dt, fixed = TRUE)
    }

    if ("u" %in% dt_lett) {
      dt <- u_gsub("{u}", dt_u(input_dt), dt, fixed = TRUE)
    }

    if ("U" %in% dt_lett) {
      dt <- u_gsub("{U}", dt_U(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{UU}", dt_U(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{UUU}", dt_U(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{UUUU}", dt_UUUU(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{UUUUU}", dt_UUUUU(input_dt, locale), dt, fixed = TRUE)
    }

    if ("Q" %in% dt_lett) {
      dt <- u_gsub("{Q}", dt_Q(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{QQ}", dt_QQ(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{QQQ}", dt_QQQ(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{QQQQ}", dt_QQQQ(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{QQQQQ}", dt_QQQQQ(input_dt, locale), dt, fixed = TRUE)
    }

    if ("q" %in% dt_lett) {
      dt <- u_gsub("{q}", dt_q(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{qq}", dt_qq(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{qqq}", dt_qqq(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{qqqq}", dt_qqqq(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{qqqqq}", dt_qqqqq(input_dt, locale), dt, fixed = TRUE)
    }

    if ("M" %in% dt_lett) {
      dt <- u_gsub("{M}", dt_M(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{MM}", dt_MM(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{MMM}", dt_MMM(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{MMMM}", dt_MMMM(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{MMMMM}", dt_MMMMM(input_dt, locale), dt, fixed = TRUE)
    }

    if ("L" %in% dt_lett) {
      dt <- u_gsub("{L}", dt_L(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{LL}", dt_LL(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{LLL}", dt_LLL(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{LLLL}", dt_LLLL(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{LLLLL}", dt_LLLLL(input_dt, locale), dt, fixed = TRUE)
    }

    if ("w" %in% dt_lett) {
      dt <- u_gsub("{w}", dt_w(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{ww}", dt_ww(input_dt), dt, fixed = TRUE)
    }

    if ("W" %in% dt_lett) {
      dt <- u_gsub("{W}", dt_W(input_dt), dt, fixed = TRUE)
    }

    if ("d" %in% dt_lett) {
      dt <- u_gsub("{d}", dt_d(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{dd}", dt_dd(input_dt), dt, fixed = TRUE)
    }

    if ("D" %in% dt_lett) {
      dt <- u_gsub("{D}", dt_D(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{DD}", dt_DD(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{DDD}", dt_DDD(input_dt), dt, fixed = TRUE)
    }

    if ("F" %in% dt_lett) {
      dt <- u_gsub("{F}", dt_F(input_dt), dt, fixed = TRUE)
    }

    if ("E" %in% dt_lett) {
      dt <- u_gsub("{E}", dt_E(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{EE}", dt_E(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{EEE}", dt_E(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{EEEE}", dt_EEEE(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{EEEEE}", dt_EEEEE(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{EEEEEE}", dt_EEEEEE(input_dt, locale), dt, fixed = TRUE)
    }

    if ("e" %in% dt_lett) {
      dt <- u_gsub("{e}", dt_e(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{ee}", dt_ee(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{eee}", dt_eee(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{eeee}", dt_eeee(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{eeeee}", dt_eeeee(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{eeeeee}", dt_eeeeee(input_dt, locale), dt, fixed = TRUE)
    }

    if ("c" %in% dt_lett) {
      dt <- u_gsub("{c}", dt_c(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{cc}", dt_cc(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{ccc}", dt_ccc(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{cccc}", dt_cccc(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{ccccc}", dt_ccccc(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{cccccc}", dt_cccccc(input_dt, locale), dt, fixed = TRUE)
    }

    if ("a" %in% dt_lett) {
      dt <- u_gsub("{a}", dt_a(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{aa}", dt_a(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{aaa}", dt_a(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{aaaa}", dt_aaaa(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{aaaaa}", dt_aaaaa(input_dt, locale), dt, fixed = TRUE)
    }

    if ("b" %in% dt_lett) {
      dt <- u_gsub("{b}", dt_b(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{bb}", dt_b(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{bbb}", dt_b(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{bbbb}", dt_bbbb(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{bbbbb}", dt_bbbbb(input_dt, locale), dt, fixed = TRUE)
    }

    if ("B" %in% dt_lett) {
      dt <- u_gsub("{B}", dt_B(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{BB}", dt_B(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{BBB}", dt_B(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{BBBB}", dt_BBBB(input_dt, locale), dt, fixed = TRUE)
      dt <- u_gsub("{BBBBB}", dt_BBBBB(input_dt, locale), dt, fixed = TRUE)
    }

    if ("h" %in% dt_lett) {
      dt <- u_gsub("{h}", dt_h(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{hh}", dt_hh(input_dt), dt, fixed = TRUE)
    }

    if ("H" %in% dt_lett) {
      dt <- u_gsub("{H}", dt_H(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{HH}", dt_HH(input_dt), dt, fixed = TRUE)
    }

    if ("K" %in% dt_lett) {
      dt <- u_gsub("{K}", dt_K(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{KK}", dt_KK(input_dt), dt, fixed = TRUE)
    }

    if ("k" %in% dt_lett) {
      dt <- u_gsub("{k}", dt_k(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{kk}", dt_kk(input_dt), dt, fixed = TRUE)
    }

    if ("m" %in% dt_lett) {
      dt <- u_gsub("{m}", dt_m(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{mm}", dt_mm(input_dt), dt, fixed = TRUE)
    }

    if ("s" %in% dt_lett) {
      dt <- u_gsub("{s}", dt_s(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{ss}", dt_ss(input_dt), dt, fixed = TRUE)
    }

    if ("S" %in% dt_lett) {
      dt <- u_gsub("{S}", dt_S(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{SS}", dt_SS(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{SSS}", dt_SSS(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{SSSS}", dt_SSSS(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{SSSSS}", dt_SSSSS(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{SSSSSS}", dt_SSSSSS(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{SSSSSSS}", dt_SSSSSSS(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{SSSSSSSS}", dt_SSSSSSSS(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{SSSSSSSSS}", dt_SSSSSSSSS(input_dt), dt, fixed = TRUE)
    }

    if ("A" %in% dt_lett) {
      dt <- u_gsub("{A}", dt_A(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{AA}", dt_AA(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{AAA}", dt_AAA(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{AAAA}", dt_AAAA(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{AAAAA}", dt_AAAAA(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{AAAAAA}", dt_AAAAAA(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{AAAAAAA}", dt_AAAAAAA(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{AAAAAAAA}", dt_AAAAAAAA(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{AAAAAAAAA}", dt_AAAAAAAAA(input_dt), dt, fixed = TRUE)
    }

    if ("z" %in% dt_lett) {
      dt <- u_gsub("{z}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{zz}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{zzz}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{zzzz}", dt_zzzz(input_dt, tz_info, locale), dt, fixed = TRUE)
    }

    if ("Z" %in% dt_lett) {
      dt <- u_gsub("{Z}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{ZZ}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{ZZZ}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{ZZZZ}", dt_ZZZZ(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{ZZZZZ}", dt_ZZZZZ(input_dt, tz_info, locale), dt, fixed = TRUE)
    }

    if ("O" %in% dt_lett) {
      dt <- u_gsub("{O}", dt_O(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{OOOO}", dt_OOOO(input_dt, tz_info, locale), dt, fixed = TRUE)
    }

    if ("v" %in% dt_lett) {
      dt <- u_gsub("{v}", dt_v(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{vvvv}", dt_vvvv(input_dt, tz_info, locale), dt, fixed = TRUE)
    }

    if ("V" %in% dt_lett) {
      dt <- u_gsub("{V}", dt_V(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{VV}", dt_VV(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{VVV}", dt_VVV(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{VVVV}", dt_VVVV(input_dt, tz_info, locale), dt, fixed = TRUE)
    }

    if ("X" %in% dt_lett) {
      dt <- u_gsub("{X}", dt_X(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{XX}", dt_XX(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{XXX}", dt_XXX(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{XXXX}", dt_XXXX(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{XXXXX}", dt_XXXXX(input_dt, tz_info, locale), dt, fixed = TRUE)
    }

    if ("x" %in% dt_lett) {
      dt <- u_gsub("{x}", dt_x(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{xx}", dt_xx(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{xxx}", dt_xxx(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{xxxx}", dt_xxxx(input_dt, tz_info, locale), dt, fixed = TRUE)
      dt <- u_gsub("{xxxxx}", dt_xxxxx(input_dt, tz_info, locale), dt, fixed = TRUE)
    }

    if ("g" %in% dt_lett) {
      dt <- u_gsub("{g}", dt_g(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{gg}", dt_gg(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{ggg}", dt_ggg(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{gggg}", dt_gggg(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{ggggg}", dt_ggggg(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{gggggg}", dt_gggggg(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{ggggggg}", dt_ggggggg(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{gggggggg}", dt_gggggggg(input_dt), dt, fixed = TRUE)
      dt <- u_gsub("{ggggggggg}", dt_ggggggggg(input_dt), dt, fixed = TRUE)
    }

    dt <- u_gsub("\\{(.*?)\\}", "\\1", dt)

    # Replace string literal markers `'<#>'` with captured literal values
    for (j in seq_along(pattern_list$literals)) {

      dt <-
        u_gsub(
          paste0("'", j, "'"),
          pattern_list$literals[j],
          dt,
          fixed = TRUE
        )
    }

    # Replace each instance of `''` with `'`
    dt <- gsub("''", "'", dt, fixed = TRUE)

    dt_out[i] <- dt
  }

  dt_out
}

sub_letters <- function() {
  c(
    "G",
    "y",
    "Y",
    "u",
    "U",
    "r",
    "Q",
    "q",
    "M",
    "L",
    "w",
    "W",
    "d",
    "D",
    "F",
    "g",
    "E",
    "e",
    "c",
    "a",
    "b",
    "B",
    "h",
    "H",
    "K",
    "k",
    "m",
    "s",
    "S",
    "A",
    "z",
    "Z",
    "O",
    "v",
    "V",
    "X",
    "x"
  )
}

extract_literals_from_pattern <- function(string) {

  gsub(
    "(^'|'$)", "",
    unlist(regmatches(string, gregexpr(pattern = "'.*?'", text = string)))
  )
}

dt_format_pattern <- function(format) {

  literals <- extract_literals_from_pattern(string = format)

  for (i in seq_along(literals)) {
    if (literals[i] != "") {
      format <- sub(paste0("'", literals[i], "'"), paste0("'", i, "'"), format)
    }
  }

  dt_letters <-
    base::intersect(
      unique(unlist(strsplit(format, ""))),
      sub_letters()
    )

  pattern <- paste0("(", paste0("[", dt_letters, "]+", collapse = "|"), ")")

  format <- gsub(pattern, "\\{\\1\\}", format)

  literals <- gsub("(^'|'$)", "", literals)

  list(
    format = format,
    literals = literals,
    dt_letters = dt_letters
  )
}

amend_week_pattern <- function(format, input) {

  if (format == "MMMMW" || format == "yw") {

    year_week <- format_yearweek(input = input)

    if (grepl("W01", year_week)) {
      format <- paste0(format, "-count-one")
    } else {
      format <- paste0(format, "-count-other")
    }
  }

  format
}

u_gsub <- function(pattern, replacement, x, fixed = FALSE) {

  if (!utf8_aware_sub) {

    # See variable definition for utf8_aware_sub for more info
    x <- enc2utf8(as.character(x))
    replacement <- enc2utf8(as.character(replacement))

    res <- gsub(pattern, replacement, x, fixed = fixed)
    Encoding(res) <- "UTF-8"
    res

  } else {
    gsub(pattern, replacement, x, fixed = fixed)
  }
}
