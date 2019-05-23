#' Get information on the different flexible date formats
#'
#' @param locale An optional output locale for inspecting the `input` value
#'   across all of the different formatting types. Example locale names include
#'   `"en_US"` for English (United States) and `"fr_FR"` for French (France). If
#'   a locale name is not provided then `"en_US"` will be used.
#' @param input an optional input date-time string. The appropriate
#'   representation should use the following construction outlined in the ISO
#'   8601:2004 standard: `YYYY-MM-DDThh:mm:ss.sTZD` although some allowances
#'   made here to ease this restrictiveness (for example, the literal `T`
#'   separating the date and time components is optional). Fractional seconds
#'   are optional as is the time-zone designation (TZD). If a value is not
#'   provided then a default date-time string will be used.
#' @export
info_fdf_types <- function(locale = NULL,
                           input = NULL) {

  if (is.null(locale)) {
    locale <- "en"
  }

  if (is.null(input)) {
    input <- "2018-07-24T14:44:22.234343-0800(America/Vancouver)"
  }

  fdf_types <- fdf_types()

  outputs <- c()

  for (fdf_type in fdf_types) {

    outputs <-
      c(outputs,
        fmt_date_time(
          input = input,
          date_format = fdf(fdf_type),
          time_format = NULL,
          combination = NULL,
          locale = locale))
  }

  message(
    paste0("For the locale '", locale, "' and the input of '", input, "',\n",
           "the following output dates can be made by each `fdf` type:\n"),
    paste0("(", seq_along(fdf_types), ") ", fdf_types, ": ", outputs, "\n"))
}

#' Get information on the different 12-hour flexible time formats
#'
#' @inheritParams info_fdf_types
#' @export
info_ftf_12_types <- function(locale = NULL,
                              input = NULL) {

  if (is.null(locale)) {
    locale <- "en"
  }

  if (is.null(input)) {
    input <- "2018-07-24T14:44:22.234343-0800(America/Vancouver)"
  }

  ftf_12_types <- ftf_12_types()

  outputs <- c()

  for (ftf_12_type in ftf_12_types) {

    outputs <-
      c(outputs,
        fmt_date_time(
          input = input,
          date_format = NULL,
          time_format = ftf_12(ftf_12_type),
          combination = NULL,
          locale = locale))
  }

  message(
    paste0("For the locale '", locale, "' and the input of '", input, "',\n",
           "the following output times can be made by each `ftf_12` type:\n"),
    paste0("(", seq_along(ftf_12_types), ") ", ftf_12_types, ": ", outputs, "\n"))
}

#' Get information on the different 24-hour flexible time formats
#'
#' @inheritParams info_fdf_types
#' @export
info_ftf_24_types <- function(locale = NULL,
                              input = NULL) {

  if (is.null(locale)) {
    locale <- "en"
  }

  if (is.null(input)) {
    input <- "2018-07-24T14:44:22.234343-0800(America/Vancouver)"
  }

  ftf_24_types <- ftf_24_types()

  outputs <- c()

  for (ftf_24_type in ftf_24_types) {

    outputs <-
      c(outputs,
        fmt_date_time(
          input = input,
          date_format = NULL,
          time_format = ftf_24(ftf_24_type),
          combination = NULL,
          locale = locale))
  }

  message(
    paste0("For the locale '", locale, "' and the input of '", input, "',\n",
           "the following output times can be made by each `ftf_24` type:\n"),
    paste0("(", seq_along(ftf_24_types), ") ", ftf_24_types, ": ", outputs, "\n"))
}

#' Get information on the date-time combining formats
#' @inheritParams info_fdf_types
#' @export
info_date_time_combine <- function(locale = NULL,
                                   input = NULL) {

  if (is.null(locale)) {
    locale <- "en"
  }

  if (is.null(input)) {
    input <- "2018-07-24T14:44:22.234343-0800(America/Vancouver)"
  }

  date_time_combine_types <- date_time_combine_types()

  outputs <- c()

  for (date_time_combine_type in date_time_combine_types) {

    outputs <-
      c(outputs,
        fmt_date_time(
          input = input,
          date_format = fdf(format = 1),
          time_format = ftf_24(format = 1),
          combination = date_time_combine(date_time_combine_type),
          locale = locale))
  }

  message(
    paste0("For the locale '", locale, "' and the input of '", input, "',\n",
           "the following date-times can be made by each `date_time_combine` type:\n"),
    paste0("(", seq_along(date_time_combine_types), ") ", date_time_combine_types, ": ",
           outputs, "\n"))
}
