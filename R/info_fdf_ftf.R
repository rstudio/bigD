#' Get information on the different flexible date formats
#' @param locale an optional output locale for inspecting the \code{input} value
#' across all of the different fdf types. Example locale names include
#' \code{"en_US"} for English (United States) and \code{"fr_FR"} for French
#' (France). If a locale name is not provided then \code{"en_US"} will be used.
#' @param input an optional input date-time string. The appropriate
#' representation should use the following construction outlined in the ISO
#' 8601:2004 standard: \code{YYYY-MM-DDThh:mm:ss.sTZD} although some
#' allowances made here to ease this restrictiveness (for example, the literal
#' \code{T} separating the date and time components is optional). Fractional
#' seconds are optional as is the time-zone designation (TZD). If a value is
#' not provided then a default date-time string will be used.
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
        format_w_pattern(
          input = input,
          date_format = fdf(fdf_type),
          time_format = NULL,
          combination = NULL,
          locale = locale))
  }

  message(
    paste0("For the locale '", locale, "' and the input of '", input, "',\n",
           "the following output dates can be made by each fdf type:\n"),
    paste0("(", seq_along(fdf_types), ") ", fdf_types, ": ", outputs, "\n"))
}

