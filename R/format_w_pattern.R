#' Get a localized date, time, or date-time string in a standard form.
#' @param input the input date, time, or date-time string.
#' @param date_format an option for whether the date portion of the \code{input} should
#' be included in the output date-time string.
#' @param time_format an option for whether the time portion of the \code{input} should
#' be included in the output date-time string.
#' @param combination an appending pattern for the date and time components.
#' @param locale the output locale for the printing of the date-time string.
#' @export
format_w_pattern <- function(input,
                             date_format = NULL,
                             time_format = NULL,
                             combination = "{0} {1}",
                             locale = "en_US") {

  # Stop function if both date_format and time_format are NULL
  if (is.null(date_format) & is.null(time_format)) {
    stop("At least one of `date_format` and `time_format` must have a pattern.", call. = FALSE)
  }

  if (inherits(date_format, "fdf")) {

    fdf_str <- as.character(date_format) %>% tolower()

    date_pattern <-
      get_localized_value(
        type = "date_and_time_gregorian",
        section = paste0("formats_flexible_date_formats_", fdf_str),
        locale = locale)
  }

  #time_pattern <- NULL

  if (exists("date_pattern") & exists("time_pattern")) {

    # Create pattern with date and time
  } else if (exists("date_pattern")) {

    pattern <- date_pattern
  }

  # Extract the date component if it exists
  date_component <- stringr::str_extract(input, "^\\d+?-\\d\\d-\\d\\d")

  # Is a date component present?
  date_present <- ifelse(!is.na(date_component), TRUE, FALSE)

  # Extract the time component if it exists
  time_component <- stringr::str_extract(input, "(\\d|\\d\\d):\\d\\d[0-9:.]*")

  # Is a time component present?
  time_present <- ifelse(!is.na(time_component), TRUE, FALSE)

  if (!date_present & !time_present) {
    stop("There are no detectable date or time components.", call. = FALSE)
  }

  extracted_text <-
    str_extract_all(pattern, "'.*?'") %>%
    unlist() %>%
    stringr::str_replace_all("'", "")

  subst_pattern <-
    stringr::str_replace_all(pattern, "'.*?'", "***")

  # Perform replacements to the `subst_pattern` string
  subst_pattern <-
    localized_date_time(
      date_input = date_component,
      time_input = time_component,
      subst_pattern = subst_pattern,
      locale = locale)

  if (str_detect(subst_pattern, "\\*\\*\\*")) {
    for (i in seq(extracted_text)) {
      subst_pattern <- stringr::str_replace(subst_pattern, "\\*\\*\\*", extracted_text[i])
    }
  }

  subst_pattern
}
