#' Get a localized date-time in a standardized form.
#' @inheritParams fmt_date_time
#' @param width The standardized output date-time format, which can be one of
#'   either `"short"`, `"medium"`, `"long"`, or `"full"`.
#' @export
fmt_date_time_standard <- function(input,
                                   locale = "en_US",
                                   width = "medium") {

  # Obtain the data section that corresponds to
  # standard date formats of the requested width
  date_section_name <- paste0("formats_standard_date_formats_", width)

  date_pattern <-
    get_localized_value(
      type = "date_and_time_generic",
      section = date_section_name,
      locale = locale)

  # Obtain the time section that corresponds to
  # standard time formats of the requested width
  time_section_name <- paste0("formats_standard_time_formats_", width)

  time_pattern <-
    get_localized_value(
      type = "date_and_time_gregorian",
      section = time_section_name,
      locale = locale)

  # Obtain the date-time combination section that
  # corresponds to those formats of the requested width
  combination_section_name <-
    paste0("formats_standard_date_time_combination_formats_", width)

  subst_pattern <-
    get_localized_value(
      type = "date_and_time_gregorian",
      section = combination_section_name,
      locale = locale)

  # Substitute the `time_pattern` and `date_pattern`
  # into the correct `subst_pattern` locations
  subst_pattern <- gsub("\\{0\\}", time_pattern, subst_pattern)
  subst_pattern <- gsub("\\{1\\}", date_pattern, subst_pattern)

  # Prepare the output vector
  output <- rep(NA_character_, length(input))

  for (i in seq(input)) {

    # Is a date component present?
    date_present <- is_date_present(input[i])

    if (!date_present) {
      stop("There is no detectable date component.", call. = FALSE)
    }

    # Is a time component present?
    time_present <- is_time_present(input[i])

    if (!time_present) {
      stop("There is no detectable time component.", call. = FALSE)
    }

    # Extract the date component
    date_component <- get_date_component(input[i])

    # Extract the time component
    time_component <- get_time_component(input[i])

    # Is a time zone component present?
    tz_present <- is_tz_present(input[i])

    # If a time zone component is not present and a `full` or
    # `long` width is requested, stop the function
    if (!tz_present & width %in% c("full", "long")) {

      stop("A time zone component must be present to render `full` and `long` date-times.",
           .call = FALSE)
    }

    # Extract the time zone if it exists
    tz_offset <- get_tz_offset(input[i])

    # Extract the IANA time zone name if it exists
    iana_tz_name <- get_iana_tz(input[i])

    # Create an empty vector to hold string literals
    extracted_text <- c()

    repeat {
      match <- gsub(".*('.*?').*", "\\1", subst_pattern)

      if (match != subst_pattern) {
        extracted_text <- c(extracted_text, gsub("'", "", match))
        subst_pattern <- sub("'.*?'", "***", subst_pattern)
      }

      if (match == subst_pattern) break
    }

    # Perform replacements to the `subst_pattern` string
    subst_pattern <-
      localized_date_time(
        date_input = date_component,
        time_input = time_component,
        tz_offset = tz_offset,
        iana_tz_name = iana_tz_name,
        subst_pattern = subst_pattern,
        locale = locale)

    # Replace `subst_pattern` placeholders with
    # the reserved literal strings
    if (grepl("\\*\\*\\*", subst_pattern)) {
      for (j in seq_along(extracted_text)) {
        subst_pattern <- sub("\\*\\*\\*", extracted_text[j], subst_pattern)
      }
    }

    output[i] <- subst_pattern
  }

  output
}
