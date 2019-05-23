#' Get a localized date, time, or date-time string in a standard form.
#'
#' @param input The input date-time. The appropriate representation should use
#'   the following construction outlined in the ISO 8601:2004 standard:
#'   `YYYY-MM-DDThh:mm:ss.sTZD` although some allowances made here to ease this
#'   restrictiveness (for example, the literal `T` separating the date and time
#'   components is optional). Fractional seconds are optional as is the
#'   time-zone designation (TZD).
#' @param date_format a date format specification using the rules of the
#'   SimpleDateFormat. The helper function [fdf()] could be used with a
#'   predefined format name or format number. The information function
#'   [info_fdf_types()] provides information on which date formatting presets
#'   are available and includes output samples for each of them.
#' @param time_format a time format specification using the rules of the
#'   SimpleDateFormat. There are two useful helper functions to use 12-hour and
#'   24-hour preset formats: [ftf_12()] and [ftf_24()]. Either could be used
#'   with a predefined format name or format number. The information functions
#'   [info_ftf_12_types()] and [info_ftf_24_types()] both provide information on
#'   which 12-hour and 24-hour time formatting presets are available. They also
#'   include output samples for each time preset.
#' @param combination a combining pattern for the localized date and time
#'   components. If this is not provided, then the combining pattern will come
#'   from the specified locale's `"full"` designation. If there is neither a
#'   `combination` nor a `locale` provided, then the time component will follow
#'   the date after a space character. If providing a pattern, the string should
#'   be composed with the `{0}` and `{1}` placeholders, representing time and
#'   date components, respectively. All other characters are taken to be string
#'   literals.
#' @param locale an optional output locale to use for formatting the `input`
#'   value according to the specified locale's rules. Example locale names
#'   include `"en_US"` for English (United States) and `"fr_FR"` for French
#'   (France). If a locale isn't provided and certain require locale- based
#'   formatting then the `"en_US"` locale is used for this purpose.
#' @export
fmt_date_time <- function(input,
                          date_format = NULL,
                          time_format = NULL,
                          combination = NULL,
                          locale = NULL) {

  # Stop function if both date_format and time_format are NULL
  if (is.null(date_format) & is.null(time_format)) {
    stop("At least one of `date_format` and `time_format` must be provided.",
         call. = FALSE)
  }

  if (inherits(date_format, "fdf")) {

    fdf_str <- as.character(date_format) %>% tolower()

    date_pattern <-
      get_localized_value(
        type = "date_and_time_gregorian",
        section = paste0("formats_flexible_date_formats_", fdf_str),
        locale = locale)

  } else if (inherits(date_format, "character")) {

    date_pattern <- date_format
  }

  if (inherits(time_format, "ftf_12")) {

    fdf_str <- as.character(time_format) %>% tolower()

    time_pattern <-
      get_localized_value(
        type = "date_and_time_gregorian",
        section = paste0("formats_flexible_12_hour_time_formats_", fdf_str),
        locale = locale)

  } else if (inherits(time_format, "ftf_24")) {

    fdf_str <- as.character(time_format) %>% tolower()

    time_pattern <-
      get_localized_value(
        type = "date_and_time_gregorian",
        section = paste0("formats_flexible_24_hour_time_formats_", fdf_str),
        locale = locale)

  } else if (inherits(time_format, "character")) {

    time_pattern <- time_format
  }


  if (exists("date_pattern") & exists("time_pattern")) {

    # Create pattern with date and time

    if (!is.null(combination) && inherits(combination, "date_time_combine")) {

      # Obtain the width
      width <- combination %>% as.character()

      # Obtain the date-time combination section that
      # corresponds to those formats of the requested width
      combination_section_name <-
        paste0("formats_standard_date_time_combination_formats_", width)

      subst_pattern <-
        get_localized_value(
          type = "date_and_time_gregorian",
          section = combination_section_name,
          locale = locale)


    } else if (is.null(combination) & !is.null(locale)) {

      width <- "full"

      combination_section_name <-
        paste0("formats_standard_date_time_combination_formats_", width)

      subst_pattern <-
        get_localized_value(
          type = "date_and_time_gregorian",
          section = combination_section_name,
          locale = locale)

    } else if (is.null(combination) & is.null(locale)) {

      subst_pattern <- "{1} {0}"

    } else {
      subst_pattern <- combination
    }

  } else if (exists("date_pattern") & !exists("time_pattern")) {

    subst_pattern <- date_pattern

  } else if (exists("time_pattern") & !exists("date_pattern")) {

    subst_pattern <- time_pattern
  }

  # Prepare the output vector
  output <- rep(NA_character_, length(input))

  for (i in seq(input)) {

    # Is a date component present?
    date_present <- is_date_present(input[i])

    # Is a time component present?
    time_present <- is_time_present(input[i])

    if (!date_present & !time_present) {
      stop("There are no detectable date or time components.", call. = FALSE)
    }

    # Extract the date component if it exists
    date_component <- get_date_component(input[i])

    # Extract the time component if it exists
    time_component <- get_time_component(input[i])

    # Extract the time zone if it exists
    tz_offset <- get_tz_offset(input[i])

    # Extract the IANA time zone name if it exists
    iana_tz_name <- get_iana_tz(input[i])

    if (exists("date_pattern") & exists("time_pattern")) {

      # Substitute the `time_pattern` and `date_pattern`
      # into the correct `subst_pattern` locations
      subst_pattern <- gsub("\\{0\\}", time_pattern, subst_pattern)
      subst_pattern <- gsub("\\{1\\}", date_pattern, subst_pattern)
    }

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
      for (i in seq_along(extracted_text)) {
        subst_pattern <- sub("\\*\\*\\*", extracted_text[i], subst_pattern)
      }
    }

    output[i] <- subst_pattern
  }

  output
}
