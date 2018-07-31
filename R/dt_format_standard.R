#' Get a localized date-time in a standardized form.
#' @param input the input date-time. The appropriate representation should use
#' the following construction outlined in the ISO 8601:2004 standard:
#' \code{YYYY-MM-DDThh:mm:ss.sTZD} although some allowances made here to
#' ease this restrictiveness. For example, the literal \code{T} is optional,
#' fractional seconds are optional, and the time-zone designation (TZD) is only
#' required if the \code{long} and \code{full} standardized forms are requested
#' in \code{width}
#' @param locale the output locale for the printing of the date-time string.
#' @param width The standardized output date-time format, which can be one of
#' either \code{short}, \code{medium}, \code{long}, or \code{full}.
#' @export
dt_format_standard <- function(input,
                               locale = "en_US",
                               width = "medium") {

  # Extract the date component if it exists
  date_component <- stringr::str_extract(input, "^\\d+?-\\d\\d-\\d\\d")

  # Is a date component present?
  date_present <- ifelse(!is.na(date_component), TRUE, FALSE)

  if (!date_present) {
    stop("There is no detectable date component.", call. = FALSE)
  }

  # Extract the time component if it exists
  time_component <- stringr::str_extract(input, "(\\d|\\d\\d):\\d\\d[0-9:.]*")

  # Is a time component present?
  time_present <- ifelse(!is.na(time_component), TRUE, FALSE)

  if (!time_present) {
    stop("There is no detectable time component.", call. = FALSE)
  }

  # Extract the time zone if it exists

  # <time>Z
  # <time>±hh:mm
  # <time>±hhmm
  # <time>±hh

  tz_component_z <-
    grepl(".*(\\d|\\d\\d):(\\d\\d|\\d\\d:\\d\\d)[0-9:.]*Z$", input, perl = TRUE)

  tz_component_hh_mm <-
    grepl(".*(\\d|\\d\\d):(\\d\\d|\\d\\d:\\d\\d)[0-9:.]*(\\+|-)\\d\\d:\\d\\d$", input, perl = TRUE)

  tz_component_hhmm <-
    grepl(".*(\\d|\\d\\d):(\\d\\d|\\d\\d:\\d\\d)[0-9:.]*(\\+|-)\\d\\d\\d\\d$", input, perl = TRUE)

  tz_component_hh <-
    grepl(".*(\\d|\\d\\d):(\\d\\d|\\d\\d:\\d\\d)[0-9:.]*(\\+|-)\\d\\d$", input, perl = TRUE)

  # Is a time zone present?
  tz_present <-
    ifelse(
      any(
        c(tz_component_z, tz_component_hh_mm,
          tz_component_hhmm, tz_component_hh)), TRUE, FALSE)

  if (!tz_present & width %in% c("full", "long")) {

    stop("A time zone component must be present to render `full` and `long` date-times.",
         .call = FALSE)
  }

  if (tz_present) {

    tz_component_provided <-
      which(
        c(tz_component_z, tz_component_hh_mm,
          tz_component_hhmm, tz_component_hh))

    if (tz_component_provided == 1) {
      offset <- 0
    }

    if (tz_component_provided == 2) {
      offset_l_char <- substr(substr_right(input, 6), 1, 3)
      offset_r_char <- substr_right(input, 2)
      offset <- as.numeric(paste0(offset_l_char, ".", offset_r_char))
    }

    if (tz_component_provided == 3) {
      offset_l_char <- substr_right(input, 5) %>% substr(1, 3)
      offset_r_char <- substr_right(input, 2)
      offset <- as.numeric(paste0(offset_l_char, ".", offset_r_char))
    }

    if (tz_component_provided == 4) {
      offset <- as.numeric(substr_right(input, 3))
    }
  }

  date_section_name <- paste0("formats_standard_date_formats_", width)

  date_pattern <-
    get_localized_value(
      type = "date_and_time_generic",
      section = date_section_name,
      locale = locale)

  time_section_name <- paste0("formats_standard_time_formats_", width)

  time_pattern <-
    get_localized_value(
      type = "date_and_time_gregorian",
      section = time_section_name,
      locale = locale)

  combination_section_name <-
    paste0("formats_standard_date_time_combination_formats_", width)

  combination_pattern <-
    get_localized_value(
      type = "date_and_time_gregorian",
      section = combination_section_name,
      locale = locale)

  pattern <- combination_pattern
  pattern <- gsub("\\{0\\}", time_pattern, pattern)
  pattern <- gsub("\\{1\\}", date_pattern, pattern)

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
