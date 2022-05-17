get_date_pattern <- function() {
  "([\\+-]?\\d{4}-\\d\\d-\\d\\d)"
}

get_time_pattern <- function() {
  "((T| )((\\d|\\d\\d):\\d\\d[0-9:.]*)|^((\\d|\\d\\d):\\d\\d[0-9:.]*))"
}

get_tz_pattern <- function() {
  "(Z|(\\+|-)\\d\\d|(\\+|-)\\d\\d:\\d\\d|(\\+|-)\\d\\d\\d\\d)"
}

get_tz_pattern_z <- function() {
  paste0(get_time_pattern(), "(Z)")
}

get_tz_pattern_hh <- function() {
  paste0(get_time_pattern(), "((\\+|-)\\d\\d)")
}

get_tz_pattern_hh_mm <- function() {
  paste0(get_time_pattern(), "((\\+|-)\\d\\d:\\d\\d)")
}

get_tz_pattern_hhmm <- function() {
  paste0(get_time_pattern(), "((\\+|-)\\d\\d\\d\\d)")
}

get_wrapped_iana_pattern <- function() {
  "(\\[|\\()([a-zA-Z0-9\\/ -\\+_]*?)(?:\\]|\\))"
}

get_attached_iana_pattern <- function() {
  "(\\s-\\s|\\s|\\/|\\|)([a-zA-Z0-9\\/ -\\+_]*?)"
}

is_date_present <- function(input) {
  grepl(get_date_pattern(), input)
}

is_time_present <- function(input) {
  grepl(get_time_pattern(), input)
}

is_tz_present <- function(input) {

  any(
    c(
      grepl(get_tz_pattern_z(), input),
      grepl(get_tz_pattern_hh(), input),
      grepl(get_tz_pattern_hh_mm(), input),
      grepl(get_tz_pattern_hhmm(), input)
    )
  )
}

get_tz_str <- function(input) {

  if (!is_tz_present(input = input)) {
    return("")
  }

  if (grepl("+", input, fixed = TRUE)) {

    out <- unlist(strsplit(input, split = "+", fixed = TRUE))[[2]]
    out <- paste0("+", out)

  } else if (grepl("Z", input)) {

    out <- "Z"

  } else {

    out <- unlist(strsplit(input, split = "-", fixed = TRUE))
    out <- out[length(out)]
    out <- paste0("-", out)
  }

  out
}

get_tz_offset_val <- function(input) {

  if (!is_tz_present(input = input)) {
    return(NA_real_)
  }

  tz_str <- get_tz_str(input = input)

  if (tz_str == "Z") {

    offset_val <- 0

  } else {

    offset_val <-
      as.numeric(substr(tz_str, 2, 3)) +
      as.numeric(substr(tz_str, nchar(tz_str) - 1, nchar(tz_str))) / 60

    if (grepl("-", tz_str)) {
      offset_val <- offset_val * (-1)
    }
  }

  offset_val
}

strip_tz <- function(input) {

  tz_str <- get_tz_str(input = input)
  gsub(tz_str, "", input, fixed = TRUE)
}

is_iana_present <- function(input) {

  grepl(
    paste0(
      get_time_pattern(), get_tz_pattern(),
      "(", get_wrapped_iana_pattern(), "|", get_attached_iana_pattern(), ")"),
    input
  )
}

which_tz_pattern <- function(input) {

  if (grepl(get_tz_pattern_z(), input)) {
    return("z")
  }

  if (grepl(get_tz_pattern_hh(), input)) {
    tz_pattern <- "hh"
  }

  if (grepl(get_tz_pattern_hh_mm(), input)) {
    return("hh_mm")
  }

  if (grepl(get_tz_pattern_hhmm(), input)) {
    return("hhmm")
  }

  if (!exists("tz_pattern")) {
    return(NA)
  } else {
    return(tz_pattern)
  }
}

which_iana_pattern <- function(input) {

  if (grepl(paste0(get_tz_pattern(), get_wrapped_iana_pattern()), input)) {
    return("wrapped")
  } else if (grepl(paste0(get_tz_pattern(), get_attached_iana_pattern()), input)) {
    return("attached")
  } else {
    return(NA_character_)
  }
}

get_tz_offset <- function(input) {

  tz_pattern <- which_tz_pattern(input)

  switch(
    tz_pattern,
    z = {
      tz_offset <- 0.0
    },
    hh = {
      iso_tz_component <- gsub(paste0(".*", get_tz_pattern_hh(), ".*"), "\\4", input)
      tz_offset <- as.numeric(iso_tz_component)
    },
    hh_mm = {
      iso_tz_component <- gsub(paste0(".*", get_tz_pattern_hh_mm(), ".*"), "\\4", input)
      offset_sign <- ifelse(substr(iso_tz_component, 1, 1) == "-", -1L, 1L)
      offset_h <- as.numeric(substr(iso_tz_component, 1, 3))
      offset_min <- as.numeric(substr_right(iso_tz_component, 2)) / 60.0
      tz_offset <- (abs(offset_h) + offset_min) * offset_sign
    },
    hhmm = {
      iso_tz_component <- gsub(paste0(".*", get_tz_pattern_hhmm(), ".*"), "\\4", input)
      offset_sign <- ifelse(substr(iso_tz_component, 1, 1) == "-", -1L, 1L)
      offset_h <- as.numeric(substr(iso_tz_component, 1, 3))
      offset_min <- as.numeric(substr_right(iso_tz_component, 2)) / 60.0
      tz_offset <- (abs(offset_h) + offset_min) * offset_sign
    })

  tz_offset
}


# The short specific non-location format (e.g., PDT)
get_tz_short_specific <- function(tz_str) {

  return(NA_character_)
}


get_long_local_gmt <- function(tz_offset) {

  minutes <- formatC(round((abs(tz_offset) %% 1) * 60, 0), width = 2, flag = "0")
  hours <- formatC(trunc(abs(tz_offset)), width = 2, flag = "0")
  sign <- ifelse(tz_offset < 0, "-", "+")

  paste0("GMT", sign, hours, ":", minutes)
}

get_short_local_gmt <- function(tz_offset) {

  minutes <- formatC(round((abs(tz_offset) %% 1) * 60, 0), width = 2, flag = "0")

  if (minutes == "00") {
    minutes <- ""
  } else {
    minutes <- paste0(":", minutes)
  }

  hours <- as.character(trunc(abs(tz_offset)))
  sign <- ifelse(tz_offset < 0, "-", "+")

  paste0("GMT", sign, hours, minutes)
}

get_date_component <- function(input) {

  if (!is_date_present(input)) {
    return(NA_character_)
  }

  gsub(paste0(".*", get_date_pattern(), ".*"), "\\1", input)
}

get_time_component <- function(input) {

  if (!is_time_present(input)) {
    return(NA_character_)
  }

  gsub(paste0(".*", get_time_pattern(), ".*"), "\\2", input)
}

get_iana_tz <- function(input) {

  iana_pattern <- which_iana_pattern(input)

  switch(
    iana_pattern,
    wrapped = {
      tz_name <-
        gsub(
          paste0(".*", get_time_pattern(), get_tz_pattern()),
          "", input)
      tz_name <- gsub("(\\(|\\[|\\)|\\])", "", tz_name)
    },
    attached = {
      tz_name <-
        gsub(
          paste0(".*", get_time_pattern(), get_tz_pattern(), "(\\s-\\s|\\s|\\/|\\|)"),
          "", input)
    })

  if (!exists("tz_name")) {
    return(NA_character_)
  }

  tz_name
}
