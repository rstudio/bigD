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

is_long_tzid_present <- function(input) {

  if (!grepl("^.*\\([^']*\\)$", input)) {
    return(FALSE)
  }

  long_tzid <- extract_long_tzid(input = input)

  validate_long_tzid(long_tzid = long_tzid)

  TRUE
}

get_long_tzid_str <- function(input) {

  if (!is_long_tzid_present(input = input)) {
    return(NA_character_)
  }

  long_tzid <- extract_long_tzid(input = input)

  normalize_long_tzid(long_tzid = long_tzid)
}

extract_long_tzid <- function(input) {
  gsub("^.*\\((.*)\\)$", "\\1", input)
}

validate_long_tzid <- function(long_tzid) {

  known_long_tzid_vals <-
    c(unique(tz_name_resolution$tz_canonical), tz_name_resolution$tz_alt)

  if (!(long_tzid %in% known_long_tzid_vals)) {
    stop("The long time zone provided is not valid.", call. = FALSE)
  }

  invisible(TRUE)
}

normalize_long_tzid <- function(long_tzid) {

  if (long_tzid %in% tz_name_resolution$tz_alt) {

    long_tzid <-
      tz_name_resolution[
        tz_name_resolution$tz_alt == long_tzid, ][["tz_canonical"]]
  }

  long_tzid
}

# A long tzid can be provided in parentheses as `(America/Vancouver)` at the
# end of a datetime string; the `get_long_tzid_str()` function will remove the
# parentheses (and should only be invoked if the datetime input contains a
# long tzid)
# The `long_tzid_to_tz_str` will convert a long tzid to a valid tz string in
# the (+/-)hhmm form
long_tzid_to_tz_str <- function(long_tzid, input_dt) {

  if (grepl("^Etc/", long_tzid)) {

    if (long_tzid %in% c("Etc/GMT", "Etc/UTC")) {
      return("+0000")
    }
  }

  tzdb_entries_tzid <- tzdb[tzdb$zone_name == long_tzid, ]

  if (nrow(tzdb_entries_tzid) < 1) {
    return(NA_character_)
  }

  input_date <- as.Date(input_dt)

  tzdb_idx <- rle(!(tzdb_entries_tzid$date_start < input_date))$lengths[1]

  tz_offset <- tzdb_entries_tzid[tzdb_idx, ]$gmt_offset_h

  minutes <- formatC(round((abs(tz_offset) %% 1) * 60, 0), width = 2, flag = "0")
  hours <- formatC(trunc(abs(tz_offset)), width = 2, flag = "0")
  sign <- ifelse(tz_offset < 0, "-", "+")

  paste0(sign, hours, minutes)
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

  # If there is an attached tzid string then remove it
  if (is_long_tzid_present(input = out)) {
    out <- gsub("\\s*\\([^']*\\)$", "", out)
  }

  out
}

get_tz_offset_val_from_tz_str <- function(tz_str) {

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

get_tz_offset_val <- function(input, tz_str = NULL) {

  if (is.null(tz_str)) {
    tz_str <- get_tz_str(input = input)
  }

  get_tz_offset_val_from_tz_str(tz_str = tz_str)
}

strip_tz <- function(input) {

  tz_str <- get_tz_str(input = input)

  out <- gsub(tz_str, "", input, fixed = TRUE)
  out <- gsub("\\s*\\([^']*\\)$", "", out)

  out
}

strip_long_tzid <- function(input) {

  long_tzid <- get_long_tzid_str(input = input)

  input_str <- gsub(paste0("(", long_tzid, ")"), "", input, fixed = TRUE)
  input_str <- strip_surrounding_whitespace(input = input_str)

  input_str
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

get_exemplar_city <- function(long_tzid) {

  exemplar_city <- unlist(strsplit(long_tzid, "/")[[1]])[2]

  exemplar_city
}

get_localized_exemplar_city <- function(
    long_tzid,
    locale,
    yield_unknown = TRUE
) {

  exemplar_city <- unlist(strsplit(long_tzid, "/")[[1]])[2]

  # TODO: Resolve links of exemplar cities to a canonical exemplar city
  #       This will require a separate lookup table

  if (!(exemplar_city %in% colnames(tz_exemplar)[-1])) {

    # Get localized variant of 'Unknown City'
    if (yield_unknown) {
      return(tz_exemplar[tz_exemplar$locale == locale, ][["Unknown"]])
    } else {
      return(NA_character_)
    }
  }

  exemplar_city_localized <-
    tz_exemplar[tz_exemplar$locale == locale, ][[exemplar_city]]

  exemplar_city_localized
}

# The short specific non-location format (e.g., 'PST') from a `long_tzid`
get_tz_short_specific <- function(long_tzid, input_dt) {

  input_date <- as.Date(input_dt)

  tzdb_entries_tzid <- tzdb[tzdb$zone_name == long_tzid, ]

  tzdb_idx <- rle(!(tzdb_entries_tzid$date_start < input_date))$lengths[1]

  tz_short_specific <- tzdb_entries_tzid[tzdb_idx, ]$abbrev

  # TODO: add check to ensure that the `abbrev` value is a valid
  # short specific non-location time zone

  tz_short_specific
}

# The long specific non-location format (e.g., 'Pacific Standard Time') from
# a `long_tzid`
get_tz_long_specific <- function(long_tzid, input_dt, locale) {

  input_date <- as.Date(input_dt)

  tzdb_entries_tzid <- tzdb[tzdb$zone_name == long_tzid, ]

  if (nrow(tzdb_entries_tzid) < 1) {
    return(NA_character_)
  }

  tzdb_idx <- rle(!(tzdb_entries_tzid$date_start < input_date))$lengths[1]

  tzdb_entries_tzid_ln <- tzdb_entries_tzid[tzdb_idx, ]

  if (tzdb_entries_tzid_ln$dst) {
    pattern_col_tz_formats <- "region_format_daylight"
  } else {
    pattern_col_tz_formats <- "region_format"
  }

  tz_long_specific_pattern <-
    tz_formats[tz_formats$locale == locale, ][[pattern_col_tz_formats]]

  # Get the metazone in its long ID format
  metazone_long_id <- long_tz_id_to_metazone_long_id(long_tzid = long_tzid)

  if (is.na(metazone_long_id)) {
    return(NA_character_)
  }

  # Get the row of the `tz_metazone_names` table based on the supplied locale
  tz_metazone_names_row <- tz_metazone_names[tz_metazone_names$locale == locale, ]

  # Get the list entry corresponding to the metazone and the locale
  tz_metazone_names_entry <-
    unlist(
      tz_metazone_names_row[, colnames(tz_metazone_names_row) == metazone_long_id][[1]]
    )

  if (tzdb_entries_tzid_ln$dst) {
    daylight_standard <- "daylight"
  } else {
    daylight_standard <- "standard"
  }

  tz_metazone_names_filtered <-
    tz_metazone_names_entry[
      grepl(daylight_standard, names(tz_metazone_names_entry))]

  tz_metazone_name <-
    unname(tz_metazone_names_filtered[
      grepl("long", names(tz_metazone_names_filtered))])

  tz_metazone_name
}

get_tz_bcp_id <- function(long_tzid) {

  # If the supplied `long_tzid` value is NA, return NA
  if (is.na(long_tzid)) {
    return(NA_character_)
  }

  tz_name <- tz_bcp_id[tz_bcp_id$tz_canonical == long_tzid, ][["tz_bcp_id"]]

  tz_name
}

# Get the non-location formatted time zone names (e.g., 'Pacific Time', 'PT')
# from a `long_tzid` (canonical tz name)
#
# The non-location format reflects "wall time" (what is on
# a clock on the wall). It's used for recurring events, meetings, or anywhere
# people do not want to be overly specific. For example, "10 am Pacific Time"
# will be GMT-8 in the winter, and GMT-7 in the summer.
get_tz_non_location <- function(
    long_tzid,
    locale,
    short_long,
    type
) {

  if (!(short_long %in% c("long", "short"))) {
    stop("The `short_long` keyword should either be 'long' or 'short'.")
  }

  if (!(type %in% c("generic", "standard", "daylight"))) {
    stop("The `short_long` keyword should either be 'long' or 'short'.")
  }

  # If the supplied `long_tzid` value is NA, return NA
  if (is.na(long_tzid)) {
    return(NA_character_)
  }

  # Get the metazone in its long ID format
  metazone_long_id <- long_tz_id_to_metazone_long_id(long_tzid = long_tzid)

  # Check if metazone is NA and return NA if that is so
  if (is.na(metazone_long_id)) {
    return(NA_character_)
  }

  # Get the row of the `tz_metazone_names` table based on the supplied locale
  tz_metazone_names_row <- tz_metazone_names[tz_metazone_names$locale == locale, ]

  # Get the list entry corresponding to the metazone and the locale
  tz_metazone_names_entry <-
    unlist(
      tz_metazone_names_row[, colnames(tz_metazone_names_row) == metazone_long_id][[1]]
    )

  target_item <- paste0(short_long, ".", type)
  available_items <- names(tz_metazone_names_entry)
  has_long_items <- any(grepl("long", names(tz_metazone_names_entry)))
  has_short_items <- any(grepl("short", names(tz_metazone_names_entry)))

  if (length(available_items) == 1) {
    tz_name <- tz_metazone_names_entry[[available_items]]
  } else if (target_item %in% available_items) {
    tz_name <- tz_metazone_names_entry[[target_item]]
  } else if (short_long == "short" && !has_short_items) {
    if (!any(grepl(type, available_items))) {
      tz_name <- tz_metazone_names_entry[["long.standard"]]
    } else {
      tz_name <- tz_metazone_names_entry[[paste0("long.", type)]]
    }
  } else {
    tz_name <- tz_metazone_names_entry[[available_items[1]]]
  }

  tz_name
}

long_tz_id_to_metazone_long_id <- function(long_tzid) {

  tzid_in_tz_metazone_users <- long_tzid %in% tz_metazone_users$canonical_tz_name

  if (!tzid_in_tz_metazone_users) {

    if (long_tzid %in% unique(tz_name_resolution$tz_canonical)) {

      alt_names <-
        tz_name_resolution[tz_name_resolution$tz_canonical == long_tzid, ][["tz_alt"]]

      if (any(alt_names %in% tz_metazone_users$canonical_tz_name)) {

        long_tzid <- alt_names[1]

      } else {
        return(NA_character_)
      }

    } else {
      return(NA_character_)
    }
  }

  tz_metazone_users_rows <-
    tz_metazone_users[tz_metazone_users$canonical_tz_name == long_tzid, ]

  # Return NA if number of rows in `tz_metazone_users_rows` is zero
  if (nrow(tz_metazone_users_rows) < 1) {
    return(NA_character_)
  }

  # TODO: develop routine to further filter multirow `tz_metazone_users_rows`
  # to a single row based on `locale`; for now, obtain the first metazone
  metazone <- tz_metazone_users_rows[1, ][["metazone_long_id"]]

  metazone
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

strip_surrounding_whitespace <- function(input) {
  gsub("(^[[:space:]]*)|([[:space:]]*$)", "", input)
}
