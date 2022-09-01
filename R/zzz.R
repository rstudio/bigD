utils::globalVariables(
  c(
    "dates_elements",
    "input_locale",
    "zone_name"
  )
)

# R 3.5 and earlier have a bug on Windows where if x is latin1 or unknown and
# replacement is UTF-8, the UTF-8 bytes are inserted into the result but the
# result is not marked as UTF-8.
# Example: gsub("a", "\u00B1", "a", fixed = TRUE)
utf8_aware_sub <- NULL

.onLoad <- function(libname, pkgname, ...) {

  utf8_aware_sub <<- identical("UTF-8", Encoding(sub(".", "\u00B1", ".", fixed = TRUE)))

  invisible(NULL)
}
