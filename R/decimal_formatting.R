#' For a non-decimal number string, insert separators
#' according to the grouping pattern
#' @param integer the integer portion of a numeric vaue. Can be supplied as an
#' integer value or a string. If supplied as a string, it must only contain
#' numeral characters.
#' @param grouping the CLDR grouping pattern of either \code{"#,##0.###"} or
#' \code{"#,##,##0.###"}.
#' @param sep_mark the character to use as the separator mark.
#' @export
insert_number_seps <- function(integer,
                               grouping = c("#,##0.###", "#,##,##0.###"),
                               sep_mark) {

  if (inherits(integer, "integer")) {
    integer <- as.character(integer)
  } else if (inherits(integer, "character") &&
             !grepl("^[0-9]+?$", integer)) {

    stop("If `integer` supplied as a character value, it must only contain numbers.",
         call. = FALSE)
  }

  if (grouping == "#,##0.###") {
    grouping <- c(3, 3, 3)
  } else if (grouping == "#,##,##0.###") {
    grouping <- c(3, 2, 2)
  }

  insertion_seq <- get_insertion_sequence(integer, grouping)

  insert_str(
    target = str_rev(integer),
    insert = rep(sep_mark, length(insertion_seq)),
    index = insertion_seq) %>%
    str_rev()
}
