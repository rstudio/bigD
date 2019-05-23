#' Provide separators to a numeric value with a grouping pattern
#' @param integer The integer portion of a numeric value. Can be supplied as an
#'   integer value or a string. If supplied as a string, it must only contain
#'   numeral characters.
#' @param grouping The grouping pattern of either "#,##0.###" with `"333"` or
#'   "#,##,##0.###" with `"322"`.
#' @param sep_mark The character to use as the separator mark. By default, this
#'   is the comma character.
#' @export
insert_number_seps <- function(integer,
                               grouping = c("333", "322"),
                               sep_mark = ",") {

  if (inherits(integer, "integer")) {
    integer <- as.character(integer)
  } else if (inherits(integer, "character") &&
             !grepl("^[0-9]+?$", integer)) {

    stop("If `integer` supplied as a character value, it must only contain numbers.",
         call. = FALSE)
  }

  if (grouping == "333") {
    grouping <- c(3, 3, 3)
  } else if (grouping == "322") {
    grouping <- c(3, 2, 2)
  }

  insertion_seq <- get_insertion_sequence(integer, grouping)

  insert_str(
    target = str_rev(integer),
    insert = rep(sep_mark, length(insertion_seq)),
    index = insertion_seq) %>%
    str_rev()
}
