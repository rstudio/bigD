#' @export
fdf <- function(format_name) {

  # Create a character object
  x <- format_name

  class(x) <- "fdf"
  x
}
