#' @export
fdf <- function(format) {

  if (inherits(format, "numeric") &&
      !(format %in% seq(fdf_types()))) {

    stop("The supplied format number is not within the range of 1-",
         length(fdf_types()), ".")

  } else if (inherits(format, "numeric") &&
             format %in% seq(fdf_types())) {

    format <- fdf_types()[format]
  }

  # Create a character object
  x <- format

  class(x) <- "fdf"
  x
}

#' @export
ftf_12 <- function(format) {

  if (inherits(format, "numeric") &&
      !(format %in% seq(ftf_12_types()))) {

    stop("The supplied format number is not within the range of 1-",
         length(ftf_12_types()), ".")

  } else if (inherits(format, "numeric") &&
             format %in% seq(ftf_12_types())) {

    format <- ftf_12_types()[format]
  }

  # Create a character object
  x <- format

  class(x) <- "ftf_12"
  x
}

#' @export
ftf_24 <- function(format) {

  if (inherits(format, "numeric") &&
      !(format_name %in% seq(ftf_24_types()))) {

    stop("The supplied format number is not within the range of 1-",
         length(ftf_24_types()), ".")

  } else if (inherits(format, "numeric") &&
             format %in% seq(ftf_24_types())) {

    format <- ftf_24_types()[format]
  }

  # Create a character object
  x <- format

  class(x) <- "ftf_24"
  x
}
