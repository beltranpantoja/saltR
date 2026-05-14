#' Create a test_parameters matrix
#'
#' Converts a matrix or numeric data.frame into a test_parameters object which
#'  is a matrix with a quality of life change for printing.
#'
#' @param x A matrix or a numeric data.frame.
#' @export
as_test_parameters <- function(x) {
  # 1. If it's a data.frame, check if it's numeric and convert
  if (is.data.frame(x)) {
    is_num <- vapply(x, is.numeric, logical(1))

    if (!all(is_num)) {
      stop("The data.frame contains non-numeric columns, all must be numeric.")
    }
    x <- as.matrix(x)
  }

  # 2. Final check: is it a matrix now?
  if (!is.matrix(x)) {
    stop("Input must be a matrix or a numeric data.frame.")
  }
 if (!check_test_parameters(x)) {
    stop("Test parameters are malformed.")
  }

  # 3. Assign the custom class
  class(x) <- c("test_parameters", "matrix")
  x
}


#' Prints a matrix to the console in a prettier format
#'
#' This function prints a pretty table and returns the original object
#'  so it can be used inside a pipeline.
#'
#' @param matrix matrix to print
#' @param digits number of rounding digits
#' @param prefix prefix for the columns.By default, uses the Greek letter
#'  lambda in interactive sessions.
#'
#' @returns the same passed object
#' @export
#'
pretty_print <- function(matrix, digits = 3, prefix = NULL) {
  # TODO: fix pretty print to change the columns labels
  if (is.null(prefix)) {
    prefix <- if (interactive()) "\U03BB" else "lambda"
  }
  params_print <- round(as.matrix(matrix), digits)
  # The rounding value plus space for -0.
  str_width <- digits + 4

  colnames(params_print) <- format(
    paste0(prefix, colnames(matrix)), # Adding the prefix
    justify = "centre",
    width = str_width
  )

  print.table(params_print, digits = digits, justify = "centre")

  # We return the original value
  invisible(matrix)
}


#' @export
print.test_matrix <- function(x, ...) {
  pretty_print(x, ...)
}
