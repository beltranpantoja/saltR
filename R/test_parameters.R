#' Test parameters
#'
#' A `test_parameters` object is a matrix used to store item/test parameters
#' with enhanced printing and validation.
#'
#' @family test parameter objects
#' @name test_parameters
NULL



#' @rdname test_parameters
#' @family test parameter objects
#' @param x A matrix or a numeric data.frame.
#'
#'
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

  # We check is properly formed
  check_test_parameters(x)

  # 3. Assign the custom class
  class(x) <- c("test_parameters", "matrix")
  x
}


#' @rdname test_parameters
#'
#' @param test_parameters matrix to print
#' @param digits number of rounding digits
#' @param prefix prefix for the columns. By default, uses the Greek letter
#'  lambda in interactive sessions.
#'
#' @family test parameter objects
#' @returns the passed test_parameters with no change
#' @export
pretty_print <- function(test_parameters, digits = 3, prefix = NULL) {
  if (is.null(prefix)) {
    prefix <- if (interactive()) "\U03BB" else "lambda"
  }


  params_print <- round(
    as_test_parameters(test_parameters),
    digits
  )

  # The rounding value plus space for -0.
  str_width <- digits + 4

  colnames(params_print) <- format(
    paste0(prefix, colnames(test_parameters)), # Adding the prefix
    justify = "centre",
    width = str_width
  )

  print.table(
    params_print,
    digits = digits,
    justify = "centre"
  )

  # We return the original value
  invisible(test_parameters)
}


#' @export
print.test_parameters <- function(x, ...) {
  pretty_print(x, ...)
}
