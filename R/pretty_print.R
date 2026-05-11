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
