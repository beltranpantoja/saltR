pretty_print <- function(matrix, digits = 3, prefix = "\U03BB") {
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
