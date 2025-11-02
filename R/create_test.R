#' Create a matrix of items
#'
#'  @description
#'  An utility function that calls the `create_item` function and returns a matrix
#'  padded with NAs that can be used directly with the responses generation.
#'  It assumes all the items are LCDM.
#'
#'
#' @param ... integer vectors with the probabilities
#'
#' @returns a matrix of parameters
#' @export
create_test <- function(...) {
  item_probs <- list(...)

  # If the probabilities are already in a matrix we create the items and return
  if (is.matrix(item_probs[[1]])) {
    item_params <- apply(
      item_probs[[1]],
      1,
      function(x) create_item(x)
    )
    return(t(item_params))
  }

  # Else, we convert the probabilities to parameters as a list
  item_params <- lapply(
    item_probs,
    function(x) create_item(x)
  )

  # We find the one with the most amount of parameters
  num_parameters <- max(sapply(item_params, length))

  # We pad all values accordingly and simplify them into a matrix
  t(sapply(item_params, function(x) .pad_with_NA(x, num_parameters)))
}


#' Pad vector with NAs
#'
#' @param x vector to right-pad
#' @param out_len expected total length
#'
#' @returns vector x padded with NAs
#' @noRd
.pad_with_NA <- function(x, out_len) {
  pad <- rep(NA, out_len - length(x))
  return(c(x, pad))
}
