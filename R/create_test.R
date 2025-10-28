#' Create a matrix of items
#'
#' @param ... integer vectors with the probabilities
#' @param test_type to be passed to the item creator (all the same type).
#' @param num_attrs Needed to determine the matrix cols.
#'
#' @returns a matrix of parameters
#' @export
#'
#' @examples
#' create_test(
#'   c(.2, .3, .5),
#'   c(.2, .4, .6),
#'   test_type="CRUM",
#'   num_attrs=2)
create_test <- function(..., test_type = "LCDM", num_attrs = NULL) {
  item_probs <- list(...)
  n_items <- length(item_probs)
  n_parameters <- 2^num_attrs

  items <- matrix(0, nrow = n_items, ncol = n_parameters)  # pre-fill with 0

  for (i in seq_len(n_items)) {
    x <- saltr::create_item(
      item_probs[[i]],
      item_type = test_type,
      num_attrs = num_attrs
    )

    # pad or truncate to fit into n_parameters
    len_x <- length(x)
    if (len_x > n_parameters) {
      warning(sprintf("Item %d has %d elements, truncating to %d.", i, len_x, n_parameters))
      x <- x[seq_len(n_parameters)]
    }
    items[i, seq_len(len_x)] <- x
  }

  items
}
