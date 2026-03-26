#' Build a Q-Matrix from a a matrix of test parameters
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`.
#' @param attributes_names vector of names for the attributes. Defaults
#'  to "Attr#"
#' @param item_names vector of names for the items. Defaults to "Item#"
#'
#' @returns a Q-matrix
#' @export
build_qmatrix <- function(test_parameters) {
  # We don't care about the parameters, just which ones are present.
  # If there's a zero it is still considered as a parameter that's present
  test_parameters[!is.na(test_parameters)] <- 1
  test_parameters[is.na(test_parameters)] <- 0

  # First figure out number of attributes
  cols <- ncol(test_parameters)
  K <- log2(cols)

  # Do a mapping from all possible profiles
  patt <- create_patterns(K)

  # This is our reference to map to the patt matrix
  # That way we rebuild the test row by row
  patt_parameters <- build_test_parameters(patt)


  item_parameters <- split(test_parameters, row(test_parameters))

  # We iterate over the rows.
  matches <- vapply(seq_along(item_parameters), function(i) {
    match <- which(
      apply(
        patt_parameters, 1, function(row) all(row == item_parameters[[i]])
      )
    )

    if (length(match) == 0) {
      stop(sprintf("Row %d in test could not be matched. Check if it is valid", i))
    }

    match[1]
  }, integer(1))

  # We construct the qmatrix
  qmatrix <- patt[matches, ]


  # Giving names to the qmatrix rows and columns
  if (is.null(attributes_names)) {
    attributes_names <- paste0("Attr", 1:num_attr)
  }
  if (is.null(item_names)) {
    item_names <- paste0("Item", seq_len(dim(qmatrix)[1]))
  }
  colnames(qmatrix) <- attributes_names
  rownames(qmatrix) <- item_names

  # We return the qmatrix
  qmatrix
}
