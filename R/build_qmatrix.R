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
build_qmatrix <- function(
  test_parameters,
  attributes_names = NULL,
  item_names = NULL
) {
  # We don't care about the parameters, just which ones are present.
  # If there's a zero it is still considered as a parameter that's present
  test_parameters[!is.na(test_parameters)] <- 1
  test_parameters[is.na(test_parameters)] <- 0

  # First figure out number of attributes
  cols <- ncol(test_parameters)
  num_attr <- log2(cols)

  if (num_attr %% 1 != 0) {
    msg <- sprintf(
      "Got %d columns, the number of columns must be a power of 2.", cols
    )
    saltr_emit(
      msg = msg,
      level = "error",
      class = "invalid_column_count"
    )
  }

  # Do a mapping from all possible profiles
  patt <- create_patterns(num_attr)

  # This is our reference to map to the patt matrix
  # That way we rebuild the test row by row
  patt_parameters <- build_test_parameters(patt)


  item_parameters <- split(test_parameters, row(test_parameters))

  # We iterate over the rows.
  matches <- vapply(seq_along(item_parameters), function(i) {
    parameters_to_q_vector(
      item_param = item_parameters[[i]],
      reference_patt = patt_parameters,
      index = i
    )
  }, integer(1))

  # We construct the qmatrix
  qmatrix <- patt[matches, ]


  # Giving names to the qmatrix rows and columns
  if (is.null(attributes_names)) {
    attributes_names <- paste0("Attr", seq_len(num_attr))
  }
  if (is.null(item_names)) {
    item_names <- paste0("Item", seq_len(dim(qmatrix)[1]))
  }
  colnames(qmatrix) <- attributes_names
  rownames(qmatrix) <- item_names

  # We return the qmatrix
  qmatrix
}


#' Find match for a single item parameter set
#' @noRd
parameters_to_q_vector <- function(item_param, reference_patt, index) {
  # logical vector of which rows in reference_patt match item_param
  matches_found <- apply(reference_patt, 1, function(row) all(row == item_param))
  match_idx <- which(matches_found)

  if (length(match_idx) == 0) {
    saltr_emit(
      msg = sprintf(
        "Item %d in test could not be transformed to a q-vector.",
        index
      ),
      level = "error",
      class = "parameter_match_error",
      row_index = index
    )
  }

  # Return the first match index
  match_idx[1]
}
