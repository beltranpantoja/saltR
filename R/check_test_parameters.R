#' Check if the test parameters are compatible with a Q-matrix
#'
#' @param test item parameters matrix
#' @param qmatrix Optional. If no qmatrix is passed, then it checks that it is
#'  compatible for some qmatrix
#'
#' @returns boolean
#' @export
#'
check_test_parameters <- function(test, qmatrix = NULL) {
  # TODO: Add more information to the error

  test[!is.na(test)] <- 1
  test[is.na(test)] <- 0

  if (is.null(qmatrix)) {
    K <- log2(ncol(test))
    full_qmatrix <- create_patterns(K)
    full_mask <- build_test_parameters(full_qmatrix)

    A <- apply(full_mask, 1, paste, collapse = "")
    B <- apply(test, 1, paste, collapse = "")

    return(all(B %in% A))
  } else {
    expected_params <- build_test_parameters(qmatrix)

    # Check if dimensions match first to avoid errors
    if (!all(dim(test) == dim(expected_params))) {
      return(FALSE)
    }

    # Return a single TRUE/FALSE
    return(all(test == expected_params))
  }
}
