#' Generate responses to a test
#'
#' @rdname generate_family
#'
#' @param examinees Binary matrix of respondents
#' @param test matrix of items parameters
#' @param get_probs if you want the probability of correct response matrix.
#' @param qmatrix Optional. All test are checked to see if they conform to some
#'  Q-matrix. If some specific qmatrix gets passed, then it is checked against
#'  that one.
#'
#' @returns a matrix of responses in the form 0/1 or the real probabilities.
#' @export
#'
generate_responses <- function(
  examinees,
  test,
  get_probs = FALSE,
  qmatrix = NULL
) {
  # First we check that the passed test is valid
  check_test_parameters(test, qmatrix = qmatrix)


  probs <- .get_prob_matrix(examinees, test)


  if (get_probs) {
    # Return
    probs
  } else {
    mask <- stats::runif(prod(dim(probs)))

    # Return
    (probs > mask) * 1
  }
}

#' Respondents-items matrix
#'
#' @param examinees Binary matrix of respondents
#' @param items matrix of items parameters
#'
#' @returns a matrix of responses
#' @noRd
.get_prob_matrix <- function(examinees, test) {
  mask <- .get_attr_mask_from_profile(examinees)

  # Make the NAs be 0 for the matrix multiplication
  test[is.na(test)] <- 0

  # This product gives us the logit matrix respondents by items
  logit_mat <- mask %*% t(test)

  # Return the probs
  exp(logit_mat) / (1 + exp(logit_mat))
}
