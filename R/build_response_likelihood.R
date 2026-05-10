#' Create the likelihood matrix of all possible response given a profile.
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`. If the test is extracted from a model
#'  using `get_test_parameters` it should be extracted using the parameter
#'  `complete=TRUE`.
#' @param profile numeric vector corresponding to the assumed profile.
#' @returns a matrix
#'
#' @export
#'
build_response_likelihood <- function(
  test_parameters,
  profile
) {
  prob_correct <- generate_responses(t(profile), test_parameters, get_probs = TRUE)

  # First column is having it wrong, second is right
  item_probs <- t(rbind(1 - prob_correct, prob_correct))

  # We construct the matrix for all patterns so the likelihood is correct.
  N <- nrow(test_parameters)

  full_response_patterns <- create_patterns(
    num_vars = N,
    column_labels = rownames(test_parameters)
  )


  # Likelihood of response conditional on mastery P(x|a)
  response_likelihood <- apply(
    full_response_patterns,
    simplify = TRUE,
    MARGIN = 1,
    FUN = function(response) {
      probs <- item_probs[cbind(seq_len(N), response + 1)]
      prob_response <- exp(sum(log(probs))) # Avoid overflows
      # Return
      prob_response
    }
  )


  # Returning the complete matrix
  cbind(full_response_patterns, response_likelihood)
}
