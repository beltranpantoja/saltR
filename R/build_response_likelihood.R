#' Create the likelihood matrix of all possible responses for a set of profiles
#'
#' @details
#' This function returns the likelihood of a given response given a profile:
#'  \eqn{P(\bold{x}\mid\bold{\alpha})}. If more than one profile is passed,
#'  then it returns the probability of the responses conditional on the union of
#'  the profiles:
#'  \deqn{
#'    P(\bold{x} \mid \bigcup_{i}^{k} \bold{\alpha}_i)
#'      = \dfrac{
#'        \sum_{i}^{k} P(\bold{x}\mid\bold{\alpha}_i) P(\bold{\alpha}_i)
#'      }{
#'        \sum_{i}^{k} P(\bold{\alpha}_i)
#'      }
#'    }
#'  If no profiles are passed then it's assumed it's marginal on all profiles,
#'  which is equivalent to \eqn{P(\bold{x})}.
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`.
#' @param profiles Vector or matrix with the assumed profiles. If `NULL`, then
#'  it assumes all possible profiles which makes the result be the likelihood
#'  of the response marginal on all attributes (i.e. \eqn{P(x)}{P(x)}).
#' @param priors The priors are the values of \eqn{P(\bold{\alpha}_i)}. Used when
#'  `profiles` is a matrix. By default assumes all are equally likely.
#' @returns a matrix
#'
#' @export
build_response_likelihood <- function(
  test_parameters,
  profiles = NULL,
  priors = NULL
) {
  test_parameters <- as_test_parameters(test_parameters)

  # If profiles is NULL assume all profiles
  if (is.null(profiles)) {
    profiles <- create_patterns(
      log2(ncol(test_parameters))
    )
  }

  # If profiles is a single vector, convert to single row matrix
  if (is.vector(profiles)) {
    profiles <- t(profiles)
  }

  # If no priors are passed then it's assumed they are all the same
  if (is.null(priors)) {
    priors <- rep(
      1 / nrow(profiles),
      nrow(profiles)
    )
  }

  # TODO: add check of priors

  # We construct the likelihoods for each profile
  likelihoods <- apply(
    profiles,
    MARGIN = 1,
    FUN = function(profile) {
      single_profile_build_response_likelihood(
        test_parameters,
        profile
      )[, "response_likelihood"]
    },
    simplify = TRUE
  )

  # We apply the priors and combine
  likelihoods <- likelihoods %*% priors

  # We get the response patterns
  full_response_patterns <- single_profile_build_response_likelihood(
    test_parameters,
    profiles[1, ]
  )[, seq_len(nrow(test_parameters))]

  # Returning the complete matrix
  cbind(full_response_patterns, response_likelihood = as.vector(likelihoods))
}


#' Build response likelihood for single profile
#'
#' This function should not be used directly. Prefer
#'  [build_response_likelihood()]
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`.
#' @param profile numeric vector corresponding to the assumed profile.
#' @returns a matrix
#'
#' @keywords internal
#' @noRd
single_profile_build_response_likelihood <- function(
  test_parameters,
  profile
) {
  prob_correct <- generate_responses(
    t(profile),
    test_parameters,
    get_probs = TRUE
  )

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
