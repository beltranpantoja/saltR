#' Create the likelihood matrix of the given response patterns.
#'
#' @details
#' This function returns the likelihood of a given response given a skill profile:
#'  \eqn{P(\bold{x}\mid\bold{\alpha_l})}. If more than one profile is passed,
#'  then it returns the sum of the likelihoods:
#'
#'  \deqn{
#'    \sum^L_{l=1} P(\bold{x} \mid \bold{\alpha}_l)
#'    }
#'
#'  If more than one response pattern is passed, then the result is a matrix
#'    where each row contains the response pattern and it's respective
#'    likelihood.
#'
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`.
#' @param responses numeric vector or matrix where each row corresponds to a
#'  response pattern.
#' @param profiles Vector or matrix with the profiles.
#' @param complete Should the result include the response patterns?
#'  default `TRUE`.
#' @returns a matrix
#'
#' @export
build_response_likelihood <- function(
  test_parameters,
  responses,
  profiles,
  complete = TRUE
) {
  test_parameters <- as_test_parameters(test_parameters)

  # If profiles is a single vector, convert to single row matrix
  if (is.vector(profiles)) {
    profiles <- t(profiles)
  }

  if (is.vector(responses)) {
    responses <- t(responses)
  }

  # We construct the likelihoods for each response as a matrix.
  likelihoods <- apply(
    responses,
    MARGIN = 1,
    FUN = function(response) {
      single_response_likelihood(
        test_parameters,
        response,
        profiles
      )
    },
    simplify = TRUE
  )

  result <- as.matrix(likelihoods)

  # We add the responses if needed.
  if (complete == TRUE) {
    result <- cbind(responses, result)
  }

  # Returning result
  result
}

#' Builds the marginal response likelihood of the given response patterns
#'
#' This function returns the likelihood of a given response marginal on all
#'  possible profiles.
#'
#'  \deqn{
#'    P(\bold{x}) =
#'      \sum^L_{l=1} P(\bold{x} \mid \bold{\alpha}_l) P(\bold{\alpha}_l)
#'    }
#'
#'  If more than one response pattern is passed, then the result is a matrix
#'    where each row contains the response pattern and it's respective
#'    likelihood.
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`.
#' @param responses numeric vector or matrix where each row corresponds to a
#'  response pattern.
#' @param priors a vector indicating the priors for each profile. Use
#'  [create_patterns()] to see the order in which you should provide the priors.
#' @param complete Should the result include the response patterns?
#'  default `TRUE`.
#' @returns a matrix
#'
build_marginal_response_likelihood <- function(
  test_parameters,
  responses,
  priors,
  complete = TRUE
) {
  # TODO: this can be cleaned up by vectorizing.

  if (sum(priors) != 1) {
    # TODO: fix the way this error is being thrown.
    stop("priors should add to 1.")
  }

  # Format responses as a matrix
  if (is.vector(responses)) {
    responses <- t(responses)
  }

  # We generate all profiles
  qmat <- build_qmatrix(test_parameters)
  full_profiles <- create_patterns(
    num_vars = ncol(qmat)
  )


  response_likelihoods <- apply(
    responses,
    MARGIN = 1,
    FUN = function(response) {
      # This makes the loop once per profile
      likelihoods <- apply(
        full_profiles,
        MARGIN = 1,
        FUN = function(profile) {
          build_response_likelihood(
            test_parameters = test_parameters,
            responses = response,
            profiles = profile,
            complete = FALSE
          )
        },
        simplify = TRUE
      )

      # Return
      return(likelihoods %*% priors)
    },
    simplify = TRUE
  )


  result <- as.matrix(response_likelihoods)

  # We add the responses if needed.
  if (complete == TRUE) {
    result <- cbind(responses, result)
  }

  # Returning result
  result
}


#' Build response likelihood for a single response
#'
#' This function should not be used directly. Prefer
#'  [build_response_likelihood()]
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`.
#' @param response numeric vector corresponding to the response pattern.
#' @param profiles numeric vector corresponding to the assumed profile.
#' @returns a matrix
#'
#' @keywords internal
#' @noRd
single_response_likelihood <- function(
  test_parameters,
  response,
  profiles
) {
  likelihoods <- apply(
    profiles,
    MARGIN = 1,
    FUN = function(profile) {
      prob_correct <- generate_responses(
        t(profile),
        test_parameters,
        get_probs = TRUE
      )

      # First column is having it wrong, second is right
      item_probs <- t(rbind(1 - prob_correct, prob_correct))

      # Matrix indexing for the probabilities of the observed responses
      idx <- cbind(seq_along(response), response + 1)

      # We return the likelihood
      prod(item_probs[idx])
    },
    simplify = TRUE
  )

  # We return the likelihood
  sum(likelihoods)
}
