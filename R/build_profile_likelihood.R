#' Build profiles likelihood and posterior probabilities for a given response.
#'
#' This function returns the likelihood of a profile given a response:
#'  \eqn{P(\bold{\alpha_l}\mid\bold{x})}.
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`.
#' @param response numeric vector corresponding to the response pattern.
#' @param profiles Vector or matrix with the profiles.
#' @param profile_prior the priors of the passed profiles
#' @param priors profiles prior distribution.
#'
#' @returns A matrix with the profiles, the priors, the likelihood and the
#'  posterior
#' @export
#'
build_profile_likelihood <- function(
  test_parameters,
  profiles,
  response,
  profile_priors,
  priors,
  complete = TRUE
) {
  if (sum(priors) != 1) {
    # TODO: fix the way this error is being thrown.
    stop("priors should add to 1.")
  }

  # If profiles is a single vector, convert to single row matrix
  if (is.vector(profiles)) {
    profiles <- t(profiles)
  }

  # TODO: change the arguments so the prior is taken from the respective
  # profile position.

  # This makes the loop once per profile
  response_likelihoods <- apply(
    profiles,
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


  marginal_response_likelihood <- build_marginal_response_likelihood(
    test_parameters,
    response,
    priors,
    complete = FALSE
  ) |> as.vector()

  profile_likelihood <-
    (response_likelihoods * profile_priors) / marginal_response_likelihood


  # Return
  result <- as.matrix(profile_likelihood)

  # We add the responses if needed.
  if (complete == TRUE) {
    result <- cbind(profiles, result)
  }

  # Returning result
  result
}
