#' Build profiles likelihood and posterior probabilities for a given response.
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`. If the test is extracted from a model
#'  using `get_test_parameters` it should be extracted using the parameter
#'  `complete=TRUE`.
#' @param response numeric vector corresponding to the response pattern.
#' @param priors profiles prior distribution. If NULL it will be assumed to be
#'  uniformly distributed.
#'
#' @returns A matrix with the profiles, the priors, the likelihood and the
#'  posterior
#' @export
#'
build_profile_likelihood <- function(
  test_parameters,
  response,
  priors = NULL
) {
  # Get the item labels
  item_labels <-
    rownames(test_parameters) %||% paste0("I", seq_len(nrow(test_parameter)))


  num_attributes <- log(ncol(test_parameters), 2)

  full_profiles <- create_patterns(
    num_vars = num_attributes,
    column_labels = item_labels
  )

  # We get the index of the response
  idx <- .get_matching_rows_index(
    create_patterns(num_vars = nrow(test_parameters)),
    response
  )

  # Iterate over the profiles to get the likelihood P(x|a)
  profile_likelihood <- apply(
    full_profiles,
    simplify = TRUE,
    MARGIN = 1,
    FUN = function(profile) {
      response_likelihood <- build_response_likelihood(test_parameters, profile)

      # We return the likelihood of the relevant response
      response_likelihood[idx, "response_likelihood"]
    }
  )


  # Priors: P(a)
  # Default priors: uniform distribution
  if (is.null(priors)) {
    priors <- rep(
      1 / nrow(full_profiles),
      nrow(full_profiles)
    )
  }

  # Validation
  if (!is.numeric(priors)) {
    stop("priors must be a numeric vector.")
  }

  if (length(priors) != nrow(full_profiles)) {
    stop(
      sprintf(
        "priors should have length %d.",
        nrow(full_profiles)
      )
    )
  }

  if (any(priors < 0)) {
    stop("priors cannot contain negative values.")
  }

  if (abs(sum(priors) - 1) > 1e-10) {
    stop("priors should sum to 1.")
  }

  # P(x | a) P(a)
  posterior_numerator <- profile_likelihood * priors

  # P(x)
  response_prob <- sum(posterior_numerator)

  # P(a | x)
  posterior <- posterior_numerator / response_prob

  # Output
  cbind(
    full_profiles,
    prior = priors,
    likelihood = profile_likelihood,
    posterior = posterior
  )
}
