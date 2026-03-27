#' Returns a matrix of profiles and the probability of a correct response for
#'  all items.
#'
#' It works by extraxcting the test parameters with `get_test_parameters` and
#'  then passing that to the `generate_responses(get_probs=TRUE)` with all
#'  possible mastery patterns to get the expected probabilities.
#'
#' @param model A GDINA object
#' @param marginal_attr returns the probabilities marginal on an attribute.
#'  It does not consider distribution, is just a mean of the probabiities.
#' @param which_items vector of items to be returnde. If Null it returns all.
#'
#' @returns a matrix of profiles and the probability of a correct response for
#'  all items.
#' @export
#'
get_probabilities_by_profile <- function(
  model,
  marginal_attr = NULL,
  which_items = NULL
) {
  # TODO: implement Throw warning if model does not have monotonicity activated

  test <- get_test_parameters(model, pretty_print = FALSE)
  qmatrix <- model$q.matrix
  num_attr <- ncol(qmatrix)

  possible_profiles <- create_patterns(
    num_attr,
    column_labels = colnames(qmatrix)
  )

  probs <- generate_responses(
    examinees = possible_profiles,
    test = test,
    get_probs = TRUE
  )

  probs_profile <- cbind(possible_profiles, probs)

  if (!is.null(which_items)) {
    # The item slicing needs to be offset to consider for the attr
    slice_indices <- c(seq_len(num_attr), (which_items + num_attr))
    probs_profile <- probs_profile[, slice_indices]
  }


  # We take the probability marginal on an attribute
  if (!is.null(marginal_attr)) {
    probs_profile <- aggregate(
      probs_profile[, -seq_len(num_attr)],
      by = list(probs_profile[, marginal_attr]),
      FUN = mean
    )
    # Labelling using the attribute name
    names(probs_profile)[1] <- colnames(qmatrix)[marginal_attr] # Returning
  }


  probs_profile
}
