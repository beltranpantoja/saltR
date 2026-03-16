#' Returns a matrix of profiles and the probability of a correct response for all items.
#'
#' @param model A GDINA object
#'
#' @returns a matrix of profiles and the probability of a correct response for all items.
#' @export
#'
get_probabilities_by_profile <- function(model) {
  # We extract some utility variables
  qmat <- model$q.matrix
  probs <- model$probitem
  num_attrs <- ncol(qmat)

  # We get the model's attributes names in order
  attr.labels <- colnames(model$attribute.patt.splitted)

  # I construct all profiles
  profiles <- create_patterns(ncol(qmat), column_prefix = "Attr")

  items_prob <- matrix(NA, ncol = nrow(qmat), nrow = 2^ncol(qmat))

  colnames(items_prob) <- paste0("V", 1:nrow(qmat)) # TODO: the name should come from the model

  # This is the matrix we will fill
  profile_probs_mat <- cbind(
    profiles,
    items_prob
  )


  for (i in seq_len(nrow(probs))) {
    # We clean the attributes combination of the prob.
    prob_attr <- as.numeric(strsplit(sub("^A", "", probs$skillcomb[i]), "")[[1]])

    # We fetch the relevant attributes position from the qmatrix
    itemno <- probs$itemno[i]
    req_attrs <- which(qmat[itemno, ] == 1)

    # We iterate over the rows to see which profile matches
    matching_rows <- apply(
      profiles[, req_attrs, drop = FALSE],
      MARGIN = 1,
      FUN = \(row) all(row == prob_attr)
    )

    # We update the matching profiles with the probability
    profile_probs_mat[matching_rows, itemno + num_attrs] <- probs$prob[i]
  }

  return(profile_probs_mat)
}
