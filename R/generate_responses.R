#' Generate responses to a test
#'
#' Given a Q-matrix and a sample of respondents this function returns a
#'  simulated
#' response matrix. Items parameter will automatically be mapped to the right
#'  attribute.
#'
#' @param examinees Binary matrix of respondents
#' @param test matrix of items parameters
#' @param get_probs if you want the probability of correct response matrix.
#'
#' @returns a matrix of responses in the form 0/1 or the real probabilities.
#' @export
#'
generate_responses <- function(
  examinees,
  test,
  get_probs = FALSE
) {
  probs <- .get_prob_matrix(examinees, test)
  if (get_probs) {
    return(probs)
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


.get_attr_mask_from_profile <- function(profiles) {
  n_attr <- ncol(profiles)

  # 1. Pre-generate the labels for the columns
  col_labels <- "0"
  for (i in 1:n_attr) {
    combs <- combn(n_attr, i)
    # Create labels like "1", "2", "1-2", etc.
    level_labels <- apply(combs, 2, function(idx) paste(idx, collapse = "-"))
    col_labels <- c(col_labels, level_labels)
  }

  # 2. Run the logic to fill the data
  profile_mat <- apply(profiles, MARGIN = 1, FUN = function(attr_vec) {
    result <- c(1) # Intercept
    for (i in 1:n_attr) {
      combs <- combn(n_attr, i)
      level_bits <- apply(combs, 2, function(idx) {
        if (all(attr_vec[idx] == 1)) 1 else 0
      })
      result <- c(result, level_bits)
    }

    # Return
    result
  })

  # We transpose it back and set the names
  final_mat <- t(profile_mat)
  colnames(final_mat) <- col_labels

  # Return
  final_mat
}
