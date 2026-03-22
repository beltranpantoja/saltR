#' Generate sample of examinees
#'
#' @param sample_size How many examinees to generate
#' @param total_attrs How many attributes are in the sample
#' @param base_rate Base rate per attribute (or one if it's the same for all)
#' @param attr_corr Correlation of attributes (or one if it's the same for all)
#' @param binary_correlation Boolean. If true, the function tries to generate a
#'  binary matrix with the given correlation. If it is not possible, then it
#'  raises and error. If false it is considered to be the correlation matrix of
#'  the underlying normal distributions.
#' @param tolerance When binary_correlation=FALSE, the actual correlation values
#'  can be far from the expected values. This gives a tolerance to avoid
#'  unexpected results.
#' @param attributes.names vector of names for the attributes. Defaults to
#'  "Attr#"
#' @param responses.names vector of names for the responses. Defaults to "ID#"
#'
#' @returns A matrix of respondents and attributes.
#' @export
#'
generate_examinees <- function(
  sample_size,
  total_attrs,
  base_rate = .5,
  attr_corr = 0,
  binary_correlation = TRUE,
  tolerance = .1,
  attributes_names = NULL,
  responses_names = NULL
) {
  # If base_rate is only one value then it is assumed they are all the same
  marginal_prob <- .extend_vector(base_rate, total_attrs)

  # If attr_corr is only one value then it is assumed they are all the same
  corr_length <- total_attrs * (total_attrs - 1) / 2
  attr_corr <- .extend_vector(attr_corr, corr_length)

  # Covariance matrix
  R <- matrix(1, total_attrs, total_attrs)
  R[lower.tri(R)] <- attr_corr
  R[upper.tri(R)] <- t(R)[upper.tri(R)]

  # This is what bindata calls commonprob
  joint_prob <- bindata::bincorr2commonprob(
    margprob = marginal_prob,
    bincorr = R
  )

  # If binary_correlation is True we need to check that we can create data
  # That actually follows the constraints of correlation
  if (binary_correlation) {
    error_messages <- .check_joint_probability_error(marginal_prob, joint_prob)
    # If we find that the probabilities don't conform to expected values we
    # show which combinations generate the problem and stop.
    if (length(error_messages) > 0) {
      stop(paste(error_messages, collapse = "\n"))
    }
  }


  # Now we call rmvbin using commonprob
  # We can still get errors though.
  sample <- bindata::rmvbin(
    sample_size,
    margprob = marginal_prob,
    commonprob = joint_prob
  )

  # We check the correlation that the sample actually got
  empirical_corr <- cor(sample)[lower.tri(cor(sample))]
  over_tolerance <- sum(abs(attr_corr - empirical_corr) > tolerance)

  if (over_tolerance > 0) {
    warning(sprintf(
      paste(
        "%d correlation values are outside tolerance range",
        "Expected: %s",
        "Obtained: %s",
        sep = "\n"
      ),
      over_tolerance,
      paste(round(attr_corr, 3), collapse = ", "),
      paste(round(empirical_corr, 3), collapse = ", ")
    ))
  }


  # Giving names to the sample
  if (is.null(attributes.names)) {
    attributes.names <- paste0("Attr", 1:total_attrs)
  }
  if (is.null(responses.names)) {
    responses.names <- paste0("ID", 1:sample_size)
  }

  colnames(sample) <- attributes.names
  rownames(sample) <- responses.names

  sample
}

# ============================================================
# Utility functions
# ============================================================

.extend_vector <- function(value, size) {
  if (length(value) %in% c(1, size)) {
    rep_len(value, size)
  } else {
    stop(sprintf(
      "Value should have length %d or 1, not %d.",
      size, length(value)
    ))
  }
}


# We check if the values follow the Fréchet bounds
.check_joint_probability_error <- function(marginal_prob, joint_prob) {
  prob_upper_limit <- outer(
    marginal_prob,
    marginal_prob,
    FUN = \(x, y) pmin(x, y)
  )

  prob_lower_limit <- pmax(
    outer(
      marginal_prob,
      marginal_prob,
      "+"
    ) - 1,
    0
  )

  invalid <- (
    (joint_prob > prob_upper_limit) |
      (joint_prob < prob_lower_limit)) &
    upper.tri(joint_prob) # We dont want to double count

  messages <- character(0)

  if (any(invalid)) {
    idx <- which(invalid, arr.ind = TRUE)
    messages <- vapply(idx, function(k) {
      i <- k[1]
      j <- k[2]

      val <- joint_prob[i, j]
      ul <- prob_upper_limit[i, j]
      ll <- prob_lower_limit[i, j]

      sprintf(
        paste(
          "Invalid joint probability of vars %d and %d.",
          "Allowed range: [%.2f, %.2f], but got: %.2f",
          sep = " "
        ),
        i, j, ll, ul, val
      )
    }, FUN.VALUE = character(1))
  }

  messages
}
