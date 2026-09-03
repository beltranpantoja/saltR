#' Generate sample of examinees
#'
#' @rdname generate_family
#'
#' @param sample_size,total_attrs Number of examinees and attributes to generate
#' @inheritDotParams generate_examinees_MVN
#' @inheritDotParams generate_examinees_HO
#' @returns A matrix of respondents and attributes.
#' @export
#'
generate_examinees <- function(
  sample_size,
  total_attrs,
  method = c("MVN", "HO"),
  ...
) {
  method <- match.arg(method)

  if (method == "MVN") {
    return(generate_examinees_MVN(sample_size, total_attrs, ...))
  } else if (method == "HO") {
    return(generate_examinees_HO(sample_size, total_attrs, ...))
  }

  examinees <- switch(method,
    "MVN"       = generate_examinees_MVN(sample_size, total_attrs, ...),
    "HO-GDINA"  = generate_examinees_HOGDINA(sample_size, total_attrs, ...)
  )

  # Giving names to the sample
  if (is.null(attributes_names)) {
    attributes_names <- paste0("Attr", seq_len(total_attrs))
  }
  if (is.null(responses_names)) {
    responses_names <- paste0("ID", seq_len(sample_size))
  }

  # Labelling
  examinees <- label_matrix(examinees, col_prefix = "A", row_prefix = "ID", )


  # Return
  examinees
}

# ============================================================
# generate_examinees_HO
# ============================================================

#' Generate examinees using a higher order structure
#'
#' @param intercepts A numeric vector of length \code{total_attrs}. The
#'  intercept parameters for each attribute, capturing the baseline probability
#'  or facility of mastering that attribute when the general ability
#'  (\code{thetas}) is zero.
#' @param slopes A numeric vector of length \code{total_attrs}. The slope
#'  (or discrimination) parameters for each attribute, indicating how strongly
#'  the higher-order latent ability predicts mastery of each specific attribute.
#' @param thetas A numeric vector of length \code{sample_size}, representing
#'  the higher-order general latent trait (ability) level for each individual.
#'  If \code{NULL} (the default), individual values are automatically sampled
#'  from a standard normal distribution: \eqn{\theta \sim N(0, 1)}.
#'
#' @return A matrix of dimensions \code{sample_size} by \code{total_attrs}
#'  filled with binary values (\code{0} or \code{1}), where \code{1} indicates
#'  mastery of an attribute and \code{0} indicates non-mastery.
#'
#' @keywords internal
generate_examinees_HO <- function(
  sample_size,
  total_attrs,
  intercepts,
  slopes,
  thetas = NULL
) {
  if (is.null(thetas)) {
    thetas <- rnorm(sample_size)
  }
  logit_matrix <-
    outer(theta, slopes, "*") +
    matrix(intercepts, nrow = sample_size, ncol = total_attrs, byrow = TRUE)

  examinees_prob <- exp(logit_matrix) / (1 + exp(logit_matrix))


  examinees <- (examinees_prob > runif(N * total_attrs)) * 1

  # Return
  examinees
}

# ============================================================
# generate_examinees.MVN
# ============================================================

#' Generate examinees using a Multivariate Normal distribution
#'
#' @param base_rate Ratio of examinees that master each attribute. If only one
#'  number is supplied then it's assumed is the same for all.
#' @param attr_corr Correlation of attributes. The order of the correlations is
#'  assumed to be 1-2, 1-3, ...., 2-3, 2-4, and so on.
#'
#' @keywords internal
generate_examinees_MVN <- function(
  sample_size,
  total_attrs,
  base_rate,
  attr_corr
) {
  # If base_rate is only one value then it is assumed they are all the same
  marginal_prob <- .extend_vector(base_rate, total_attrs)

  # If attr_corr is only one value then it is assumed they are all the same
  corr_length <- total_attrs * (total_attrs - 1) / 2
  attr_corr <- .extend_vector(attr_corr, corr_length)


  # Building the covariance matrix for the MVN
  sigma_mat <- matrix(1, total_attrs, total_attrs)
  sigma_mat[lower.tri(sigma_mat)] <- attr_corr
  sigma_mat[upper.tri(sigma_mat)] <- t(sigma_mat)[upper.tri(sigma_mat)]

  # Creating the sample
  sample <- bindata::rmvbin(
    sample_size,
    margprob = marginal_prob,
    sigma = sigma_mat
  )

  # Return
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
