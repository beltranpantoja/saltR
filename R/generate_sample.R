#' Generate sample of respondents
#'
#' @param sample_size How many respondents
#' @param total_attrs How many attributes are in the sample
#' @param base_rate Base rate per attribute (or one if it's the same for all)
#' @param attr_corr Correlation of attributes (or one if it's the same for all)
#' @param attributes.names vector of names for the attributes. Defaults to "Attr#"
#' @param responses.names vector of names for the responses. Defaults to "#"
#' @param id simulation id, to be used as a seed within this function.
#'
#' @returns A matrix of respondents and attributes.
#' @export
#'
generate_sample <- function(
  sample_size,
  total_attrs,
  base_rate = .5,
  attr_corr = .5,
  attributes.names = NULL,
  responses.names = NULL,
  id = NULL
) {
  seed <- saltr:::.seed_from_id(id)
  if (!is.null(seed)) {
    withr:::with_seed(
      seed,
      saltr:::.generate_sample(
        sample_size,
        total_attrs,
        base_rate,
        attr_corr,
        attributes.names,
        responses.names
      )
    )
  } else {
    saltr:::.generate_sample(
      sample_size,
      total_attrs,
      base_rate,
      attr_corr,
      attributes.names,
      responses.names
    )
  }
}


#' Internal - Generate sample of respondents
#' Note: Not encapsulated for randomness.
#'
#' @param sample_size How many respondents
#' @param total_attrs How many attributes are in the sample
#' @param base_rate Base rate per attribute (or one if it's the same for all)
#' @param attr_corr Correlation of attributes (or one if it's the same for all)
#' @param attributes.names vector of names for the attributes. Defaults to "Attr#"
#' @param responses.names vector of names for the responses. Defaults to "#"
#'
#' @returns A matrix of respondents and attributes.
#'
.generate_sample <- function(
  sample_size,
  total_attrs,
  base_rate = .5,
  attr_corr = .5,
  attributes.names = NULL,
  responses.names = NULL
) {
  marginal_prob <- .extend_vector(base_rate, total_attrs)

  num_pairs <- total_attrs * (total_attrs - 1) / 2
  bin_corr <- .extend_vector(attr_corr, num_pairs)

  R <- matrix(1, total_attrs, total_attrs)
  R[lower.tri(R)] <- bin_corr
  R[upper.tri(R)] <- t(R)[upper.tri(R)]


  sample <- bindata::rmvbin(
    sample_size,
    margprob = marginal_prob,
    bincorr = R
  )


  # Giving names to the qmatrix rows and columns
  if (is.null(attributes.names)) {
    attributes.names <- paste0("Attr", 1:total_attrs)
  }
  if (is.null(responses.names)) {
    responses.names <- 1:sample_size
  }

  colnames(sample) <- attributes.names
  rownames(sample) <- responses.names

  return(sample)
}


.extend_vector <- function(value, size) {
  if (length(value) == 1) {
    return(rep(value, size))
  } else if (length(value) == size) {
    return(value)
  } else {
    stop(paste0(
      "Value should have length ", size, " or 1, not ", length(value), "."
    ))
  }
}
