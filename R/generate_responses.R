#' Generate responses to a test
#'
#' Given a Q-matrix and a sample of respondents this function returns a simulated
#' response matrix. Items parameter will automatically be mapped to the right attribute.
#'
#' @param qmat Q-matrix
#' @param respondents Binary matrix of respondents
#' @param items matrix of items-parameters
#' @param get_probs if you want the probability of correct response matrix.
#' @param id simulation id, to be used as a seed within this function.
#'
#' @returns a matrix of responses in the form 0/1 or the real probabilities.
#' @export
#'
generate_responses <- function(
  qmat,
  respondents,
  items,
  get_probs = FALSE,
  id = NULL
) {
  seed <- .seed_from_id(id)
  if (!is.null(seed)) {
    withr::with_seed(
      seed,
      .generate_responses(
        qmat,
        respondents,
        items,
        get_probs
      )
    )
  } else {
    .generate_responses(
      qmat,
      respondents,
      items,
      get_probs
    )
  }
}


#' Internal - Generate responses to a test
#'
#' Note: Not encapsulated for randomness.
#' Given a Q-matrix and a sample of respondents this function returns a simulated
#' response matrix. Items parameter will automatically be mapped to the right attribute.
#'
#' @param qmat Q-matrix
#' @param respondents Binary matrix of respondents
#' @param items matrix of items-parameters
#' @param get_probs if you want the probability of correct response matrix.
#'
#' @returns a matrix of responses in the form 0/1 or the real probabilities.
#'
.generate_responses <- function(
  qmat,
  respondents,
  items,
  get_probs = FALSE
) {
  probs <- .get_prob_matrix(qmat, respondents, items)
  if (get_probs) {
    return(probs)
  } else {
    mask <- stats::runif(prod(dim(probs)))
    return((probs > mask) * 1)
  }
}

#' Respondents-items matrix
#'
#' @param qmat Q-matrix
#' @param respondents Binary matrix of respondents
#' @param items dataframe of items
#'
#' @returns a matrix of responses
#' @noRd
.get_prob_matrix <- function(qmat, respondents, items) {
  parameter_item_matrix <- .get_parameter_item_matrix(qmat, items)
  examinees_parameter_matrix <- .get_examinees_parameter_matrix(respondents)
  apply(
    examinees_parameter_matrix %*% parameter_item_matrix,
    c(1, 2),
    .logistic_fn
  )
}


# Examinees-attribute matrix
.get_examinees_parameter_matrix <- function(examinees) {
  t(apply(examinees, 1, .parameter_mask))
}


#' Qmatrix to parameters-items matrix
#'
#' @param qmat Qmatrix
#' @param items matrix of items-parameters
#'
#' @returns parameter-item matrix
#' @noRd
.get_parameter_item_matrix <- function(qmat, items) {
  param_attr_map_matrix <- t(apply(qmat, 1, .parameter_mask))

  result <- t(.parameter_mapping(param_attr_map_matrix, items))

  return(result)
}


#' maps a matrix of values to a binary matrix (attr_map) rowwise.
#'
#' @details
#' The 0s in the value matrix will be mapped, the NAs will be omited.
#' This allows for items with different amounts of parameters.
#'
#' @param values matrix of values
#' @param attr_map map matrix
#'
#' @returns a matrix of the same shape that attr_map
#' @noRd
.parameter_mapping <- function(attr_map, values) {
  result <- attr_map * 0

  for (i in 1:nrow(attr_map)) {
    # extract row
    values_to_map <- values[i, ]

    # drop NA values
    values_to_map <- stats::na.omit(values_to_map)

    # ensure it is a plain vector
    values_to_map <- as.vector(values_to_map)


    # We map the non-NA values to the attr_map row.
    result[i, attr_map[i, ] == 1] <- values_to_map
  }

  return(result)
}
