#' Generate responses to a test
#'
#' Given a Q-matrix and a sample of respondents this function returns a simulated
#' response matrix. Items parameter will automatically be mapped to the right attribute.
#'
#' @param qmat Q-matrix
#' @param respondents Binary matrix of respondents
#' @param items matrix of items-parameters
#' @param get_probs if you want the probability of correct response matrix.
#'
#' @returns a matrix of responses in the form 0/1 or the real probabilities.
#' @export
#'
generate_responses <- function(qmat, respondents, items, get_probs=FALSE) {
  probs <- .get_prob_matrix(qmat, respondents, items)

  if (get_probs) {
    return(probs)
  } else {
    mask <- runif(prod(dim(probs)))
    return((probs > mask)*1)
  }

}

#' Respondents-items matrix
#'
#' @param qmat Q-matrix
#' @param respondents Binary matrix of respondents
#' @param items dataframe of items
#'
#' @returns a matrix of responses
#'
.get_prob_matrix <- function(qmat, respondents, items) {
  parameter_item_matrix <- .get_parameter_item_matrix(qmat, items)
  examinees_parameter_matrix <- .get_examinees_parameter_matrix(respondents)
  apply(
    examinees_parameter_matrix %*% parameter_item_matrix,
    c(1,2),
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
#'
.get_parameter_item_matrix <- function(qmat, items) {
  mask <- apply(qmat, 1, .parameter_mask)
  n_items <- ncol(mask)
  n_rows  <- nrow(mask)

  # make sure item_params is a matrix
  item_params <- as.matrix(items)
  if (nrow(items) != n_items) {
    stop("Number of rows in item_params must match number of items (columns in qmat).")
  }

  result <- matrix(0, nrow = n_rows, ncol = n_items)

  for (i in seq_len(n_items)) {
    vec <- numeric(n_rows)
    ones <- which(mask[, i] == 1)
    n_params <- length(ones)
    # assign parameters in order to the positions of 1s
    vec[ones] <- items[i, seq_len(n_params)]
    result[, i] <- vec
  }

  result
}


#' Parameter mask
#'
#' @param attr_vector binary vector showing presence of attribute
#' @param num_attrs number of attributes if longer than the passed vector
#' @param add_intercept if the intercept is added at the start of the result
#'
#' @returns A binary vector with all the parameter that the item should include
#'
.parameter_mask <- function(attr_vector, num_attrs=NULL, add_intercept=T) {
  if (is.null(num_attrs)) {num_attrs <- length(attr_vector)}
  attrs <- which(attr_vector == 1) # convert (1,0,1) to (1,3)
  mask <- (apply(.parameter_attr_matrix(num_attrs)[,attrs, drop=F], 1, sum)==1)*1
  if (add_intercept) {
    return(c(1, mask))
  } else {
    return(mask)
  }
}

#' items parameter - attribute matrix
#'
#' @param num_att Number of attributes
#'
#' @returns a matrix
#'
.parameter_attr_matrix <- function(num_att) {
  I <- diag(num_att)
  attr <- split(I, row(I))

  link_matrix <- I

  if (num_att == 1) {
    return(link_matrix)
  }

  for (i in 2:num_att) {
    parameter <- combn(attr, i, FUN = function(x) Reduce(`+`, x), simplify = T)
    new_rows <- t(parameter)/i
    link_matrix <- rbind(link_matrix, new_rows)
  }
  return(link_matrix)
}


.logistic_fn <- function(parameters) {
  logit <- sum(parameters)
  exp(logit)/(1+exp(logit))
}


