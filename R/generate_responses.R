#' Generate responses to a test
#'
#' @param examinees Binary matrix of respondents
#' @param test matrix of items parameters
#' @param get_probs if you want the probability of correct response matrix.
#' @param method What method to use for the generation of responses.
#' @param FUN If `method="UDF"`, then the function will use a user provided
#'  function for response generation.
#'
#' @returns a matrix of responses in the form 0/1 or the real probabilities.
#' @export
#'
#'
generate_responses <- function(
  examinees,
  test,
  get_probs = FALSE,
  method = c("DCM", "IRT", "MIRT", "UDF"),
  FUN = NULL,
  ...
) {
  # Check method
  method <- match.arg(method)

  if (method == "UDF" && !is.function(FUN)) {
    saltr_emit(
      "When method = 'UDF', the 'FUN' argument must be a valid function.",
    )
  }


  probs_fun <- switch(method,
    "DCM" = DCM_prob_fn,
    "IRT" = IRT_prob_fn,
    "MIRT" = MIRT_prob_fn,
    "UDF" = function(examinees, test, ...) {
      # Sanitize inputs
      examinees <- as.matrix(examinees)
      test <- as.matrix(test)

      # We wrap the passed function in index-wise vectorized function
      index_FUN <- Vectorize(
        function(i, j) {
          FUN(examinees[i, ], test[j, ], ...)
        }
      )

      # Outer will make it so we iterate over all possible indices.
      outer(
        seq_len(nrow(examinees)),
        seq_len(nrow(test)),
        index_FUN
      )
    }
  )

  probs <- probs_fun(examinees, test, ...)

  if (get_probs) {
    return(probs)
  }

  mask <- stats::runif(length(probs))
  responses <- ((probs > mask) * 1)

  # Return
  responses
}

# ========================================================
# Internal functions
# ========================================================

#' Probability Functions for Response Generation
#'
#' These internal functions calculate the item response probability matrices
#' for different psychometric frameworks.
#'
#' @param examinees A matrix of respondent traits or abilities.
#' @param test A matrix of item parameters.
#'
#' @return A matrix of probabilities where rows correspond to examinees
#'   and columns correspond to items.
#' @keywords internal
#' @name probability_functions
NULL


#' @rdname probability_functions
DCM_prob_fn <- function(examinees, test) {
  # First we check that the passed test is valid
  check_test_parameters(test)
  mask <- .get_attr_mask_from_profile(examinees)

  # Make the NAs be 0 for the matrix multiplication
  test[is.na(test)] <- 0

  # This product gives us the logit matrix respondents by items
  logit_mat <- mask %*% t(test)

  # Return the probs
  exp(logit_mat) / (1 + exp(logit_mat))
}


#' @rdname probability_functions
IRT_prob_fn <- function(examinees, test) {
  # Sanitize inputs
  examinees <- as.matrix(examinees)
  test <- as.matrix(test)

  beta <- test[, 1]
  alpha <- if (ncol(test) >= 2) test[, 2] else 1
  c <- if (ncol(test) >= 3) test[, 3] else 0

  # Check arguments
  if (any(alpha <= 0)) {
    warning("Second column should be discrimination.")
  }

  if (any(abs(c) > 1 | c < 0)) {
    stop("The third column corresponds to the c or guessing parameter.")
  }

  # complete grid of (theta - beta)
  diff_matrix <- outer(as.vector(examinees), beta, FUN = "-")

  # R multiplies row-wise, so we use t()
  logit <- t(alpha * t(diff_matrix))

  prob <- exp(logit) / (1 + exp(logit))

  # Same as before, we need to do the t() trick
  t(c + (1 - c) * t(prob))
}


#' @rdname probability_functions
MIRT_prob_fn <- function(examinees, test) {
  # Sanitize inputs
  examinees <- as.matrix(examinees)
  test <- as.matrix(test)

  # Get the length of the thetas
  num_dimensions <- ncol(examinees)
  c_index <- (num_dimensions + 2)

  # Getting the relevant parameters
  disc_vector <- test[, seq_len(num_dimensions)]
  d <- test[, (num_dimensions + 1)]
  c <- if (ncol(test) >= c_index) test[, c_index] else 0


  logit <- t(d + t(examinees %*% t(disc_vector)))
 prob <- exp(logit) / (1 + exp(logit))

  Return
  t(c + (1 - c) * t(prob))
}
