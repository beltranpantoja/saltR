#' Create the likelihood matrix for a matrix of probabilities of correct
#'  response for non-masters and masters marginal only on that attribute.
#'
#'
#' @param item_probs a matrix of probabilities for non-masters and masters.
#'    Each row is an item.
#' @param response_patterns A binary matrix where each row is one response
#'    pattern. If none is passed, then all possible patterns are generated
#' @param prior The prior for non-mastery and mastery ratio of the focus
#'    attribute
#' @param monotonicity if true, function will throw an error if any number on the
#'    first column is equal or higher than the second column.
#'
#' @returns a matrix where the first columns contain the response patterns,
#'  the likelihood of seeing that pattern given a certain level of mastery.
#'  The probability of being a (non)master and the MLE (1 if estimated master,
#'  0 otherwise).
#'
#' @export
#'
build_response_likelihood <- function(
  item_probs,
  response_patterns = NULL,
  prior = NULL,
  monotonicity = TRUE
) {
  # TODO: properly test this
  if (monotonicity) {
    if (!all(item_probs[, 2] >= item_probs[, 1])) {
      stop("All the probabilities on the second column have to be equal or larger")
    }
  }
  # If a prior is not given, it is assumed to be equal for both.
  if (is.null(prior)) {
    prior <- c(.5, .5)
  }

  # Checking prior arguments
  if (length(prior) != 2) stop("prior has to have two elements.")
  if (sum(prior) != 1) stop("Sum of priors has to be 1.")


  N <- nrow(item_probs)

  # We construct the matrix for all patterns so the likelihood is correct.
  full_response_patterns <- create_patterns(num_vars = N)


  # Likelihood of response conditional on mastery P(x|a)
  patt_likelihood <- apply(
    full_response_patterns,
    simplify = TRUE, MARGIN = 1, FUN = \(x) .response_likelihood(x, item_probs)
  ) |> t()

  # Response likelihood P(x)
  response_lik <- (patt_likelihood %*% prior)

  # Non-master prob marginal on response P(a|x)
  non_master_lik <-
    (patt_likelihood[, 1, drop = FALSE] * prior[1]) / response_lik

  # Master prob marginal on response P(a|x)
  master_lik <-
    (patt_likelihood[, 2, drop = FALSE] * prior[2]) / response_lik

  # MLE classification does not consider prior
  MLE <- (patt_likelihood[, 2] >= patt_likelihood[, 1]) * 1

  # Joining the elements of the likelihood matrix
  lik_matrix <- cbind(
    patt_likelihood,
    response_lik,
    non_master_lik,
    master_lik,
    MLE
  )

  colnames(lik_matrix) <- c(
    "lik_non",
    "lik_master",
    "lik_response",
    "prob_non",
    "prob_master",
    "MLE"
  )


  # If patterns were passed we filter the lik_matrix

  if (!is.null(response_patterns)) {
    num_cols <- ncol(response_patterns)
    if (num_cols != N) {
      stop(sprintf(
        "Response patterns are not the correct size. Expected: %d elements", N
      ))
    } else {
      # This is going row by row and seeing if there's any match
      # only keeping the rows that are present in response_patterns
      idx <- .get_matching_rows_index(full_response_patterns, response_patterns)

      lik_matrix <- lik_matrix[idx, , drop = FALSE]
    }
  } else {
    response_patterns <- full_response_patterns
  }

  # Returning the complete matrix
  cbind(response_patterns, lik_matrix)
}


.response_likelihood <- function(response, items) {
  # Checking that arguments are well formed
  if (typeof(response) != "double") {
    stop(
      "filter argument must be a double, instead got:", typeof(response)
    )
  }

  if (length(response) != nrow(items)) {
    stop(
      "The response vector has to be the same length as the number of items."
    )
  }

  # The likelihood values get clamped to avoid potential overflow

  # non-masters (first column)
  guess <- items[, 1, drop = FALSE]
  non_master <-
    prod(ifelse(response == 1, guess, 1 - guess)) |>
    .clamp_prob()

  # masters (slip is the complement)
  slip <- 1 - items[, 2, drop = FALSE]
  master <-
    prod(ifelse(response == 1, 1 - slip, slip)) |>
    .clamp_prob()

  # Return
  c(non_master, master)
}

.clamp_prob <- function(value, eps = 1e-10) {
  min(max(value, eps), 1 - eps)
}



# B is a subset to match in A, so the indeces are with respect to A
.get_matching_rows_index <- function(A, B) {
  # Checking inputs
  if (!is.matrix(A) || !is.matrix(B)) {
    stop("Both A and B must be matrices.")
  }

  if (ncol(A) != ncol(B)) {
    stop("Matrices must have the same number of columns.")
  }
  cols <- seq_len(ncol(B))

  # Matching logic
  idx <- vapply(seq_len(nrow(B)), function(i) {
    hits <- which(
      apply(A[, cols, drop = FALSE], 1, function(a_row) {
        all(a_row == B[i, ])
      })
    )

    if (length(hits) == 0) {
      stop(sprintf("Row %d of B was not found in A.", i))
    }

    if (length(hits) > 1) {
      stop(sprintf("Row %d of B has multiple matches in A.", i))
    }

    hits
  }, integer(1))

  # Return the value
  idx
}
