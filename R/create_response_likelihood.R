#' Returns the likelihood and probability of mastery of the response patterns.
#'
#' @param items a matrix of probabilities for non-masters and masters. Each row is an item.
#' @param response_patterns A binary matrix where each row is one response pattern. If none is passed, then all possible patterns are generated
#' @param prior The prior for non-mastery and mastery ratio.
#' @param EAP_treshold Posterior threshold to be considered a master. By default is .5
#'
#' @returns a matrix where the first columns contain the response patterns, the likelihood of seeing
#' that pattern given a certain level of mastery. The probability of being a (non)master and the EAP (final classification)
#'
#' @export
#'
create_response_likelihood <- function(
  items,
  response_patterns = NULL,
  prior = NULL,
  EAP_treshold = .5
) {
  # If a prior is not given, it is assumed to be equal for both.
  if (is.null(prior)) {
    prior <- c(.5, .5)
  }

  # Checking prior arguments
  if (length(prior) != 2) stop("prior has to have two elements.")
  if (sum(prior) != 1) stop("Sum of priors has to be 1.")


  N <- nrow(items)

  # If a set of response patterns is not passed, we create one that fits the items
  if (is.null(response_patterns)) {
    response_patterns <- create_patterns(num_vars = N)
  } else {
    # Checking the response patterns are well formed
    if (ncol(response_patterns) != N) {
      stop(
        "The response patterns passed are not the correct size, they should have ",
        N, " elements"
      )
    }
  }

  # This is the likelihood of each response conditional on their response
  # P(x|a)
  patt_likelihood <- apply(
    response_patterns,
    simplify = T, MARGIN = 1, FUN = \(x) .response_likelihood(x, items)
  ) |> t()

  # P(a|x) Non-master
  non_master_lik <-
    (patt_likelihood[, 1, drop = FALSE] * prior[1]) / (patt_likelihood %*% prior)

  # P(a|x) Master
  master_lik <-
    (patt_likelihood[, 2, drop = FALSE] * prior[2]) / (patt_likelihood %*% prior)

  # EAP classification (or MAP?)
  EAP <- (master_lik >= EAP_treshold) * 1

  # Joining the elements of the likelihood matrix
  lik_matrix <- cbind(patt_likelihood, non_master_lik, master_lik, EAP)

  colnames(lik_matrix) <- c(
    "lik_non",
    "lik_master",
    "prob_non",
    "prob_master",
    "EAP"
  )

  # Returning the complete matrix
  return(
    cbind(response_patterns, lik_matrix)
  )
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

  return(
    c(non_master, master)
  )
}

.clamp_prob <- function(value, eps = 1e-10) {
  return(min(max(value, eps), 1 - eps))
}
