#' Check that the monotonicity condition of the gdina model is fullfilled. This
#'  is useful for when you don't want to force it, but you want to know if the
#'  model was able to achieve it.
#'
#' @param model A gdina object
#' @param tol tolerance for difference
#'
#' @returns a boolean and raises a warning
#' @export
#'
check_monotonicity <- function(model, tol = 1e-12) {
  mat <- get_probabilities_by_profile(model)

  n_attr <- ncol(model$q.matrix)
  attr_cols <- seq_len(n_attr)
  prob_cols <- (n_attr + 1):ncol(mat)

  attrs <- mat[, attr_cols, drop = FALSE]
  probs <- mat[, prob_cols, drop = FALSE]
  n <- nrow(mat)

  # Utility function to check which profiles are subsets
  is_superset <- function(i, j) {
    all(attrs[i, ] >= attrs[j, ])
  }

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next # Skip comparing a profile to itself

      # Logic: If 'i' masters EVERYTHING 'j' masters (i is a superset of j)
      is_superset <- all(attrs[i, ] >= attrs[j, ])

      if (is_superset) {
        # THEN probs[i] MUST be >= probs[j]
        # We look for the violation:
        violation_mask <- probs[i, ] < (probs[j, ] - tol)

        if (any(violation_mask)) {
          offending_items <- colnames(probs)[violation_mask]

          warning(sprintf(
            paste(
              "Monotonicity violated:",
              "Profile %s has LOWER probabilities than profile %s on items %s"
            ),
            paste(attrs[i, ], collapse = ""),
            paste(attrs[j, ], collapse = ""),
            paste(offending_items, collapse = ", ")
          ))
          return(FALSE) # Stop at first violation
        }
      }
    }
  }
  return(TRUE)
}
