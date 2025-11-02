#' Create item parameters from probabilities of right response
#'
#' @details
#' This function returns a vector of item parameters from a vector
#' of probabilities of having right response for a specific attribute vector.
#' Notice: This function doesn't check for  possible _real_ values
#' (e.g. main effects could be negative).
#'
#' @param ... probabilities of right answer by attribute profile.
#' @param type type of item (LCDM, DINA, DINO, RRUM).
#' @param num_attrs Number of attributes in the item. Not needed for LCDM.
#'
#' @returns an item parameters in their standard format
#' @export
#' @examples
#' lcdm_item <- create_item(.2, .4, .6, .8)
#' rrum_item <- create_item(.2, .4, .6, type = "RRUM")
#' dino_item <- create_item(.2, .8, type = "DINO", num_attrs = 2)
create_item <- function(..., type = "LCDM", num_attrs = NULL) {
  item_type <- toupper(type)
  probs <- c(...)

  if (!is.double(probs) && all(abs(probs) <= 1)) {
    stop("Probabilities should all be doubles between 0 and 1.", asString(probs))
  }

  normalized_probs <- switch(item_type,
    LCDM = probs,
    DINA = .dina_probs(probs, num_attrs),
    DINO = .dino_probs(probs, num_attrs),
    RRUM = .rrum_probs(probs),
  )

  return(
    .create_lcdm_item(normalized_probs)
  )
}


# =======================================================================
# LCDM Item
# =======================================================================

.create_lcdm_item <- function(...) {
  probs <- c(...)

  # Checking right length
  num_attrs <- log(length(probs), 2)
  if (num_attrs - round(num_attrs, 0) != 0) {
    stop("Not the right number of probabilities!", toString(probs))
  } else if (num_attrs > 2) {
    stop("not implemented for more than 2 attributes")
  }

  att_mat <- matrix(
    c(
      0, 0, 0, 0,
      1, 0, 0, 0,
      1, 0, 0, 0,
      -1, 1, 1, 0
    ),
    4, 4,
    byrow = T
  )

  logits <- .get_logit(probs)
  size <- max(2, num_attrs^2)

  params <- logits - att_mat[1:size, 1:size] %*% logits

  return(as.double(params))
}


# =======================================================================
# Getting the LCDM probabilities vector for each DCM model
# =======================================================================

.dina_probs <- function(probs, num_attrs) {
  if (length(probs) != 2) {
    stop("Only pass the non-master and master of all for DINA items")
  }
  full_probs <- c(rep(probs[1], num_attrs + 1), probs[2])
  return(full_probs)
}

.dino_probs <- function(probs, num_attrs) {
  if (length(probs) != 2) {
    stop("Only pass the non-master and master of all for DINO items")
  }
  full_probs <- c(probs[1], rep(probs[2], num_attrs + 1))
  return(full_probs)
}

.rrum_probs <- function(probs) {
  if (length(probs) == 3) {
    full_probs <- c(probs, sum(probs) - 2 * probs[1])
  } else {
    stop("Only implemented for 2 attributes")
  }
  return(full_probs)
}



# I think with this i can replace the att_mask that is hardcoded.
# t((saltr:::.parameter_attr_matrix(3) %*% attrs_prof == 1)*1)-diag(7)

# attrs_prof <- matrix(c(
#   1, 0, 0,
#   0, 1, 0,
#   0, 0, 1,
#   1, 1, 0,
#   1, 0, 1,
#   0, 1, 1,
#   1, 1, 1
# ), 3)



items <- matrix(
  c(
    -2, 1, NA, NA,
    -2, 1, NA, NA,
    -2, 1, NA, NA,
    -2, 2.1, 0, -1,
    -2, 2.2, 4, -1.4,
    -2, 1, 2, -2
  ),
  ncol = 4, byrow = T
)
