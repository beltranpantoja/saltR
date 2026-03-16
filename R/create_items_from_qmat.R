#' Creates a matrix of items-parameters given a qmatrix.
#'
#' This is an utility function to quickly create a matrix of parameters that is consistent with a given qmatrix. If no extra parameters are passed, then it returns a mask for the parameters where each number represents a factors order (e.g. 1 indicatens main effect, 2 indicates two-way effect, and so on.).
#'
#' @param qmat Q-matrix
#' @param ... Values for the main effects, two interaction and so on. The values are all assumed equal. Do not need to pass interaction effects that don't exist in the Q-matrix.
#'
#' @returns A matrix where the rows are the items and the columns all the possible parameters.
#' @export
#'
create_items_from_qmat <- function(qmat, ...) {
  params_values <- c(...)

  mask <- .get_item_params_mask(qmat)

  # This give us the factor order
  factor_levels <- colnames(mask) |>
    strsplit("-") |>
    lengths()
  # We need to put 0 on the intercept
  factor_levels[1] <- 0

  for (level in unique(factor_levels)) {
    level_params <- mask[, factor_levels == level]
    cols <- (1:length(factor_levels))
    mask[col(mask) %in% cols[factor_levels == level] & !is.na(mask)] <- level
  }

  if (!is.null(params_values)) {
    # Check that the params value is the right size
    num_pars <- length(params_values)
    num_factors <- length(unique(mask[!is.na(mask)]))
    if (num_pars != num_factors) {
      stop(
        "need to pass values for all terms. Expected ",
        num_factors,
        " parameters, but got ",
        num_pars
      )
    }

    # TODO: this can be improved
    # Get parameters mask first to avoid overwritting later
    par_masks <- lapply(1:3, FUN = \(i) {
      (mask == (i - 1) & !is.na(mask))
    })

    # Now we use the mask to replace the values
    for (i in 1:num_pars) {
      mask[par_masks[[i]]] <- params_values[i]
    }
  }

  return(mask)
}


.get_item_params_mask <- function(qmat) {
  attr_matrix <- qmat * col(qmat)

  attr_per_item <- apply(attr_matrix, MARGIN = 1, FUN = function(row) {
    x <- row[row != 0] |> as.double()
    .get_params_names(x)
  })

  # Now we create the parameter mask
  parameter_names <- .get_params_names(1:ncol(qmat))

  item_params <-
    sapply(attr_per_item, FUN = \(x) parameter_names %in% x) |>
    t() * 1

  # Adding the intercept
  item_params <- cbind(
    rep(1, nrow(qmat)),
    item_params
  )

  item_params[item_params == 0] <- NA


  item_names <- rownames(qmat)

  if (is.null(item_names)) {
    item_names <- paste0("V", 1:nrow(qmat))
  }

  colnames(item_params) <- c(0, parameter_names)
  rownames(item_params) <- item_names

  return(item_params)
}

# Utility function
.get_params_names <- function(attrs) {
  # If there's only one element we just return it
  if (length(attrs) == 1) {
    return(as.character(attrs))
  }

  # For more than 2 items, combn give us all combinations
  order_effects <- 1:length(attrs)
  unlist(lapply(order_effects, function(i) {
    apply(combn(attrs, i), 2, paste, collapse = "-")
  }))
}
