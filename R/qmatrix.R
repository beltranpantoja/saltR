#' Create Q-Matrix
#'
#' @param total_attrs attributes in the Q-matrix
#' @param item_attrs How many attributes should the items have
#' @param repeat_items Items per unique combination
#'
#' @returns a Q-matrix
#' @export
#'
#' @examples
#' # This returns a simple Q-matrix of 3 attributes
#' create_qmatrix(3)
#' # This returns a simple Q-matrix of 3 attributes with 2 items per attribute
#' create_qmatrix(3,2)
#' # This makes the items be associated with two attributes
#' create_qmatrix(3,item_attrs=2)
create_qmatrix <- function(
  total_attrs= 3L,
  item_attrs=1L,
  repeat_items= 1L
  ) {
  stopifnot(total_attrs >= 1L, item_attrs >= 1L, item_attrs <= total_attrs)

  simple_items <- asplit(diag(total_attrs), 1, drop=T)


  base_items <- t(combn(
    simple_items,
    item_attrs,
    function(x) Reduce(`+`, x), simplify = T)
    )

  qmat <- kronecker(base_items, matrix(1, repeat_items, 1))

  return(qmat)
}


#' Extend a Q-Matrix
#'
#' This function takes a Q-Matrix and adds items with certain
#' amount of attributes.
#'
#' @param .qmat Qmatrix to be extended
#' @param item_attrs How many attributes should the items have
#' @param repeat_items Items per unique combination
#'
#' @returns An extended version of the passed Q-Matrix
#' @export
#'
#' @examples
#' # This creates three more items with two attributes each
#' create_qmatrix(3) |>
#'   extend_qmatrix(1, 2)
extend_qmatrix <- function(
    .qmat,
    item_attrs=1L,
    repeat_items= 1L
    ) {
  stopifnot("The first argument should be a matrix!" = is.matrix(.qmat))

  total_attrs <- ncol(.qmat)

  extra_items <- create_qmatrix(
    total_attrs,
    item_attrs,
    repeat_items)

  new_qmatrix <- rbind(.qmat, extra_items)

  return(new_qmatrix)
}


#' Q-Matrix composition
#'
#' Check the composition of a Q-Matrix
#'
#' @param qmat Qmatrix to analyze
#'
#' @returns Summary like text
#' @export
#'
check_qmatrix <- function(qmat) {

  .count_elements <- function(x, range) {
    sapply(range, function(i) sum(x == i))
  }

  num_attrs <- ncol(qmat)
  attrs <- if (!is.null(colnames(qmat))) colnames(qmat) else paste0("attribute", 1:num_attrs)
  items_per_attr <- as.integer(apply(qmat, 2, sum))


  cat("----- Q-Matrix Summary -----\n\n")

  cat("Number of attributes: ", num_attrs, "\n")
  cat("Number of items: ", sum(items_per_attr), "\n\n")

  cat("-----------------------------\n")

  cat("Items per attribute: ", "\n\n")
  for (i in 1:num_attrs) {
    cat(sprintf("%-10s: %2d\n", attrs[i], items_per_attr[i]))
  }

  cat("-----------------------------\n")

  cat("Items by type: ", "\n\n")

  cat("-----------------------------\n")

  cat("Type of items: ", "\n\n")
  # measuring i attributes:

  cat("-----------------------------\n")

  cat("Common items between attributes: ", "\n\n")
  # for each attribute pair or combination, how many 1s do they have in common?

}

