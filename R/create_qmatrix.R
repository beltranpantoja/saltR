#' Create Q-Matrix
#'
#' @param num_attr attributes in the Q-matrix
#' @param items_per_type number or vector with the amount of items per unique
#'  combination
#' @param attributes_names vector of names for the attributes. Defaults
#'  to "Attr#"
#' @param item_names vector of names for the items. Defaults to "Item#"
#'
#' @returns a Q-matrix
#' @export
#' @examples
#' # example code
#' # This creates a 4 attribute qmatrix with 3 items per attribute.
#' create_qmatrix(4, 3)
#'
#' # This creates a 3 attribute qmatrix with 2 simple items per attribute,
#' # 1 double attribute item per combination, and 1 triple attribute item.
#' create_qmatrix(3, c(2, 1, 1))
create_qmatrix <- function(
  num_attr = 3L,
  items_per_type = 1,
  attributes_names = NULL,
  item_names = NULL
) {
  # We generate the partial qmatrices
  partial_qmatrices <- lapply(
    seq_along(items_per_type),
    function(i) {
      .simple_qmatrix(num_attr, i, items_per_type[i])
    }
  )

  qmatrix <- do.call(rbind, partial_qmatrices)

  # Giving names to the qmatrix rows and columns
  if (is.null(attributes_names)) {
    attributes_names <- paste0("Attr", 1:num_attr)
  }
  if (is.null(item_names)) {
    item_names <- paste0("Item", seq_len(dim(qmatrix)[1]))
  }
  colnames(qmatrix) <- attributes_names
  rownames(qmatrix) <- item_names

  qmatrix
}


#' Utility function for the creation of a partial Qmatrix
#'
#' @param num_attr attributes in the Q-matrix
#' @param item_attrs How many attributes should the items have
#' @param repeat_items Items per unique combination
#'
#' @returns a Q-matrix
#' @noRd
.simple_qmatrix <- function(
  num_attr = 3L,
  item_attrs = 1L,
  repeat_items = 1L
) {
  stopifnot(num_attr >= 1L, item_attrs >= 1L, item_attrs <= num_attr)

  simple_items <- asplit(diag(num_attr), 1, drop = TRUE)


  base_items <- t(utils::combn(
    simple_items,
    item_attrs,
    function(x) Reduce(`+`, x),
    simplify = TRUE
  ))

  qmat <- kronecker(base_items, matrix(1, repeat_items, 1))

  qmat
}
