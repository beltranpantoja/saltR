#' Get the matrix of the possible ideal responses patterns from a qmatrix
#'
#' @param qmat Q-matrix from which to generate the patterns
#' @param rule DINA (only students with all relevant attributes should have the item correct) or DINO (students with at least one relevant attribute should have the item correct)
#'
#' @returns a matrix of responses patterns and items
#' @export
get_ideal_patterns <- function(qmat, rule) {
  # Normalize the writting of rule
  rule <- toupper(rule)

  profiles <- .create_attr_profiles(ncol(qmat))
  base_patterns <- profiles %*% t(qmat)

  if (rule == "DINA") {
    ideal_patterns <- ((profiles %*% t(qmat)) == rowSums(qmat)) * 1
  } else if (rule == "DINO") {
    ideal_patterns <- ((profiles %*% t(qmat)) != 0) * 1
  } else {
    stop("rule should be DINA or DINO")
  }
  return(unique(ideal_patterns))
}
