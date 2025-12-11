#' Generate random simulation ids
#'
#' @param n number of ids to generate
#'
#' @returns a vector of random ids to be used in simulations
#' @export
#'
generate_id <- function(n = 1L) {
  sapply(
    1:n,
    ids::random_id(use_openssl = FALSE)
  )
}
