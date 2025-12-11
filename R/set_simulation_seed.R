#' Set simulation seed
#'
#' @description
#' This function is meant to be used inside the generation functions for a
#' finer replication of simulations. It works by taking an ID (any kind of R object),
#' hashing it, converting to an integer and setting that as a seed.
#'
#' @param id An arbitrary R object which will then be hashed and converted to an int.
#'
#' @returns The function returns the seed that was setted.
#' @export
#'
#' @examples
#' # We choose an id an the seed will be set
#' set_simulation_seed(id = "my-simulation-id")
#' a <- runif(10)
#' set_simulation_seed(id = "my-simulation-id")
#' b <- runif(10)
#' a == b # These values will be equal
set_simulation_seed <- function(id = NULL) {
  if (is.null(id)) {
    return(id)
  }
  hash <- digest::digest(id, algo = "md5")
  simul_seed <- digest::digest2int(hash)
  set.seed(simul_seed)
  return(simul_seed)
}
