generate_id <- function(n = 1L) {
  sapply(
    1:n,
    FUN = function(x) ids::random_id(use_openssl = FALSE)
  )
}
