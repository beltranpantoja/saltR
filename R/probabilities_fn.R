.get_logit <- function(p) {
  return(
    log(p/(1-p))
  )
}
