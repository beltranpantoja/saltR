.get_logit <- function(p) {
  return(
    log(p / (1 - p))
  )
}

.logistic_fn <- function(parameters) {
  logit <- sum(parameters)
  exp(logit) / (1 + exp(logit))
}
