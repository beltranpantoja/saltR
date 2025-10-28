#' Get attribute information
#'
#' @param model a fitted gdina model
#' @param responses the response data
#'
#' @returns a list with the info and the info proportion by attribute
#' @export
get_information <- function(model, responses) {
  I <- length(unique(model$coef$item))
  J <- length(model$NAttr)
  Q <- model$q.matrix

  propinfo <- matrix(NA, nrow = I, ncol = J)

  # attribute information
  info <- as.matrix(
    CDM::cdi.kli(model)$summary[, 3:ncol(CDM::cdi.kli(model)$summary)]
  )
  # row 1 is total attribute information
  # rows 2 - 6 are each individual item's attribute information

  # Complex Item Loop
  for (i in 1:I) {
    for (j in 1:J) {
      # item proportion of attribute information
      propinfo[i, j] <- info[i + 1, j] / info[1, j]
    }
  }

  # Clean up
  propinfo[!sapply(as.data.frame(Q), as.logical)] <- NA
  colnames(propinfo) <- sprintf("Att%d", seq(1:J))

  # Simple Structure Only
  if (all(rowSums(Q) == 1)) {
    propinfo <- as.numeric(t(propinfo)[!is.na(t(propinfo))])
    propinfo <- data.frame(Item = 1:I, Attribute = max.col(Q), propinfo)
  }

  # Complex Structure
  if (any(rowSums(Q) != 1)) {
    propinfo <- data.frame(Item = 1:I, propinfo)
  }

  return(list(
    info = info,
    propinfo = propinfo
  ))
}
