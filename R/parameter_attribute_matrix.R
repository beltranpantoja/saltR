
# This creates a matrix that links item parameters to attributes
.parameter_attr_matrix <- function(num_att) {
  I <- diag(num_att)
  attr <- split(I, row(I))

  link_matrix <- I
  for (i in 2:num_att) {
    parameter <- combn(attr, i, FUN = function(x) Reduce(`+`, x), simplify = T)
    new_rows <- t(parameter)/i
    link_matrix <- rbind(link_matrix, new_rows)
  }
  return(link_matrix)
}
