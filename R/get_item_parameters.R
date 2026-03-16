#' Returns a matrix item-parameters from model
#'
#' This extracts a model's item parameters as a dataframe. If `print_table` is true, then it also prints a pretty version of the table.
#'
#' @param model a GDINA object. It has to be logit link function.
#' @param print_table boolean. If true it prints a pretty table. It returns a data frame silently.
#' @param digits rounding digits when printing. The return is not approximated.
#'
#' @returns matrix of item-parameters
#' @export
#'
get_item_parameters <- function(model, print_table = TRUE, digits = 2) {
  # TODO: This could down the line also give the SE

  # Ensuring only logit linkfct
  if (model$linkfct != "logit") {
    stop(
      "only implemented for models with link logit, not: ", model$linkfct
    )
  }

  model.coef <- model$coef

  # We get the model's attributes names in order
  attr.labels <- colnames(model$attribute.patt.splitted)

  # Each attribute label is changed by a number. So the parameters are normalized.
  partype.attr <- model.coef[, "partype.attr"]
  for (i in seq_along(attr.labels)) {
    partype.attr <- gsub(attr.labels[i], i, partype.attr)
  }
  partype.attr[partype.attr == ""] <- "0"

  # We replace the result of the change in the coef matrix
  model.coef[, "partype.attr"] <- partype.attr


  # tapply crosses the item and partype.attr columns to obtain the est values
  item_params <- with(
    model.coef,
    tapply(
      est,
      list(item, partype.attr),
      function(x) x
    ) # The identity function allows for the return of NAs
  )

  # In some edgecases this reordering will not work as expected.
  items_order <- rownames(item_params) |>
    nchar() |>
    order()
  params_order <- colnames(item_params) |>
    nchar() |>
    order()
  item_params <- item_params[items_order, params_order]

  if (print_table == TRUE) {
    params_print <- as.matrix(item_params)
    str_width <- digits + 3

    colnames(params_print) <- format(
      paste0("\U03BB", colnames(item_params)), # Adding the lambda
      justify = "centre",
      width = digits + 3
    ) # The rounding value plus space for -0.

    print.table(params_print, digits = digits)

    # We return the data frame silently
    invisible(as.data.frame(item_params))
  } else {
    return(as.data.frame(item_params))
  }
}
