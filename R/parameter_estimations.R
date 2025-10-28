# qmat <- saltR::create_qmatrix(3L, repeat_items = 3) |>
#   saltR::extend_qmatrix(2,2)
# respondents <- saltR::generate_sample(100, 3)
# responses <- saltR::generate_responses(qmat, respondents, c(-2, 2))
# model <- CDM::gdina(responses, qmat, linkfct = "logit")


nice_estimations <- function(model) {

  items <- model$item[,3:8]

  dense <- items |>
    dplyr::mutate(
      num_attrs = stringr::str_count(partype.attr, "\\d"),
      type_parameter = dplyr::case_match(
        num_attrs,
        0 ~ "intercept",
        1 ~ "main effect",
        2 ~ "interaction",
        3 ~ "three-interaction"
      ),
      attributes = stringr::str_remove_all(partype.attr, "Attr")
    ) |>
    dplyr::select(itemno, type_parameter, attributes, est, se)

  long <- items |>
    dplyr::mutate(
      attributes = ifelse(partype.attr == "", "intercept", partype.attr)) |>
    dplyr::select(itemno, attributes, est) |>
    tidyr::pivot_wider(names_from = attributes, values_from = est )

  return(list(
   dense = dense,
   long=long
  ))
}
