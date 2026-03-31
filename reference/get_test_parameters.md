# Returns a matrix item-parameters from model

This extracts a model's item parameters as a matrix If `pretty_print` is
true, then it also prints a pretty version of the table.

## Usage

``` r
get_test_parameters(model, complete = TRUE, pretty_print = TRUE, digits = 2)
```

## Arguments

- model:

  a GDINA object. It has to be logit link function.

- complete:

  If the model only estimated some parameters this will not show in the
  table. So, for example, the ACDM model will only return main effects.
  If true this returns the complete table and fills the corresponding
  parameters with 0. This also ensures a consistent order of the
  parameters.

- pretty_print:

  boolean. If true it prints a pretty table. It returns a data frame
  silently.

- digits:

  rounding digits when printing. The return is not approximated.

## Value

matrix of item-parameters
