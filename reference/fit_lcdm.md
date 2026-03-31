# Utility function to fits a LCDM model using the CDM package with some convenient defaults.

Utility function to fits a LCDM model using the CDM package with some
convenient defaults.

## Usage

``` r
fit_lcdm(
  responses,
  qmatrix,
  monotonicity = TRUE,
  rule = "GDINA",
  verbose = FALSE,
  ...
)
```

## Arguments

- responses:

  examinees responses

- qmatrix:

  qmatrix.

- monotonicity:

  Should the monotonicity constraint be forced?

- rule:

  model to use in the gdina. defaults to GDINA

- verbose:

  should it print progress? defaults to FALSE.

- ...:

  extra parameters to pass to the gdina call.

## Value

an object gdina.
