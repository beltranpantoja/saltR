# Create a matrix of items and their parameters

This matrix already encodes the Q-matrix. It can be useful to save the
results for future simulations or further wrangling as a csv.

## Usage

``` r
build_test_parameters(qmat, ...)
```

## Arguments

- qmat:

  Q-matrix

- ...:

  values to use in the different factor levels. i.e. the first number is
  assumed to be the intercept, then main effects, then two-way
  interaction and so on.

## Value

A matrix of items and their parameters.
