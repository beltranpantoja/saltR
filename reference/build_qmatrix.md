# Build a Q-Matrix from a a matrix of test parameters

Build a Q-Matrix from a a matrix of test parameters

## Usage

``` r
build_qmatrix(test_parameters, attributes_names = NULL, item_names = NULL)
```

## Arguments

- test_parameters:

  a Matrix containing the items and their parameters as created by
  `build_test_parameters`.

- attributes_names:

  vector of names for the attributes. Defaults to "Attr#"

- item_names:

  vector of names for the items. Defaults to "Item#"

## Value

a Q-matrix
