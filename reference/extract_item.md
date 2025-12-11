# Convert the parameters of a model and/or test into a tibble for easier analysis

Convert the parameters of a model and/or test into a tibble for easier
analysis

## Usage

``` r
extract_item(model = NULL, qmat = NULL, test = NULL)
```

## Arguments

- model:

  fitted gdina model

- qmat:

  Q-matrix

- test:

  test matrix (obtained from
  [create_test](https://beltranpantoja.github.io/saltR/reference/create_test.md))

## Value

a tibble with the estimation and/or real values of the items.
