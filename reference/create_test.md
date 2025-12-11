# Create a matrix of items

@description An utility function that calls the `create_item` function
and returns a matrix padded with NAs that can be used directly with the
responses generation. It assumes all the items are LCDM.

## Usage

``` r
create_test(...)
```

## Arguments

- ...:

  integer vectors with the probabilities

## Value

a matrix of parameters
