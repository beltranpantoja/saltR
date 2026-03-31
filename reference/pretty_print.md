# Prints a matrix to the console in a prettier format

This function prints a pretty table and returns the original object so
it can be used inside a pipeline.

## Usage

``` r
pretty_print(matrix, digits = 3, prefix = "λ")
```

## Arguments

- matrix:

  matrix to print

- digits:

  number of rounding digits

- prefix:

  prefix for the columns. By default is the 'lambda' letter.

## Value

the same passed object
