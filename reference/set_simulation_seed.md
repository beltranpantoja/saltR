# Set simulation seed

This function is meant to be used inside the generation functions for a
finer replication of simulations. It works by taking an ID (any kind of
R object), hashing it, converting to an integer and setting that as a
seed.

## Usage

``` r
set_simulation_seed(id = NULL)
```

## Arguments

- id:

  An arbitrary R object which will then be hashed and converted to an
  int.

## Value

The function returns the seed that was setted.

## Examples

``` r
# We choose an id an the seed will be set
set_simulation_seed(id = "my-simulation-id")
#> [1] 1214404737
a <- runif(10)
set_simulation_seed(id = "my-simulation-id")
#> [1] 1214404737
b <- runif(10)
a == b # These values will be equal
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```
