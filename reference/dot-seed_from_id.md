# Create simulation seed from ID

This function is meant to be used inside the generation functions for a
finer replication of simulations. It works by taking an ID (any kind of
R object), hashing it, converting to an integer and setting that as a
seed.

## Usage

``` r
.seed_from_id(id = NULL)
```

## Arguments

- id:

  An arbitrary R object which will then be hashed and converted to an
  int.

## Value

The function returns a seed to be used.
