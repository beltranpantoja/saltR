# Check that the monotonicity condition of the gdina model is fullfilled. This is useful for when you don't want to force it, but you want to know if the model was able to achieve it.

Check that the monotonicity condition of the gdina model is fullfilled.
This is useful for when you don't want to force it, but you want to know
if the model was able to achieve it.

## Usage

``` r
check_monotonicity(model, tol = 1e-12)
```

## Arguments

- model:

  A gdina object

- tol:

  tolerance for difference

## Value

a boolean and raises a warning
