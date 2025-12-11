# Create item parameters from probabilities of right response

Create item parameters from probabilities of right response

## Usage

``` r
create_item(..., type = "LCDM", num_attrs = NULL)
```

## Arguments

- ...:

  probabilities of right answer by attribute profile.

- type:

  type of item (LCDM, DINA, DINO, RRUM).

- num_attrs:

  Number of attributes in the item. Not needed for LCDM.

## Value

an item parameters in their standard format

## Details

This function returns a vector of item parameters from a vector of
probabilities of having right response for a specific attribute vector.
Notice: This function doesn't check for possible *real* values (e.g.
main effects could be negative).

## Examples

``` r
lcdm_item <- create_item(.2, .4, .6, .8)
rrum_item <- create_item(.2, .4, .6, type = "RRUM")
dino_item <- create_item(.2, .8, type = "DINO", num_attrs = 2)
```
