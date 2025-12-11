# Create Q-Matrix

Create Q-Matrix

## Usage

``` r
create_qmatrix(
  num_attr = 3L,
  items_per_type = 1,
  attributes.names = NULL,
  item.names = NULL
)
```

## Arguments

- num_attr:

  attributes in the Q-matrix

- items_per_type:

  number or vector with the amount of items per unique combination

- attributes.names:

  vector of names for the attributes. Defaults to "Attr#"

- item.names:

  vector of names for the items. Defaults to "Item#"

## Value

a Q-matrix

## Examples

``` r
# example code
# This creates a 4 attribute qmatrix with 3 items per attribute.
create_qmatrix(4, 3)
#>        Attr1 Attr2 Attr3 Attr4
#> Item1      1     0     0     0
#> Item2      1     0     0     0
#> Item3      1     0     0     0
#> Item4      0     1     0     0
#> Item5      0     1     0     0
#> Item6      0     1     0     0
#> Item7      0     0     1     0
#> Item8      0     0     1     0
#> Item9      0     0     1     0
#> Item10     0     0     0     1
#> Item11     0     0     0     1
#> Item12     0     0     0     1

# This creates a 3 attribute qmatrix with 2 simple items per attribute,
# 1 double attribute item per combination, and 1 triple attribute item.
create_qmatrix(3, c(2, 1, 1))
#>        Attr1 Attr2 Attr3
#> Item1      1     0     0
#> Item2      1     0     0
#> Item3      0     1     0
#> Item4      0     1     0
#> Item5      0     0     1
#> Item6      0     0     1
#> Item7      1     1     0
#> Item8      1     0     1
#> Item9      0     1     1
#> Item10     1     1     1
```
