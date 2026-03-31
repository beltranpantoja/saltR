# Create all possible permutations of the levels passed.

Create all possible permutations of the levels passed.

## Usage

``` r
create_patterns(
  num_vars,
  include_filter = NULL,
  exclude_filter = NULL,
  levels = c(0, 1),
  column_prefix = "V",
  column_labels = NULL
)
```

## Arguments

- num_vars:

  number of columns

- include_filter:

  vector of length `length(num_vars)`. It only includes rows that match.
  NA values work like wildcards.

- exclude_filter:

  vector of length `length(num_vars)`. It excludes rows that match. NA
  values work like wildcards.

- levels:

  Elements to be used in the combination. By default `c(0, 1)`, thus
  creating a binary matrix.

- column_prefix:

  Prefix for the column names. If column_labels is passed then this is
  ignored.

- column_labels:

  Labels for the columns output.

## Value

a matrix of all possible permutations.
