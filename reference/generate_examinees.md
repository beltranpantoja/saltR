# Generate sample of examinees

Generate sample of examinees

## Usage

``` r
generate_examinees(
  sample_size,
  total_attrs,
  base_rate = 0.5,
  attr_corr = 0,
  strict = TRUE,
  tolerance = NULL,
  attributes_names = NULL,
  responses_names = NULL
)
```

## Arguments

- sample_size:

  How many examinees to generate

- total_attrs:

  How many attributes are in the sample

- base_rate:

  Base rate per attribute (or one if it's the same for all)

- attr_corr:

  Correlation of attributes (or one if it's the same for all)

- strict:

  Boolean. If true, the function tries to generate a binary matrix with
  the given correlation. If it is not possible, then it raises and
  error. If false it is considered to be the correlation matrix of the
  underlying normal distributions.

- tolerance:

  When strict_binary_corr=FALSE, the actual correlation values can be
  far from the expected values. This gives a tolerance to avoid
  unexpected results.

- attributes.names:

  vector of names for the attributes. Defaults to "Attr#"

- responses.names:

  vector of names for the responses. Defaults to "ID#"

## Value

A matrix of respondents and attributes.
