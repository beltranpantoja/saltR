# Internal - Generate sample of respondents Note: Not encapsulated for randomness.

Internal - Generate sample of respondents Note: Not encapsulated for
randomness.

## Usage

``` r
.generate_sample(
  sample_size,
  total_attrs,
  base_rate = 0.5,
  attr_corr = 0.5,
  attributes.names = NULL,
  responses.names = NULL
)
```

## Arguments

- sample_size:

  How many respondents

- total_attrs:

  How many attributes are in the sample

- base_rate:

  Base rate per attribute (or one if it's the same for all)

- attr_corr:

  Correlation of attributes (or one if it's the same for all)

- attributes.names:

  vector of names for the attributes. Defaults to "Attr#"

- responses.names:

  vector of names for the responses. Defaults to "#"

## Value

A matrix of respondents and attributes.
