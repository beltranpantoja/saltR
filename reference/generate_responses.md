# Generate responses to a test

Given a Q-matrix and a sample of respondents this function returns a
simulated response matrix. Items parameter will automatically be mapped
to the right attribute.

## Usage

``` r
generate_responses(qmat, respondents, items, get_probs = FALSE, id = NULL)
```

## Arguments

- qmat:

  Q-matrix

- respondents:

  Binary matrix of respondents

- items:

  matrix of items-parameters

- get_probs:

  if you want the probability of correct response matrix.

- id:

  simulation id to be used when setting the seed. If NULL, the seed
  doesn't get changed

## Value

a matrix of responses in the form 0/1 or the real probabilities.
