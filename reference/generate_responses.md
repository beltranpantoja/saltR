# Generate responses to a test

Given a Q-matrix and a sample of respondents this function returns a
simulated response matrix. Items parameter will automatically be mapped
to the right attribute.

## Usage

``` r
generate_responses(examinees, test, get_probs = FALSE, qmatrix = NULL)
```

## Arguments

- examinees:

  Binary matrix of respondents

- test:

  matrix of items parameters

- get_probs:

  if you want the probability of correct response matrix.

- qmatrix:

  Optional. All test are checked to see if they conform to some
  Q-matrix. If some specific qmatrix gets passed, then it is checked
  against that one.

## Value

a matrix of responses in the form 0/1 or the real probabilities.
