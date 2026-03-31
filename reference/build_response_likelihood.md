# Create the likelihood matrix for a matrix of probabilities of correct response for non-masters and masters marginal only on that attribute.

Create the likelihood matrix for a matrix of probabilities of correct
response for non-masters and masters marginal only on that attribute.

## Usage

``` r
build_response_likelihood(
  item_probs,
  response_patterns = NULL,
  prior = NULL,
  monotonicity = TRUE
)
```

## Arguments

- item_probs:

  a matrix of probabilities for non-masters and masters. Each row is an
  item.

- response_patterns:

  A binary matrix where each row is one response pattern. If none is
  passed, then all possible patterns are generated

- prior:

  The prior for non-mastery and mastery ratio of the focus attribute

- monotonicity:

  if true, function will throw an error if any number on the first
  column is equal or higher than the second column.

## Value

a matrix where the first columns contain the response patterns, the
likelihood of seeing that pattern given a certain level of mastery. The
probability of being a (non)master and the MLE (1 if estimated master, 0
otherwise).
