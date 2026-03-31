# Returns a matrix of profiles and the probability of a correct response for all items.

It works by extraxcting the test parameters with `get_test_parameters`
and then passing that to the `generate_responses(get_probs=TRUE)` with
all possible mastery patterns to get the expected probabilities.

## Usage

``` r
get_probabilities_by_profile(model, marginal_attr = NULL, which_items = NULL)
```

## Arguments

- model:

  A GDINA object

- marginal_attr:

  returns the probabilities marginal on an attribute. It does not
  consider distribution, is just a mean of the probabiities.

- which_items:

  vector of items to be returnde. If Null it returns all.

## Value

a matrix of profiles and the probability of a correct response for all
items.
