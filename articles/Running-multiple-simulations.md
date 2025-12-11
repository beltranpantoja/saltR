# Running multiple simulations

Let’s create a simulation to measure the precision of the parameter
estimation for DINO items when we don’t have simple items.

## Setting the base design

First we create a Q-matrix and a sample of respondents.

``` r
library(saltr)

qmatrix <- create_qmatrix(3, c(0,2))
sample <- generate_sample(1000, 3, base_rate = .5)
```

We can create all items to be the same for simplicity.

``` r
item_probs <- matrix(
  rep(c(.2, .7, .7, .7), 6), # DINO item, non-master .2 probability, masters .7
  nrow=6, byrow=TRUE
)

test <- create_test(item_probs)
test
#>           [,1]     [,2]     [,3]      [,4]
#> [1,] -1.386294 2.233592 2.233592 -2.233592
#> [2,] -1.386294 2.233592 2.233592 -2.233592
#> [3,] -1.386294 2.233592 2.233592 -2.233592
#> [4,] -1.386294 2.233592 2.233592 -2.233592
#> [5,] -1.386294 2.233592 2.233592 -2.233592
#> [6,] -1.386294 2.233592 2.233592 -2.233592
```

Let’s simulate one set of responses:

``` r
responses <- generate_responses(qmatrix, sample, test)
head(responses)
#>   Item1 Item2 Item3 Item4 Item5 Item6
#> 1     0     1     0     0     0     1
#> 2     0     0     1     1     1     1
#> 3     0     0     0     1     0     0
#> 4     0     0     1     1     0     0
#> 5     1     0     1     1     1     1
#> 6     0     0     1     0     0     0
```

Now, because this a simulation we need to: generate multiple responses,
fit a model and somehow save the results.

## Creating a simulation function

We can use a simple function like this. This function encapsulates the
creation of both the responses and the fitting of the model. This is key
for using `doFuture`.

``` r
simulation <- function() {
  
  # Generating a new set of responses
  responses <- generate_responses(qmatrix, sample, test)

  # Fitting the model 
  model <- CDM::gdina(
    responses, 
    qmatrix, 
    linkfct = "logit", 
    progress = FALSE)

}
```

We can then create a function which will return a simple value that we
can save and report on.

``` r
extract_parameters <- function(model) {
  parameters <- model$coef[ , c(3,4,6)] 
  intercepts <- parameters[parameters$partype==0, 3]
  main_effects <- parameters[parameters$partype %in% c(1,2), 3]
  
  return(
    c(mean(intercepts),
      mean(main_effects))
  )
}
```

The new simulation function is now this:

``` r
simulation <- function() {
  
  # Generating a new set of responses
  responses <- generate_responses(qmatrix, sample, test)

  # Fitting the model 
  # We supress the warnings of the simulations with all 1 or 0s.
  model <-suppressWarnings(CDM::gdina(
    responses, 
    qmatrix, 
    linkfct = "logit", 
    progress = FALSE))

  # Now we return a measure, not the model itself.
  results <- extract_parameters(model)
  return(results)
}
```

We can run it to see it’s working as expected:

``` r
simulation()
```

## Iterating

We can now use this function to iterate and save all the results, for
example like this:

``` r
reps <- 10

results <- matrix(NA, ncol=2, nrow=reps)

for (i in 1:reps) {
  results[i, ] <- simulation()  
}
```

## Parallel processing

saltr can be used together with the
[doFuture](https://cran.r-project.org/web/packages/doFuture/vignettes/doFuture-1-overview.html)
package to take advantage of parallel processing through the library
`doFuture`with very few changes.

Although, it comes with some overhead so it’s benefits don’t really show
until we do bigger simulations.

``` r
library(doFuture)
reps <- 10

results <- foreach(
  rep = 1:reps, .combine = rbind, .options.future = list(seed = TRUE)
  ) %dofuture% {
    simulation()
}
```
