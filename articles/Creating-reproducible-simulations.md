# Creating reproducible simulations

An desirable trait of a simulation study is the reproducibility of it’s
results. Even when using good practices like sharing the code, stating
the software and library versions and setting seeds for the
replications, a lot of problems persist.

An important, weakness of this traditional approach is that when setting
a seed, the reproducibility of the code depends on the amount of calls
to the random number generation of R which means that modifications to
the code have a cascading effect down the chain.

## Cascading effect of calls to random

Lets imagine we have an original simulation function that was later
modified by another researcher.

``` r
original_simulation <- function() {
  a <- runif(1)  
  b <- runif(1)  
  return(a+b) 
}

modified_simulation <- function() {
  a <- runif(1) 
  another_number <- runif(1)
  b <- runif(1)  
  return(a+b)
}
```

Now we run both functions setting the seed beforehand for
reproducibility:

``` r
set.seed(314)
original_simulation()
#> [1] 0.3703098

set.seed(314)
modified_simulation()
#> [1] 0.8653602
```

When we call the modified simulation, even if we use the same seed
before running the function we will get different results, because the
modified version calls the `runif` function one in between the
generation of a and b.

There is not a clear way to fix this unless we can ensure the call to
the random numbers maintains it’s order and that we can ensure the call
to seed at the right time. This could quickly become a headache.

To simplify this problem, saltr encapsulates all random generation on
the `generate_*` functions which accept an `id` parameter that works as
a seed.

## Set simulation seed

The `set_simulation_seed` function takes any kind of R object and uses
it internally to set a seed, meaning it works really similarly to how
the `set.seed` function works. The function also returns its value so it
can be saved explicitly.

``` r
id <- "my simulation id"

seed1 <- saltr::set_simulation_seed(id) # seed1: 941561063
A <- runif(10)

seed2 <- saltr::set_simulation_seed(id)
B <- runif(10)

all(A==B) # The values generated are the same
#> [1] TRUE
```

### Using it with the `generate` functions

The `set_simulation_seed` is not really meant to be called directly,
it’s meant to be used within the `generate_*` functions to create
reproducible pipelines.

``` r
# Generating to random samples
A <- saltr::generate_sample(100, 3)
B <- saltr::generate_sample(100, 3)

# These samples are going to be different
all(A==B)
#> [1] FALSE
```

``` r
# We can generate samples passing an id which will ensure the same result as it acts as a seed.
A <- saltr::generate_sample(100, 3, id=id)
B <- saltr::generate_sample(100, 3, id=id)

# Equivalent samples
all(A==B)
#> [1] TRUE
```

## Reproducing *some* cases

The powerful use case of this logic is that we can run simulations and
save the ids which we used to generate the corresponding sample of
examinees and responses without the need to run the whole simulation
again. This means that we can ensure we generate the same conditions
without needing to replicate the original processing.

### Running a simulation study

Let’s create a mock-up simulation study to see this idea in action. We
will run some simulations using a fixed Q-matrix and test.

``` r
qmatrix <- create_qmatrix(3, 4)
item_probs <- matrix(rep(c(.4,.6), each=12), ncol=2)
test <- create_test(item_probs)
```

We define a simulation function to simplify the process. This function
only returns the estimated parameters under our defined model, *it
doesn’t save any information about the sample or the responses*. Also,
note that we define an argument for the simulation id that we are then
passing to the `generate_*` functions which we can later use to
reconstruct the sample and the responses.

``` r
simulation_function <- function(qmatrix, test, simulation_id) {

  # Generating the responses with the id argument for reproducibility
  sample <- generate_sample(1000, 3, id=simulation_id)
  responses <- generate_responses(qmatrix, sample, test, id=simulation_id)
  
  # Fitting the model
  model <- CDM::gdina(responses, qmatrix, linkfct = "logit", progress = FALSE)
  
  # Extracting the results 
  results <- extract_item(model, qmat = qmatrix, test = test) %>%
    mutate(id = simulation_id) %>% 
    select(-partype.attr)
}
```

Now, we will run the simulation 10 times:

``` r
set.seed(314)

# We generate the ids for the simulation.
ids <- saltr::generate_id(10)

# We run the simulations
results <- lapply(ids, function(id) simulation_function(qmatrix, test, id))
results_df <- do.call("rbind",results)
```

On the results we can see that the standard error of the main effect of
item 10 in one of the simulations is really big and the estimation is
way off of the real value.

``` r
results_df %>% 
  filter(item == "Item10", type==1) %>% 
  arrange(desc(se)) %>% 
  head()
#> # A tibble: 6 × 6
#>   item    type  real   est    se id                              
#>   <chr>  <dbl> <dbl> <dbl> <dbl> <chr>                           
#> 1 Item10     1 0.811 7.54  5.17  d917dc543060d21da589c1e3686ab4b3
#> 2 Item10     1 0.811 1.42  0.254 f22cc59b0a4e872e9d645b54749c9db9
#> 3 Item10     1 0.811 1.04  0.219 967475ea9ec41b0e8c51f30c02c82dc3
#> 4 Item10     1 0.811 1.71  0.171 332ada5d1554e2ccf3d6c57a0629e31f
#> 5 Item10     1 0.811 0.347 0.165 6507423e3347e3b1a1c612c28be8cb72
#> 6 Item10     1 0.811 0.854 0.164 736e3d6d3d6e5a92b8c34ba10e8dd010
```

Because we have the id with which the random elements of the simulation
were generated, we can use it to reconstruct them for further
inspection. But, first, let’s make sure that we actually get the same
results if we run the model “manually”.

``` r
# id of the simulation with a high estimation
reproduce_id <- "d917dc543060d21da589c1e3686ab4b3"

# We reconstruct the sample and responses passing the id
sample <- generate_sample(1000, 3, id=reproduce_id)
responses <- generate_responses(qmatrix, sample, test, id=reproduce_id)

# We fit the model to the responses
model <- CDM::gdina(responses, qmatrix, linkfct = "logit", progress = FALSE)

# We check that the values obtained are the same that we saw from the simulations
model$item[20,] 
#>     link   item itemno partype  rule      est       se partype.attr
#> 20 logit Item10     10       1 GDINA 7.544344 5.174643        Attr3
```

Now we can inspect the sample and the responses to see what could have
led to this estimation. For example, seeing the correlation between the
mastery of attribute 3 and the response in that item or a contingency
table. We can even fit another model or run statistical test to see if
we keep or not that result.

``` r
mastery <- sample[, 3]
responses <- responses[,"Item10"]

table(mastery, responses)
#>        responses
#> mastery   0   1
#>       0 295 201
#>       1 216 288

cor.test(mastery, responses)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  mastery and responses
#> t = 5.3252, df = 998, p-value = 1.246e-07
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.1053137 0.2258870
#> sample estimates:
#>       cor 
#> 0.1662215
```
