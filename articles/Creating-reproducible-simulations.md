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
id <- "my-simulation-id" # Can be any kind of hashable R object

# We can generate samples passing an id which will ensure the same result as it acts as a seed.
A <- saltr::generate_sample(100, 3, id=id)
B <- saltr::generate_sample(100, 3, id=id)

# Equivalent samples
all(A==B)
#> [1] TRUE
```

An additional advantage of using an id, is that the function is called
using
[`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
which makes all the random calls be local and not affect the randomness
of the rest of the code.

``` r
set.seed(314)
v1 <- runif(1)

set.seed(314)
A <- saltr::generate_sample(100, 3, id=id)
B <- saltr::generate_sample(100, 3, id=id)
v2 <- runif(1)

all(v1 == v2)
#> [1] TRUE
```

## Warning

The possible downside of doing this, is that the if we pass the same id
to the functions they may be generating the same random numbers as they
will have effectively the same seed and lead to hard to debug problems.
To avoid this, we recommend to generate a single simulation id and use
that to generate sub-ids using `generate_ids`.

``` r
simulation_id <- "my-general-simulation-id"
saltr::generate_ids(2, simulation_id)
#> [1] "ed18e2b156edb99dfef4ecd97fdd4bab" "70318f99aa32fbb9440133945a6a4fe3"
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

  # To avoid generating the same numbers 
  ids <- saltr::generate_ids(2, simulation_id)
  
  # Generating the responses with the id argument for reproducibility
  sample <- generate_sample(1000, 3, id=ids[1])
  responses <- generate_responses(qmatrix, sample, test, id=ids[2])
  
  # Fitting the model
  model <- CDM::gdina(responses, qmatrix, linkfct = "logit", progress = FALSE)
  
  # Extracting the results 
  results <- extract_item(model, qmat = qmatrix, test = test) 
  results$id <- simulation_id
  return(results)
}
```

Now, we will run the simulation 10 times:

``` r
set.seed(314)

# We generate the ids for the simulation.
ids <- saltr::generate_ids(10)

# We run the simulations
results <- lapply(ids, function(id) simulation_function(qmatrix, test, id))
results_df <- do.call("rbind",results)
```

On the results we can see that the standard error of the main effect of
item 10 in one of the simulations is really big and the estimation is
way off of the real value.

``` r
item_10 <- results_df[results_df$item=="Item10", ]

# Row 75 has a high value
item_10[item_10$type == 1, c("real", "est", "se", "id")]
#>          real       est        se                               id
#> 3   0.8109302 0.9326978 0.1471117 4d7f3b82d3a2a601f53e6b9f0cc54b27
#> 27  0.8109302 1.4951888 0.1375087 d917dc543060d21da589c1e3686ab4b3
#> 51  0.8109302 0.5392927 0.1322969 967475ea9ec41b0e8c51f30c02c82dc3
#> 75  0.8109302 5.3798600 1.0983974 428e1f425671e000254ba74d2884ba7b
#> 99  0.8109302 1.7902512 0.2725193 6507423e3347e3b1a1c612c28be8cb72
#> 123 0.8109302 0.8090937 0.1294268 f22cc59b0a4e872e9d645b54749c9db9
#> 147 0.8109302 0.4636806 0.1416791 d5226c00eefe9d80b774bef762be2210
#> 171 0.8109302 0.6633324 0.1284041 332ada5d1554e2ccf3d6c57a0629e31f
#> 195 0.8109302 1.0508208 0.1746198 5e5bce2398d2bbd1e3a8d28ca01b81ce
#> 219 0.8109302 4.5923011 0.8676384 736e3d6d3d6e5a92b8c34ba10e8dd010
```

Because we have the id with which the random elements of the simulation
were generated, we can use it to reconstruct them for further
inspection. But, first, let’s make sure that we actually get the same
results if we run the model “manually”.

``` r
# id of the simulation with a high estimation
reproduce_id <- saltr::generate_ids(2, "428e1f425671e000254ba74d2884ba7b")

# We reconstruct the sample and responses passing the respective id
sample <- generate_sample(1000, 3, id=reproduce_id[1])
responses <- generate_responses(qmatrix, sample, test, id=reproduce_id[2])

# We fit the model to the responses
model <- CDM::gdina(responses, qmatrix, linkfct = "logit", progress = FALSE)

# We check that the values obtained are the same that we saw from the simulations
model$item[20,] 
#>     link   item itemno partype  rule     est       se partype.attr
#> 20 logit Item10     10       1 GDINA 5.37986 1.098397        Attr3
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
#>       0 288 200
#>       1 197 315

cor.test(mastery, responses)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  mastery and responses
#> t = 6.6313, df = 998, p-value = 5.444e-11
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.1452888 0.2640618
#> sample estimates:
#>       cor 
#> 0.2054316
```
