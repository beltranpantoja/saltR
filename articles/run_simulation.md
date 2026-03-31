# Running a simulation

## Introduction

This vignette will show you how to create a single simulation using the
basic functions from the package. To create a DCM simulation we need: a
test, a Q-matrix and examinees. We can do all these quickly using saltr.

## Creating our test

Let’s first create a Q-matrix.

``` r
library(saltr)

qmatrix <- create_qmatrix(num_attr=3, items_per_type = c(2,1))
qmatrix
#>       Attr1 Attr2 Attr3
#> Item1     1     0     0
#> Item2     1     0     0
#> Item3     0     1     0
#> Item4     0     1     0
#> Item5     0     0     1
#> Item6     0     0     1
#> Item7     1     1     0
#> Item8     1     0     1
#> Item9     0     1     1
```

The `create_qmatrix` objective is give us a fast template for a Q-matrix
which we then can modify. Let’s make item 9 measure all attributes.

``` r
qmatrix[9,] <- c(1,1,1) 
```

Perfect. So far, so good. Now, we want to create a test that has to be
compatible with our Q-matrix. This is normally a tricky thing to do, but
using `build_test_parameters` the operation is simplified.

``` r
# We pass our qmatrix as an argument 
test <- build_test_parameters(qmatrix)
test
#>       0 1 2 3 1-2 1-3 2-3 1-2-3
#> Item1 1 1 0 0   0   0   0     0
#> Item2 1 1 0 0   0   0   0     0
#> Item3 1 0 1 0   0   0   0     0
#> Item4 1 0 1 0   0   0   0     0
#> Item5 1 0 0 1   0   0   0     0
#> Item6 1 0 0 1   0   0   0     0
#> Item7 1 1 1 0   1   0   0     0
#> Item8 1 1 0 1   0   1   0     0
#> Item9 1 1 1 1   1   1   1     1

# Item 8 has the intercept 0, the main effects 1 and 3, 
# and the two-way interaction 1-3
test[8,]
#>     0     1     2     3   1-2   1-3   2-3 1-2-3 
#>     1     1     0     1     0     1     0     0
```

Because we did not pass any extra arguments, the result of
`build_test_parameters` is just a mask of the parameters that should be
present in the test. The columns represent the attribute parameters. We
can also pass the values we want for each interaction level starting
with the intercept.

``` r
# Only intercept and main effects
test <- build_test_parameters(qmatrix, -2, 2)
test
#>        0  1  2  3 1-2 1-3 2-3 1-2-3
#> Item1 -2  2 NA NA  NA  NA  NA    NA
#> Item2 -2  2 NA NA  NA  NA  NA    NA
#> Item3 -2 NA  2 NA  NA  NA  NA    NA
#> Item4 -2 NA  2 NA  NA  NA  NA    NA
#> Item5 -2 NA NA  2  NA  NA  NA    NA
#> Item6 -2 NA NA  2  NA  NA  NA    NA
#> Item7 -2  2  2 NA   0  NA  NA    NA
#> Item8 -2  2 NA  2  NA   0  NA    NA
#> Item9 -2  2  2  2   0   0   0     0

# Intercept, main effects and two-way interactions
test <- build_test_parameters(qmatrix, -2, 2, .5)
test
#>        0  1  2  3 1-2 1-3 2-3 1-2-3
#> Item1 -2  2 NA NA  NA  NA  NA    NA
#> Item2 -2  2 NA NA  NA  NA  NA    NA
#> Item3 -2 NA  2 NA  NA  NA  NA    NA
#> Item4 -2 NA  2 NA  NA  NA  NA    NA
#> Item5 -2 NA NA  2  NA  NA  NA    NA
#> Item6 -2 NA NA  2  NA  NA  NA    NA
#> Item7 -2  2  2 NA 0.5  NA  NA    NA
#> Item8 -2  2 NA  2  NA 0.5  NA    NA
#> Item9 -2  2  2  2 0.5 0.5 0.5     0
```

In the same way that before, this function greatly simplifies the
creation of a starting template for the test which can be then adjusted.
Notice that the 3-way interaction effect of item 9 is 0 and not NA, this
is important for compatibility with the matrix. For example, let’s make
one of the main effects of items 6 and 7 smaller.

``` r
test[6, 4] <- 1
test[7, 3] <- 1

# Using pretty_print makes it easier to read
pretty_print(test)
#>         λ0      λ1      λ2      λ3     λ1-2    λ1-3    λ2-3   λ1-2-3 
#> Item1    -2.0     2.0                                                
#> Item2    -2.0     2.0                                                
#> Item3    -2.0             2.0                                        
#> Item4    -2.0             2.0                                        
#> Item5    -2.0                     2.0                                
#> Item6    -2.0                     1.0                                
#> Item7    -2.0     2.0     1.0             0.5                        
#> Item8    -2.0     2.0             2.0             0.5                
#> Item9    -2.0     2.0     2.0     2.0     0.5     0.5     0.5     0.0
```

## Creating our examinees

Great! now we have our test we need our examinees. This is a function
that has a random component so we use a `generate_*` function. This
function is a wrapper for the
[`bindata::rmvbin`](https://rdrr.io/pkg/bindata/man/rmvbin.html) with
some extra logic for errors and conciseness. For example, if only one
value is passed, it assumes they are all the same.

``` r
examinees <- generate_examinees(
  1000, # sample size
  total_attrs = 3,
  base_rate = .5, # All three attributes will have the same base-rate
  attr_corr = c(.3, 0, 0)
)

# Checking results
colMeans(examinees)
#> Attr1 Attr2 Attr3 
#> 0.488 0.499 0.502
cor(examinees) |> round(2)
#>       Attr1 Attr2 Attr3
#> Attr1  1.00  0.34  0.02
#> Attr2  0.34  1.00  0.00
#> Attr3  0.02  0.00  1.00
```

An important attribute is `strict`, if it is `FALSE`, the attribute
correlation is assumed to be the correlation of the underlying normal
variables. By default this value is `TRUE`, meaning it will try and
create a binary matrix that follows those restrictions.If not possible
it will throw an error.

``` r
# This will throw an error
sample_1 <- generate_examinees(
  1000, 
  total_attrs = 3,
  base_rate = c(.8, .5, .2),
  attr_corr = c(.8, .1, 0),
  strict = TRUE
)
#> Error in `generate_examinees()`:
#> ! Invalid joint probability of vars 1 and 2. Allowed range: [0.30, 0.50], but got: 0.56

# This wont
sample_2 <- generate_examinees(
  1000, 
  total_attrs = 3,
  base_rate = c(.8, .5, .2),
  attr_corr = c(.8, .1, 0),
  strict = FALSE
)

# Although it is always better to pass some value for tolerance
# To get info on the generation.
sample_3 <- generate_examinees(
  1000, 
  total_attrs = 3,
  base_rate = c(.8, .5, .2),
  attr_corr = c(.8, .1, 0),
  tolerance = .1
)
#> Warning in generate_examinees(1000, total_attrs = 3, base_rate = c(0.8, : 1 correlation values are outside tolerance range
#> Expected: 0.8, 0.1, 0
#> Obtained: 0.439, 0.081, 0.007
```

## Responses

Now that we have a test and our examinees we can generate our responses.
We do not need to pass a Q-matrix as that information is already encoded
in the test. That way there’s less chances of making mistakes.

``` r
responses <- generate_responses(examinees, test)
```

A nice feature of this function is that it can help us avoid mistakes
that could have happened during the modification of the test. For
example, what if we inadvertently added a second main effect on item 2.

``` r
wrong_test <- test

# Now item 2 has a main effect but the interaction 1-2 is NA 
wrong_test[2,3] <- .5

responses <- generate_responses(examinees, wrong_test)
#> Error in `generate_responses()`:
#> ! The test is not properly formed
```

And now we can just fit our model.

## Wrapping it all together

Our complete code to start from zero and get simulated responses to a
test is just a couple of lines!

``` r
qmatrix <- create_qmatrix(num_attr=3, items_per_type = c(2,1))
qmatrix[9,] <- c(1,1,1) 

test <- build_test_parameters(qmatrix, -2, 2, .5)
test[6, 4] <- 1
test[7, 3] <- 1

examinees <- generate_examinees(
  1000, 
  total_attrs = 3,
  base_rate = .5, 
  attr_corr = c(.3, 0, 0)
)

responses <- generate_responses(examinees, test)
```
