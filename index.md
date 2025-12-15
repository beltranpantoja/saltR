# saltr [![package logo](reference/figures/saltr_logo.png)](https://beltranpantoja.github.io/saltR/)

The goal of saltR, pronounced as the spanish word *saltar*, is to
simplify the running of simulations in the context of DCMs by providing
utility functions that can make your code more readable and let you jump
straight into the simulation design without worrying too much about the
implementation.

## Why?

Normally, psychometric libraries have functions that generate
simulations, the downside is that it all happens inside a black box.
This library is meant to only provide semantic functions that do one
simple thing at a time.

Another difference with traditional psuchometric libraries is that saltr
tries to be more integrated with the tidyverse packages. For example,
`extract_item` takes the model and returns a tibble for easier work in a
pipeline.

## Get Started

You can install saltr from
[GitHub](https://github.com/beltranpantoja/saltR) with:

``` r
devtools::install_github("beltranpantoja/saltR")
```

And then jump to [Creating a single
simulation](https://beltranpantoja.github.io/saltR/vignettes/Creating-a-single-simulation.Rmd)
tutorial
