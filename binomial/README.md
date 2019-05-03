<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

`"binomial"` is an [R](http://www.r-project.org/) package 
that provides functions to calculate the probability, measures, and distributions of binomial random variables.

* `bin_choose()` calculates the binomial coefficient
* `bin_probability()` calculates binomial probabilities
* `bin_distribution()` displays the binomial probability distribution of class `"bindis"`
* `bin_cumulative()` displays the cumulative binomial probability distribution of class `"bincum"`
* `plot()` method for `"bindis"` and `"bincum"` objects to plot the probability distribution
* `bin_variable()` creates a binomial random variable of class `"binvar"`
* `summary()` method for a `"binvar"` object
* `bin_mean()` (and others of same structure) calculates individual measures of binomial random variable


## Motivation

This package has been developed as part of the [Stat 133](https://github.com/ucb-stat133) course at UC Berkeley as an exercize in R package creation.


## Installation

Install the development version from GitHub via the package `"devtools"`:

```r
# development version from GitHub:
#install.packages("devtools") 

# install "cointoss" (without vignettes)
devtools::install_github("stat133-sp19/hw-stat133-mashapaley/binomial")

# install "cointoss" (with vignettes)
devtools::install_github("stat133-sp19/hw-stat133-mashapaley/binomial", build_vignettes = TRUE)
```


## Usage

```{r}
library(binomial)

# binomial coeffcient
bin_choose(5, 2)

# binomial probability
bin_probability(success = 2, trials = 5, prob = 0.5)

# binomial distribution
dis1 <- bin_distribution(trials = 5, prob = 0.5)
dis1

# plot distribution
plot(dis1)

# cumulative binomial distribution
dis2 <- bin_cumulative(trials = 5, prob = 0.5)
dis2

# plot cumulative distribution
plot(dis2)

# binomial random variable
var1 <- bin_variable(trials = 10, p = 0.3)
var1

# summary
summary(var1)

# individual measure (variance)
bin_variance(10, 0.3)
```
