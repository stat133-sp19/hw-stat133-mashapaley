
# Description: private checker function to test prob
# Inputs
#   prob: probability input
# Ouput
#   TRUE or error if needed
check_prob <- function(prob) {
  if (length(prob) != 1 | is.numeric(prob) == 0) {
    stop("\n'prob' must be a numeric value of length 1")
  }
  if (prob < 0 | prob > 1) {
    stop("\n'prob' must be a number between 0 and 1")
  }
  TRUE
}

# Description: private checker function to test trials
# Inputs
#   trials: trials input
# Output
#   TRUE or error if needed
check_trials <- function(trials) {
  if (length(trials) != 1 | is.numeric(trials) == 0) {
    stop("\ninvalid 'trials' value")
  }
  if (trials < 0) {
    stop("\ninvalid 'trials' value")
  }
  TRUE
}

# Description: private checker function to test success
# Inputs
#   success: success input
#   trials: trials input
# Output
#   TRUE or error if needed
check_success <- function(trials, success) {
  if (any(is.na(success))) {
    stop("\ninvalid 'success' value")
  }
  if (any(success < 0) | is.numeric(success) == 0 | is.vector(success) == 0) {
    stop("\ninvalid 'success' value")
  }
  if (any(success > trials)) {
    stop("\n'success' cannot be greater than 'trials'")
  }
  TRUE
}

# Description: private auxiliary function for mean
# Inputs
#   trials: trials input
#   prob: probability input
# Output
#   binomial distribution mean(s)
aux_mean <- function(trials, prob) {
  trials * prob
}

# Description: private auxiliary function for variance
# Inputs
#   trials: trials input
#   prob: probability input
# Output
#   binomial distribution variance(s)
aux_variance <- function(trials, prob) {
  trials * prob * (1 - prob)
}

# Description: private auxiliary function for mode
# Inputs
#   trials: trials input
#   prob: probability input
# Output
#   binomial distribution mode(s)
aux_mode <- function(trials, prob) {
  if ((trials * prob + prob)%%1 == 0) {
    mode <- c(floor(trials * prob + prob), floor(trials * prob + prob) - 1)
  } else {
    mode <- floor(trials * prob + prob)
  }
  return(mode)
}

# Description: private auxiliary function for skewness
# Inputs
#   trials: trials input
#   prob: probability input
# Output
#   binomial distribution skewness
aux_skewness <- function(trials, prob) {
  (1 - 2 * prob) / (sqrt(trials * prob * (1 - prob)))
}

# Description: private auxiliary function for kurtosis
# Inputs
#   trials: trials input
#   prob: probability input
# Output
#   binomial distribution kurtosis
aux_kurtosis <- function(trials, prob) {
  (1 - (6 * prob) * (1 - prob)) / ((trials * prob) * (1 - prob))
}

#' @title Number of combinations
#' @description calculates number of combinations of `k` successes in `n` trials
#' @param n number of trials
#' @param k vector or single number of successes
#' @return number of combinations
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#'
#' bin_choose(6, 2)
bin_choose <- function(n, k) {
  if(any(k > n)) {
    stop("\nk cannot be greater than n")
  }
  factorial(n) / (factorial(k) * factorial(n - k))
}

#' @title Binomial probability
#' @description calculates probability of `k` successes in `n` trials given a binomial distribution
#' @param success vector or single number of successes
#' @param trials number of trials
#' @param prob probability of success
#' @return binomial probability
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
bin_probability <- function(success, trials, prob) {
  check_success(trials, success)
  check_trials(trials)
  check_prob(prob)

  bin_choose(trials, success) * prob^success * (1 - prob)^(trials - success)
}

#' @title Binomial probability distribution
#' @description displays binomial probability distribution for `n` trials with probability `prob`
#' @param trials number of trials
#' @param prob probability of success
#' @return data frame with binomial probability distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob) {
  probs = bin_probability(0:trials, trials, prob)
  successes = 0:trials
  dist <- data.frame(
    success = successes,
    probability = probs
  )
  class(dist) <- c("bindis", "data.frame")
  dist
}

#'@export
plot.bindis <- function(x) {
  barplot(x$probability,
          xlab = "successes",
          ylab = "probabiliy",
          names.arg = x$success)
}

#' @title Cumulative binomial distribution
#' @description displays binomial distrbution with cumulative probabilities
#' @param trials number of trials
#' @param prob probability of success
#' @return data frame with cumulative binomial probability distribution
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  csum <- cumsum(bin_probability(0:trials, trials, prob))
  cdist <- data.frame(
    success = 0:trials,
    probability = bin_probability(0:trials, trials, prob),
    cumulative = csum
  )
  class(cdist) <- c("bincum", "data.frame")
  cdist
}

#' @export
plot.bincum <- function(x) {
  plot(x = x$success,
       y = x$cumulative,
       type = "o",
       xlab = "successes",
       ylab = "probability")
}

#' @title Binomial random variable
#' @description creates an object of class \code{"binvar"}
#' @param trials number of trials
#' @param prob probability of success
#' @return an object of class \code{"binvar"}
#' @export
#' @examples
#' bin_variable(trials = 5, prob = 0.5)
bin_variable <- function(trials, prob) {
  variable <- list(
    trials = trials,
    prob = prob
  )
  class(variable) <- "binvar"
  variable
}

#' @export
print.binvar <- function(x, ...) {
  cat('"Binomial variable"\n')
  cat('\n')
  cat("Parameters", "\n")
  cat("- number of trials:", x$trials, "\n")
  cat("- prob of success :", x$prob, "\n")
  invisible(x)
}

#' @export
summary.binvar <- function(x) {
  summary <- list(
    trials = x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials, x$prob),
    variance = aux_variance(x$trials, x$prob),
    mode = aux_mode(x$trials, x$prob),
    skewness = aux_skewness(x$trials, x$prob),
    kurtosis = aux_kurtosis(x$trials, x$prob)
  )
  class(summary) <- "summary.binvar"
  summary
}

#' @export
print.summary.binvar <- function(x, ...) {
  cat('"Summary Binomial"\n')
  cat('\n')
  cat("Parameters", "\n")
  cat("- number of trials:", x$trials, "\n")
  cat("- prob of success :", x$prob, "\n")
  cat("\n")
  cat("Measures", "\n")
  cat("- mean    :", x$mean, "\n")
  cat("- variance:", x$variance, "\n")
  cat("- mode    :", x$mode, "\n")
  cat("- skewness:", x$skewness, "\n")
  cat("- kurtosis:", x$kurtosis, "\n")
}

#' @title Binomial mean
#' @description calculates mean of binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return mean of binomial distribution
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_mean(trials, prob)
}

#' @title Binomial variance
#' @description calculates variance of binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return variance of binomial distribution
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_variance(trials, prob)
}

#' @title Binomial mode
#' @description calculates mode(s) of binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return mode(s) of binomial distribution
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_mode(trials, prob)
}

#' @title Binomial skewness
#' @description calculates skewness of binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return skewness of binomial distribution
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_skewness(trials, prob)
}

#' @title Binomial kurtosis
#' @description calculates kurtosis of binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return kurtosis of binomial distribution
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  aux_kurtosis(trials, prob)
}
