% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binomial.R
\name{bin_distribution}
\alias{bin_distribution}
\title{Binomial probability distribution}
\usage{
bin_distribution(trials, prob)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{probability of success}
}
\value{
data frame with binomial probability distribution
}
\description{
displays binomial probability distribution for `n` trials with probability `prob`
}
\examples{
bin_distribution(trials = 5, prob = 0.5)
}
