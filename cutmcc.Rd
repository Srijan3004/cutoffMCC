\name{cutoffbasedonMCC}
\alias{cutoffbasedonMCC}
\title{This package takes a data, takes a n, takes the coefficient of the fitted model and takes the output observed, then for each i in 1:n gives the best cutoff based on MCC. Valid only for binary classification. The cutoff is below 0.5, otherwise you can just reverse.}
\usage{
function(n,data,beta,k)
}
\description{
             n = maximum number of data points for which we want to check
             data = data
             beta = coefficient of the fitted model
             output = observed binary output for the testing data
}

}
