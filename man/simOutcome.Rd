% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{simOutcome}
\alias{simOutcome}
\alias{is_simOutcome}
\title{Create an Instance of the simOutcome Class.}
\usage{
simOutcome(x, type = c("regression", "binary", "multi_class"), ...)

is_simOutcome(x)
}
\arguments{
\item{x}{a data frame with the columns \code{outcome} and \code{predicted}.}

\item{type}{type of the classifier: \code{regression} (default), \code{binary}
or \code{multi_class}.}

\item{...}{extra arguments, currently none.}
}
\description{
Creates an object of the \code{simOutcome} class.
}
\details{
Technically a data frame with two columns, \code{outcome} and \code{predicted}, which
store the actual and model-predicted values of the outcome variable.
It has and additional attribute \code{type} which specifies the type of the
simulated model (\code{regression}, \code{binary} or \code{multi_class}).
The method \code{summary()} was defined for the \code{simOutcome} class which returns
a data frame with fit statistics appropriate for the type of the model.
}
