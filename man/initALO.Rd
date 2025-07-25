% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ALO_opt.R
\name{initALO}
\alias{initALO}
\title{Initialize Position on Ant Lion Optimizer}
\usage{
initALO(N, dim, ub, lb)
}
\arguments{
\item{N}{An integer indicate population size.}

\item{dim}{An integer show the number of dimension (parameters) of the problem to optimize. It indicate the number of parameters to be optimized.}

\item{ub}{A numeric vector that show upper bounds of the search space. One value per dimension.}

\item{lb}{A numeric vector that show lower bounds of the search space. One value per dimension.}
}
\value{
A numeric matrix of shape \code{(N, dim)} representing initialized positions.
}
\description{
This function generates the initial position of antlions and ants within the defined upper and lower bound in every dimension.
}
\note{
This function used inside ALO function for initialization process.
}
\keyword{internal}
