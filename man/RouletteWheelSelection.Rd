% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ALO_opt.R
\name{RouletteWheelSelection}
\alias{RouletteWheelSelection}
\title{Roulette Wheel Selection}
\usage{
RouletteWheelSelection(weights)
}
\arguments{
\item{weights}{A numeric vector of weights.}
}
\value{
An integer representing the selected index.
}
\description{
Function used to select an individual index based on fitness-proportional selection (inverse fitness weight).
}
\note{
This function used inside ALO function to probabilistically select antlions for guiding ants.
}
\keyword{internal}
