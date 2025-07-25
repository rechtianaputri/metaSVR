% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HHO_opt.R
\name{HHO}
\alias{HHO}
\title{Harris Hawks Optimization}
\usage{
HHO(N, Max_iter, lb, ub, dim, fobj)
}
\arguments{
\item{N}{An integer indicate population size.}

\item{Max_iter}{An integer indicate maximum number of iterations.}

\item{lb}{A numeric vector that show lower bounds of the search space. One value per dimension.}

\item{ub}{A numeric vector that show upper bounds of the search space. One value per dimension.}

\item{dim}{An integer show the number of dimension (parameters) of the problem to optimize. It indicate the number of parameters to be optimized.}

\item{fobj}{An objective function used to be minimized. It is return single numeric value that show evaluation matrix result in every iteration.
It used to calculate the best fitness in every iteration.}
}
\value{
A list containing:
\describe{
  \item{best_fitness}{The best (minimum) fitness value found.}
  \item{best_position}{The parameter vector (position) corresponding to the best fitness.}
  \item{jml_iter}{The number of iterations executed.}
  \item{param}{Matrix of best parameters found across every iterations (dim × iter).}
  \item{param_list}{Vector of best fitness values at each iteration.}
}
}
\description{
An algorithm built by Heidari et al. (2019) that inspired by the movement of Harris Hawks on cooperative hunting behaviour
to optimized real-valued objective function in continous search space in a population-based manner.
}
\details{
There are two phase of Harris Hawks hunting, namely exploration and exploitation that will be modelized to find optimization result.
The movement used in this algorithms such as: exploration phase; transition between exploitation and exploration phase; and exploitation phase
that has 4 different strategies based on E and r (soft besiege, hard besiege, soft besiege with progressive rapid, and hard besiege with progressive rapid).

The algorithm performs until maximum iteration reached or convergence condition when the difference
in objective values for ten consecutive times is less than 10^-5.
}
\note{
The input vectors `lb` and `ub` must have the same length as the number of dimensions `dim`.

This optimization function used inside svrHybrid function.
}
\examples{
\dontrun{
sphere_fn <- function(x) sum(x^2) # simple function for objective function

# HHO optimization
set.seed(123)
result <- HHO(N = 20, Max_iter = 50, lb = [-5,-5,-5], ub = [5,5,5], dim = 3, fobj = sphere_fn)

# View best fitness and position found
result$best_fitness
result$best_position
}

}
\references{
Heidari, A. A., Mirjalili, S., Faris, H., Aljarah, I., Mafarja, M., & Chen, H. (2019).
Harris hawks optimization: Algorithm and applications. Future generation computer systems, 97, 849-872.
https://doi.org/10.1016/j.future.2019.02.028
}
