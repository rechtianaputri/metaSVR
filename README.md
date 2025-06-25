
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaSVR

<!-- badges: start -->
<!-- badges: end -->

metaSVR is an R Package to integrates Support Vector Regression (SVR)
with various metaheuristic algorithms for automated hyperparameter
tuning which is: Cost, Gamma, and Epsilon.

This package can be used to do prediction using SVR for small dataset
and to do task where required optimal performance, such as forecasting
renewable energy production, forecasting air pollutant, or modelling any
regression that involve nonlinear patterns.

## Features

Features provided in this package including: - Hybrid of SVR with some
metaheuristic algorithms such as: - Archimedes Optimization (AO) - Coot
Bird Optimization (CBO) - Combined Archimedes Optimization with Coot
Bird Optimization (AOCBO) - Harris Hawks Optimization (HHO) - Grey Wolf
Optimizer (GWO) - Ant Lion Optimization (ALO) - Enhanced Harris Hawks
Optimization with Coot Bird Optimization (EHHOCBO) - SVR can be
customized by kernel, options: radial, polynomial, sigmoid. - Multiple
objective function can be used, options: sMAPE, MAPE, RMSE, MAE -
Flexible hyperparameter search space for cost, gamma, and epsilon -
Built-in data with min-max normalization and automatic denormalization
handling of prediction result. - Output produced best parameter set,
iteration history, time used, and evaluation metrics.

## Installation

You can install the development version of metaSVR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rechtianaputri/metaSVR")
```

## Example

Here is a simple usage example of metaSVR using synthetic data:

``` r
library(metaSVR)

#Generate synthetic data
set.seed(123)
x <- matrix(rnorm(100), ncol = 2)
y <- x[,1] * 3 + rnorm(50)

# Run SVR Hybird with AOCBO
result <- svrHybrid(x_train = x[1:40,], y_train = y[1:40],
                   x_test = x[41:50,], y_test = y[41:50],
                   kernel = "radial", optimizer = "AOCBO",
                   objective = "SMAPE", is.y.normalize = FALSE,
                   max_iter = 100, N = 40)
#> Warning in param[1] <- Xbest: number of items to replace is not a multiple of
#> replacement length
#> At iteration 1 the best fitness is 54.33552

# Access Results
result$best_params # show the best parameter result
#> $cost
#> [1] 80.37593
#> 
#> $gamma
#> [1] 0.003935142
#> 
#> $epsilon
#> [1] 0.1139266
result$total_iter # show total iteration to optimize
#> [1] 39
result$model # SVR model optimizing by metaheuristic algorithms
#> 
#> Call:
#> svm.default(x = x_train, y = y_train, type = "eps-regression", kernel = kernel, 
#>     degree = degree, gamma = best_params$gamma, coef0 = coef0, cost = best_params$cost, 
#>     nu = nu, class.weights = class.weights, cachesize = cachesize, 
#>     tolerance = tolerance, epsilon = best_params$epsilon, shrinking = shrinking, 
#>     cross = cross, probability = probability, fitted = fitted, subset = subset, 
#>     na.action = na.action)
#> 
#> 
#> Parameters:
#>    SVM-Type:  eps-regression 
#>  SVM-Kernel:  radial 
#>        cost:  80.37593 
#>       gamma:  0.003935142 
#>     epsilon:  0.1139266 
#> 
#> 
#> Number of Support Vectors:  26
result$time # Time used to generate optimization
#> 00:00:09.068762
```

## Documentation

Each metaheuristic is implemented as a standalone optimizer, used by
each optimization to improve SVR. For full documentation see the help
files:

``` r
?svrHybrid
?AO
?CBO
?AOCBO
?loss_calculate
```
