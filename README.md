
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaSVR: Support Vector Regression with Metaheuristic Algorithms Optimization

<!-- badges: start -->
<!-- badges: end -->

metaSVR is an R Package to integrates Support Vector Regression (SVR)
with various metaheuristic algorithms for automated hyperparameter
tuning which is: `cost`, `gamma`, and `epsilon`.

This package can be used to do prediction using SVR for either small or
big dataset and to do task where required optimal performance, such as
forecasting renewable energy production, forecasting air pollutant, or
modelling any regression that involve nonlinear patterns.

## Features

Features provided in this package include:

- Hybrid of SVR with several **metaheuristic algorithms** such as:
  - Archimedes Optimization (AO)
  - Coot Bird Optimization (CBO)
  - Combined Archimedes Optimization with Coot Bird Optimization (AOCBO)
  - Harris Hawks Optimization (HHO)
  - Grey Wolf Optimizer (GWO)
  - Ant Lion Optimization (ALO)
  - Enhanced Harris Hawks Optimization with Coot Bird Optimization
    (EHHOCBO)
- SVR can be customized by kernel: `radial`, `polynomial`, or `sigmoid`
- Multiple objective functions supported: `sMAPE`, `MAPE`, `RMSE`, `MAE`
- Flexible hyperparameter search space for `cost`, `gamma`, and
  `epsilon`
- Built-in support for min-max normalization and automatic
  denormalization
- Outputs include best parameter set, iteration history, time used, and
  evaluation metrics

## Dependencies

Features provided in this package include:

- **e1071** (for Support Vector Regression Modelling and Kernel)
- **stats** (for prediction and generate random number)
- **hms** (for calculating optimization duration)

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

# Access Results
result$best_params # show the best parameter result
result$total_iter # show total iteration to optimize
result$model # SVR model optimizing by metaheuristic algorithms
result$time # Time used to generate optimization
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

You can also check the full manual on:
<https://s.stis.ac.id/dokumentasi-metaSVR>.

## Authors and Contributors

This package was developed by:

- Rechtiana Putri Arini
- Robert Kurniawan

## Citation

To cite the metaSVR package, please use:

Arini, Rechtiana Putri & Kurniawan, Robert. (2025). metaSVR: Support
Vector Regression with Metaheuristic Algorithms Optimization. R package
version 0.1.0. <https://github.com/rechtianaputri/metaSVR>.

## References

- Setiawan, I. N., Kurniawan, R., Yuniarto, B., Caraka, R. E., &
  Pardamean, B. (2021). Parameter Optimization of Support Vector
  Regression Using Harris Hawks Optimization. Procedia Computer Science,
  196, 175–182.
- Mirjalili, S. (2015). The Ant Lion Optimizer. Advances in Engineering
  Software, 83, 80-98.
- Hashim, F. A., Hussain, K., Houssein, E. H., Mabrouk, M. S., &
  Al-Atabany, W. (2021). Archimedes Optimization Algorithm: A New
  Metaheuristic Algorithm for Solving Optimization Problems. Applied
  Intelligence, 51(3), 1531–1551.
- Naruei, I., & Keynia, F. (2021). A New Optimization Method Based on
  COOT Bird Natural Life Model. Expert Systems with Applications, 183.
- Cui, H., Guo, Y., Xiao, Y., Wang, Y., Li, J., Zhang, Y., & Zhang, H.
  (2023). Enhanced Harris Hawks Optimization Integrated with Coot Bird
  Optimization for Solving Continuous Numerical Optimization Problems.
  CMES - Computer Modeling in Engineering and Sciences, 137(2),
  1635–1675.
- Mirjalili, S., Mirjalili, S. M., & Lewis, A. (2014). Grey wolf
  optimizer. Advances in engineering software, 69, 46-61.
- Heidari, A. A., Mirjalili, S., Faris, H., Aljarah, I., Mafarja, M., &
  Chen, H. (2019). Harris hawks optimization: Algorithm and
  applications. Future generation computer systems, 97, 849-872.
