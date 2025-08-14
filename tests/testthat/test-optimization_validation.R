# 1. ALO Testing Validation Testing
test_that("ALO throws error for invalid population size", {
  expect_error(ALO(N = -10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("ALO throws error for non-integer Max_iter", {
  expect_error(ALO(N = 10, Max_iter = 10.5, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("ALO throws error if lb do not match dim", {
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("ALO throws error if ub do not match dim", {
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(-5, -5, -5), ub = c(5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("ALO throws error if lb >= ub", {
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(1, 5), ub = c(1, 4), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("ALO throws error if fobj does not return numeric scalar", {
  f_bad <- function(x) return(c(1, 2, 3))  # Not scalar
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = f_bad))
})
test_that("ALO throws error if fobj is not a function", {
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = 123))
})
test_that("ALO throws error for non-numeric dim", {
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = "2", fobj = function(x) sum(x^2)))
})
test_that("ALO throws error for non-integer dim", {
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = 2.5, fobj = function(x) sum(x^2)))
})
test_that("ALO throws error for negative or zero dim", {
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = 0, fobj = function(x) sum(x^2)))
  expect_error(ALO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = -1, fobj = function(x) sum(x^2)))
})

# 2. AO Testing Validation
test_that("AO throws error for invalid population size", {
  expect_error(AO(N = -10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("AO throws error for non-integer Max_iter", {
  expect_error(AO(N = 10, Max_iter = 10.5, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("AO throws error if lb do not match dim", {
  expect_error(AO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("AO throws error if ub do not match dim", {
  expect_error(AO(N = 10, Max_iter = 50, lb = c(-5, -5, -5), ub = c(5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("AO throws error if lb >= ub", {
  expect_error(AO(N = 10, Max_iter = 50, lb = c(1, 5), ub = c(1, 4), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("AO throws error if fobj does not return numeric scalar", {
  f_bad <- function(x) return(c(1, 2, 3))  # Not scalar
  expect_error(AO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = f_bad))
})
test_that("AO throws error if fobj is not a function", {
  expect_error(AO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = 123))
})
test_that("AO throws error for non-numeric dim", {
  expect_error(AO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = "2", fobj = function(x) sum(x^2)))
})
test_that("AO throws error for non-integer dim", {
  expect_error(AO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = 2.5, fobj = function(x) sum(x^2)))
})
test_that("AO throws error for negative or zero dim", {
  expect_error(AO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = 0, fobj = function(x) sum(x^2)))
  expect_error(AO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = -1, fobj = function(x) sum(x^2)))
})

# 3. AOCBO Testing Validation
test_that("AOCBO throws error for invalid population size", {
  expect_error(AOCBO(N = -10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("AOCBO throws error for non-integer Max_iter", {
  expect_error(AOCBO(N = 10, Max_iter = 10.5, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("AOCBO throws error if lb do not match dim", {
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("AOCBO throws error if ub do not match dim", {
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(-5, -5, -5), ub = c(5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("AOCBO throws error if lb >= ub", {
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(1, 5), ub = c(1, 4), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("AOCBO throws error if fobj does not return numeric scalar", {
  f_bad <- function(x) return(c(1, 2, 3))  # Not scalar
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = f_bad))
})
test_that("AOCBO throws error if fobj is not a function", {
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = 123))
})
test_that("AOCBO throws error for non-numeric dim", {
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = "2", fobj = function(x) sum(x^2)))
})
test_that("AOCBO throws error for non-integer dim", {
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = 2.5, fobj = function(x) sum(x^2)))
})
test_that("AOCBO throws error for negative or zero dim", {
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = 0, fobj = function(x) sum(x^2)))
  expect_error(AOCBO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = -1, fobj = function(x) sum(x^2)))
})

# 4. CBO Testing Validation
test_that("CBO throws error for invalid population size", {
  expect_error(CBO(N = -10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("CBO throws error for non-integer Max_iter", {
  expect_error(CBO(N = 10, Max_iter = 10.5, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("CBO throws error if lb do not match dim", {
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("CBO throws error if ub do not match dim", {
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(-5, -5, -5), ub = c(5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("CBO throws error if lb >= ub", {
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(1, 5), ub = c(1, 4), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("CBO throws error if fobj does not return numeric scalar", {
  f_bad <- function(x) return(c(1, 2, 3))  # Not scalar
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = f_bad))
})
test_that("CBO throws error if fobj is not a function", {
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = 123))
})
test_that("CBO throws error for non-numeric dim", {
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = "2", fobj = function(x) sum(x^2)))
})
test_that("CBO throws error for non-integer dim", {
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = 2.5, fobj = function(x) sum(x^2)))
})
test_that("CBO throws error for negative or zero dim", {
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = 0, fobj = function(x) sum(x^2)))
  expect_error(CBO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = -1, fobj = function(x) sum(x^2)))
})

# 5. EHHOCBO Testing Validation
test_that("EHHOCBO throws error for invalid population size", {
  expect_error(EHHOCBO(N = -10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("EHHOCBO throws error for non-integer Max_iter", {
  expect_error(EHHOCBO(N = 10, Max_iter = 10.5, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("EHHOCBO throws error if lb do not match dim", {
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("EHHOCBO throws error if ub do not match dim", {
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(-5, -5, -5), ub = c(5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("EHHOCBO throws error if lb >= ub", {
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(1, 5), ub = c(1, 4), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("EHHOCBO throws error if fobj does not return numeric scalar", {
  f_bad <- function(x) return(c(1, 2, 3))  # Not scalar
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = f_bad))
})
test_that("EHHOCBO throws error if fobj is not a function", {
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = 123))
})
test_that("EHHOCBO throws error for non-numeric dim", {
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = "2", fobj = function(x) sum(x^2)))
})
test_that("EHHOCBO throws error for non-integer dim", {
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = 2.5, fobj = function(x) sum(x^2)))
})
test_that("EHHOCBO throws error for negative or zero dim", {
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = 0, fobj = function(x) sum(x^2)))
  expect_error(EHHOCBO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = -1, fobj = function(x) sum(x^2)))
})

# 6. GWO Testing Validation
test_that("GWO throws error for invalid population size", {
  expect_error(GWO(N = -10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("GWO throws error for non-integer Max_iter", {
  expect_error(GWO(N = 10, Max_iter = 10.5, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("GWO throws error if lb do not match dim", {
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("GWO throws error if ub do not match dim", {
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(-5, -5, -5), ub = c(5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("GWO throws error if lb >= ub", {
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(1, 5), ub = c(1, 4), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("GWO throws error if fobj does not return numeric scalar", {
  f_bad <- function(x) return(c(1, 2, 3))  # Not scalar
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = f_bad))
})
test_that("GWO throws error if fobj is not a function", {
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = 123))
})
test_that("GWO throws error for non-numeric dim", {
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = "2", fobj = function(x) sum(x^2)))
})
test_that("GWO throws error for non-integer dim", {
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = 2.5, fobj = function(x) sum(x^2)))
})
test_that("GWO throws error for negative or zero dim", {
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = 0, fobj = function(x) sum(x^2)))
  expect_error(GWO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = -1, fobj = function(x) sum(x^2)))
})

# 7. HHO Testing Validation
test_that("HHO throws error for invalid population size", {
  expect_error(HHO(N = -10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("HHO throws error for non-integer Max_iter", {
  expect_error(HHO(N = 10, Max_iter = 10.5, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("HHO throws error if lb do not match dim", {
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("HHO throws error if ub do not match dim", {
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(-5, -5, -5), ub = c(5, 5), dim = 3, fobj = function(x) sum(x^2)))
})
test_that("HHO throws error if lb >= ub", {
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(1, 5), ub = c(1, 4), dim = 2, fobj = function(x) sum(x^2)))
})
test_that("HHO throws error if fobj does not return numeric scalar", {
  f_bad <- function(x) return(c(1, 2, 3))  # Not scalar
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = f_bad))
})
test_that("HHO throws error if fobj is not a function", {
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(-5, -5), ub = c(5, 5), dim = 2, fobj = 123))
})
test_that("HHO throws error for non-numeric dim", {
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = "2", fobj = function(x) sum(x^2)))
})
test_that("HHO throws error for non-integer dim", {
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(-5, -5),
                     ub = c(5, 5), dim = 2.5, fobj = function(x) sum(x^2)))
})
test_that("HHO throws error for negative or zero dim", {
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = 0, fobj = function(x) sum(x^2)))
  expect_error(HHO(N = 10, Max_iter = 50, lb = c(-5),
                     ub = c(5), dim = -1, fobj = function(x) sum(x^2)))
})
