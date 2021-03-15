hw4
========
<!-- badges: start -->
[![Travis build status](https://travis-ci.com/gnbosma/hw4.svg?branch=master)](https://travis-ci.com/gnbosma/hw4)
[![Codecov test coverage](https://codecov.io/gh/gnbosma/hw4/branch/master/graph/badge.svg)](https://codecov.io/gh/gnbosma/hw4?branch=master)
<!-- badges: end -->

This package is created for the purpose of BIOSTAT 625 HW4 and is intended to execute linear regressions on a specified data frame, mimicing the more commonly known lm function.

## Install 
with `devtools`:

```S
devtools::install_github("gnbosma/hw4")
```

## Use 
This R Package contains two functions, lm2 and summarylm2. Call lm2 if you are interested in obtaining residuals, rank, fitted values, degrees freedom, coefficients, or predicted values of a linear model. Call summarylm2 if you are interested in obtaining sigma value, (adjusted) R squared values, F-statistics, p-values or covariance structures.
