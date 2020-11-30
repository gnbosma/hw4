#'summarylm2
#'
#'Summarize fit for linear regression models
#'
#'@param output return of of lm2.
#'
#'@return a list that contains the following values: call, residuals, coefficients, sigma, df, r.squared, adj.r.squared, fstatistic, cov.unscaled, F.pval
#'
#'@export
#'
summarylm2 <- function(output){
  x <- output$data.frame
  n <- nrow(x)
  p <- ncol(x)
  y <- output$y
  SSE= sum(output$resid^2)
  SSyy=sum((y-mean(y))^2)
  s <- sqrt( SSE/(n-p))

  resid <- output$residuals
  resid.table <- quantile(resid)
  names(resid.table) <- c("Min", "1Q", "Median", "3Q", "Max")

  df <- c(ncol(x), n-p, p)
  vcov <- solve(t(x) %*% x)*s^2

  # Standard Error, R-Squared, F-Stat
  R2 <- 1- sum(resid^2) / SSyy
  R2adj <- 1 - (1-R2)* (n-1)/(n-p)
  Fstat <- ((SSyy-SSE)/(p-1)) / (SSE/(n-p))
  Fstatdf <- c(Fstat, p-1, n-p)
  names(Fstatdf) <- c("value", "numdf", "dendf")
  fpval <- pf(Fstat, p-1, n-p-1, lower.tail = FALSE)

  sumoutput <- list(output$call, resid.table, output$cf, s, df, R2, R2adj, Fstat, vcov/s^2, fpval)
  names(sumoutput) <- c("call", "residuals", "coefficients", "sigma", "df",
                        "r.squared", "adj.r.squared", "fstatistic", "cov.unscaled", "F.pval")

  class(sumoutput) <- "summarylm2"
  return(sumoutput)
}
