#'summarylm2
#'
#'Summarize fit for linear regression models
#'
#'@param output return of of lm2.
#'
#'@return a list that contains the following values: call, residuals, coefficients, sigma, df, r.squared, adj.r.squared, fstatistic, cov.unscaled, F.pval
#'
#'@examples
#'lm2output<-lm2(formula = quality ~ free.sulfur.dioxide:pH + pH, data = wine)
#'summarylm2(lm2output)
#'example <- summarylm2(lm2output)
#'example$sigma
#'example$coefficients
#'
#'@export

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

  pvalues <- as.vector(output$cf[,4])
  sig <- ifelse (pvalues <0.001, '***',
                 ifelse (pvalues <0.01, '** ',
                         ifelse(pvalues < 0.05, '*  ',
                                ifelse(pvalues < 0.1, '.  ', ''))))

  cat("Call: ", output$call, ' ',  "Residuals: ", sep = '\n')
  print(resid.table)
  cat(" ", "Coefficients: \n")
  print(cbind(output$cf, sig))
  cat("---\n")
  sig.codes <- "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"

  msg <- paste(c("Residual standard error: ", sumoutput$s, " on ", sumoutput$df[2], " degrees of freedom \nMultiple R-Squared: ",
                 sumoutput$r.squared, ", Adjusted R-squared: ", sumoutput$adj.r.squared, "\nF-Statistic: ", sumoutput$fstat, " on ", sumoutput$df[3], " and ", sumoutput$df[2],
                 " DF, p-value: ", sumoutput$F.pval), collapse = '')
  cat(msg)

  return(invisible(sumoutput))
}
