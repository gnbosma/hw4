#' print.summarylm2
#'
#' Formats printing style for summary.lm2 function
#'
#' @param m output of summarylm2 function

print.summarylm2 <- function(m){
  pvalues <- as.vector(m$coefficients[,4])
  sig <- ifelse (pvalues <0.001, '***',
                 ifelse (pvalues <0.01, '** ',
                         ifelse(pvalues < 0.05, '*  ',
                                ifelse(pvalues < 0.1, '.  ', ''))))

  cat("Call: ", m$call, ' ',  "Residuals: ", sep = '\n')
  print(m$residuals)
  cat(" ", "Coefficients: \n")
  print(cbind(m$coefficients, sig))
  cat("---\n")
  sig.codes <- "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"

  msg <- paste(c("Residual standard error: ", m$s, " on ", m$df[2], " degrees of freedom \nMultiple R-Squared: ",
                 m$r.squared, ", Adjusted R-squared: ", m$adj.r.squared, "\nF-Statistic: ", m$fstat, " on ", m$df[3], " and ", m$df[2],
                 " DF, p-value: ", m$F.pval), collapse = '')
  cat(msg)
}
