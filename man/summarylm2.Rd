% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarylm2.R
\name{summarylm2}
\alias{summarylm2}
\title{summarylm2}
\usage{
summarylm2(output)
}
\arguments{
\item{output}{return of of lm2.}
}
\value{
a list that contains the following values: call, residuals, coefficients, sigma, df, r.squared, adj.r.squared, fstatistic, cov.unscaled, F.pval
}
\description{
Summarize fit for linear regression models
}
\examples{
lm2output<-lm2(formula = quality ~ free.sulfur.dioxide:pH + pH, data = wine)
summarylm2(lm2output)
example <- summarylm2(lm2output)
example$sigma
example$coefficients

}
