#' print.lm2
#'
#' Formats printing style for lm2 function
#'
#' @method print lm2
#'
#' @export

print.lm2 <- function(m){
  cat("Call: ", m$call, ' ', "Coefficients: ", sep = '\n')
  print(m$coefficients)
}
