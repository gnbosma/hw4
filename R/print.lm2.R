#' print.lm2
#'
#' Formats printing style for lm2 function
#'
#' @param m output of lm2 function
#'
#' @export

print.lm2 <- function(m){
  cat("Call: ", m$call, ' ', "Coefficients: ", sep = '\n')
  print(m$coefficients)
}
