#'lm2
#'
#'Used fit linear regression models
#'
#'@param formula an object of class "formula" that models regression to be fit
#'
#'@param data data frame to perform the linear regression on
#'
#'@param na.action character input that determines how the regression model should handle missing data. Options include 'omit' (remove rows with NA values), 'fail'(halt regression), or 'mean impute' (replace NA values with column mean)
#'
#'@return a list that contains the following values: residuals, rank, fitted.values, df.residual, coefficients, call, model, data.frame, cf, y
#'
#'@export

lm2 <- function(formula, data, na.action = 'omit'){
  #obtain indexes of which columns / covariates to keep
  covariates <- all.vars(formula)
  index <- rep(0,length(covariates))
  for (i in 1:length(covariates)){
    index[i] <- which(colnames(data) == covariates[i])
  }
  data <- data[,index]

  # Handle missing values
  if (anyNA(data) == TRUE){
    if (na.action == 'omit'){
      data <- na.omit(data)
    } else if (na.action == 'fail'){
      stop("Dataset contains missing values.")
    } else if (na.action == 'mean impute'){
      for(i in 1:ncol(data)){
        data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
      }
    }
  }

  y <- as.matrix(data[,which(colnames(data) == all.vars(formula)[1])])
  x <- model.matrix(formula, data = data)
  n <- nrow(x)
  p <- ncol(x)

  # Betas
  colnames(x)[1] <- '(Intercept)'
  betas <- solve(t(x) %*% x) %*% t(x) %*% y
  fitted <- as.vector(as.vector(x %*% betas))
  names(fitted) <- 1:nrow(x)

  # Variance & Residuals
  resid <- as.vector(y - x %*% betas)
  names(resid) <- c(1:nrow(x))
  SSyy=sum((y-mean(y))^2)
  SSE=sum(resid^2)
  s <- sqrt( SSE/(n-p))
  ses <- sqrt(diag(solve(t(x) %*% x)))*s

  # T scores and P-values
  tscores <- betas / ses
  pvalues <- 2*pt(abs(tscores),length(y) - ncol(x),lower.tail=FALSE)
  coefs <- data.frame("Estimate" = betas, "Std. Error" = ses,
                      "t value" = tscores, "Pr(>|t|)" = pvalues, check.names = FALSE)

  call <- noquote(paste(c('lm(', formula, ')'), collapse = ''))

  output <- list(resid, nrow(betas), fitted, n-p, betas[,1], call, data, x, coefs, y)
  names(output) <- c("residuals", "rank", "fitted.values", "df.residual", "coefficients", "call", "model", "data.frame", "cf", "y")
  class(output) <- "lm2"
  return(output)
}

#'@method print lm2
#'@export
print.lm2 <- function(m){
  cat("Call: ", m$call, ' ', "Coefficients: ", sep = '\n')
  print(m$coefficients)
}
