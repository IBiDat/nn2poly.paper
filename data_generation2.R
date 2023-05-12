data_generation2 <- function(n_sample, p, polynomial, error_var) {
  
  # Predictor variables
  X <- matrix(0,n_sample,p)
  for (i in 1:p){
    X[,i] <- rnorm(n = n_sample,0,1)
  }
  
  # Response variable + small error term
  Y <- as.vector(eval_poly(X,polynomial)) + stats::rnorm(n_sample, 0, error_var)
  
  # Store all as a data frame
  data <- as.data.frame(cbind(X, Y))
  
  # Store all as a data frame
  data <- as.data.frame(cbind(X, Y))
  
  return(data)
}
