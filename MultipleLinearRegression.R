lr_multiple <- function(target, first_guess, step_size, tolerance, ...) {
  predictors <- list(...)
  
  target_matrix <- cbind(rep(1, length(predictors[[1]])))
  
  for (i in 1:length(predictors))
  {
    target_matrix <- cbind(target_matrix, predictors[[i]])
  }
  
  guess <- first_guess
  converged <- FALSE
  
  while (converged == FALSE) {
    applied_matrix <- sweep(target_matrix, MARGIN=2, STATS=guess, '*')
    predicted <- rowSums(applied_matrix)
    errors <- predicted - target
    
    gradient_sum_squares <- 0
    
    for (i in 1:length(guess))
    {
      #Recall that twice the sum of the product of two vectors is just twice the dot product of the two vectors. 
      #Therefore the derivative for the weight for feature_i is just two times the dot product between the values of predictor_i and the current errors. 
      derivative <- 2 * (errors %*% target_matrix[,i])
      
      guess[i] <- guess[i] - step_size * derivative
      
      gradient_sum_squares <- derivative^2 + gradient_sum_squares
    }
    
    tolerance_test <- gradient_sum_squares^(1/2)
    if (tolerance_test < tolerance)
      converged <- TRUE
  }
  
  guess
}

lr_multiple_ridge <- function(target, first_guess, step_size, l2_penalty, 
                              max_iterations = 100, DEBUG = FALSE, ...) {
  predictors <- list(...)
  counter <- 1
  
  target_matrix <- cbind(rep(1, length(predictors[[1]])))
  
  for (i in 1:length(predictors))
  {
    target_matrix <- cbind(target_matrix, predictors[[i]])
  }
  
  guess <- first_guess
  
  while (counter < max_iterations) {
    applied_matrix <- sweep(target_matrix, MARGIN=2, STATS=guess, '*')
    predicted <- rowSums(applied_matrix)
    errors <- predicted - target
    
    for (i in 1:length(guess))
    {
      #Recall that twice the sum of the product of two vectors is just twice the dot product of the two vectors. 
      #Therefore the derivative for the weight for feature_i is just two times the dot product between the values of predictor_i and the current errors. 
      derivative <- 2 * (errors %*% target_matrix[,i])
      new_step <- step_size
      
      if (i != 1)
        derivative <- derivative + 2 * l2_penalty * guess[i]
      else
        new_step <- 0.01 #for some reason, we seem to need a different step size for the constant
      
      change <- new_step * derivative / length(errors)
      
      if (change > 10000)
        change <- 10000
      else if (change < -10000)
        change <- -10000
      
      guess[i] <- guess[i] - change
    }
    
    if (DEBUG == TRUE)
      print(guess)
    
    counter <- counter + 1
  }
  
  guess
}