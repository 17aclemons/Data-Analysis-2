#Andrew Clemons
#Data 2

transform <- function(input){
  temp <- (input - min(input)) /
    (max(input) - min(input))
  return(temp)
}

gradient_descent <- function(x, y, n = 100,
                             learning_coefficent = .1, 
                             set_seed = 1){
  tran_x <- transform(x)
  tran_y <- transform(y)
  b0 <- runif(1)
  b1 <- runif(1)
  yhat <- b0 + b1 + tran_x
  error <- yhat - tran_y
  loss <- sum(error^2)
  
  for(i in 1:n){
    b0_der <- sum(error)
    b1_der <- sum(error * tran_x)
    b0 <- b0 - learning_coefficent * b0_der
    b1 <- b1 - learning_coefficent * b1_der
    yhat <- b0 + b1 * tran_x
    error <- yhat - tran_y
    loss <- sum(error^2)
  }
  return(list(input_x = tran_x, input_y = tran_y, 
              total_loss = loss, numberOfRounds = n))
}
