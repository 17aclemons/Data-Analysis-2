Question 1) What effect does ridge or lasso have on the variance vs bias tradeoff?
  
  Question 2) Rank order the following in terms of variance: lasso, linear, non-linear. Explain your answer

Question 3) Rank-order the following in terms of bias: ridge, linear, non-linear. Explain your answer.

Question 4) For the ISLR::College dataset, run a linear, ridge, and lasso model with applications as the dependent variable. Choose the tuning parameter for ridge and lasso through cross-validation. (See lab if stuck.) Report the MSE of the test set for each approach. Comment on why you believe the results are in this order.

Something like the following is a straightforward manual method to calculate MSE:
  
  mean((test$Apps - linearModelPrediction)^2)