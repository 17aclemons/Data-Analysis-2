####Question 1)####
#The following code is the result of 10 models predicting a strike for the next pitch in a baseball game. 
#Solve using majority vote, then solve using average vote. Comment on your findings.

set.seed(10)
results <- round(rnorm(10, 0.55, .1))
mean(results)
median(results)

#Question 2) You will walk through the lab using sales as a numerical rather than categorical variable. Similar to the lab, show the following steps:
#A) Split into train and test.
#B) Create a model using a regression tree. What is your test RMSE?
#C) Use cross-validation to find the optimal pruning level.
#D) Create a bagging model. What is your test RMSE now? Did it improve or get worse? Which variables are important in your bagging model?
#E) Repeat D but with random forest. 