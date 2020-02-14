#Andrew Clemons
#ISLR 6


####Question 1)####
#What effect does ridge or lasso have on the variance vs bias tradeoff?
#For both lasso and ridge have a similar effect, that as lambda increases, the variance decreases and the bias increases. 
#Lasso tends to outperform ridge in terms of bias and variance

####Question 2)####
#Rank order the following in terms of variance: lasso, linear, non-linear. Explain your answer
#1.non-linear 2.Lasso 3.Linear
#non-linear has the potential to fit a model to every observation or drawing a line as close as possible, so it has a high variance.
#lasso reduces the coefficents of certain variables so that the variance is lower and useless variables are used as little as possible
#linear tries to draw the best fit line and it has the lowest variance as it can't curve itself to overfit to specific observations
#this is for 1 = the model with the most variance and 3 has the least

####Question 3)####
#Rank-order the following in terms of bias: ridge, linear, non-linear. Explain your answer.
#1. Linear, 2. Ridge, 3. non-linear
#Linear has the highest bias as has the most potential to not fit the data correctly, due to only being able to draw a line based on all variables
#Ridge has a lower bias than linear as reduceing the number of predictor variables means it is less likely to predict on noise and only use relevant variables
#non-linear has the lowest as you could predict every observation, this isn't necessarily good. But, in a training set non-linear will have the least amount of bias
#Model 1 has the most bias and 3 has the least

####Question 4)#### 
#For the ISLR::College dataset, run a linear, ridge, and lasso model with applications as the dependent variable. 
#Choose the tuning parameter for ridge and lasso through cross-validation. (See lab if stuck.) 
#Report the MSE of the test set for each approach. Comment on why you believe the results are in this order.
library(ISLR)
library(caret)
library(e1071)
model_data <- College
model_data$Apps <- as.numeric(model_data$Apps)
inTrain <- createDataPartition(y = model_data$Apps, p = .8, list = FALSE)

train <- model_data[inTrain,]
test <- model_data[-inTrain,]

myControl <- trainControl(
  method = "cv", number = 5, verboseIter = TRUE)
set <- c(.001,01,.1)
zero <- expand.grid(alpha = 0, 
                      lambda = set)
one <- expand.grid(alpha = 1, 
                           lambda = set)
set.seed(1)
#ridge
ridge.model <- train(Apps ~., data = train,
               method = "glmnet",
               tuneGrid = zero,
               trControl = myControl)
#lasso
lasso.model <- train(Apps ~., data = train,
               method = "glmnet",
               tuneGrid = one,
               trControl = myControl)

lm.model <- lm(Apps~. ,data = train)


#Something like the following is a straightforward manual method to calculate MSE:
 # mean((test$Apps - linearModelPrediction)^2)
#mse of lm
lm.predict <- predict(lm.model, test)
ridge.predict <- predict(ridge.model, test)
lasso.predict <- predict(lasso.model, test)

#MSE lm
mean((test$Apps - lm.predict))

#MSE lasso 
mean((test$Apps - lasso.predict))
#MSE ridge
mean((test$Apps - ridge.predict))

#Ridge could be removing important variables and that is why it has a lower MSE. I think the lasso model might be overfitting the
#data as we aren't training on a large number of observations ~600.  I think the reason the linear model is the performing the best
#is that it has the most optimal bias variance trade off. 