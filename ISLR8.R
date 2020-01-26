####Question 1)####
#The following code is the result of 10 models predicting a strike for the next pitch in a baseball game. 
#Solve using majority vote, then solve using average vote. Comment on your findings.

set.seed(10)
results <- (rnorm(10, 0.55, .1))
summary(results)
round <- round(results)
summary(round)
mean(round)
median(round)

#the average gives a 50% predicatbility and the median predicts slightly higher at 51%
#with rounding them because a whether or not a strike is thrown is a binary answer
#I rounded them for a simpler look.
#They both predict that the next pitch will be a strike and both numbers are very close.
#I tend to favor the median as it isn't as affected by outliers as the mean is.
#using either as your prediction, the average and median of the models both say that the
#next pitch will be a strike. If you round them, then the median is indicating with more
#certainty that the next pitch is a strike while the mean says there is a 60% chance of 
#a strike.


####Question 2)####
#You will walk through the lab using sales as a numerical rather than categorical variable. 
#Similar to the lab, show the following steps:
library(tree)
library(ISLR)
attach(Carseats)

#### (A)####
#Split into train and test.
size <- floor(.8*nrow(Carseats))
index <- sample(nrow(Carseats), size)

train <- Carseats[index,]
test <- Carseats[-index,]

####(B)#### 
#Create a model using a regression tree. What is your test RMSE?
tree.carseats <- tree(Sales~., Carseats)
summary(tree.carseats)
pred <- predict(tree.carseats, test)
err <- pred - train$Sales
rmse <- sqrt(mean(err^2))
rmse
#RMSE is 3.672987 for the test set
plot(tree.carseats)
text(tree.carseats, pretty = 0)

####C)####
#Use cross-validation to find the optimal pruning level.
cv.carseats <- cv.tree(tree.carseats)
names(cv.carseats)
cv.carseats$size
min(cv.carseats$dev)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, "b")
prune.carseats <- prune.tree(tree.carseats, best = 9)
plot(prune.carseats)
#optimal pruning level is 9 

####D)#### 
#Create a bagging model. What is your test RMSE now? Did it improve or get worse? Which variables are important in your bagging model?
library(randomForest)
set.seed(1)
bag.carseats <- randomForest(Sales~., train, mtry = 10, importance = TRUE)
bag.carseats
yhat.bag <- predict(bag.carseats, test)
plot(yhat.bag, test$Sales)
mean((yhat.bag-test$Sales)^2)
#test RMSE is 2.833069 so it has improved performance compared to the regression tree.
importance(bag.carseats)
varImpPlot(bag.carseats)
#ShelveLoc, Price are the most important variables.

####E)####
#Repeat D but with random forest. 
set.seed(1)
rf.carseats <- randomForest(Sales ~., train, importance = TRUE)
rf.carseats
yhat.bag <- predict(rf.carseats, test)
plot(yhat.bag, test$Sales)
mean((yhat.bag - test$Sales)^2)
#test RMSE is 2.494158 so it has improved performance compared to the bagging and regression model
importance(rf.carseats)
varImpPlot(rf.carseats)
#ShelveLoc and Price are still the most important variables in the model
