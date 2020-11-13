#Andrew Clemons
#Titanic Project
#Data Analysis 1

library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library(caTools)
library(mlbench)
library(ranger)
library(glmnet)
library(MASS)
#setwd("C:/Users/XPS/Desktop/Software/ANLY-6100/Titanic Project")

#function to create confusion matrix of test and validation sets
#based on Survived column
confMatrix <- function(model, validation){
  test_predict <- predict(model, newdata = validation, type =  "response")
  p_class <- ifelse(test_predict >.5, 1,0)
  #confusion matrix to check outcome
  confusionMatrix(factor(p_class), factor(validation[["Survived"]]))
}

#read in csv ####
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#clean then convert to factors

#clean the age column using mean insertion
mean_age <- mean(train$Age, na.rm = TRUE)
train$Age <- ifelse(is.na(train$Age)== TRUE, mean_age, train$Age)

#S is the most common, so replace blank cells via medianImputation
#error here

train$Embarked = as.character(train$Embarked)
train$Embarked <- ifelse(train$Embarked == "", "S", train$Embarked)
train$Embarked <- as.factor(train$Embarked)


#If there is a cabin, mark true if not mark false
train$Cabin <- ifelse(train$Cabin == "" , FALSE, TRUE)

#change Survived column so my glmnet model likes it
train$Categorical <- ifelse(train$Survived == 1, "Survived", "Dead")

#set catagorical values to factors
train$Sex <- as.factor(train$Sex)
train$Pclass <- as.factor(train$Pclass)
train$Embarked <- as.factor(train$Embarked)
train$Survived <- as.factor(train$Survived)
#train$Parch <- as.factor(train$Parch)

#do the same to the testing set
test$Sex <- as.factor(test$Sex)
test$Pclass <- as.factor(test$Pclass)
test$Embarked <- as.factor(test$Embarked)
test$Parch <- as.factor(test$Parch)

#split training data into train and validation (80/20 split)
numberRows <- nrow(train)
pnumberRows <- sample(numberRows)
trainShuffle <- train[pnumberRows, ]
split <- round(numberRows *.8)
train <- trainShuffle[1:split,]
validation <- trainShuffle[(split+1):numberRows,]

#how many NA you have for each variable
sapply(train,function(x) sum(is.na(x)))

#create at least 4 models ####

#70% correct prediction rate standard glm 
model <- glm(Survived ~ Sex + Parch, family = "binomial", data = train)
confMatrix(model,validation)


#Random Forest
#set seed to make it recreatable 
set.seed(1)
rfmodel <- train(Survived ~ Sex + Parch, data = train, method = "ranger", tuneLength = 2)
test_predict <- predict(rfmodel, newdata = validation, type = "raw")
rfcf <- confusionMatrix(factor(test_predict), factor(validation[["Survived"]]))


#glmnet model
control <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verbose = TRUE
)
valid <- validation[,-2]
glmModel <- train(Categorical ~ Sex + Parch, train, method = "glmnet", trControl = control)
glm_predict <- predict(glmModel, valid, type = "raw")
glm_predict <- ifelse(glm_predict == "Dead", 0, 1)
glmcf <- confusionMatrix(factor(glm_predict),factor(validation[["Survived"]]))

#####
#QDA model
qdaModel <- qda(Survived ~ Sex + Parch, data = train)
qda_predict <- predict(qdaModel, validation)
qdacf <- confusionMatrix(factor(qda_predict$class), validation$Survived)
#how to submit to Kaggle
test_predict <- predict(rfmodel, newdata = test,
                        type = "raw")
test_predict <- round(test_predict, 0)
submission <- cbind(test$PassengerId, test_predict)
submission <- data.frame(submission)
names(submission) <- c("PassengerId", "Survived")
write.csv(submission, "submission.csv", 
          row.names = FALSE)


#a) Accurately predicts probability of survival for each person on the Titanic. 
#Include a variety of measures for the validity of your model. (More than just "accuracy." 
#Kaggle only cares about accuracy, but you know better!)

#b) You should run and compare AT LEAST four different types of models. 
#Comment on why you think some are more accurate than others. 
#Are some models better for specific purposes? 
#By examining the residuals, where are each succeeding and failing? Comment thoroughly.