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
setwd("C:/Users/XPS/Desktop/Software/ANLY-6100/Titanic Project")

#function to create confusion matrix of test and validation sets
#based on Survived column
confMatrix <- function(model, validation){
  test_predict <- predict(model, newdata = validation, type =  "response")
  p_class <- ifelse(test_predict >.5, 1,0)
  #confusion matrix to check outcome
  confusionMatrix(factor(p_class), factor(validation[["Survived"]]))
}

#train function for reading in and cleaning training set ####
df_train <- function(){
train <- read.csv("train.csv")
#clean the age column using mean insertion
mean_age <- mean(train$Age, na.rm = TRUE)
train$Age <- ifelse(is.na(train$Age)== TRUE, mean_age, train$Age)

#creatCategorical Survived column so my glmnet model likes it
train$Categorical <- ifelse(train$Survived == 1, "Survived", "Dead")

#create feature call can swim, I picked 14 because thats what the pool signs
#say about kids swimming
train$canSwim <- ifelse(train$Age < 14, "Swim", "NoSwim")

#set catagorical values to factors
train$Sex <- as.factor(train$Sex)
train$Survived <- as.factor(train$Survived)
train <- train[,c("Survived", "Sex", "Parch",
               "canSwim","Categorical")]
}

#test function for making testing set####
df_test <- function(){
test <- read.csv("test.csv")
#do the same to the testing set
test_mean_age <- mean(test$Age, na.rm = TRUE)
test$Age <- ifelse(is.na(test$Age)== TRUE, test_mean_age, test$Age)
test$canSwim <- ifelse(test$Age < 14, "Swim", "NoSwim")
test$Sex <- as.factor(test$Sex)
test <- test[,c("Sex", "Parch", 
                   "canSwim")]
return(test)
}

#split training data into train and validation (80/20 split)
train <- df_train()
test <- df_test()
numberRows <- nrow(train)
pnumberRows <- sample(numberRows)
trainShuffle <- train[pnumberRows, ]
split <- round(numberRows *.8)
train <- trainShuffle[1:split,]
validation <- trainShuffle[(split+1):numberRows,]

#create at least 4 models ####

#glm model
model <- glm(Survived ~ Sex + Parch + canSwim, family = "binomial", data = train)
glm_predict <- predict(model, newdata = validation, type =  "response")
confMatrix(model,validation)
#81% accurace rate  

#Random Forest
#set seed to make it recreatable 
set.seed(1)
rfmodel <- train(Survived ~ Sex + Parch + canSwim, data = train, method = "ranger", tuneLength = 2)
rf_predict <- predict(rfmodel, newdata = validation, type = "raw")
rfcf <- confusionMatrix(factor(rf_predict), factor(validation[["Survived"]]))
print(rfcf)
#82% accuracy rate

#glmnet model
control <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verbose = TRUE
)
valid <- validation[,-1]
glmModel <- train(Categorical ~ Sex + Parch + canSwim, train, method = "glmnet", trControl = control)
glmnet_predict <- predict(glmModel, valid, type = "raw")
#converting the predict for the confusion matrix
glm_predict <- ifelse(glm_predict == "Dead", 0, 1)
glmcf <- confusionMatrix(factor(glm_predict),factor(validation[["Survived"]]))
print(glmcf)
#80% accuracy rate

#####
#QDA model
qdaModel <- qda(Survived ~ Sex + Parch + canSwim, data = train)
qda_predict <- predict(qdaModel, validation)
qdacf <- confusionMatrix(factor(qda_predict$class), validation$Survived)
print(qdacf)




