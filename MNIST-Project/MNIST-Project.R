#Andrew Clemons
#MNIST Project
#Data 2

#base line beat 80% prediction
library(caret)
library(tidyverse)
library(randomForest)
df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/MNIST-Project/train.csv")


#scale the data ie divide by 255

sapply(df[,2:ncol(df)], max)

zero_feature <- sapply(df[,2:ncol(df)], max) != 0
zero_feature <- c(TRUE, zero_feature)
df <- df[,zero_feature]

set.seed(1)
inTrain <- createDataPartition(df$label, p = .05, list = FALSE)
train <- df[inTrain,]
test <- df[-inTrain,]

rf <- randomForest(y = train[,1], x = train[,2:ncol(train)], ntree = 20)



