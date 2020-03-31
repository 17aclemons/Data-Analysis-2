#Andrew Clemons
#MNIST Project
#Data 2

library(caret)
library(tidyverse)
library(randomForest)
library(ggbiplot)
library(stats)
library(ranger)
library(dplyr)
library(e1071)
library(RSNNS)

#Desktop
train <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/MNIST-Project/train.csv")
test <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/MNIST-Project/test.csv")

#Laptop
#train <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/MNIST-Project/train.csv")
#test <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/MNIST-Project/test.csv")

progress <- data.frame("Attempt" = c(1,2,3,4,5,6), 
                       "Score" = c(.42,.68,91,.92,.11,.97), 
                       "Model" = c("RandomForest",
                                   "Tuned Ranger",
                                   "the same ranger with different seed?",
                                   "Tuned Poly SVM",
                                   "Tuned Radial SVM",
                                   "Tuned Poly on Full Training set"))
####Functions####
submission <- function(label, pred){
  submission <- data.frame("ImageId" = label, "Label" = pred)
  
  #desktop
  write.csv(submission,"C:/Users/XPS/Desktop/Software/Data-Analysis-2/MNIST-Project/submission.csv", row.names = FALSE)
  
  #Laptop
  #write.csv(submission,"C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/MNIST-Project/submission.csv", row.names = FALSE)
}

####Data Exploration with PCA####
#Principle component analysis
data.pca <- train[, apply(train, 2, var, na.rm = TRUE) != 0]
train.pca <- prcomp(data.pca, center =TRUE, scale = TRUE)
ggbiplot(train.pca)
train.var <- train.pca$sdev^2
pve <- train.var/sum(train.var)
plot(pve, xlab = "Principle Component", 
     ylab = "Proportion of Variance Explained",
     ylim = c(0,.06), type = "b") with 

####Data Cleaning####
#scale the data ie divide by 255
#clean the data
train[,1:ncol(train)] <- train[2:ncol(train)]/255
zero_feature <- sapply(train[,2:ncol(train)], max) != 0 #could change this for columns with close to zero values
zero_feature <- c(TRUE, zero_feature)
train <- train[,zero_feature]

test[,1:ncol(train)] <- train[2:ncol(train)]/255
zero_feature <- sapply(test[,1:ncol(test)], max) != 0
zero_feature <- c(TRUE, zero_feature)
test <- test[,zero_feature]
num <- 1:nrow(test)
set.seed(1)
#split the data
inTrain <- createDataPartition(train$label, p = .05, list = FALSE)
train <- train[inTrain,]
validation <- train[-inTrain,]

####Random Forest####
#default Random Forest
rf <- randomForest(label ~., data = train, 
                   importance = TRUE,
                   verbose = TRUE)

#tune grid for Ranger
#got the base code from https://uc-r.github.io/random_forests#tune

tune.grid <- expand.grid(mtry = seq(100, 200, by = 10),
                         num.trees = seq(400,500, by = 100),
                         node_size = seq(9,9, by =2),
                         sampe_size = c(.80),
                         OOB_RMSE = 0
                         )

for(i in 1:nrow(tune.grid)){
  model <- ranger(formula = label~.,
                  data = train,
                  num.trees = tune.grid$num.trees[i],
                  mtry = tune.grid$mtry[i],
                  min.node.size = tune.grid$node_size[i],
                  sample.fraction = tune.grid$sampe_size[i],
                  seed = 1
  )
  tune.grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

tune.grid %>%
  arrange(OOB_RMSE) %>%
  head(10)

#build ranger model using values from tune.grid
opt.ranger <- ranger(label~.,
                     data = train,
                     num.trees = 500,
                     mtry = 140,
                     min.node.size = 9, 
                     sample.fraction = .8,
                     seed = 1
                     )

rf.pred <- predict(opt.ranger, validation)
confusionMatrix(as.factor(rf.pred$predictions), as.factor(validation$label))

rf.pred <- predict(opt.ranger, test)
num <- 1:nrow(test)
submission(num, rf.pred$predictions)

####SVM####
train$label <- as.factor(train$label)
train.poly <- svm(label~., data = train, kernel = "polynomial", cost = 10)
train.rad <- svm(label~., data = train, kernel = "radial", cost = 10)

poly.pred <- predict(train.poly, validation)
poly.pred <- round(poly.pred$predictions)
set.seed(1)
c <- c(.001,.01,.1,1)
g <- c(.5,1,10)

tune.poly <- tune(svm, label~., data = train, kernel = "polynomial", ranges = list(cost = c, gamma = g))
tune.rad <- tune(svm, label~., data = train, kernel = "radial", ranges = list(cost = c, gamma = g))

tune.poly$best.parameters
tune.rad$best.parameters

train.opt.poly <- svm(label~., data = train, kernel = "polynomial", cost = .001, gamma = .5)
train.opt.rad <- svm(label~., data = train, kernel = "radial", cost = , gamma = .5)

tune.poly.pred <- predict(train.opt.poly, validation)
confusionMatrix(as.factor(tune.poly.pred), as.factor(validation$label))


tune.rad.pred <- predict(train.opt.rad, validation)
confusionMatrix(as.factor(tune.rad.pred), as.factor(validation$label))

opt.poly.pred <- predict(train.opt.poly, test)
#submission(num, opt.poly.pred)

#opt.rad.pred <- predict(train.opt.rad, test)
#submission(num,opt.rad.pred)

####Neural Network####
set.seed(1)
    targetLabel <- decodeClassLabels(train[,1])
    split <- splitForTrainingAndTest(train[,2:ncol(train)],
                                     y = targetLabel,
                                     ratio = .9)
    
    nn1 <- mlp(x = split$inputsTrain, y = split$targetsTrain,
               size = c(10,10), maxit = 100,
               inputsTest = split$inputsTest,
               targetsTest = split$targetsTest)
    
    nn1.pred <- predict(nn1, split$inputsTest)
    nn1.preds.value <- max.col(nn1.pred)-1
    true_values <- max.col(split$targetsTest)-1
    caret::confusionMatrix(as.factor(nn1.preds.value), as.factor(true_values))
