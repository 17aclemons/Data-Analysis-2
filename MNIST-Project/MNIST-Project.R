#Andrew Clemons
#MNIST Project
#Data 2

#base line beat 80% prediction
library(caret)
library(tidyverse)
library(randomForest)
library(ggbiplot)
library(stats)
library(ranger)
library(dplyr)
library(e1071)
#Desktop
#train <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/MNIST-Project/train.csv")
#test <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/MNIST-Project/test.csv")

#Laptop
train <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/MNIST-Project/train.csv")
test <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/MNIST-Project/test.csv")

progress <- data.frame("Attempt" = c(1,2), "Score" = c(.42,.68), "Model" = c("RandomForest","Tuned Ranger"))
####Functions####
submission <- function(label, pred){
  submission <- data.frame("ImageId" = label, "Label" = pred)
  
  #desktop
  #write.csv(submission,"C:/Users/XPS/Desktop/Software/Data-Analysis-2/MNIST-Project/submission.csv", row.names = FALSE)
  
  #Laptop
  write.csv(submission,"C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/MNIST-Project/submission.csv", row.names = FALSE)
}

####Data Exploration####
#Principle component analysis
data.pca <- train[, apply(train, 2, var, na.rm = TRUE) != 0]
train.pca <- prcomp(data.pca, center =TRUE, scale = TRUE)
ggbiplot(train.pca)
train.var <- train.pca$sdev^2
pve <- train.var/sum(train.var)
plot(pve, xlab = "Principle Component", 
     ylab = "Proportion of Variance Explained",
     ylim = c(0,.06), type = "b")

#Hierarchical clustering 

####Data Cleaning####
#scale the data ie divide by 255
#clean the data
zero_feature <- sapply(train[,2:ncol(train)], max) != 0 #could change this for columns with close to zero values
zero_feature <- c(TRUE, zero_feature)
train <- train[,zero_feature]

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
rf.pred <- round(rf.pred$predictions)
num <- 1:nrow(test)
confusionMatrix(as.factor(rf.pred), as.factor(validation$label))

submission(num, rf.pred)

####SVM####
train$label <- as.factor(train$label)
train.svm <- svm(label~., data = train, kernel = "polynomial", cost = 10)
tune.svm <- tune()

####Neural Network####

