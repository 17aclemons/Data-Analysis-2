#Andrew Clemons
#Housing Project 

library(caret)
library(e1071)
library(ggplot2)
library(dplyr)
library(mltools)
library(randomForest)
library(gbm)
#To Do
#Tune GBM model

progress = data.frame("Attempt" = c(1,2,3,4,5,6,7), "Score" = c(18.16, 18.5, 17.85, 17, 16.7,14.6,13.2))

####Import Laptop####
#train_df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/train.csv", stringsAsFactors = TRUE)
#test_df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/test.csv", stringsAsFactors = TRUE)

####Import Desktop####
train_df <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/HousingProject/train.csv",stringsAsFactors = TRUE)
test_df <- test <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/HousingProject/test.csv", stringsAsFactors = TRUE)

####Functions####
#Using the old Noggin
#got mode function from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#more accurate clean function for each variable
clean_df <- function(df){
  df$LotFrontage <- ifelse(is.na(df$LotFrontage) == TRUE, median(df$LotFrontage), df$LotFrontage) #editable
  df$Alley <- ifelse(is.na(df$Alley) == TRUE, as.factor("None"), df$Alley) #explained in documentation
  df$MasVnrType <- ifelse(is.na(df$MasVnrType) == TRUE, as.factor("None"),df$MasVnrType) #editable
  df$MasVnrArea <- ifelse(is.na(df$MasVnrArea) == TRUE, median(df$MasVnrArea), df$MasVnrArea) #Editable with type
  df$BsmtQual <- ifelse(is.na(df$BsmtQual) == TRUE, as.factor("No Basement"), df$BsmtQual) #explained in documentation
  df$BsmtCond <- ifelse(is.na(df$BsmtCond) == TRUE, as.factor("No Basement"), df$BsmtCond) #explained in documentation
  df$BsmtExposure <- ifelse(is.na(df$BsmtExposure) == TRUE, as.factor("No Basement"), df$BsmtExposure) #explained in documentation
  df$BsmtFinType1 <- ifelse(is.na(df$BsmtFinType1) == TRUE, as.factor("No Basement"), df$BsmtFinType1) #explained in documentation
  df$BsmtFinType2 <- ifelse(is.na(df$BsmtFinType2) == TRUE, as.factor("No Basement"), df$BsmtFinType2) #explained in documentation
  df$Electrical <- ifelse(is.na(df$Electrical) == TRUE, as.factor("SBrkr"), df$Electrical) #editable, SBrkr is the mode of the column
  df$FireplaceQu <- ifelse(is.na(df$FireplaceQu) == TRUE,as.factor("No Fireplace"), df$FireplaceQu) #explained in documentation
  df$GarageType <- ifelse(is.na(df$GarageType) == TRUE, as.factor("No Garage"), df$GarageType) #explained in documentation
  df$GarageYrBlt <- ifelse(is.na(df$GarageYrBlt) == TRUE, 0, df$GarageYrBlt) #explained in documentation
  df$GarageFinish <- ifelse(is.na(df$GarageFinish) == TRUE, as.factor("No Garage"), df$GarageFinish) #explained in documentation
  df$GarageQual <- ifelse(is.na(df$GarageQual) == TRUE, as.factor("No Garage"), df$GarageQual) #explained in documentation
  df$GarageCond <- ifelse(is.na(df$GarageCond) == TRUE, as.factor("No Garage"), df$GarageCond) #explained in documentation
  df$PoolQC <- ifelse(is.na(df$PoolQC) == TRUE, as.factor("No Pool"), df$PoolQC) #explained in documentation
  df$Fence <- ifelse(is.na(df$Fence) == TRUE, as.factor("No Pool"), df$Fence) #explained in documentation
  df$MiscFeature <- ifelse(is.na(df$MiscFeature) == TRUE, as.factor("No Pool"), df$MiscFeature) #explained in documentation
  return(df)
}

#remove all missing values by calculating mean for numerics and mode for factors
clean_all_values <- function(df_column){
  if(class(df_column) == "integer" | class(df_column) == "numeric"){
    mean <- mean(df_column,na.rm = TRUE)
    df_column <- ifelse(is.na(df_column)== TRUE, mean, df_column)
    return(df_column)
  }else if(class(df_column) == "factor"){
    if(is.na(getmode(df_column))){
      df_column <- ifelse(is.na(df_column) == TRUE, as.factor("other"), df_column)
    }else{
    mode <- getmode(df_column)
    df_column <- ifelse(is.na(df_column) == TRUE, mode, df_column)
    return(df_column)
    }
  }
}


#run clean_values on a dataframe
run_clean_all_values <- function(df){
  for(i in 1:ncol(df)){
    df[,i] <- clean_all_values(df[,i])
  }
  #see which variables are na
  print(sum(is.na(df)))
  return(df)
}

#create kaggle submission
submission <- function(test_id, pred){
  submission <- data.frame("Id" = test_id, "SalePrice" = pred)
  #laptop
  #write.csv(submission, "C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/submission.csv",row.names = FALSE)
  
  #desktop
  write.csv(submission, "C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/HousingProject/submission.csv", row.names = FALSE)
}

####Clean Data ####
#copy data frames for ease
train <- train_df
test <- test_df

#clean the rest of the variables
train <- clean_df(train)
test <- clean_df(test)

#get a list of the columns with NA's
list <- apply(is.na(train), 2, which)
list_test <- apply(is.na(test), 2, which)

train <- run_clean_all_values(train)
test <- run_clean_all_values(test)

#create validation set
set.seed(1)
inTrain <- createDataPartition(y = train$SalePrice, p = .8, list = FALSE)
train_set <- train[inTrain,]
validation <- train[-inTrain,]

####Run Linear Regression####
lm.model <- lm(SalePrice ~., data = train_set)
lm_pred <- predict(object = lm.model, newdata = validation)
RMSE(lm_pred, pred = validation$SalePrice)

####Lasso and Ridge Regression ####
control <- trainControl(
  method = "cv", number = 5, verboseIter = TRUE
)
set.seed(1)
#how do I use this to tune ridge and lasso
set <- seq(from = 0, to = 60000, by = 1000)
zero <- expand.grid(alpha = 0, lambda = set)

ridge.model <- train(SalePrice ~., data = train, #cv.glmnet see if data is scaled
                     method = "glmnet",
                     tuneGrid = zero,
                     trControl = control
                     )
ridge_pred <- predict(ridge.model, newdata = test)

set.seed(1)
set <- seq(from = 0, to = 5000, by = 100)
set <- 2400
one <- expand.grid(alpha = 1, lambda = set)
lasso.model <- train(SalePrice ~., data = train, #cv.glmnet
                     method = "glmnet",
                     tuneGrid = one,
                     trControl = control
                     )
lasso_pred <- predict(lasso.model, newdata = test)

#### PCA ####
pca <- prcomp(train[,1:80], scale = TRUE)
temp <- summary(pca)
temp$importance[,1:5]
pca <- data.frame(pca$x)
pca <- pca[,1:20]
pca <- cbind(pca, train[,81])
names(pca)[21] <- "SalePrice"
pca_model <- lm(SalePrice ~., data = pca)

temp <- test[,which(apply(test,2,var) != 0)]
pca_test <- prcomp(temp, scale = TRUE)
pca_test <- data.frame(pca_test$x)
pca_test <- pca_test[,1:20]
pca_pred <- predict(pca_model, newdata = pca_test)

#### Random Forest ####
rf.model <- randomForest(SalePrice~.,
                         data = train,
                         importance = TRUE,
                         verbose = TRUE)

rf_pred <- predict(rf.model, newdata = test)

#### Boosting ####
gbm.model <- gbm(
    formula = SalePrice~.,
    distribution = "gaussian",
    data = train,
    n.trees = 810,
    interaction.depth = 20,
    cv.folds = 3,
    verbose = FALSE
)

gbm_pred <- predict(gbm.model, newdata = test)
RMSE(gbm_pred, validation$SalePrice)

