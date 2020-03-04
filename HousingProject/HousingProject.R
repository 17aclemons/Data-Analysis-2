#Andrew Clemons
#Housing Project 

library(caret)
library(e1071)
library(ggplot2)
library(dplyr)

#To Do
#get a 
#Tune Linear, Ridge and Lasso models
#figure out PCA model
#what other models do I need?
#

progress = data.frame("Attempt" = c(1,2,3,4,5), "Score" = c(18.16, 18.5, 17.85, 17, 16.7))

####Import Laptop####
#train_df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/train.csv", stringsAsFactors = TRUE)
#test_df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/test.csv", stringsAsFactors = TRUE)

####Import Desktop####
train_df <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/HousingProject/train.csv",stringsAsFactors = TRUE)
test_df <- test <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/HousingProject/test.csv", stringsAsFactors = TRUE)

####Functions####
#Using the old Noggin
#more accurate clean function for each variable
clean_df <- function(df){
  #df$MSZoning <- ifelse(df$MSZoning == as.factor("C (all)"), as.facor("C", ))
  df$LotFrontage <- ifelse(is.na(df$LotFrontage) == TRUE, 0, df$LotFrontage) #editable
  df$Alley <- ifelse(is.na(df$Alley) == TRUE, as.factor("None"), df$Alley) #explained in documentation
  df$MasVnrType <- ifelse(is.na(df$MasVnrType) == TRUE, as.factor("None"),df$MasVnrType) #editable
  df$MasVnrArea <- ifelse(is.na(df$MasVnrArea) == TRUE, 0, df$MasVnrArea) #Editable with type
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

#got mode function from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
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
# inTrain <- createDataPartition(y = train$SalePrice, p = .8, list = FALSE)
# train_set <- train[inTrain,]
# validation <- train[-inTrain,]

####Run Linear Regression####
lm.model <- lm(SalePrice ~., data = train)
lm_pred <- predict(object = lm.model, newdata = test)
RMSE(lm_pred, pred = validation$SalePrice)

####Lasso and Ridge Regression ####
control <- trainControl(
  method = "cv", number = 3, verboseIter = TRUE
)
#how do I use this to tune ridge and lasso
set <- c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1,2,100)
zero <- expand.grid(alpha = 0, lambda = set)
one <- expand.grid(alpha = 1, lambda = set)

ridge.model <- train(SalePrice ~., data = train,
                     method = "glmnet",
                     tuneGrid = zero,
                     trControl = control
                     )
ridge_pred <- predict(ridge_model, newdata = test)

lasso.model <- train(SalePrice ~., data = train,
                     method = "glmnet",
                     tuneGrid = one,
                     trControl = control
                     )
lasso_pred <- predict(lasso.model, newdata = test)

#### PCA ####
pca_model <- prcomp(train, scale = TRUE)
pca.var <- pca_model$sdev^2
pve <- pca.var/sum(pca.var)
head(pve)
biplot(pca_model, scale = TRUE)

####Knn impute trial (doesnt' work as it gives some factors only one factor####
# list <- colnames(train)[colSums(is.na(train)) >0]
# process <- preProcess(train %>%
#                         dplyr::select(list),
#                       method = c("knnImpute"),
#                       k = 10,
#                       knnSummary = mean)
# 
# list <- colnames(test)[colSums(is.na(test)) >0]
# process <- preProcess(test %>%
#                         dplyr::select(list),
#                       method = c("knnImpute"),
#                       k = 10,
#                       knnSummary = mean)
# 
# test_sample <- sapply(test, function(x) is.factor(x))
# train_sample <- sapply(train, function(x) is.factor(x))
# train_sample


#group missing values in test set to similiar entries (in progress)
temp <- test %>%
  filter(is.na(MSZoning))

