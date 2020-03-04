#Andrew Clemons
#Housing Project 

library(caret)
library(e1071)
library(ggplot2)
library(bnstruct)

progress = data.frame("Attempt" = c(1,2,3), "Score" = c(18.16, 18.5, 17.85))

####Import Laptop####
train_df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/train.csv", stringsAsFactors = TRUE)
test_df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/test.csv", stringsAsFactors = TRUE)

####Import Desktop####
#train_df <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/HousingProject/train.csv",stringsAsFactors = TRUE)
#test_df <- test <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/HousingProject/test.csv", stringsAsFactors = TRUE)

####Functions####
#Using the old Noggin
#more accurate clean function for each variable
clean_df <- function(df){
  df$LotFrontage <- ifelse(is.na(df$LotFrontage) == TRUE, 0, df$LotFrontage)
  df$Alley <- ifelse(is.na(df$Alley) == TRUE, as.factor("None"), df$Alley)
  df$MasVnrType <- ifelse(is.na(df$MasVnrType) == TRUE, as.factor("None"),df$MasVnrType)
  df$MasVnrArea <- ifelse(is.na(df$MasVnrArea) == TRUE, 0, df$MasVnrArea)
  df$BsmtQual <- ifelse(is.na(df$BsmtQual) == TRUE, as.factor("NA"), df$BsmtQual)
  df$BsmtCond <- ifelse(is.na(df$BsmtCond) == TRUE, as.factor("NA"), df$BsmtCond)
  df$BsmtExposure <- ifelse(is.na(df$BsmtExposure) == TRUE, as.factor("NA"), df$BsmtExposure)
  df$BsmtFinType1 <- ifelse(is.na(df$BsmtFinType1) == TRUE, as.factor("NA"), df$BsmtFintype1)
  
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
  write.csv(submission, "C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/submission.csv",row.names = FALSE)
  
  #desktop
  #write.csv(submission, "C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/HousingProject/submission.csv", row.names = FALSE)
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

train <- run_clean_all_values(train)
test <- run_clean_all_values(test)

#create validation set
inTrain <- createDataPartition(y = train$SalePrice, p = .8, list = FALSE)
train_set <- train[inTrain,]
validation <- train[-inTrain,]

####Run Linear Regression####
lm_model <- lm(SalePrice ~., data = train_set)
lm_pred <- predict(object = lm_model, newdata = test)
RMSE(lm_pred, pred = validation$SalePrice)

####Lasso and Ridge Regression ####
control <- trainControl(
  method = "cv", number = 3, verboseIter = TRUE
)
set <- c(.001,.01,.1)
zero <- expand.grid(alpha = 0, lambda = set)
one <- expand.grid(alpha = 1, lambda = set)

ridge_model <- train(SalePrice ~., data = train_df)

#### PCA ####
pca_model <- prcomp(train, scale = TRUE)
pca.var <- pca_model$sdev^2
pve <- pca.var/sum(pca.var)
head(pve)
biplot(pca_model, scale = TRUE)





