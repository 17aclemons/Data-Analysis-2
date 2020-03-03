#Andrew Clemons
#Housing Project 

library(caret)
library(e1071)
#import for laptop
train_df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/train.csv", stringsAsFactors = TRUE)
test_df <- read.csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/test.csv", stringsAsFactors = TRUE)

#import desktop

#copy data frames for ease
train <- train_df
test <- test_df

####Functions####
#remove missing values by calculating mean for numerics and mode for factors
clean_values <- function(df_column){
  if(class(df_column) == "integer"){
    mean <- mean(df_column,na.rm = TRUE)
    df_column <- ifelse(is.na(df_column)== TRUE, mean, df_column)
    return(df_column)
  }else if(class(df_column) == "factor"){
    if(is.na(getmode(df_column))){
      df_column <- ifelse(is.na(df_column) == TRUE, as.factor("other"), df_column)
    }else{
    mode <- getmode(df_column)
    df_column <- ifelse(is.na(df_column) == TRUE, mode, df_column)
    }
  }
}
#got mode function from https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#run clean_values on a dataframe
run_clean_values <- function(df){
  for(i in 1:ncol(df)){
    df[,i] <- clean_values(df[,i])
  }
  #see which variables are na
  print(sum(is.na(df)))
  return(df)
}

#create kaggle submission
submission <- function(test_id, pred){
  submission <- data.frame("Id" = test_id, "SalePrice" = pred)
  write.csv(submission, "C:/Users/XPS/Desktop/Software/Data-Analysis-2/HousingProject/submission.csv",row.names = FALSE)
}

####Clean Data ####
train <- run_clean_values(train)
test <- run_clean_values(test)

#create validation set
inTrain <- createDataPartition(y = train$SalePrice, p = .8, list = FALSE)
train_df <- train[inTrain,]
validation <- train[-inTrain,]

####Run Linear Regression####
lm_model <- lm(SalePrice ~., data = train)
lm_pred <- predict(object = lm_model, newdata = test)


####Lasso and Ridge Regression
control <- trainControl(
  method = "cv", number = 3, verboseIter = TRUE
)
set <- c(.001,.01,.1)
zero <- expand.grid(alpha = 0, lambda = set)
one <- expand.grid(alpha = 1, lambda = set)

ridge_model <- train(SalePrice ~., data = train_df)


