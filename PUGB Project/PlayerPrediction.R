#Andrew Clemons
#PUBG Project
library(caret)
library(dplyr)
library(Metrics)
library(randomForest)
library(gmb)

train <- read.csv("C:/Users/17acl/OneDrive/Desktop/Software/Data-Analysis-2/PUGB Project/train_partition.csv")
test <- read.csv("test_V2.csv")


####Data cleaning####
train <- train[!is.na(train$winPlacePerc),]

####Split to Solo and group games functions ####

soloGroup <- function(train_partition) {
  soloTrain <- train_partition %>%
  filter(matchType == "solo" |  matchType == "normal-solo" | matchType == "normal-solo-fpp" 
         | matchType == "solo-fpp" )
  #drop group columns in solo set
  drop <- c("DBNOs", "revives", "teamKills","X", "Id", "groupId","matchId","assists")
  soloTrain <- soloTrain[,!names(soloTrain) %in% drop]
  return(soloTrain)
}

squadGroup <- function(train_partitioin){
  temp <- train_partition %>%
  filter(matchType == "duo" |matchType == "squad" | matchType == "duo-fpp" |
         matchType == "normal-duo-fpp" |  matchType == "normal-squad" 
         | matchType == "normal-squad-fpp" | matchType == "squad-fpp")
  #might not want to drop group ID
  drop <- c("X", "Id", "groupId","matchId")
  temp <- temp[,!names(temp) %in% drop]
  return(temp)
}


#Data splitting
#partition the data to make it easier to work with
inTrain <- createDataPartition(y = train$winPlacePerc, p =.8, list = FALSE)
#create train and testing set
train_partition <- train[inTrain,]
test_partition <- train[-inTrain,]

#save for later
#write.csv(train_partition, "train_partition.csv")
#write.csv(test_partition, "test_partition.csv")

####Training model on solo####

#### LM model ####
soloTrain <- soloGroup(train_partition)
lm.model <- lm(winPlacePerc ~., data = soloTrain)

soloTest <- soloGroup(test_partition)
pred <- predict(lm.model, newdata = soloTest)



#### Random Forest ####
bagging.model <- randomForest(winPlacePerc ~., data = soloTrain, mtry = 21, importance = TRUE)

soloTest <- soloGroup(test_partition)
pred <- predict(bagging.model, newdata = soloTest)

RMSE(pred, test_partition$winPlacePerc)

#### GBM model ####
#solo model
gbm.model <- gbm(
  formula = winPlacePerc ~.,
  distribution = "gaussian",
  data = soloTrain,
  n.trees = 10000,
  cv.folds = 5,
  verbose = TRUE
)

pred <- predict(gbm.model, newdata = soloTest)
RMSE(pred, test_partition$winPlacePerc)
#optimal number of tree estimate
ntree_opt_cv <- gbm.perf(gbm.model, method = "cv")

squad <- squadGroup(train_partition)

####group model####
squadTrain <- squadGroup(train_partition)
squadTest <- squadGroup(test_partition)

gbm.model <- gbm(
  formula = winPlacePerc ~.,
  distribution = "gaussian",
  data = squadTrain,
  n.trees = 9210,
  cv.folds = 5,
  verbose = TRUE
)

pred <- predict(gbm.model, newdata = squadTest)
RMSE(pred, test_partition$winPlacePerc)

#optimal number of tree estimate
ntree_opt_cv <- gbm.perf(gbm.model, method = "cv")