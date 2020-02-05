#Andrew Clemons
#PUBG Project
library(caret)
library(dplyr)
library(Metrics)
library(randomForest)
library(gbm)
library(data.table)

train <- fread("C:/Users/XPS/Desktop/Software/Data-Analysis-2/PUBG CSV's/train_V2.csv")
setwd("C:/Users/XPS/Desktop/Software/Data-Analysis-2/PUGB Project")
####Data cleaning####
train <- train[!is.na(train$winPlacePerc),]

####Split to Solo and group games functions ####

soloGroup <- function(df) {
  soloTrain <- df %>%
    filter(matchType == "solo" |  matchType == "normal-solo" | matchType == "normal-solo-fpp" 
         | matchType == "solo-fpp" | matchType == "flaretpp"| matchType == "flarefpp")
  return(soloTrain)
}

squadGroup <- function(df){
  temp <- df %>%
    filter(matchType == "duo" |matchType == "squad" | matchType == "duo-fpp" |
         matchType == "normal-duo-fpp" |  matchType == "normal-squad" 
         | matchType == "normal-squad-fpp" | matchType == "squad-fpp" | matchType == "crashfpp"
         | matchType == "crashtpp" | matchType == "normal-duo"| matchType == "normal-duo-fpp")
  return(temp)
}


####Data splitting####
#partition the data to make it easier to work with
inTrain <- createDataPartition(y = train$winPlacePerc, p =.01, list = FALSE)
#create train and testing set
train_partition <- train[inTrain,]
#test_partition <- train[-inTrain,]
remove(train)
#save for later
#write.csv(train_partition, "train_partition.csv")
#write.csv(test_partition, "test_partition.csv")

####Training model on solo####
#group the train and test partitions into only solo games and group games
soloTrain <- soloGroup(train_partition)

drop <- c("DBNOs", "revives", "teamKills","X", "groupId","matchId","assists","roadKills","vehicleDestroys","Id")
soloTrain <- soloTrain[,!names(soloTrain) %in% drop]
#soloTest <- soloTest[,!names(soloTest) %in% drop]
squadTrain <- squadGroup(train_partition)

drop <- c("X", "groupId","matchId","Id")
squadTrain <- squadTrain[,!names(squadTrain) %in% drop]
#squadTest <- squadTest[,!names(squadTrain) %in% drop]

  #convert match type to factor
  soloTrain$matchType <- as.factor(soloTrain$matchType)
  squadTrain$matchType <- as.factor(squadTrain$matchType)
  
  
#### LM model ####
#lm.model <- lm(winPlacePerc ~., data = soloTrain)

#pred <- predict(lm.model, newdata = soloTest)
#mae(test_partition$winPlacePerc, pred)

#### Random Forest ####
#bagging.model <- randomForest(winPlacePerc ~., data = soloTrain, mtry = 21, importance = TRUE)

#soloTest <- soloGroup(test_partition)
#pred <- predict(bagging.model, newdata = soloTest)

#MAE(pred, test_partition$winPlacePerc)

#### GBM model ####
#Solo Model
solo_gbm.model <- gbm(
  formula = winPlacePerc ~., #the model ignores the id column
  distribution = "gaussian",
  data = soloTrain,
  n.trees = 10000,
  cv.folds = 5,
  verbose = FALSE
)

#optimal number of tree estimate
#ntree_opt_cv <- gbm.perf(solo_gbm.model, method = "cv")

####Group Model####
group_gbm.model <- gbm(
  formula = winPlacePerc ~.,
  distribution = "gaussian",
  data = squadTrain,
  n.trees = 7200,
  cv.folds = 5,
  verbose = FALSE
)

#optimal number of tree estimate
#ntree_opt_cv <- gbm.perf(group_gbm.model, method = "cv")

remove(soloTrain,inTrain,squadTrain,test_partition,train_partition)
#run on the testing set
test <- fread("C:/Users/XPS/Desktop/Software/Data-Analysis-2/PUBG CSV's/test_V2.csv")
test$X <- c(1:nrow(test))

soloTest <- soloGroup(test)
soloX <- soloTest$X
soloTest$matchType <- as.factor(soloTest$matchType)

squadTest <- squadGroup(test)
squadX <- squadTest$X
squadTest$matchType <- as.factor(squadTest$matchType)

soloPred <- predict(solo_gbm.model, newdata = soloTest)
groupPred <- predict(group_gbm.model, newdata = squadTest)

soloData <- data.frame("Id" = soloX, "winPlacePerc" = soloPred)
groupData <- data.frame("Id" = squadX, "winPlacePerc" = groupPred)

submission <- rbind(soloData,groupData)
submission <- submission %>%
  arrange(submission$Id)
submission$Id <- as.character(submission$Id)
fwrite(submission, "submission.csv",quote = TRUE, row.names = FALSE)

