#Andrew Clemons
#PUBG Project
library(caret)
library(dplyr)

train <- read.csv("train_V2.csv")
test <- read.csv("test_V2.csv")

#### Create a sample of  entire test set ####
set.seed(55555)
  #remove the rows with missing win placement percents in training set
  train <- train_V2
  train <- train[!is.na(train$winPlacePerc),]
  #partition the data to make it easier to work with
  inTrain <- createDataPartition(y = train$winPlacePerc, p =.05, list = FALSE)
  #create train and testing set
  train_partition <- train[inTrain,]
  test_partition <- train[-inTrain,]
  #save for later
  write.csv(train_partition, "train_partition.csv")
  write.csv(test_partition, "test_partition.csv")

####Split to Solo and group games ####
  soloTrain <- train_partition %>%
    filter(matchType == "solo")
  
  groupTrain <- train_partition %>%
    filter(matchType == "duo" |matchType == "squad")
  
  drop <- c("DBNOs", "matchType", "revives", "teamKills")
  soloTrain <- soloTrain[,!names(soloTrain) %in% drop]
  
  lm.model <- lm(winPlacePerc ~., data = soloTrain)
  