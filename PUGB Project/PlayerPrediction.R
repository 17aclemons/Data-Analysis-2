#Andrew Clemons
#PUBG Project

train <- read.csv("train_V2.csv")
test <- read.csv("test_V2.csv")

part <- round((nrow(train) *.01))

sample.Train <- sample(train, 1:part)
write.csv(sample.Train, "sampleTrain.csv")

