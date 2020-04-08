#Andrew Clemons
#Pattern Project
#Data Analytics Final Project

library(data.table)
library(ggplot2)
library(dplyr)
library(randomForest)

#Laptop
df <- fread("C:/Users/XPS/Desktop/Software/Data-Analysis-2/Pattern-Project/training_data_20200403.csv")

#Subsetting train to a smaller data.frame
train <- df
id <- unique(train$asin)
id_subset <- id[1:150]

train <- train[train$asin %in% id_subset]
train <- train[,c(-1:-4)]
forest <- randomForest(order_items_total ~., data = train,
                       importance = TRUE,
                       verbose = TRUE)
varImpPlot(forest,n.var = 15)
