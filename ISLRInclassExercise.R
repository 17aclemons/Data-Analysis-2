library(caret)
library(doParallel) #Parallel computing package
library(glmnet)
library(e1071)
model_data <- nlp_bag_model_data_example

myControl <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE, verboseIter = TRUE)

myGrid <- expand.grid(alpha = 0:1, 
                      lambda = seq(0,0.1, length = 4))

set.seed(42)
t1 = Sys.time()

cl <- makePSOCKcluster(5)
registerDoParallel(cl, 3) 
model <- train(category ~. data = model_data, 
               method = "glmnet",
               tuneGrid = myGrid,
               trControl = myControl)
stopCluster(cl)
t2 <- Sys.time()
print(difftime(t2,t1,units = 'mins'))