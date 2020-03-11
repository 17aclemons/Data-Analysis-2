#Andrew Clemons
#ISLR Chapter 9
library(ISLR)
library(e1071)
library(ROCR)
####1)####
#Do problem 7 in the homework. It is essentially the same as the lab. Just walk through each of the steps.
#A
df <- Auto
df$mpg <- ifelse(df$mpg > median(df$mpg),1, 0)
df$mpg <- as.factor(df$mpg)

#B
svm.linear <- svm(mpg ~., data = df, type = "C-classification", kernel = "linear", cost = 10)
set.seed(1)
linear.tune <- tune(svm, mpg~., data = df, ranges = list(cost = c(.001,.01,.1, 1, 5, 10,100,1000)))
summary(linear.tune)
tune.bestmod <- linear.tune$best.model
#cost = 100 leads to the best cross validation performance compared to the other values of the cost.

#C
svm.radial <- svm(mpg ~., data = df, type = "C-classification",kernel = "radial", gamma = 1, cost = 100, scale = FALSE)
tune.radial <- tune(svm, mpg~., data = df, kernel = "radial", ranges = list(cost = c(.1,1,10,100,1000), gamma = c(.5,1,2,3,4)))
summary(tune.radial)
tune.radial$best.parameters
#best parameters are cost = 1 and gamma = 1

svm.poly <- svm(mpg ~., data = df, type = "C-classification",kernel = "polynomial", gamma = 1, cost = 100, scale = FALSE)
tune.poly <- tune(svm, mpg~., data = df, kernel = "polynomial", ranges = list(cost = c(.1,1,10,100,1000), gamma = c(.5,1,2,3,4)))
summary(tune.poly)
tune.poly$best.parameters
#best parameters are cost = .1 and gamma = 1

#D)

df$mpg <- as.character(df$mpg)
df$mpg <- as.numeric(df$mpg)

#linear kernel
plot(svm.linear, df, mpg ~ cylinders)
plot(linear.tune)
#radial kernal 
plot(tune.radial)
#polynomial kernel
plot(tune.poly)

#I don't know why my plots are different, the book doesn't say what I need to plot
#and it errors when I put mpg ~ anyVariable.

####2)####
#Perform an ROC on the previous problem. I recommend follow the steps in the lab.
rocplot =function (pred , truth){
  predob = prediction (pred , truth)
  perf = performance (predob , "tpr", "fpr")
  plot(perf)
}

svm.linear.opt <- svm(mpg ~., data = df, kernel = "radial", cost = 10, decision.values = TRUE)
fitted <- attributes(predict(svm.linear.opt, df, decision.values = TRUE))$decision.values

rocplot(fitted, df, add = TRUE, col = "red")