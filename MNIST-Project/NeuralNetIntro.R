library(ISLR)
library(caret)
library(neuralnet)
College <- College
maxs <- apply(College[,2:18],2, max)
mins <- apply(College[,2:18],2, min)

scaled.data <- as.data.frame(scale(College[,2:18], 
                                   center = mins,
                                   scale = maxs - mins))

# Cool trick with levels: as.numeric(College$Private)-1
scaled.data <- cbind(as.numeric(College$Private)-1, 
                     scaled.data)
colnames(scaled.data)[1] <- "Private"
trainIndex <- createDataPartition(scaled.data$Private,
                                  p = .75, list = FALSE)
train <- scaled.data[trainIndex,]
test <- scaled.data[-trainIndex,]

colnames(scaled.data[,2:length(scaled.data)])

features <- colnames(scaled.data[,2:length(scaled.data)])
f <- paste(features, collapse = ' + ')
f <- paste('Private ~', f)

# Why?
set.seed(54321)
nn <- neuralnet(formula = f, data = train, hidden = c(3,3),
                linear.output = FALSE, err.fct = 'ce', stepmax = 1e6)
plot(nn)
three.three.prediction<- compute(nn,test[2:18])
head(three.three.prediction$net.result)

three.three.output <- sapply(three.three.prediction$net.result,round,digits=0)
table(three.three.output, test$Private)
confusionMatrix(as.factor(three.three.output), as.factor(test$Private))

set.seed(54321)
nn1010 <- neuralnet(formula = f, data = train, hidden = c(10,10), stepmax = 1e6,
                linear.output = FALSE, err.fct = 'ce')
plot(nn1010)
# Which one was faster?

ten.ten.prediction<- compute(nn,test[2:18])
head(ten.ten.prediction$net.result)
ten.ten.output <- sapply(ten.ten.prediction$net.result,round,digits=0)
# What if we wanted to create a ROC?
# We would need to convert these into probabilities, change
# function to log-likelihood, like logistic regression.
# See documentation.

table(ten.ten.output, test$Private)
confusionMatrix(as.factor(ten.ten.output), as.factor(test$Private))
# Is it any more accurate?

# Sparsity of good neural net tools in R because it's too big for
# RAM. H20 and other solutions available so you can push
# it onto cloud computing.


#http://topepo.github.io/caret/train-models-by-tag.html#Neural_Network

