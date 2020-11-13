#Andrew Clemons
#Chapter 4 ISLR Excercises

####5####
#a -If the Bayes decision boundary is linear, do we expect LDA or
#QDA to perform better on the training set? On the test set?

#QDA will always perform at least as well as LDA on a training set. 
#But, QDA could overfit data in a testing set SO LDA should be better

#b - If the Bayes decision boundary is non-linear, do we expect LDA or QDA 
#to perform better on the training set? On the test set ?

#QDA should have a better fit due to being able to account for curves in the
#graphs. LDA wouldn't be able to account for large curves in a graph due to 
#being a straight line. 

#c - In general, as the sample size n increases, do we expect the test prediction 
#accuracy of QDA relative to LDA to improve, decline, or be unchanged? Why ?

#increasing n typically decrease the varicance so since QDA is more flexible that 
#LDA, QDA's performance should improve

#d - True or False: Even if the Bayes decision boundary for a given
#problem is linear, we will probably achieve a superior test 
#error rate using QDA rather than LDA because QDA is flexible
#enough to model a linear decision boundary. Justify your answer.

#False, QDA would perform better in training error rate, but could overfit data
#and produce a higher test error rate. 

####6####
#a -Estimate the probability that a student who studies for 40 h and
#has an undergrad GPA of 3.5 gets an A in the class.

#e^B0 + B1X / 1+ e^B0+ B1X
exp(1)^(-6 + .05*40 + 1*3.5)/(1+exp(1)^(-6 + .05*40 + 1*3.5)) 
# = .3775 probability

#b - How many hours would the student in part (a) need to study to have a
#50% chance of getting an A in the class ?

#exp(1)^(-6 + .05*X1 + 1*3.5)/(1+exp(1)^(-6 + .05*x1 + 1*3.5)) = .5
#copying how the book converts it
#exp(1)^(-6 + .05*X1 + 1*3.5) = .5, taking the log of both sides and simplifying
# = 50 hours of studying

####8####

#QDA with a training set can have 0% error rate, since they took the average that means
#the test set had a 36% error rate
#Logistic regression is better because 30% error rate is better than 36% error rate
# - Jacob Miller 11/22/19 ~3:00pm during his office hours

####9####
#a - fraction of .27/100 people defaluting
#p(x) = .37 - .37p(x) = .27

#b - 19/100 that she will default
.16/(1-.16)
####10####
library(ISLR)
df <- Weekly

#a
dim(df)
summary(df)
cor(Weekly[,-9])
attach(Weekly)
plot(Volume)

#b
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
             Volume, data = fit, family = binomial)
summary(glm.fit)
coef(glm.fit)
#c
glm.probs = predict(glm.fit, type = "response")
glm.pred = rep("Down", 1089)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction)
#the model is correct 56% percent of the time

#d 
train <- (Year < 2009)
test <- Weekly[!train, ]
testDirection <- Direction[!train ]
glm.fit2 <- glm(Direction ~ Lag2, data = Weekly,family = binomial, subset = train)
glm.probs2 <- predict(glm.fit2, test, type = "response")
glm.pred2 <- rep("Down", 104)
glm.pred2[glm.probs2 > .5] <- "Up"
table(glm.pred2, testDirection)
#the model is correct 62.5% of the time

#e
library(MASS)
lda.fit <- lda(Direction ~ Lag2, data = Weekly, subset = train)

lda.pred <- predict(lda.fit, test)
table(lda.pred$class, testDirection)
#the model is correct  62.5% of the time

#f
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.pred <- predict(qda.fit, test)
table(qda.pred$class, testDirection)
#the model is correct ~59% of the time

library(class)
trainX <- cbind(Lag1, Lag2)[train,]
testX <- cbind(Lag1, Lag2)[!train,]
trainDirectionX <- Direction[train]

set.seed(1)
knn.pred = knn(trainX, testX, trainDirectionX, k= 1)
table(knn.pred, testDirection)
#the model is correct about 50% of the time

#h LDA has the smallest error

####11####
Auto
mpg01 <- rep(0,length(Auto$mgp))
mpg01 <- Auto$mpg > median(Auto$mpg)
Auto <- data.frame(Auto,mpg01)













