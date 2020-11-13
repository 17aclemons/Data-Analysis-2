#Andrew Clemons
#Chapter 3 ISLR Questions

####2####
#The KNN classifier is used to solve classification problems that have by identifying the neighborhood of X0
#and then estimating the conditional probability for a class j as the fraction of the poionts in the neighborhood
#that have responce values equal to j. The KNN regression method is used to solve regression problems by identifying
#the neighborhood of x0 and then estimationg F(x0) as the average of all the training responces in that neighborhood.
####3####
#a - Anser iii is correnct, the starting salary for males is higher than females if the gpa high enough (about 3.5 gpa)
#b - 137.1 or $137,100
#c - False - if we wanted to check this statement, set GPA or IQ to 0 and look at the p value to draw a conclusion.
# a conclusion can't be drawn just from the coefficents. 

####4####
#a - we don't know which RSS is lower because we don't have all the details of the training set but, as the relationship
# between X and Y is linear, we could expect the least squares line to be closer to the true regression line
# and the RSS for the linear regression may be lower than the cubic regression.

#b - the test RSS depends on test data so we need more details to know. We may assume that cubic regression will have a
# higher test RSS because overfit from training would have more error than linear regression.

#c - Polynomial regression has a lower RSS than the linear fit because of the higher flexibility.
# The more flexible model will follow points closer and reduce the RSS.

#d - We don't know which one would be lower, because we don't know how far from linear the relationship is.
# If it is closer to linear than cubic, the linear test RSS could be lower than the cubic or vice versa. It isn't clear
# what level of flexibility will fit the data better.

####10####
library(ISLR)
#a 
model <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(model)

#b - 
#given all other predictors remain fixed,
# Price - the average effect of a price increase of $1 is a decrease of ~54 sales units 
# UrbanYes - average unit sales in urban locations are ~21.91 units less than in rural locations
# USYes - average sales in a US store are ~1200 units more than non US stores

#c - Sales = 13.043469 + (-.054459) * Price + (-.021916) * Urban + (1.200573) * US + error
  # Urban = 1 if true and 0 if false
  # US = 1 if true and 0 if false

#d - Can reject the Price and US variables

#e 
small_model <- lm(Sales ~ Price + US, data = Carseats)

#f - R^2 is somewhat better than the bigger model. About 20% of the variability is explained by the model

#g 
confint(small_model, level = .95)

#h
plot(small_model)
#the Residuals vs Leverage charge shows some outliers and leverage points

####15####
library(MASS)
#a
fitZn <- model1 <- lm(crim ~ zn, Boston)
fitIndus <- lm(crim ~ indus, Boston)
fitChas <- lm(crim ~chas, Boston)
fitNox <- lm(crim ~nox, Boston)
fitRm <- lm(crim ~ rm, Boston)
fitAge <- lm(crim ~ age, Boston)
fitDis <- lm(crim ~ dis, Boston)
fitRad <- lm(crim ~ rad, Boston)
fitTax <- lm(crim ~ tax, Boston)
fitPtratio <- lm(crim ~ ptratio, Boston)
fitBlack <- lm(crim ~ black, Boston)
fitLstat <- lm(crim ~ lstat, Boston)
fitMedv <- lm(crim ~ medv, Boston)
#all predictors have a p-value < .05 except "chas". So there is a statistically significant association 
#between each predicter and response except for the "chas" predictor

#b 
bostonRegression <- lm(crim ~ ., Boston)
#reject the null for zn, dis, rad, black and medv

#c 
simpleReg <- vector("numeric", 0)
simpleReg <- c(simpleReg,fitZn$coefficient[2])
simpleReg <- c(simpleReg,fitIndus$coefficient[2])
simpleReg <- c(simpleReg,fitChas$coefficient[2])
simpleReg <- c(simpleReg,fitNox$coefficient[2])
simpleReg <- c(simpleReg,fitRm$coefficient[2])
simpleReg <- c(simpleReg,fitAge$coefficient[2])
simpleReg <- c(simpleReg,fitDis$coefficient[2])
simpleReg <- c(simpleReg,fitRad$coefficient[2])
simpleReg <- c(simpleReg,fitTax$coefficient[2])
simpleReg <- c(simpleReg,fitPtratio$coefficient[2])
simpleReg <- c(simpleReg,fitBlack$coefficient[2])
simpleReg <- c(simpleReg,fitLstat$coefficient[2])
simpleReg <- c(simpleReg,fitMedv$coefficient[2])
multipleReg <- vector("numeric", 0)
multipleReg <- c(multipleReg, bostonRegression$coefficients)

plot(simpleReg, multipleReg[-1], col = "blue")
# There is a difference between the simple and multiple regression coefficents. For simple regression the slope represents the 
#average effect of an increase in the predictor without considering other pedictors. For multiple regression the slope represent
#the average effect of an increase in the predictor while holding other predictors fixed. 

#d
dzn <- lm(crim ~ poly(zn, 3), Boston)
summary(dzn)

dindus <- lm(crim ~ poly(indus, 3), Boston)
summary(dindus)

dnox <- lm(crim ~ poly(nox,3), Boston)
summary(dnox)

drm <- lm(crim ~ poly(rm,3),Boston)
summary(dnox)

dage <- lm(crim ~ poly(age,3),Boston)
summary(dage)

ddis <- lm(crim ~ poly(dis,3),Boston)
summary(ddis)

drad <- lm(crim ~ poly(rad,3),Boston)
summary(drad)

dtax <- lm(crim ~ poly(tax,3),Boston)
summary(dtax)

dpratio <- lm(crim ~ poly(pratio, 3), Boston)
summary(dpratio)

dblack <- lm(crim ~ poly(black, 3), Boston)
summary(dblack)

dlstat <- lm(crim ~ poly(lstat, 3), Boston)
summary(dlstat)

dmedv <- lm(crim~ poly(medv,3), Boston)
summary(dlstat)
