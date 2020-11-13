#### 1 ####
#a - Better, a flexible method will fit better. With the large sample size it would perform better than an inflexible approach
#b - Worse, a flexible method would overfit the small sample size
#c - Better, with more degrees of freedom a flexible model is better 
#d - Worse, a flexible method would fit to the noise and not the data increasing variance 
#### 2 ####
#a - Regression, inference, n = 500 and p = 3
#b - Classification, prediction, n = 20 and p = 12
#c - Regression, prediction, n = 52 and p = 3
#### 5 ####
#The advantages of a flexible method are that it might have a better fit for non-linear models
#and it helps decrease the bias. The disadvantages of a flexible approach is that it requires estimating a larger
#number of parameters. It could follow noise to closely and increase variance.
#A more flexible method would be better than a less flexible if we are interested in prediction and not interpreting the
#results. Less flexible approach would be better than a more flexible approach when inference is more important
#than interpretation of results
#### 6 ####
#parametric approach turns the problem of estimating F to estimating a set of parameters because it 
#assumes the form of F. Non-parametric approach doesn't assue the form of F so it needs a large sample
#to accurately estimate F. The advantages of a parametric to regression or classification are the simplifying
#of modeling F to fewer parameters and not as many observations are required compared to non-parametric approaches.
#### 7 ####
#a - in order of obs 1 - 6, Distance is 3 , 2 , 3.16, 2.23, 1.41, 1.73
#b - Green
#c - Red
#d - When K is larger, the boundary becomes more linear, so we would expect the best value for K to be small
#### 8 ####
#a 
college <- read.csv("College.csv")
#b
names(college)
#c
summary(college)
c <- college[,1:10]
pairs(c)
plot(college$Private,college$Outstate)
elite <- rep("No", nrow(college))
elite[college$Top10perc >50] = "Yes"
elite = as.factor(elite)
college = data.frame(college,elite)
summary(college)
plot(college$Outstate, college$Elite)
hist(college$Outstate)
hist(college$PhD)


#### 10 ####
library(MASS)
boston <- Boston
?Boston
#a - 506 rows, 14 columns, rows are observations, columns are different info like crime per capita, proportions of non-retail buisness acres per town etc
#b
library(ggplot2)
ggplot(boston, aes(x = age, y = crim)) + geom_point()
ggplot(boston, aes(x = crim, y = dis)) + geom_point()
ggplot(boston, aes(x = crim, y = ptratio)) + geom_point()
#more crime is closer to the city centers than far away
#There is more crime where there are more older buildings

#c
hist(Boston$crim)
#Most suburbs don't have a lot of crime

#d
hist(Boston$crim)
nrow(Boston[Boston$crim > 20,])
# only one suburb with really high crime
hist(Boston$tax)
nrow(Boston[Boston$tax ==650])
hist(Boston$ptratio)
nrow(Boston[Boston$ptratio > 20,])
#e
nrow(Boston[Boston$chas == 1,])
#35 suburbs
median(Boston$ptratio)
#19.05
row.names(Boston[min(Boston$medv),])
#entry 5
range(Boston$tax)
Boston[min(Boston$medv),]$tax
#h
nrow(Boston[Boston$rm > 7,])
#64 suburbs
nrow(Boston[Boston$rm > 8,])
#13 suburbs