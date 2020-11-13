#Andrew Clemons
#Data Analysis 1
#1 Load Iris data set
iris

#2 help function
help(iris)

#3 structure and summary
summary(iris)
str(iris)

#4 average sepal length of virginica where 1 is sepal length column
virginica <- iris[101:150,1]
mean(virginica)
#answer is 6.58 is average sepal length of virginica

#5 average sepal length, width, petal length and width
mean(iris$Sepal.Length)  #5.84333
mean(iris$Sepal.Width)   #3.057333
mean(iris$Petal.Length)  #3.758
mean(iris$Petal.Width)   #1.199333

#6 average sepal length, width, petal length and width of each species
setosa = iris[1:50,]
versicolor = iris[51:100,]
virginica <- iris[101:150,]

#setosa values
mean(setosa$Sepal.Length) # 5.006
mean(setosa$Sepal.Width)  #3.428
mean(setosa$Petal.Length) #1.462
mean(setosa$Petal.Width)  #.246

#versicolor values
mean(versicolor$Sepal.Length)  #5.936
mean(versicolor$Sepal.Width)   #2.77
mean(versicolor$Petal.Length)  #4.26
mean(versicolor$Petal.Width)   #1.326

#virginica values
mean(virginica$Sepal.Length)  #6.588
mean(virginica$Sepal.Width)   #2.974
mean(virginica$Petal.Length)  #5.552
mean(virginica$Petal.Width)   #2.026

#7 exponent function
exponent <- function(base, exponent){
  return(base^exponent)
}
exponent(2,3)

#8 function that outputs the mean of a column
columnMean <- function(dataFrame,columnName ){
  return(mean(dataFrame[[columnName]]))
}
columnMean(iris,"Sepal.Length")

#9 function can be used to generate random output on normal distribution
# 10 random numbers from a normal distribution
rnorm(10)
 
#10 random number on a uniform distribution
runif(10)

#11 Set the seed to 5 generate 10 random normally distributed numbers
set.seed(5)
rnorm(10)

set.seed(5)
rnorm(10)
#for creating random numbers/objects that can be reproduced