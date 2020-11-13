#Andrew Clemons
#Data Analysis 1 Intro to Vectors

#1. Character - like letters or strings
  #numeric - real or decimal numbers
  #integer - numbers without decimals
  #logical- true or false boolean values
  #comples - complex numbers like imaginary numbers
  #raw - raw type are used to hold raw bytes

#2.
x <- 3.5
class(x)
#Numeric data type

#3.
y <- c(5, "z", FALSE)
class(y)
#character class, because all data types can be resolved to characters

#4.
x <- c(1,2,3,4)
z <- c(5,6,7,8)
cbind(x,z)
#cbind takes two separate vectors and combines them by columns

#5.Yes, similar to question 3 you can have different classed elements
# in the same vector
#6.
a <- 1:3
b <- 2:3
a+b
#R adds vectors together similar to matrix addition.
# I think 5 is repeated because it returns to the start
# of the second vector because it is shorter than the first

#7.

z <- c(3,5,3,4,1,11,13,1,5)
z[z > 5] <- 5
z