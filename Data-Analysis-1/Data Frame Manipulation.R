#Andrew Clemons
#Data Analytics  1 - 6100
#Data Frame Manipulation Ch 4-5 Homework

#assign mtcars to object mcars
mcars <- mtcars

#1
help(mcars)

#2 Honda Civic has lowest horsepower at 52
#one way to do it
mcars[which.min(mcars$hp),]
#other way to do it
x <- min(mcars$hp)
row.names(mcars[mcars$hp == x,])

#3 Lincoln continental, Cadillac Fleetwood
y <- min(mcars$mpg)
y
row.names(mcars[mcars$mpg == y, ])

#4
mcars[order(mcars$hp,decreasing = TRUE),]

#5
mcars[order(mcars$mpg),]

#6 Maserati Bora
x <- mcars[order(mcars$hp/mcars$mpg, decreasing = TRUE),]
row.names(x[1,])
