#Andrew Clemons
#Data Analysis 1

height <- c(60,48,61,67,72,70)
weight <- c(145, 155, 180, 225, 155, 140)
x <- 149.4

#1 average height is 63
mean(height)

#2 average weight is 166.66667
mean(weight)

#3 both have 6 observations
length(height)
length(weight)

#4 sum is 1000
sum(weight)

#5 sum of height is 378
sum(height)

#6 max height is 72
max(height)

#7 max weight is 225
max(weight)

#8
fat_weight <- weight + 5
fat_weight

#9
ppi <- weight/height
ppi

#10
if(max(height) > 60){
  print("yes")
}else{
  print("no")
}

#11
if(min(weight) > x){
  print("yes")
}else{
  print("no")
}


