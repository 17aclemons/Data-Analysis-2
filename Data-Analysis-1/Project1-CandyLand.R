#Andrew Clemons
#Data Analysis 1
spin <- function(current_pos) {
  result <- sample(c(1:12,44,54,74,94), 1, 
                   prob = c())
  if(result <= 12) {
    return(current_pos + result)
  } else {
    return(result)
  }
}

turnsPerGame <- rep(NA, 1000)
for(i in 1:1000){
  player <- 0 #current position assuming you don't automatically start on the red tile
  turn <- 0 #number of turns taken
  #the while loop checks if the play has finished the game
  while (player < 134) {
    turn <- turn +1
    player <- spin(player)
    
    #one of the bridges
    if(player == 9){
      player <- 44
    }
    #one of the bridges
    if(player == 20){
      player <- 34
    }
    if(player %in% c(55, 95)) {turn <- turn + 1}
  }
  turnsPerGame[i] <- turn
}
#1 average number of turns per game, we know this because it is the average
#of the total turns per game
mean(turnsPerGame)

#2 largest number of turns is 28
max(turnsPerGame)
#It isn't an outlier as it has a duplicated value and has values very close to it 
head(sort(turnsPerGame, TRUE))

#3 shortest amount of turns
#12 turns is the shortest game, and it occures multiple times with values
#close to it
min(turnsPerGame)
head(sort(turnsPerGame))

#4 add summary stats
hist(turnsPerGame)
summary(turnsPerGame)
fivenum(turnsPerGame)


# average number of rounds and the game length in minutes
mean(turnsPerGame)*25*4/60
