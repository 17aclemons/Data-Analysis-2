#Andrew Clemons Data Analysis 1
#Data Munging

#This won't compile cause I never pulled the CSV file from the internet

#This reads in a comma deliminated list and creates a data frame
testFrame <- read.csv(fileFromInternet)


#Displayes the structure of the data frame similar to summary()
# shows the number of objects and variables and the first few entrys of each variable
str(testFrame)

#this line gets rid of the first 8 rows in the dataframe
testFrame <- testFrame[-1:-8]

#looks at a summary of the last 5 (specifically rows 6 to 10) 
#columns and shows information about them
summary(testFrame[,6:10])

#keeps the first 5 columns of the data frame
testFrame <- testFrame[,1:5]

#shows last 5 rows of the testFrame
tail(testFrame, 5)

#deletes the last five rows in testFrame
testFrame <- testFrame[-52:-58,]

#changes the first column to stateName
testFrame$stateName <- testFrame[,1]

#colnames returns the names of the columns within the dataframe
colnames(testFrame)

#using colnames when given a data frame as a parameter
#returns the column names and stores it in a new variable
cnames <- colnames(testFrame)

#changes the first element name to "newName"
cname[1] <- "newName"

#prints out the data frame
cnames

#changes the colnames in test frame to the names found in cnames
colnames(testFrame) <- cnames

#prints the dataframe
colnames(testFrame)

#removes the first column in the dataframe
testFrame <- testFrame[, -1]

#gsub replaces all occurrences of a pattern and returns it as a new string. 
#the backslashes are escape characters, the force the next dot to be treated literally
#rather than as a wildcard
testFrame$stateName <- gsub("\\.","",testFrame$stateName)

#this and the next four lines similar to it remove the commas in the data
testFrame$april10census <- gsub(",","", testFrame$X)

#get rid of spaces and converts to a number with as.numeric
testFrame$april10census <- as.numeric(gsub(",","",testFrame$april10census))

#remove the columns with the X names
testFrame <- testFrame[,-1:-4]

#Displayes the structure of the data frame similar to summary()
# shows the number of objects and variables and the first few entrys of each variable
str(testFrame)

#shows first five rows
head(testFrame,5)

#removes the row names in the dataframe
rownames(testFrame)<- NULL

#shows first five rows
head(testFrame, 5)

#sorts in the order of the population, then provides a list of row
#indices
sortStates <- testFrame[order(testFrame$july11pop),]

#shows the first five rows
head(sortedStates,5)

#sorts the from largest to smallest by inversing it with the - sign
#making the largest rows to the smallest
sortedStates <- testFrame[order(-testFrame$july11pop), ]

#shows first five rows again
head(sortedStates,5)