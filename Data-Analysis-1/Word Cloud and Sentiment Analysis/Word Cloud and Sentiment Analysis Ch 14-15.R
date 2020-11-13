#Andrew Clemons
#Data Analysis 1

#install packages that R told me to
install.packages("tm")
install.packages("NLP")
library(NLP)
library(tm)
install.packages("wordcloud")
install.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud)

#wordcloud
#change my working directory
setwd("C:/Users/XPS/Desktop/Software/ANLY 6100/Word Cloud and Sentiment Analysis")

#import Donald Trumps Dallas rally speach
txt <- scan("DallasSpeachTrump.txt", character(0))

#convert to vector then to a corpus
words.vec <- VectorSource(txt)
words.corpus <- Corpus(words.vec)
words.vec

#clean data to such as converting all to lowercase, removing punctation
#numbers and stop words
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)

words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

#convert to document matrix
tdm <- TermDocumentMatrix(words.corpus)

#convert to regular matrix
m <- as.matrix(tdm)

#sum the occurences of every word and the sort them in decreasing order
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing = TRUE)
head(wordCounts)

#create a cloud frame and then a 
#word clouding using columns of the wordframe
cloudFrame <- data.frame(word = names(wordCounts), freq = wordCounts)
wordcloud(cloudFrame$word, cloudFrame$freq)

#fix the margins
par(mar=rep(0,4))
#creating a wordcloud with row names, how many times they appear, 
#only displaying the top 50, words that appear more than 10 times and added color
wordcloud(names(wordCounts), wordCounts, min.freq = 10, 
          max.words = 50, rot.per = .5, colors = brewer.pal(8, "Dark2"))

#sentiment analysis

#get Positive and negative Lexicons
pos <- "positive-words.txt"
neg <- "negative-words.txt"

#clean Lexicon files
p <- scan(pos, character(0), sep = "\n")
n <- scan(neg, character(0), sep = "\n")

#remove the beginning of the document that we don't need
p <- p[-1:-34]
n <- n[-1:-34]
#total number of words
totalWords <- sum(wordCounts)
#names of all the words
words <- names(wordCounts)

#words that matched in with the lexicon
matched <- match(words, p, nomatch = 0)
head(match,10)

#number of words that match
mCounts <- wordCounts[which(matched != 0)]

#names and sum of positive words
mWords <- names(mCounts)
nPos <- sum(mCounts)

#number of negative words that match with the lexicon
matched <- match(words, n , nomatch = 0)
#number of all negative words that match
nCounts <- wordCounts[which(matched != 0)]

#Names and sum of negative words
nNeg <- sum(nCounts)
nWords <- names(nCounts)

#finding the total positive words and the ratio compared to the document
totalWords <- length(words)
ratioPos <- nPos/totalWords
ratioPos

#finding the total negative words and the ratio compared to the document
ratioNeg <- nNeg/totalWords
ratioNeg

"The document is 34% positive based on the books analysis, 
while only 17% negative. I would consider this a positive document,
while not really knowing the standard range of what qualifies as
a positive document. Also, a lot of the words in my word cloud are 
conjunctions or what seems to be other stop words. I would like to clean
the data further to remove these, because I don't know what value
knowing how many times 'got' was said. It makes seense that Texas is 
said a lot because the rally was in texas but its also interesting 
what names appear the most."