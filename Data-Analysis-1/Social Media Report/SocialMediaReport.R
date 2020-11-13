#Andrew Clemons
#Social Media Report

library(NLP)
library(dplyr)
library(tidyr)
library(lubridate)
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(textdata)
library(topicmodels)
library(ggplot2)

fentanyl <- read.csv("twitterFentanyl.csv", stringsAsFactors = FALSE)

#1
#read it nrc dictionary 
#has every word and its corresponding sentiment in the nrc dictionary
sentiment <- get_sentiments("nrc")

#clean dates so that I can place them on a graph, created multiple new columns
#format Day/Month/Year format
df <- separate(fentanyl, date_created, into = c("DayOfWeek", "Month", "Day", "time", "TimeZone", "Year")," ")
df$Date <- as.Date(dmy_hms(paste(df$Day, df$Month, df$Year, df$time)))
df$floord <- floor_date(df$Date, unit = "months")
#format time, might not need
df$Time <- hms(df$time)


#tokenize data, remove stop words and join with nrc sentiment
tidy_text <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(sentiment)

#count type of sentiment found in fentanyl data
#create multiple graphs based on different sentiment analysis
tidy_text <- tidy_text %>%
  group_by(floord, sentiment) %>%
  summarise(n = n())

ggplot(tidy_text, aes(x = floord, y = n, group = sentiment)) + 
  geom_line() + facet_wrap(~sentiment)

#2
text <- df$text
text <- str_to_lower(text)
text <- gsub(pattern = "fentanyl", replacement = "", text)
#convert to vector then to a corpus
words.vec <- VectorSource(text)
words.corpus <- Corpus(words.vec)
#words.vec

#clean data to such as converting all to lowercase, removing punctation
#numbers, stop words, fentanyl and retweets
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeWords, "fentanyl")
#remove retweets
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

#create text document matrix
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
          max.words = 30, rot.per = .5, colors = brewer.pal(8, "Dark2"))

#3
#if looking at a range and one has days an other has minutes, that means the topic is being discussed more in minutes
df$Date_Full <- df$Date + df$Time
numberFour <- df %>%
  group_by(floord) %>%
  summarise(time_diff = difftime(max(Date_Full), min(Date_Full), 
                                 units = "hours"))
#people are talking about Fentanyl more

#4 Time it took to generate a 100 fentanyl related tweets
ggplot(numberFour, aes(x = floord, y = time_diff)) + geom_line() + xlab("Time") + ylab("Time Difference between tweets")

#percentage of retweets
retweet <- df %>%
  group_by(floord) %>%
  summarise(retweet = sum(is_retweet))

ggplot(retweet, aes(x = floord, y =(retweet/1400) )) +geom_line() + xlab("Time") + ylab("Retweet Percentage")


#5 LDA Analysis

#clean data 
text_unique <- unique(fentanyl$text)
text_unique <- str_to_lower(text_unique)
text_unique <- gsub(pattern = "fentanyl", replacement = "", x = text_unique)
text_unique <- text_unique[!(text_unique == "")]
words.vec <- VectorSource(text_unique)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

#LDA analysis with topics = 2 a seed of 1
lda <- DocumentTermMatrix(words.corpus)
lda2 <- LDA(
  lda,
  k = 2,
  control = list(seed = 1)
)
#tidy function on Lda2
lda2_topics <- lda2 %>%
  tidy(matrix = "beta")
#showing the top 10 from highest to lowest beta values
lda2_topics <- lda2_topics %>%
  top_n(10, beta) %>%
  arrange(desc(beta))

  