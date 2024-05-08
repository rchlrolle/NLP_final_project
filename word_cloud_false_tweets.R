# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(tidyverse)
library(data.table)

fake_news <- fread("fakeNews.csv")

glimpse(fake_news)


fake_news[fake_news$Origin == "Twitter", .(Text)]



# Load the data as a corpus
docs <- Corpus(VectorSource(fake_news$Text))


inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Let's clean the text

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("can", ' will')) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(function(x) gsub('["“”]', "", x)))
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)


# Text stemming
# docs <- tm_map(docs, stemDocument)

# Let's create a Term Document Matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Now Let's create the Word Cloud !!

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

