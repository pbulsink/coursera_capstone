#processing with TM

library(tm)

#make our corpus
docs <- Corpus(DirSource("./data/en_US"), readerControl = list(language = "en_US", load=TRUE))

summary(docs)

#Preprocess.

#Do punctuation removal
docs <- tm_map(docs, removePunctuation)

#Don't remove numbers, we want full predictions
#docs <- tm_map(docs, removeNumbers)

#Don't lowercase, we want proper predictions
#docs <- tm_map(docs, tolower)

#Don't remove stopwords, we want to predict those
#docs <- tm_map(docs, removeWords, stopwords('english'))

#Do however, remove curse words:
badwords_url<-"https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
if (!file.exists("./data/badwords.txt")){download.file(badwords_url, destfile="./data/badwords.txt")}
bw<-read.table("./data/badwords.txt", sep="\n", strip.white = TRUE)
bw<-as.character(bw[,1])
docs<-tm_map(docs, removeWords, bw[,1])

#Also remove crud (particularly from Twitter)
crudwords<-c("RT")#, "AP", "via")
docs <- tm_map(docs, removeWords, crudwords)

#Pull out twitter usernames (not real words). Leave hashtags, as they might be good for predicting
#Also email addresses
#Have to do on corpus before TM
#(ie file<-readlines(), then gsub('@[a-zA-Z0-9]+', '', file), and gsub('[a-zA-Z\.\-\_]+@[a-zA-Z\.\_\-]+', '', file))


#Don't stem, we want full predictions
library(SnowballC)
#docs <- tm_map(docs, stemDocument)

#Do whitespace removal
docs <- tm_map(docs, stripWhitespace)

#Finalize the docs
docs <- tm_map(docs, PlainTextDocument)

#Make the term matrix
dtm <- DocumentTermMatrix(docs)
dtm
