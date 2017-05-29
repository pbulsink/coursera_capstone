library(readr)
library(stringr)
library(stringi)
library(ngram)

clean_stream<-function(text_stream, badwords=NULL){
    #get badwords if not supplied.
    if(is.null(badwords)){
        badwords<-unlist(read.table("./data/badwords.txt"))
    }

    #clean formatting
    text_stream<-stri_trans_general(text_stream,"Latin-ASCII")
    text_stream <- gsub("[µºˆ_]+", '', text_stream, perl = TRUE)

    #set to lowercase
    text_stream <- char_tolower(text_stream)

    #Remove URLS
    text_stream <- gsub("(f|ht)tp(s?):\\/\\/(.*)[.][a-z=\\?\\/]+", "", text_stream, perl = TRUE)

    #Remove Twitter 'Via @username' and '@usernames' and 'rt' and hashtags
    text_stream <- gsub("via\\s*@[a-z_]{1,20}","", text_stream, perl = TRUE)
    text_stream <- gsub("\\s@[a-z_]{1,20}", "", text_stream, perl = TRUE)
    text_stream <- gsub("\\brt\\b", "", text_stream, perl = TRUE)
    text_stream <- gsub("#[a-z0-9_]+", "", text_stream, perl = TRUE)

    #Cut punctuation/symbols/numbers
    text_stream <- gsub("[^a-zA-Z\\ ]", "", text_stream, perl = TRUE)

    #Load a 'bad words' list to remove from the corpus
    badwords_url <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    badwords <- unlist(strsplit(RCurl::getURL(badwords_url), "\n"))
    #last 'bad word' is an emoji, remove
    badwords <- badwords[1:length(badwords)-1]
    #replacement builder code from getAnywhere(tm::removeWords.character)
    text_stream <- gsub(sprintf("(*UCP)\\b(%s)\\b", paste(badwords, collapse = "|")), "", text_stream, perl = TRUE)

    #Remove multiple spaces from deleted words
    text_stream <- gsub("\\s\\s+", " ", text_stream, perl = TRUE)

    #add start ^ and end $ tags to the sentences.
    text_stream<-paste("^",text_stream)

    return(text_stream)
}

cut_n_max<-function(text_stream, nmax=5){
    t<-unlit(strsplit(text_stream, " "))
    if(length(t)<nmax)
        nmax<-length(t)
    return(t[1:nmax])
}


