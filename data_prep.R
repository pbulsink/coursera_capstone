library(readr)
library(quanteda)
library(stringr)
library(stringi)

load_files<-function(dir="./data/en_US/"){
    en_blogs<-read_lines("./data/en_US/en_US.blogs.txt", progress=FALSE)
    en_news<-read_lines("./data/en_US/en_US.news.txt", progress=FALSE)
    en_twitter<-read_lines("./data/en_US/en_US.twitter.txt", progress=FALSE)
    return(list(en_blogs=en_blogs, en_news=en_news, en_twitter=en_twitter))
}

split_files<-function(files, what='dev'){
    set.seed(1)
    fraction_dev<-0.05
    fraction_training<-0.65
    fraction_test<-0.30
    en_blogs<-files[['en_blogs']]
    en_news<-files[['en_news']]
    en_twitter<-files[['en_twitter']]
    if(what!='all'){
        blog_role<-sample(1:3, size = length(en_blogs), replace = TRUE, prob = c(fraction_dev, fraction_training, fraction_test))
        news_role<-sample(1:3, size = length(en_news), replace = TRUE, prob = c(fraction_dev, fraction_training, fraction_test))
        twitter_role<-sample(1:3, size = length(en_twitter), replace = TRUE, prob = c(fraction_dev, fraction_training, fraction_test))
        ifelse(what=='test', whatn<-3, ifelse(what=='training', whatn<-2, whatn<-1))
        blog<-en_blogs[blog_role == whatn]
        news<-en_news[news_role == whatn]
        twitter<-en_twitter[twitter_role == whatn]
    }
    else{
        blog<-en_blogs
        news<-en_news
        twitter<-en_twitter
    }
    return(list(blog=blog, news=news,twitter=twitter))
}

clean_corpus<-function(corpus){
    blog<-corpus[['blog']]
    news<-corpus[['news']]
    twitter<-corpus[['twitter']]

    #Split into sentences, not lines
    blog<-as.character(tokens(blog, what='sentence', removeNumbers=TRUE, removePunc=TRUE, removeSymbols=TRUE, removeSeparators=TRUE, removeTwitter=TRUE, removeHyphens=TRUE, removeURL=TRUE))
    news<-as.character(tokens(news, what='sentence', removeNumbers=TRUE, removePunc=TRUE, removeSymbols=TRUE, removeSeparators=TRUE, removeTwitter=TRUE, removeHyphens=TRUE, removeURL=TRUE))
    twitter<-as.character(tokens(twitter, what='sentence', removeNumbers=TRUE, removePunc=TRUE, removeSymbols=TRUE, removeSeparators=TRUE, removeTwitter=TRUE, removeHyphens=TRUE, removeURL=TRUE))

    #add together
    corp_tot<-c(blog, news, twitter)
    rm(blog, news, twitter)

    corp_tot<-stri_trans_general(corp_tot,"Latin-ASCII")
    corp_tot <- gsub("[µºˆ_]+", '', corp_tot, perl = TRUE)

    #set to lowercase
    corp_tot <- char_tolower(corp_tot)

    #Remove URLS
    corp_tot <- gsub("(f|ht)tp(s?):\\/\\/(.*)[.][a-z=\\?\\/]+", "", corp_tot, perl = TRUE)

    #Remove Twitter 'Via @username' and '@usernames' and 'rt' and hashtags
    corp_tot <- gsub("via\\s*@[a-z_]{1,20}","", corp_tot, perl = TRUE)
    corp_tot <- gsub("\\s@[a-z_]{1,20}", "", corp_tot, perl = TRUE)
    corp_tot <- gsub("\\brt\\b", "", corp_tot, perl = TRUE)
    corp_tot <- gsub("#[a-z0-9_]+", "", corp_tot, perl = TRUE)

    #Cut punctuation/symbols/numbers
    corp_tot <- gsub("[^a-zA-Z\\ ]", "", corp_tot, perl = TRUE)

    #Get a 'bad words' list to remove from the corpus
    badwords_url <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    badwords <- unlist(strsplit(RCurl::getURL(badwords_url), "\n"))
    #last 'bad word' is an emoji, remove
    badwords <- badwords[1:length(badwords)-1]
    #replacement builder code from getAnywhere(tm::removeWords.character)
    corp_tot <- gsub(sprintf("(*UCP)\\b(%s)\\b", paste(badwords, collapse = "|")), "", corp_tot, perl = TRUE)

    #Remove multiple spaces from deleted words
    corp_tot <- gsub("\\s\\s+", " ", corp_tot, perl = TRUE)

    #add start ^ and end $ tags to the sentences.
    corp_tot<-paste("^",corp_tot,"$", sep=" ")

    return(corp_tot)
}

get_corpus<-function(what='dev'){
    f<-load_files()
    message('files loaded')
    c<-split_files(f, what = what)
    message('files split')
    c<-clean_corpus(c)
    message('files cleaned')
    return(c)
}
