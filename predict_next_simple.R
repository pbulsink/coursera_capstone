library(readr)
library(stringr)
library(stringi)
library(ngram)

cleanStream<-function(text_stream, badwords=NULL){
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

cutNMax<-function(text_stream, nmax=5){
    t<-unlit(strsplit(text_stream, " "))
    if(length(t)<nmax)
        nmax<-length(t)
    return(t[1:nmax])
}

loadAndPrepGramsFromFile<-function(n, train = TRUE){
    f<-ifelse(train, paste0("./data/ng",n,"_train.RDS"), paste0("./data/ng",n,".RDS"))
    ng<-readRDS(f)
    setkey(ng, pregrams)
    ng
}


#Prepare for predictoins
# ng1<-loadAndPrepGramsFromFile(1)
# ng2<-loadAndPrepGramsFromFile(2)
# ng3<-loadAndPrepGramsFromFile(3)
# ng4<-loadAndPrepGramsFromFile(4)
# ng5<-loadAndPrepGramsFromFile(5)



stupid_backoff<-function(text,ng1,ng2,ng3,ng4,ng5){
    text_stream<-cleanStream(text)

    is4gram<-is3gram<-is2gram<-is1gram<-FALSE
    cand5<-cand4<-cand3<-cand2<-cand1<-NULL

    text4<-cutNMax(text_stream, 4)
    text3<-cutNMax(text4, 3)
    text2<-cutNMax(text3, 2)
    text1<-cutNMax(text2, 1)

    if(length(text4) == 4)
        cand5<-ng5[pregram == text4]
        cand5<-cand5[,pregram, by=freq]
        t5<-sum(cand5[,freq])
    if(length(text3) == 3)
        cand4<-ng4[pregram == text3]
    if(length(text2) == 2)
        cand3<-ng3[pregram == text2]
    if(length(text1) == 1)
        cand2<-ng2[pregram == text2]
}
