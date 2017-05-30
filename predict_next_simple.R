library(stringi)

cleanStream<-function(text_stream, badwords=NULL){
    #get badwords if not supplied.
    if(is.null(badwords)){
        badwords<-unlist(read.csv("./badwords.txt", header = FALSE, as.is = TRUE)[,1])
    }

    #clean formatting
    text_stream<-stri_trans_tolower(text_stream,"Latin-ASCII")
    text_stream <- gsub("[µºˆ_]+", '', text_stream, perl = TRUE)

    #Remove URLS
    text_stream <- gsub("(f|ht)tp(s?):\\/\\/(.*)[.][a-z=\\?\\/]+", "", text_stream, perl = TRUE)

    #Remove Twitter 'Via @username' and '@usernames' and 'rt' and hashtags
    text_stream <- gsub("via\\s*@[a-z_]{1,20}","", text_stream, perl = TRUE)
    text_stream <- gsub("\\s@[a-z_]{1,20}", "", text_stream, perl = TRUE)
    text_stream <- gsub("\\brt\\b", "", text_stream, perl = TRUE)
    text_stream <- gsub("#[a-z0-9_]+", "", text_stream, perl = TRUE)

    #Cut punctuation/symbols/numbers
    text_stream <- gsub("[^a-zA-Z\\ ]", "", text_stream, perl = TRUE)

    #replacement builder code from getAnywhere(tm::removeWords.character)
    text_stream <- gsub(sprintf("(*UCP)\\b(%s)\\b", paste(badwords, collapse = "|")), "", text_stream, perl = TRUE)

    #Remove multiple spaces from deleted words
    text_stream <- gsub("\\s\\s+", " ", text_stream, perl = TRUE)

    #add start ^ and end $ tags to the sentences.
    text_stream<-paste("^",text_stream)

    return(text_stream)
}

cutNMax<-function(text_stream, nmax=5){
    t<-unlist(strsplit(text_stream, " "))
    if(length(t)<nmax)
        nmax<-length(t)
    return(t[(length(t)-nmax+1):length(t)])
}

loadAndPrepGramsFromFile<-function(train = TRUE){

    if(train){
        f<-paste0("./data/ng",1:5,"_train.RDS")
    }else{
        f<-paste0("./data/ng",1:5,".RDS")
    }
    message('reading 1')
    ng1<-readRDS(f[1])
    setkey(ng1, pregrams)
    message('2')
    ng2<-readRDS(f[2])
    setkey(ng2, pregrams)
    message('3')
    ng3<-readRDS(f[3])
    setkey(ng3, pregrams)
    message(4)
    ng4<-readRDS(f[4])
    setkey(ng4, pregrams)
    message(5)
    ng5<-readRDS(f[5])
    setkey(ng5, pregrams)

    nglist<-list(ng1, ng2, ng3, ng4, ng5)
    nglist
}


#Prepare for predictoins
# ng1<-loadAndPrepGramsFromFile(1)
# ng2<-loadAndPrepGramsFromFile(2)
# ng3<-loadAndPrepGramsFromFile(3)
# ng4<-loadAndPrepGramsFromFile(4)
# ng5<-loadAndPrepGramsFromFile(5)

#allNGrams<-list()


stupid_backoff<-function(text, allNGrams){
    text_stream<-cleanStream(text)

    is4gram<-is3gram<-is2gram<-is1gram<-FALSE

    text4<-cutNMax(text_stream, 4)
    text3<-cutNMax(text4, 3)
    text2<-cutNMax(text3, 2)
    text1<-cutNMax(text2, 1)

    if(length(text4) == 4){
        candidates<-pgProp(text4, 4, lambdas=0, allNGrams)
    } else if(length(text3) == 3){
        candidates<-pgProp(text3, 3, lambdas=0, allNGrams)
    } else if(length(text2) == 2){
        candidates<-pgProp(text2, 2, lambdas=0, allNGrams)
    } else if(length(text1) == 1){
        candidates<-pgProp(text1, 1, lambdas=0, allNGrams)
    } else {
        candidates<-c(pgprop(text1, 0, lambdas=0, allNGrams))
    }
    candidates
}

pgProp<-function(text, ng, lambdas, allNGrams, lambdaval=0.4){
    message(paste("pg", ng))
    if(ng > 1){
        candidates<-allNGrams[[ng+1]]['pregrams' == paste(text, collapse = ' ')]
        tf<-sum(candidates[,'freq'])
        candidates[,prop:=(freq/tf)]
        if(lambdas > 0)
            candidates[,prop:=prop*lambdas*lambdaval]
        cless<-pgProp(cutNMax(text, ng-1), ng-1, lambdas+1, allNGrams, lambdaval)
        candidates<-rbind(candidates, cless)
    } else {
        candidates<-head(allNGrams[[1]], 5)
        candidates[,postgrams:=pregrams]
        candidates[,pregrams:=NULL]
        tf<-sum(allNGrams[[1]]['freq'])
        candidates[,prop:=freq/tf]
        if(lambdas > 0)
            candidates[,prop:=prop*lambdas*lambdaval]
    }
    return(candidates)

}
