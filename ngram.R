#ngrams
library(doParallel)
library(stringr)
library(dplyr)

# HASH_TABLE <- function() new.env()
# INSERT <- function(key, value, ht)  ht[[key]] <- value
# LOOKUP <- function(key, ht) ht[[key]]
# DELETE <- function(key, ht) rm(list=key,envir=ht,inherits=FALSE)

get_ngrams<-function(n, corpus){
    #For sentence, split
    # m<-HASH_TABLE()

    cl<-makeCluster(5)
    registerDoParallel(cl)
    g<-foreach(i = 1:length(corpus), .combine = 'rbind', .export = c('str_split')) %dopar% {
    #for(i in 1:length(corpus)){
        a<-unlist(str_split(corpus[i], " "))
        b<-lapply(1:((length(a)-n)+1), function(x) head(a[x:(x+n)], n))
        pre<-sapply(b, function(x) paste(head(x, n-1), collapse = "_"))
        post<-sapply(b, function(x) tail(x, 1))
        d<-data.frame('pregram'=pre, 'postgram'=post, 'count'=1, stringsAsFactors = FALSE)
    }

    message('grams collected, aggregating')

    g <- g %>%
        group_by(pregram, postgram) %>%
        summarize(count = sum(count))

    stopCluster(cl)
    gc(verbose=FALSE)
    return(g)
}

condense_grams<-function(g, allg=NULL){
    if(!is.null(allg)){
        g <- rbind(g, allg)
        g <- g %>%
            group_by(pregram, postgram) %>%
            summarize(count = sum(count))
    }
    return(g)
}

# teach_ngram<-function(gram, ht){
#     k<-paste(unlist(gram[1:(length(gram)-1)]), collapse="_")
#     v<-gram[length(gram)]
#     o<-LOOKUP(k, ht)
#     if(is.null(o)){
#         INSERT(k, data.frame(v, 1, stringsAsFactors = FALSE), ht)
#     } else {
#         if(v %in% o[,1]){
#             o[o[,1]==v, 2]<-o[o[,1]==v, 2]+1
#             INSERT(k,o,ht)
#         } else {
#             a<-rbind(o, data.frame(v, 1, stringsAsFactors = FALSE))
#             INSERT(k,a,ht)
#         }
#     }
# }
#
# v_teach_ngram<-Vectorize(teach_ngram, 'gram')

