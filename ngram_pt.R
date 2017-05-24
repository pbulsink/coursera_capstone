#ngram package splitting

source('./data_prep.R')

library(ngram)
library(doParallel)
library(dplyr)
library(data.table)

proc_ngram_pt<-function(n=2, corp=NULL){
    #corp<-get_corpus(what='all')
    if(is.null(corp)){
        corp<-readRDS("./data/cleaned_input_text.RDS")
    }

    cl<-makeCluster(2)
    registerDoParallel(cl)
    indices<-c(1:length(corp))
    groups<-split(indices, ceiling(seq_along(indices)/(length(indices)/20)))
    rm(indices)
    g<-foreach(i = 1:20, .combine = 'rbind', .packages = c('ngram')) %dopar% {
        corpus<-concatenate(corp[unname(unlist(groups[i]))])
        ng<-ngram(corpus, n=n)
        rm(corpus)
        ngpt<-get.phrasetable(ng)
        rm(ng)
        saveRDS(ngpt, paste0("./data/ng",n,"pt",i,".RDS"))
        rm(ngpt)
        return(TRUE)
    }
    stopCluster(cl)
}

combine_ng_pt<-function(n=2){
    f<-paste0("./data/ng",n,"pt1.RDS")
    message(paste0('Loading file ',f,'.'))
    ngpt<-setDT(readRDS(f))
    ngpt[,prop:=NULL]
    ngpt<-ngpt[!ngrams %like% '\\$ \\^']
    ngpt[,c('pregrams') := paste(tstrsplit(ngrams, " ", fixed=TRUE, keep=c(1:(n-1))), collapse=" ")]
    ngpt[,c('postgrams') := (tstrsplit(ngrams, " ", fixed=TRUE, keep=c(n)))]
    ngpt[,ngrams:=NULL]
    for(i in 2:20){
        f<-paste0("./data/ng",n,"pt",i,".RDS")
        message(paste0('Loading file ', f, "."))
        ng<-setDT(readRDS(f))
        ng[,prop:=NULL]
        ng<-ng[!ngrams %like% '\\$ \\^']
        ng[,c('pregrams') := paste(tstrsplit(ngrams, " ", fixed=TRUE, keep=c(1:(n-1))), collapse=" ")]
        ng[,c('postgrams') := (tstrsplit(ngrams, " ", fixed=TRUE, keep=c(n)))]
        ng[,ngrams:=NULL]
        message('Merging Files')
        ngpt<-rbindlist(list(ngpt, ng))
        rm(ng)
        message('Aggregating Files')
        ngpt[, .(freq=sum(freq)), by=c('pregrams','postgrams')]
        ngpt[, .(pgfreq=sum(freq)), by=c('pregrams')]
    }
    return(ngpt)
}
#aggregate using dplyr
#
#
# ngpt5<-combine_ng_pt(5)
# saveRDS(ngpt5, "./data/ngpt5.RDS")
# rm(ngpt5)
# gc()
#
#Drop grams with ending and starting of sentence (ngram package artifact)
#ngpt5<-ngpt5[!ngrams %like% '\\$ \\^']
#split to pregrams and postgrams
#ngpt5[,c('pregrams') := paste(tstrsplit(ngrams, " ", fixed=TRUE, keep=c(1:4)), collapse=" ")]
#ngpt5[,c('postgrams') := (tstrsplit(ngrams, " ", fixed=TRUE, keep=c(5)))]
#ngpt5[,ngrams:=NULL]
#group by pregram, then drop pregams with <2?
#ngpt5[freq>1]

# for(i in 2:5){
#     ngpt<-combine_ng_pt(n=i)
#     saveRDS(ngpt, paste0("./data/ngpt",i,".RDS"))
#     rm(ngpt)
#     gc()
# }


