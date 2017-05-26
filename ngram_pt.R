#ngram package splitting

source('./data_prep.R')

library(ngram)
library(doParallel)
library(dplyr)
library(data.table)

proc_ngram_pt<-function(n=2, corp=NULL){
    #corp<-get_corpus(what='all')
    if(is.null(corp)){
        corp<-readRDS("./data/corp.RDS")
    }

    cl<-makeCluster(2)
    registerDoParallel(cl)
    indices<-c(1:length(corp))
    groups<-split(indices, ceiling(seq_along(indices)/(length(indices)/40)))
    rm(indices)
    g<-foreach(i = 1:40, .combine = 'rbind', .packages = c('ngram')) %dopar% {
        message("concatenate")
        corpus<-concatenate(corp[unname(unlist(groups[i]))])
        message("ngrams")
        ng<-ngram::ngram(corpus, n=n)
        rm(corpus)
        message("phrasetable")
        ngpt<-get.phrasetable(ng)
        rm(ng)
        message("saving")
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
    message(paste0('Cleaning file ',f,'.'))
    ngpt[,prop:=NULL]
    if(n == 1){
        for(i in 2:20){
            f<-paste0("./data/ng",n,"pt",i,".RDS")
            message(paste0('Loading file ', f, "."))
            ng<-setDT(readRDS(f))
            ng[,prop:=NULL]
            ngpt<-rbindlist(list(ngpt, ng))
            rm(ng)
            ngpt[, .(freq=sum(freq)), by=c('ngrams')]
        }
        return(ngpt)
    }
    ngpt<-ngpt[!ngrams %like% '\\$ \\^']
    ngpt[,c(paste0('word',1:n)) := tstrsplit(ngrams, " ", fixed=TRUE)]
    if(n==2){
        setnames(ngpt, c("word1","word2"), c('pregrams', 'postgrams'))
    } else {
        ngpt[,pregrams := do.call(paste, .SD), .SDcols = c(paste0('word', 1:(n-1)))]
        ngpt[,(paste0('word',1:(n-1))):=NULL]
        setnames(ngpt, paste0('word',n), 'postgrams')
    }
    ngpt[,ngrams:=NULL]
    for(i in 2:20){
        f<-paste0("./data/ng",n,"pt",i,".RDS")
        message(paste0('Loading file ', f, "."))
        ng<-setDT(readRDS(f))
        message(paste0('Cleaning file ',f,'.'))
        ng[,prop:=NULL]
        ng<-ng[!ngrams %like% '\\$ \\^']
        ng[,c(paste0('word',1:n)) := tstrsplit(ngrams, " ", fixed=TRUE)]
        if(n==2){
            setnames(ng, c("word1","word2"), c('pregrams', 'postgrams'))
        } else {
            ng[,pregrams := do.call(paste, .SD), .SDcols = c(paste0('word', 1:(n-1)))]
            ng[,(paste0('word',1:(n-1))):=NULL]
            setnames(ng, paste0('word',n), 'postgrams')
        }
        ng[,ngrams:=NULL]
        message('Merging Files')
        ngpt<-rbindlist(list(ngpt, ng))
        rm(ng)
        message('Aggregating Files')
        ngpt[, .(freq=sum(freq)), by=c('pregrams','postgrams')]
    }

    message("Done assembly.")
    pgfreq<-ngpt[,.(pgfreq=sum(freq)), by = 'pregrams']
    setkey(ngpt, pregrams)
    setkey(pgfreq, pregrams)
    ngpt2<-ngpt[pgfreq, nomatch=0]
    rm(pgfreq, ngpt)
    return(ngpt2)
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
#     message(paste0("Combining ", i))
#     ngpt<-combine_ng_pt(n=i)
#     message(paste0("Saving ", i))
#     saveRDS(ngpt, paste0("./data/ngpt",i,".RDS"))
#     rm(ngpt)
#     gc()
# }
# Try writing as data.table::fwrite()

