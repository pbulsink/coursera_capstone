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
    message('Loading file 1.')
    ngpt<-as.data.table(readRDS(paste0("./data/ng",n,"pt1.RDS")))
    ngpt$prop<-NULL
    for(i in 2:20){
        message(paste0('Loading file ', i, "."))
        ng<-as.data.table(readRDS(paste0("./data/ng",n,"pt",i,".RDS")))
        ng$prop<-NULL
        message('Merging Files')
        ngpt<-rbindlist(list(ngpt, ng))
        rm(ng)
        message('Aggregating Files')
        ngpt[, .(freq=sum(freq)), by=ngrams]
    }
    return(ngpt)
}
#aggregate using dplyr
#
#
# ngpt3<-combine_ng_pt(3)
# saveRDS(ngpt3, "./data/ngpt3.RDS")
# rm(ngpt3)
# gc()
