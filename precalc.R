#precalculate

source("./predict_next_knelder_ney.R")
require(dplyr)
require(doParallel)

allNGrams<-loadAndPrepGramsFromFile()

for(j in 4:1){
    message(paste0("Starting ", j))
    cl<-makeCluster((5-j)*2)
    registerDoParallel(cl)

    u<-unique(allNGrams[[j]][freq>3,pregrams])
    nj<-foreach(i = 1:length(u), .combine = rbind,.inorder = FALSE, .packages = c('data.table', 'stringi')) %dopar% {
        text<-u[i]
        freq<-sum(allNGrams[[j]][pregrams == text, freq])
        text_stream <- cutNMax(text, j)
        lngth<-length(unlist(strsplit(text_stream, " ")))

        candidates <- kn_cand(text_stream, lngth+1, allNGrams)
        candidates[postgrams == '$', postgrams:='.']
        w<-cand[1:5, postgrams]
        p<-cand[1:5, prop]
        d<-data.frame(pregram=text, freq=freq, w1=w[1], p1=p[1], w2=w[2], p2=p[2], w3=w[3], p3=p[3], w4=w[4], p4=p[4], w5=w[5], p5=p[5])
        d
    }

    message(paste0("Saving ", j))
    write.table(nj, file = paste0("./data/ng",j,"precalc.csv"), append = FALSE,quote = TRUE,sep = ",", row.names = FALSE)
    rm(nj)
    allNGrams[[j]]<-NULL
    stopCluster()
    gc()
}
# pb<-progress_estimated(length(unique(allNGrams[[4]][freq>1,pregrams])))

# for(i in 1:length(unique(allNGrams[[4]][freq>1,pregrams]))) {
#     text<-unique(allNGrams[[4]][,pregrams])[i]
#     freq<-sum(allNGrams[[4]][pregrams == text, freq])
#     cand<-kn_predict(text, allNGrams)
#     w<-cand[1:5, postgrams]
#     p<-cand[1:5, prop]
#     d<-data.frame(pregram=text, freq=freq, w1=w[1], p1=p[1], w2=w[2], p2=p[2], w3=w[3], p3=p[3], w4=w[4], p4=p[4], w5=w[5], p5=p[5])
#     write.table(d, file="./data/ng4precalc.csv", append = TRUE, col.names = FALSE, row.names=FALSE, sep=",")
#     pb$tick()$print()
# }
#
# pb<-progress_estimated(length(unique(allNGrams[[3]][freq>1,pregrams])))
#
# for(i in 1:length(unique(allNGrams[[3]][freq>1,pregrams]))) {
#     text<-unique(allNGrams[[3]][,pregrams])[i]
#     freq<-sum(allNGrams[[3]][pregrams == text, freq])
#     cand<-kn_predict(text, allNGrams)
#     w<-cand[1:5, postgrams]
#     p<-cand[1:5, prop]
#     d<-data.frame(pregram=text, freq=freq, w1=w[1], p1=p[1], w2=w[2], p2=p[2], w3=w[3], p3=p[3], w4=w[4], p4=p[4], w5=w[5], p5=p[5])
#     write.table(d, file="./data/ng3precalc.csv", append = TRUE, col.names = FALSE, row.names=FALSE, sep=",")
#     pb$tick()$print()
# }
#
# pb<-progress_estimated(length(unique(allNGrams[[2]][freq>1,pregrams])))
#
# for(i in 1:length(unique(allNGrams[[2]][freq>1,pregrams]))) {
#     text<-unique(allNGrams[[2]][,pregrams])[i]
#     freq<-sum(allNGrams[[2]][pregrams == text, freq])
#     cand<-kn_predict(text, allNGrams)
#     w<-cand[1:5, postgrams]
#     p<-cand[1:5, prop]
#     d<-data.frame(pregram=text, freq=freq, w1=w[1], p1=p[1], w2=w[2], p2=p[2], w3=w[3], p3=p[3], w4=w[4], p4=p[4], w5=w[5], p5=p[5])
#     write.table(d, file="./data/ng3precalc.csv", append = TRUE, col.names = FALSE, row.names=FALSE, sep=",")
#     pb$tick()$print()
# }
