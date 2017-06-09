#Hash table of grams
require(data.table)
require(dplyr)

message('ng1')
ng1<-readRDS("./data/ng1.RDS")
ung<-unique(trimws(ng1$pregrams))

ng1$ID<-0

message('ng2')
ng2<-readRDS("./data/ng2.RDS")
ung<-c(ung, unique(trimws(ng2[!pregrams %in% ung, pregrams])))
ung<-c(ung, unique(trimws(ng2[!postgrams %in% ung, postgrams])))
ng2$preID<-0
ng2$postID<-0
ng3<-readRDS("./data/ng3.RDS")
ng3$preID<-0
ng3$postID<-0

p<-progress_estimated(length(ung), min_time = 5)


for(i in 1:length(ung)){
    ng1[pregrams == ung[i], ID:=i]
    ng2[pregrams == ung[i], preID:=i]
    ng2[postgrams== ung[i], postID:=i]
    ng3[pregrams == ung[i], preID:=i]
    p$tick()$print()
}

ng2[,ng:=paste(pregrams,postgrams)]
ng2[,pregrams:=NULL]
ng2[,postgrams:=NULL]

p2<-progress_estimated(nrow(ng2), min_time = 5)

for(i in 1:nrow(ng2)){
    ng3[postgrams == ng2[i, ng], postID=i]
    p2$tick()$print()
}






for(i in 1:length(ung)){

    p$tic()$print()
}
