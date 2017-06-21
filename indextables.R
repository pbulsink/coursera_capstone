#Hash table of grams
require(data.table)
require(dplyr)

message('ng1')
ng1<-readRDS("./data/ng1.RDS")
ung<-unique(trimws(ng1$pregrams))

ng1[,ID:=0]

message('ng2')
ng2<-readRDS("./data/ng2.RDS")
ng2[,preID:=0]
ng2[,postID:=0]
message('ng3')
ng3<-readRDS("./data/ng3.RDS")
ng3[,preID:=0]
ng3[,postID:=0]
message('ng4')
ng4<-readRDS("./data/ng4.RDS")
ng4[,preID:=0]
ng4[,postID:=0]

p<-progress_estimated(length(ung), min_time = 10)


for(i in 1:length(ung)){
    ng1[pregrams == ung[i], ID:=i]
    ng2[pregrams == ung[i], preID:=i]
    ng2[postgrams== ung[i], postID:=i]
    ng3[pregrams == ung[i], preID:=i]
    ng4[pregrams == ung[i], preID:=i]
    p$tick()$print()
}

message('save ng1')
saveRDS(ng1, "./data/ng1_index.RDS")
rm(ng1)
gc()
message('ng ng2')
ng2[,ng:=paste(pregrams,postgrams)]
ng2[,pregrams:=NULL]
ng2[,postgrams:=NULL]

p<-progress_estimated(nrow(ng2), min_time = 10)

for(i in 1:nrow(ng2)){
    ng3[postgrams == ng2[i, ng], postID:=i]
    p$tick()$print()
}

message('save ng2')
ng2[,ng:=NULL]
saveRDS(ng2, "./data/ng2_index.RDS")
rm(ng2)
gc()
message('ng ng3')
ng3[,ng:=paste(pregrams,postgrams)]
ng3[,pregrams:=NULL]
ng3[,postgrams:=NULL]

p<-progress_estimated(length(ung), min_time = 10)
for(i in 1:length(ung)){

    p$tick()$print()
}

p<-progress_estimated(nrow(ng3), min_time = 10)

for(i in 1:nrow(ng3)){
    ng4[postgrams == ng3[i, ng], postID:=i]
    p$tick()$print()
}

message('save ng3')
ng3[,ng:=NULL]
saveRDS(ng3, "./data/ng3_index.RDS")
rm(ng3)
gc()
message('ng ng4')
ng4[,ng:=paste(pregrams,postgrams)]
ng4[,pregrams:=NULL]
ng4[,postgrams:=NULL]

message('ng5')
ng5<-readRDS("./data/ng5.RDS")
ng5[,preID:=0]
ng5[,postID:=0]

p<-progress_estimated(length(ung), min_time = 10)
for(i in 1:length(ung)){
    ng5[pregrams == ung[i], preID:=i]
    p$tick()$print()
}

p<-progress_estimated(nrow(ng4), min_time = 10)

for(i in 1:nrow(ng4)){
    ng5[postgrams == ng4[i, ng], postID:=i]
    p$tick()$print()
}

message('save ng4')
ng4[,ng:=NULL]
ng5[,postgrams:=NULL]
ng5[,pregrams:=NULL]
saveRDS(ng4, "./data/ng4_index.RDS")
rm(ng4)
gc()
message('save ng5')
saveRDS(ng5, "./data/ng5_index.RDS")
rm(ng5)
gc()

