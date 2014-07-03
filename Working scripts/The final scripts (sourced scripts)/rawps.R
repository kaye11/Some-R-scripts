
VN<- readline("What data did you analyse?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/",VN,".csv")
write.table(t1, Vid, sep=";", col.names=T, row.names=F)
