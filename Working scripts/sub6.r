
VideoName<- readline("What data did you analyse?")
fact.t1<- as.factor(t1$A)
savefile <- sapply( levels(fact.t1) , function(x) write.table(t1[x==fact.t1,3:4], paste("d:/Karen's/PhD/R program/Processed_data/trackdata/(",VideoName,")",x,".csv", sep=""), sep=";", row.names = F))