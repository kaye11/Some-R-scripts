
##source this! name the file into s1
library (data.table)
library (ggplot2)
library(grid)

##direct input from trackmate 
NT=data.table(s1, key="Label")
NT=NT[, list(ID=Label, V=TRACK_MEAN_SPEED, Vlog=log(TRACK_MEAN_SPEED+1)), by=c("Label")]

##saving data
VN<- readline("What data did you analyse?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/si_addition/",VN,".csv")
write.table(NT, Vid, sep=";", col.names=T, row.names=F)
