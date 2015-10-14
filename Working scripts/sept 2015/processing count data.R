##source this! name the file into s1
library (data.table)
library (ggplot2)
library(grid)

##direct input from imagej

#calling data
#BinA <- read.delim("C:/Users/Karen.Standard-PC/Desktop/P36 counts/1hr videos/P36 Plate C1 Si-012/Bin A.xls")
#BinB <- read.delim("C:/Users/Karen.Standard-PC/Desktop/P36 counts/1hr videos/P36 Plate C1 Si-012/Bin B.xls")
#BinC <- read.delim("C:/Users/Karen.Standard-PC/Desktop/P36 counts/1hr videos/P36 Plate C1 Si-012/Bin C.xls")

#BinA
NT=data.table(BinA, key="Slice")
BinA=NT[, list(T=0:59, Count=Count, bin="BinA", cond="Si")]

#BinB
NT=data.table(BinB, key="Slice")
BinB=NT[, list(T=0:59, Count=Count, bin="BinB", cond="Si")]

#BinC
NT=data.table(BinC, key="Slice")
BinC=NT[, list(T=0:59, Count=Count, bin="BinC", cond="Si")]

t1 <- rbind (BinA, BinB, BinC)
t1$rep <- "B4Si011"

VN<- readline("What data did you analyse? SPECIFY DATA TYPE:")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/counts P36/",VN,".csv")
write.table(t1, Vid, sep=";", col.names=T, row.names=F)
