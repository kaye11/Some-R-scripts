SD <-ddply(NM, .(A), head, n = 80)
SD1 <- readline("What subset data is this?")
SD2 <- readline("What year were you born in?")
write.table(SD, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/",SD1,".csv", sep=""), sep=";", row.names = F, append=F)