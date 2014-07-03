#for max number of tracks with the start of each track forced to 0
t1=c1
NT = data.table(t1)
NT2=NT[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]

SD1 <- readline("What data is this? On this data set, time points are forced to be zero:")
write.table(NT2, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/",SD1,".csv"), sep=";", row.names = F)


con=as.data.frame(NT2)
con$A=as.numeric(con$A)
con.count2=ddply(con,~T,summarise,Con=(max((A))))

t2=s1
NT = data.table(t2)
NT2=NT[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
SD1 <- readline("What data is this? On this data set, time points are forced to be zero:")
write.table(NT2, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/",SD1,".csv"), sep=";", row.names = F)


si=as.data.frame(NT2)
si$A=as.numeric(si$A)
si.count2=ddply(si,~T,summarise,Si=(max((A))))

#data binding (Si=A, Con=Con)
CB2=cbind(si.count2, con.count2)

ggplot(CB2, aes(T)) + geom_line(aes(y = Si, colour = "Si"),  size=1) + 
  geom_line(aes(y = Con, colour = "Control"), size=1)+ 
  labs(list(x = "Time (s)", y = "Number of Tracks")) 

