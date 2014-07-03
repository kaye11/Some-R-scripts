library(ggplot2)

##always use data from t1
library(data.table)
NT = data.table(t1)
NT2=NT[, T := seq(from = 0.1, by = 0.1, length.out = .N), by = A]

##for distrib particles analyis
NumTracks <- aggregate( A ~ T , data = NT2 , max, na.rm = TRUE )
qplot(T,A, data=NumTracks)

##if you want to check untransformed data
TracksOrig <- aggregate( A ~ T , data = t1 , max, na.rm = TRUE )
qplot(T,A, data=TracksOrig)

#optional to use
NT3 <- readline("What particle in space data is this?")
write.table(NT2, paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/",NT3,".csv"), sep=";", row.names = F)