library(ggplot2)
library(data.table)
library(gdata)
library(reshape)


## for both control and treatment
#normal(n1)
NT = data.table(n1)
NT2=NT[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
NTS <- aggregate( A ~ T , data = NT2 , max, na.rm = TRUE )
NTS=rename(NTS, c(A="Norm"))


#s2(starving)
CN = data.table(s2)
CN2=CN[, T := seq(from = 1L, by = 1L, length.out = .N), by = A]
CNS <- aggregate( A ~ T , data = CN2, max, na.rm = TRUE )
CNS=rename(CNS, c(A="St"))
CNS$T=NULL

#data binding (Si=A, Con=Con)
CB=cbindX(NTS, CNS)

ggplot(CB, aes(T)) + geom_line(aes(y = Norm, colour = "Normal"),  size=1) + 
  geom_line(aes(y = St, colour = "Starving"), size=1)+ 
  labs(list(x = "Time (s)", y = "Number of Tracks")) + theme(legend.title=element_blank())


##frequency of track change (angle change)
norm <- NT[, list(angN=length(unique(P)), lenN=length(T), VmN=mean(V)), by=c("A")] 
star <- CN[, list(angS=length(unique(P)), lenS=length(T), VmS=mean(V)), by=c("A")]

norm$freqN=norm$ang/norm$len
star$freqS=star$ang/star$len

##exporting
VN<- readline("What data did you analyse?")
Vid<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/motility/",VN,".csv")
write.table(norm, Vid, sep=";", col.names=T, row.names=F)

VN2<- readline("What data did you analyse?")
Vid2<-paste ("d:/Karen's/PhD/R program/Processed_data/trackdata/motility/",VN2,".csv")
write.table(norm, Vid2, sep=";", col.names=T, row.names=F)

##mean and standard deviation
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
res <- as.data.frame(colMeans(norm))
res$normSd=colSd(norm)
res$starAve=colMeans(star)
res$starSd=colSd(star)
res$normcount=length(unique(n1$A))
res$starcount=length(unique(s2$A))

##glm
library(lme)

glm(formula = cbind(using, notUsing) ~ age + education + wantsMore, 
    family = binomial) 