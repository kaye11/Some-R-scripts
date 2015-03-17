library(mgcv)

all.binned$time=as.numeric(all.binned$time)

## GAM models for Bins

BinA<-gam(V ~  s(time,by = as.numeric(cond == "Control")) +
            s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binA",])  #checked k=10 lowest aic


BinAA<-gam(V ~  s(time,by = as.numeric(cond == "Control"), bs="cs") +
            s(time,by = as.numeric(cond == "Si"), bs="cs"), data = all.binned[all.binned$bin == "binA",])  


BinB<-gam(V ~  s(time,by = as.numeric(cond == "Control")) +
            s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binB",])  #k=10

BinC<-gam(V ~  s(time,by = as.numeric(cond == "Control")) +
            s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binC",])  #k=10




summary(BinA)
summary(BinB)
summary(BinC)


#whole dataset GAMs

all.binned$cb <- paste(all.binned$cond, all.binned$bin, sep = "")

BinAll<-gam(V ~  s(time,by = as.numeric(cb == "SibinA")) +
              s(time,by = as.numeric(cb == "SibinB")) + 
              s(time,by = as.numeric(cb == "SibinC")) +
              s(time,by = as.numeric(cb == "ControlbinA")) + 
              s(time,by = as.numeric(cb == "ControlbinB"))+ 
              s(time,by = as.numeric(cb == "ControlbinC")),
             data = all.binned)  

summary(BinAll)

#####ACCELERATION

## GAM models for Bins

BinA_Ac<-gam(Ac ~  s(time,by = as.numeric(cond == "Control")) +
               s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binA",])  
BinB_Ac<-gam(Ac ~  s(time,by = as.numeric(cond == "Control")) +
               s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binB",])  
BinC_Ac<-gam(Ac ~  s(time,by = as.numeric(cond == "Control")) +
               s(time,by = as.numeric(cond == "Si")), data = all.binned[all.binned$bin == "binC",])  



summary(BinA_Ac)
summary(BinB_Ac)
summary(BinC_Ac)


#whole dataset GAMs


BinAll_Ac<-gam(Ac ~  s(time,by = as.numeric(cb == "SibinA")) +
                 s(time,by = as.numeric(cb == "SibinB")) + 
                 s(time,by = as.numeric(cb == "SibinC")) +
                 s(time,by = as.numeric(cb == "ControlbinA")) + 
                 s(time,by = as.numeric(cb == "ControlbinB"))+ 
                 s(time,by = as.numeric(cb == "ControlbinC")),
               data = all.binned)  

summary(BinAll_Ac)




