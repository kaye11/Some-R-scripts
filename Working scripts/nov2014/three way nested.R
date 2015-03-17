boxplot(CellsN~treatment, data=count)
boxplot(CellsN~reptreat, data=count)
boxplot (CellsN~binn, data=count)
boxplot (CellsN~T, data=count)

f2<- formula (CellsN~treatment*T*Bin)

gls1<- gls(f2, method="REML", data=count)

lm1<-lme (f2, random=~1 | treatment/reptreat, data=count, method="REML")

lm2<-lme (f2, random=~1 | treatment/reptreat, weights = varIdent (form= ~1|Bin), 
          data=count, method="REML")

f3 <- formula (CellsN~treatment*Bin)

lm3<-lme (f3, random=~1 | treatment/reptreat, data=count, method="REML")

lm4<-lme (f3, random=~1 | treatment/reptreat, weights = varIdent (form= ~1|T), 
          data=count, method="REML")

f4 <- formula (CellsN~treatment)

lm5<-lme (f4, random=~1 | treatment/reptreat, data=count, method="REML")

lm6<-lme (f4, random=~1 | treatment/reptreat, weights = varIdent (form= ~1|T), 
          data=count, method="REML")

lm7<-lme (f4, random=~1 | treatment/reptreat, weights = varIdent (form= ~1|Bin), 
          data=count, method="REML")