library(data.table)
library(ggplot2)
#GD

NT = data.table(binned, key="ID")

GDtable <- NT[, list(GD=max(GD), T=length(T)), by=c("cond", "bin", "ID")] 

qplot(cond,GD, data = GDtable, geom = "boxplot")+facet_grid(~bin)
shapiro.test(GDtable$GD)

library(lawstat)
levene.test(GDtable$GD, group=GDtable$cond, location="mean")
shapiro.test(GDtable$GD)#normal

#not normal but pop variances are equal

wilcox.test(GD~cond, data=GDtable, paired=FALSE)#no sig diff


#Bin A
BinAGD=subset (GDtable, bin=="binA")
levene.test(BinAGD$GD, group=BinAGD$cond, location="mean") #normal
shapiro.test(BinAGD$GD)#normal

t.test(GD~cond, data=BinAGD) #no sig diff


#Bin B

BinBGD=subset (GDtable, bin=="binB")
levene.test(BinBGD$GD, group=BinBGD$cond, location="mean") #normal
shapiro.test(BinBGD$GD)#not normal

wilcox.test(GD~cond, data=BinBGD, paired=FALSE) #no sig diff


#Bin C

BinCGD=subset (GDtable, bin=="binC")
levene.test(BinCGD$GD, group=BinCGD$cond, location="mean") #normal
shapiro.test(BinCGD$GD)#not normal

wilcox.test(GD~cond, data=BinCGD, paired=FALSE) #sig diff





