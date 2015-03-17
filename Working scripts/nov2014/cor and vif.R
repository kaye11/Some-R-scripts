boxplot(CellsN~treatment, data=count)
boxplot(CellsN~reptreat, data=count)
boxplot (CellsN~Bin, data=count)
boxplot (CellsN~T, data=count)

#used to detect relationships between variables and collinearity
#all
pr=data.table(cbind(cond=binned$condn, bin=binned$binn, T=binned$time, Vlog=binned$Vlog))
pairs(pr)
#explanatory variables
pairs(exp)
#response variables
resp=data.table(cbind(vs=binned$meanvs, freq=binned$meanfreq, dir=binned$meandir))
pairs(resp)

##check for collinearity and correlation, this only applies to the explanatory variables!
source("vif.R")
exp=as.data.frame(data.table(cbind(bin=binned$bin, cond=binned$cond, time=binned$T, A=binned$A)))
cor(exp, method = "spearman")
cor(exp, method = "kendall")
cor.test(exp$bin, exp$cond, method="spearman")

vif_func(in_frame=exp,thresh=5,trace=T)
corvif(exp)
pairs(exp, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)



expA=as.data.frame(data.table(cbind(cond=binAdata$cond, time=binAdata$T, A=binAdata$A)))
cor(expA, method = "spearman")
vif_func(in_frame=expA,thresh=5,trace=T)


pairs(expA, lower.panel = panel.smooth2,  upper.panel = panel.cor, diag.panel = panel.hist)