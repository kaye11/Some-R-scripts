#GD

GDtable <- as.data.table(binned, key="A")
group[group[, .I[pt == max(pt)], by=Subject]$V1]

NT = data.table(binned, key="Ac")

GDtable <- NT[, list(GD=max(GD), T=length(T)), by=c("Ac", "cond", "A")] 

qplot(cond,GD, data = GDtable,  geom = "boxplot")
shapiro.test(GDtable$GD)

library(lawstat)
levene.test(GDtable$GD, group=GDtable$cond, location="mean")

#not normal but pop variances are equal

wilcox.test(GD~cond, data=GDtable, paired=FALSE)

GDlme <- lm(GD~cond, data=GDtable)
