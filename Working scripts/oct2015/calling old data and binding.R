library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)
library(data.table)
source("summarySE.R")
source ("lang.R")

control <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/old_data/ day3 control-001 .csv", sep=";")
si <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/old_data/ day3 si-002 .csv", sep=";")

control$cond= "Control"
si$cond ="dSi"

old_data=rbind(control, si)

old_data$Vlog  <- log(old_data$V+1)
old_data$timef <- as.factor(old_data$time)
old_data$ID <- as.factor(paste (old_data$A, old_data$cond, sep="-"))
old_data$time <- as.numeric(old_data$time)

old_data.sum <- summarySE(old_data, measurevar="Vlog", groupvars=c("cond", "bin", "time"))
old_data.sum_bin <- summarySE(old_data, measurevar="Vlog", groupvars=c("cond", "bin"))


qplot(timef, Vlog, color = cond, data = old_data,  geom = "boxplot") + facet_wrap(cond~bin, scales="free") 
qplot(x=T, y=Vlog, data=old_data)+geom_line()+facet_grid(bin~cond, scales="free")+geom_smooth(method="gam", formula=y~s(x, k=5))



qplot(timef, angs, color = cond, data = old_data,  geom = "boxplot") + facet_wrap(cond~bin, scales="free") 
