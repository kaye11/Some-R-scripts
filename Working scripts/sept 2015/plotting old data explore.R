library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)


##old data
si_old <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/old_data/day3 si.binned.cor .csv", sep=";")
con_old <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/old_data/day3 control.binned.cor .csv", sep=";")
mot_old <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/old_data/day3 motility_binned .csv", sep=";")

si_old$scalar=NULL
si_old$angle=NULL
si_old$cond="dSi"

con_old$scalar=NULL
con_old$angle=NULL
con_old$cond="Control"

mot_old$cond="No bead"

#bin by time
tm=seq(0, 600, by = 60)

si_old$time <- cut(si_old$T, tm, labels=paste(tail(tm, -1L)))
si_old$time[is.na(si_old$time)] <- 60 #replace NAs with 60. some data time points have the starting point as NA

con_old$time <- cut(con_old$T, tm, labels=paste(tail(tm, -1L)))
con_old$time[is.na(con_old$time)] <- 60 #replace NAs with 60. some data time points have the starting point as NA

mot_old$time <- cut(mot_old$T, tm, labels=paste(tail(tm, -1L)))
mot_old$time[is.na(mot_old$time)] <- 60 #replace NAs with 60. some data time points have the starting point as NA

#combining data sets
old_data <- rbind(mot_old, con_old, si_old)

#drop outbin
old_data <- old_data[old_data$bin!="outbin", ]

#make ID
old_data$ID = as.factor(paste(old_data$A, old_data$cond, sep = "-"))


#summary SE
source("summarySE.R")
summary_olddata <- summarySE(old_data, measurevar = "V", groupvars = c("bin", "cond", "time"))

#barplots
ggplot(data=summary_olddata, aes(x=time, y=V, shape=cond, color=cond)) + geom_point(size=5)+ 
  geom_errorbar(aes(ymin=V-se, ymax=V+se), width=2, size=1) + facet_grid(~bin)

ggplot(data=summary_olddata, aes(x=time, y=V, color=cond))+   geom_boxplot()+ facet_grid(bin~.)

old_data2 <- rbind(con_old, si_old)
old_data2 <- old_data2[old_data2$bin!="outbin", ]

summary_olddata2 <- summarySE(old_data2, measurevar = "V", groupvars = c("bin", "cond", "time"))

old_data$Vlog <- log(old_data$V+1)

summary_olddata$normV <- summary_olddata$V/summary_olddata$N #normalization V

