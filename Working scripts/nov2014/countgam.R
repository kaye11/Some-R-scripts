library(ggplot2)
library(grid)
library(gtable)
library(ggthemes)
library(gridExtra)
library(mgcv)

grid.newpage()
text <- element_text(size = 20, face="bold") #change the size of the axes
theme_set(theme_bw())

count$T=count$time*60

countsum <- summarySE(count, measurevar="CellsN", groupvars=c("treatment","T", "Bin"))


mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Bin") { 
    value[value=="A"] <- "Bin A"
    value[value=="B"]   <- "Bin B"
    value[value=="C"] <- "Bin C"
  }
  return(value)
}

countplot= ggplot(data = count, aes(x=T,y=CellsN, color=treatment))+ 
  stat_smooth(method="gam", formula=y~s(x, k=10), size=2, se=TRUE)+ 
  facet_grid(.~Bin, labeller=mf_labeller)+ 
  labs(list(x = "Time (sec)", y = "Normalized Cell Count"))+ labs (color="Experimental Condition")+
  theme(axis.text=element_text(size=15, face="bold"), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text,  legend.position="bottom",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit (2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#check knots in gam, use bs="cr" or the shrinked version bs="cs" (without 0 eigen values)

simp<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control")) + 
            s(time,by = as.numeric(treatment == "Si")),data = count[count$Bin == "A",])
AIC(simp)

simp2<-gam(CellsN ~  s(time) + s(time,by = as.numeric(treatment == "Control")) + 
            s(time,by = as.numeric(treatment == "Si")),data = count[count$Bin == "A",])
AIC(simp2)

cs<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr") + 
            s(time,by = as.numeric(treatment == "Si"), bs="cr"),data = count[count$Bin == "A",])
AIC(cs)

k3<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr", k=3) + 
            s(time,by = as.numeric(treatment == "Si"), bs="cr", k=3),data = count[count$Bin == "A",])
AIC(k3)

k4<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr", k=4) + 
          s(time,by = as.numeric(treatment == "Si"), bs="cr", k=4),data = count[count$Bin == "A",])
AIC(k4)

k5<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr", k=5) + 
          s(time,by = as.numeric(treatment == "Si"), bs="cr", k=5),data = count[count$Bin == "A",])
AIC(k5)

k5a <- gam (CellsN~ s (Tn, by=treatment), data = count[count$Bin == "A",])

k6<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr", k=6) + 
          s(time,by = as.numeric(treatment == "Si"), bs="cr", k=6),data = count[count$Bin == "A",])
AIC(k6)

k7<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr", k=7) + 
          s(time,by = as.numeric(treatment == "Si"), bs="cr", k=7),data = count[count$Bin == "A",])
AIC(k7)

k8<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr", k=8) + 
          s(time,by = as.numeric(treatment == "Si"), bs="cr", k=8),data = count[count$Bin == "A",])
AIC(k8)

k9<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr", k=9) + 
          s(time,by = as.numeric(treatment == "Si"), bs="cr", k=9),data = count[count$Bin == "A",])
AIC(k9)

k10<-gam(CellsN ~  s(time,by = as.numeric(treatment == "Control"), bs="cr", k=10) + 
          s(time,by = as.numeric(treatment == "Si"), bs="cr", k=10),data = count[count$Bin == "A",])
AIC(k10)

##smoothing curve

plot(k8, se = TRUE)
k8pred <- predict(k8, se = TRUE, type = "response")
plot(count$time, count$CellsN, type = "p")
I1 <- order(count$time)
lines(count$time[I1], k8pred$fit[I1], lty=1)
lines(count$time[I1], k8pred$fit[I1]+2*k8pred$se[I1],lty=2)
lines(count$time[I1], k8pred$fit[I1]-2*k8pred$se[I1],lty=2)

