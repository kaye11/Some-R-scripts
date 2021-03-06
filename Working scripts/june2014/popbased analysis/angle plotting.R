library(gridExtra)
library(plyr)
library(ggplot2)

#run summarySE script

source("summarySE.R")
con$cond="Con"
si$cond="Si"
raw.binned=rbind(con, si)

write.table (raw.binned, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw.binned.csv", 
             sep=";", col.names=T, row.names=F)

dfc2 <- summarySE(raw.binned, measurevar="angs", groupvars=c("cond","bin", "time"), na.rm=TRUE)

ggplot(dfc2, aes(x=bin, y=angs, group=cond, shape=cond, color=cond)) + geom_point()+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se),
                width=.2)+facet_grid(~time)

##plotting proper
raw.binned$Treatment=raw.binned$cond

grid.newpage()
text <- element_text(size = 18) #change the size of the axes
theme_set(theme_bw())


r1=subset(raw.binned, raw.binned$T<120, )
rdfc1 <- summarySE(r1, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p1=ggplot(rdfc1, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=8)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +ylim(-0.2, 0.2)+ 
  labs(title="0-120 s", y="Sine angle")+ scale_colour_manual(values = c("lightcoral", "steelblue2")) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size=20, face="bold"), legend.position="none", 
        legend.title=text, legend.text=text, axis.title.x = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +geom_hline(yintercept=0)+
  scale_x_discrete(breaks=c("binA", "binB", "binC"), labels=c("Bin a", "Bin b", "Bin c"))


r2=subset(raw.binned, raw.binned$T>121 & raw.binned$T<240, )
rdfc2 <- summarySE(r2, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p2=ggplot(rdfc2, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=8)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +ylim(-0.2, 0.2)+ 
  labs(title="121-240 s", y="Sine angle")+ scale_colour_manual(values = c("lightcoral", "steelblue2")) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size=20, face="bold"), legend.position="none", axis.title.y=element_blank(),
        legend.title=text, legend.text=text, axis.title.x = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +geom_hline(yintercept=0)+
  scale_x_discrete(breaks=c("binA", "binB", "binC"), labels=c("Bin a", "Bin b", "Bin c"))


r3=subset(raw.binned, raw.binned$T>241 & raw.binned$T<360, )
rdfc3 <- summarySE(r3, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p3=ggplot(rdfc3, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=8)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +
  ylim(-0.2, 0.2)+labs(title="241-360 s", y="Sine Angle")+ scale_colour_manual(values = c("lightcoral", "steelblue2")) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), axis.title.y=element_blank(),
        plot.title = element_text(size=20, face="bold"), legend.position="none", 
        legend.title=text, legend.text=text, axis.title.x = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +geom_hline(yintercept=0)+
  scale_x_discrete(breaks=c("binA", "binB", "binC"), labels=c("Bin a", "Bin b", "Bin c"))


r4=subset(raw.binned, raw.binned$T>361 & raw.binned$T<480, )
rdfc4 <- summarySE(r4, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p4=ggplot(rdfc4, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=8)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1)+
  ylim(-0.2, 0.2)+labs(title="361-480 s", y="Sine angle")+ scale_colour_manual(values = c("lightcoral", "steelblue2")) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size=20, face="bold"), legend.position="none", 
        legend.title=text, legend.text=text, axis.title.x = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +geom_hline(yintercept=0)+
  scale_x_discrete(breaks=c("binA", "binB", "binC"), labels=c("Bin a", "Bin b", "Bin c"))


r5=subset(raw.binned, raw.binned$T>481 & raw.binned$T<600, )
rdfc5 <- summarySE(r5, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p5=ggplot(rdfc5, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=8)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +
  ylim(-0.2, 0.2)+labs(title="481-600 s", y="Sine Angle")+ scale_colour_manual(values = c("lightcoral", "steelblue2")) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size=20, face="bold"), legend.position="right", 
        legend.title=text, legend.text=text, axis.title.x = element_blank(), axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +geom_hline(yintercept=0)+
  scale_x_discrete(breaks=c("binA", "binB", "binC"), labels=c("Bin a", "Bin b", "Bin c"))

grid.arrange(p1, p2, p3, p4, p5, ncol=3)


##from 0

r11=subset(raw.binned, raw.binned$T<120, )
rdfc11 <- summarySE(r11, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p11=ggplot(rdfc11, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=10)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +
  ylim(-0.2, 0.2)+labs(title="0-2 mins", y="Sine Angle")+ 
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size=25, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))+geom_hline(yintercept=0)

r21=subset(raw.binned, raw.binned$T<240, )
rdfc21 <- summarySE(r21, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p21=ggplot(rdfc21, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=10)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +
  ylim(-0.2, 0.2)+labs(title="0-4 mins", x="Bin", y="Angle")+ 
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size=25, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))+geom_hline(yintercept=0)


r31=subset(raw.binned, raw.binned$T<360, )
rdfc31 <- summarySE(r31, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p31=ggplot(rdfc31, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=10)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +
  ylim(-0.2, 0.2)+labs(title="0-6 mins", x="Bin", y="Angle")+ 
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size=25, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))+geom_hline(yintercept=0)

r41=subset(raw.binned, raw.binned$T<480, )
rdfc41 <- summarySE(r41, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p41=ggplot(rdfc41, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=10)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +
  ylim(-0.2, 0.2)+labs(title="0-8 mins", x="Bin", y="Angle")+ 
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size=25, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))+geom_hline(yintercept=0)

r51=subset(raw.binned, raw.binned$T<600, )
rdfc51 <- summarySE(r51, measurevar="angs", groupvars=c("Treatment","bin"), na.rm=TRUE)
p51=ggplot(rdfc51, aes(x=bin, y=angs, group=Treatment, shape=Treatment, color=Treatment)) + geom_point(size=10)+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se), width=.2, size=1) +
  ylim(-0.2, 0.2)+labs(title="0-10 mins", x="Bin", y="Angle")+
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size=25, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))+geom_hline(yintercept=0)

grid.arrange(p11, p21, p31, p41, p51, ncol=3)

