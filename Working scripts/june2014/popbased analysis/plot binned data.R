
NT=data.table(aloxall3)
dt=NT[, list (meanVm=mean(Vm), count=count), by=c("cond", "bin", "time")]
dt$time=relevel(dt$time, "60")

ggplot(data=dt, aes (x=time, y=meanVm, colour=cond)) + facet_grid (cond~bin) + geom_bar(stat="identity")

NT=data.table(alox3)
dt=NT[, list (meanVm=mean(Vm), count=count), by=c("cond", "bin", "time")]
dt$time=relevel(dt$time, "60")

ggplot(data=dt, aes (x=time, y=meanVm, colour=cond)) + facet_grid (cond~bin) + geom_bar(stat="identity")

NT=data.table(alox3, key="cond")
dt=NT[, list (meanVm=mean(Vm), count=count), by=c("cond", "time")]
dt$time=relevel(dt$time, "60")

ggplot(data=dt, aes (x=time, y=meanVm, colour=cond)) + facet_grid (~cond) + geom_bar()

##another method instead if using data.table
raw.binned=rbind(si.binned, con.binned)
dfc <- summarySE(raw.binned, measurevar="V", groupvars=c("cond","time"))

ggplot(dfc, aes(x=time, y=V, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=V-se, ymax=V+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+facet_grid(~cond)

dfc2 <- summarySE(raw.binned, measurevar="V", groupvars=c("cond","time", "bin"))

ggplot(dfc2, aes(x=time, y=V, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=V-se, ymax=V+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+facet_grid(cond~bin)


#using binned data already
pro.binned=rbind(si.alox, con.alox)
dfc <- summarySE(pro.binned, measurevar="Vm", groupvars=c("cond","time"))

ggplot(dfc, aes(x=time, y=Vm, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Vm-se, ymax=Vm+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+facet_grid(~cond)

ggplot(data=dfc, aes (x=time, y=V, group=cond, shape=cond, color=cond)) + geom_smooth() + 
  labs(title="Mean Velocity (All Cells)", x="Time", y="Mean Velocity (µm/sec)")+ 
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size =20, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))


dfc2 <- summarySE(pro.binned, measurevar="Vm", groupvars=c("cond","time", "bin"))

ggplot(data=dfc2, aes (x=time, y=V, group=cond, shape=cond, color=cond)) + geom_smooth() + 
  labs(title="Mean Velocity (Binned)", x="Time", y="Mean Velocity (µm/sec)")+ facet_wrap (~bin, ncol=2)+
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size =20, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))

##another method instead if using data.table
raw.binned=rbind(si.binned, con.binned)
count1 <- summarySE(raw.binned, measurevar=length(unique("A")), groupvars=c("cond","time"))

ggplot(dfc, aes(x=time, y=V, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=V-se, ymax=V+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+facet_grid(~cond)

dfc2 <- summarySE(raw.binned, measurevar="angs", groupvars=c("cond","time", "bin"))

ggplot(dfc2, aes(x=time, y=V, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=V-se, ymax=V+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+facet_grid(cond~bin)


#plotting count data

ggplot(data=dt, aes (x=time, y=count, group=cond, shape=cond, color=cond)) + facet_grid (cond~bin) + geom_point() + geom_line()


ggplot(dfc, aes(x=time, y=angs, group=cond, shape=cond, color=cond)) + geom_point()+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se),
                width=.2)

ggplot(dfc2, aes(x=time, y=angs, group=cond, shape=cond, color=cond)) + geom_point()+ 
  geom_errorbar(aes(ymin=angs-se, ymax=angs+se),
                width=.2)+facet_grid(~bin)


ggplot(dfc2, aes(x=time, y=angs, group=cond, shape=cond, color=cond)) + geom_point() +
  geom_errorbar(aes(ymin=angs-se, ymax=ang+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+facet_grid(~bin)

ggplot(dfc2, aes(x=time, y=V, group=cond, shape=cond, color=cond)) + facet_grid(~bin)+geom_smooth()
ggplot(dfc, aes(x=time, y=V, group=cond, shape=cond, color=cond)) +geom_smooth()

