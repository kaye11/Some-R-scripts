dfc <- summarySE(ns, measurevar="Vm", groupvars=c("cond"))

ggplot(dfc, aes(x=cond, y=Vm, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Vm-se, ymax=Vm+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

dfc2 <- summarySE(ns, measurevar="freq", groupvars=c("cond"))

ggplot(dfc2, aes(x=cond, y=freq, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=freq-se, ymax=freq+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))