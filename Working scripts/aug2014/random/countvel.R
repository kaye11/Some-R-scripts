# Use T as a factor rather than numeric
countsum2 <- countsum
countsum2$T <- factor(countsum2$T)

# Error bars represent standard error of the mean
ggplot(countsum2, aes(x=T, y=CellsN, fill=treatment)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=CellsN-se, ymax=CellsN+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


velsum <- summarySE(vel, measurevar="V", groupvars=c("cond","time", "bin"))

ggplot(velsum [velsum$bin=="binA", ], aes(x=time, y=V, fill=cond)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=V-se, ymax=V+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+facet_grid(cond~bin)

velsum2 <- summarySE(vel, measurevar="V", groupvars=c("cond","bin"))

write.table (velsum, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/velsum.csv", 
             sep=";", col.names=T, row.names=F)

write.table (velsum2, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/velsum2.csv", 
             sep=";", col.names=T, row.names=F)