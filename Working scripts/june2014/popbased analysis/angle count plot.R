
am=seq(-1, 1, by = 0.2)
raw.binned$angdir <- cut(raw.binned$angs, am, labels=paste(tail(am, -1L)))


ft <- as.data.frame(ftable(angdir~cond+bin, data=raw.binned))
ft$total <- ave(ft$Freq, ft$cond, ft$bin,  FUN = sum)
ft$count <- (ft$Freq/ft$total)*100

ft$total2 <- ave(ft$Freq, ft$cond,FUN = sum)
ft$count2 <- (ft$Freq/ft$total2)*100

ggplot(data=ft, aes (x=angdir, y=count, colour=cond, fill=cond)) + facet_wrap (~bin, ncol=2) + 
  geom_bar(stat="identity", position=position_dodge())+labs(title="Cell direction", x="Angle", y="Count")+ 
  theme_classic()+
  theme(axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=15,face="bold"), 
        plot.title = element_text(size =20, face="bold"), 
        legend.title = element_text(colour="black", size=10, face="bold"), 
        legend.text = element_text(colour="black", size = 10, face = "bold"))+
  geom_vline(aes(xintercept = which(levels(angdir) %in% '0'), size=2, alpha=0.5))

