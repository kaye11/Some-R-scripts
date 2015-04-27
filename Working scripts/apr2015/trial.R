library(data.table)
library(ggplot2)

Con$cond="Con"
Si$cond="Si"

# Combine A and cond
Con$ID <- paste(Con$A, Con$cond, sep = "-")
Si$ID <- paste(Si$A, Si$cond, sep = "-")


#subsetting
ConBinA=Con[Con$bin=="binA", ]
ConBinB=Con[Con$bin=="binB", ]
ConBinC=Con[Con$bin=="binC", ]

SiBinA=Con[Si$bin=="binA", ]
SiBinB=Con[Si$bin=="binB", ]
SiBinC=Con[Si$bin=="binC", ]

source("summarySE.R")
SiSum <- summarySE(Si, measurevar="NGDR", groupvars=c("bin"), na.rm=TRUE)
ConSum <- summarySE(Con, measurevar="NGDR", groupvars=c("bin"), na.rm=TRUE)

library(plyr)

binned=rbind(Con, Si)

Sumall <- ddply(binned, c("T", "bin", "cond"), summarise,
               N    = length(scalar),
               mean = mean(scalar),
               sd   = sd(scalar),
               se   = sd / sqrt(N))

Sumall2 <- ddply(binned, c("bin", "cond"), summarise,
                N    = length(scalar),
                mean = mean(scalar),
                sd   = sd(scalar),
                se   = sd / sqrt(N))

grid.newpage()
text <- element_text(size = 30, face="bold") #change the size of the axes
theme_set(theme_bw())


Sumbin=Sumall[Sumall$bin != "outbin", ] 

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="bin") { 
    value[value=="binA"] <- "Bin a"
    value[value=="binB"]   <- "Bin b"
    value[value=="binC"] <- "Bin c"
  }
  return(value)
}

ggplot(data=Sumbin, aes(x=T, y=mean, color=cond)) +  geom_line() + geom_point()+
  labs(list(x = "Experimental condition", y = "Distance from the bead (µm)")) +
  facet_grid(cond~bin, labeller=mf_labeller) +
  scale_colour_manual(values = c("lightcoral", "steelblue2"), 
                      breaks=c("Control", "Si"), labels=c("Control", "dSi")) +
  theme(axis.text.y=element_text(size=30), axis.text.x=element_text(size=30, face="bold"),
        axis.title=element_text(size=35,face="bold"),  axis.title.x = element_blank(),
        plot.title = element_text(size =20, face="bold"),legend.position="none",
        strip.text.x = text, strip.text.y = text,  
        legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +geom_hline(yintercept=0)

