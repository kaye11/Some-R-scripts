library(ggplot2)
library(grid)
library(ggthemes)
library(gridExtra)
library(reshape2)

grid.newpage()
text <- element_text(size = 20) #change the size of the axes
theme_set(theme_bw())

##RMS plotting overlayed
df <- data.frame(RMS.Si, RMS.control)
df$Si=df$MF
df$control=df$MF.1


ggplot(df, aes(time, y = value, color = Treatment)) + 
  geom_point(aes(y = Si, col = "Si"), size=3) + 
  geom_point(aes(y = control, col = "Control"), size=3)+ labs(list(x = "Time (s)", y = "RMS (µm)")) + 
  scale_colour_manual(values = c("black","red")) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"), axis.text=text,  
        legend.position=c(0,1), legend.justification=c(0,1),
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#ND

## Split the data
dfs <- split(binned,binned$ID)

## Find hypotenuse between first and last rows for each A
NDtable2 <- as.data.frame(lapply( dfs , function(x){
  j <- nrow(x)
  str <- x[1,c("X","Y")]
  end <- x[j,c("X","Y")]
  dist <- sqrt( sum( (end - str)^2 ) )
  return( dist )
} ))

library(reshape)
NDmelt=melt(NDtable2)
#NDtable saved in processed data folder as NDtable and then imported as ND

shapiro.test(ND$ND) # not normal
library(lawstat)
levene.test(ND$ND, group=ND$cond, location="mean")

wilcox.test(ND~cond, data=ND, paired=FALSE) #not significantly different



grid.newpage()
text <- element_text(size = 18, face="bold") #change the size of the axes
theme_set(theme_bw())


ggplot(data=ND, aes(x=cond, y=ND, color=cond)) +  geom_boxplot(aes(color=cond)) +
  labs(list(x = "Experimental Condition", y = "Gross distance traveled (µm)")) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20,face="bold"), 
        plot.title = element_text(size =20, face="bold"),legend.position="none",
        strip.text.x = text, strip.text.y = text, legend.title=text, legend.text=text, panel.margin=unit(1, "lines"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#GD

NT = data.table(binned, key="ID")

GDtable <- NT[, list(GD=max(GD), T=length(T)), by=c("ID", "cond")] 

qplot(cond,GD, data = GDtable,  geom = "boxplot")
shapiro.test(GDtable$GD)

library(lawstat)
levene.test(GDtable$GD, group=GDtable$cond, location="mean")

#not normal but pop variances are equal

wilcox.test(GD~cond, data=GDtable, paired=FALSE) # no sig difference in GD
