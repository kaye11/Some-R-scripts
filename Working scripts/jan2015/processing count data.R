#data consists of treatment (control or Si), reptreat (replicate ID), bintreat (bin+treatment, I just used it as a grouping variable), T (time), 
#Cells (Cell counts),Bin (A, B or C), all (the total number of cells in a specific time point), rad (the radius of the bead)

library(ggplot2)

##Correction of cell counts relative to area and total cell count
#Radii
ra = 112
rb = 224
rc = 336

# Correction for space (CS)
countraw$CellsCS=NA
countraw$CellsCS[countraw$Bin  == "A"] = ((countraw$Cells[countraw$Bin  == "A"])/(pi*(ra^2 - countraw$rad[countraw$Bin  == "A"]^2)))
countraw$CellsCS[countraw$Bin  == "B"] = countraw$Cells[countraw$Bin == "B"]/(pi*(rb^2-ra^2))
countraw$CellsCS[countraw$Bin  == "C"]= countraw$Cells[countraw$Bin  == "C"]/(pi*(rc^2-rb^2))


#Raw data corrected for mean cell count, as percent (MCC)
countraw$CellsMCC = NA
KG2<-split(countraw, countraw$reptreat)
Celltemp <- lapply( KG2 , function(x) mean(x[,c("all")]))
Cellstemp2<- unsplit(Celltemp, countraw$reptreat)
countraw$CellsMCC<- (countraw$CellsCS/Cellstemp2)*100


#Normalization of data per treatment per bin (z-score)
countraw$CellsN = NA
KG3<-split(countraw, countraw$bintreat)
Cellsnorm <- lapply( KG3 , function(x) scale(x[,c("CellsMCC")], center=T, scale=T))
countraw$CellsN<- unsplit(Cellsnorm, countraw$bintreat)

#Plotting
qplot(Bin,CellsN, color = treatment, data = countraw,  geom = "boxplot") + facet_wrap(~treatment) 
