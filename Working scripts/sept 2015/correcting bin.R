#read data
s1 <- read.delim("D:/Karen's/PhD/TRACKMATE DATA/Si paper new videos/VM4-1-si-007/Spots in tracks statistics_2.xls")

#source trackmate.R

#subset dataset for every 120s

t60 <- s1[s1$T <61, ]
t120 <- s1[s1$T >60 & s1$T < 121, ]
t180 <- s1 [s1$T >120 & s1$T < 181, ]
t240 <- s1 [s1$T >180 & s1$T < 241, ]
t300 <- s1 [s1$T >240 & s1$T < 301, ]
t360 <- s1 [s1$T >300 & s1$T < 361, ]
t420 <- s1 [s1$T >360 & s1$T < 421, ]
t480 <- s1 [s1$T >420 & s1$T < 481, ]
t540 <- s1 [s1$T >480 & s1$T < 541, ]
t600 <- s1 [s1$T > 540, ]

write.table(t60, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t60.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t120, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t120.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t180, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t180.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t240, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t240.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t300, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t300.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t360, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t360.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t420, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t420.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t480, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t480.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t540, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t540.csv", 
            sep=";", col.names=T, row.names=F)

write.table(t600, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si-t600.csv", 
            sep=";", col.names=T, row.names=F)

#source computing angle.R

#read individual data with computed angles

t60 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t60 .csv", sep=";")
t120 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t120 .csv", sep=";")
t180 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t180 .csv", sep=";")
t240 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t240 .csv", sep=";")
t300 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t300 .csv", sep=";")
t360 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t360 .csv", sep=";")
t420 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t420 .csv", sep=";")
t480 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t480 .csv", sep=";")
t540 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t540 .csv", sep=";")
t600 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4-si/ vm4-si-t600 .csv", sep=";")

#source binning spatial new.R

#read individual binned data

t60 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t60 .csv", sep=";")
t120 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t120 .csv", sep=";")
t180 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t180 .csv", sep=";")
t240 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t240 .csv", sep=";")
t300 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t300 .csv", sep=";")
t360 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t360 .csv", sep=";")
t420 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t420 .csv", sep=";")
t480 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t480 .csv", sep=";")
t540 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t540 .csv", sep=";")
t600 <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si/ vm4-si-t600 .csv", sep=";")


#combining data sets
vm4_si <- rbind(t60, t120, t180, t240, t300, t360, t420, t480, t540, t600)

write.table(vm4_si, "d:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4-si-correctedbinned.csv", 
            sep=";", col.names=T, row.names=F)