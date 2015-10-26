
#read all data files in separate data frames
myDir <- "d:/Karen's/PhD/R program/Processed_data/si_addition/"
filenames <- list.files(myDir) 
filenames <- filenames[grep("[.]csv", filenames)] 

data_names <- gsub("[.]csv", "", filenames) 

for(i in 1:length(filenames)) assign(data_names[i], read.csv(sep=";", file.path(myDir, filenames[i]))) 

ls() #lists all objects in your workspace

#putting replicate ID and condition and binding into one dataframe
nonstarved_Si_A1$rep = "A1"
nonstarved_Si_A2$rep = "A2"
nonstarved_Si_A3$rep = "A3"
nonstarved_Si_A4$rep = "A4"

nonstarved_Si <- rbind (nonstarved_Si_A1, nonstarved_Si_A2, nonstarved_Si_A3, nonstarved_Si_A4)
nonstarved_Si$cond= "nonstarved"
nonstarved_Si$add= "Si"

nonstarved_B1$rep = "B1"
nonstarved_B2$rep = "B2"
nonstarved_B3$rep = "B3"
nonstarved_B4$rep = "B4"

nonstarved <- rbind (nonstarved_B1, nonstarved_B2, nonstarved_B3, nonstarved_B4)
nonstarved$cond= "nonstarved"
nonstarved$add ="mot"

starved_blank_A2$rep = "A2"
starved_blank_A4$rep = "A4"
starved_blank_B1$rep = "B1"
starved_blank_B3$rep = "B3"

starved_blank <- rbind (starved_blank_A2, starved_blank_A4, starved_blank_B1, starved_blank_B3)
starved_blank$cond = "starved"
starved_blank$add = "blank"

starved_Si_A1$rep = "A1"
starved_Si_A3$rep = "A3"
starved_Si_B2$rep = "B2"
starved_Si_B4$rep = "B4"

starved_Si <- rbind (starved_Si_A1, starved_Si_A3, starved_Si_B2, starved_Si_B4)
starved_Si$cond = "starved"
starved_Si$add = "Si"

starved_B1$rep = "B1"
starved_C1$rep = "C1"
starved_C2$rep = "C2"
starved_C3$rep = "C3"

starved <- rbind (starved_B1, starved_C1, starved_C2, starved_C3)
starved$cond = "starved"
starved$add = "mot"

#put it into one magical data frame :)
siadd <- rbind(nonstarved, nonstarved_Si, starved, starved_blank, starved_Si) 

siadd$condadd = paste (siadd$cond, siadd$add, sep="-")
siadd$ID = paste (siadd$Label, siadd$condadd, sep="-")

write.table (siadd, "d:/Karen's/PhD/R program/Processed_data/si_addition/siadd.csv", 
                         sep=";", col.names=T, row.names=F)
