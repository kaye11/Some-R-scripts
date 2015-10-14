old.si.forced <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/forced data/old_data/ old.si.forced.binned .csv", sep=";")

old.control.forced <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/forced data/old_data/old.control.forced.binned.csv", sep=";")

p36.control.forced <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/forced data/p36/p36.control.forced.binned .csv", sep=";")

p36.si.forced <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/forced data/p36/p36.si.forced.binned .csv", sep=";")


vm4.si07.binned <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4_not corrected yet for angle/vm4-si-007-correctedbinned.csv", sep=";")
vm4.si04.binned <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4_not corrected yet for angle/vm4-si-004-correctedbinned.csv", sep=";")
vm4.control <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/binneddata/vm4_not corrected yet for angle/VM4-1-control-008 .csv", sep=";")


#sorting
vm4.control.sort <- arrange(vm4.control, A, T)
vm4.si07.sort <- arrange(vm4.si07.binned, A, T)
vm4.si04.sort <- arrange(vm4.si04.binned, A, T)

vm4.control.raw <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4/VM4-1-control-008 .csv", sep=";")
vm4.si.raw <- read.csv("D:/Karen's/PhD/R program/Processed_data/trackdata/densecells_popbased/raw data/vm4/VM4-1-si-007.csv", sep=";")

