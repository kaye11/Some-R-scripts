##merge all data sets into one data frame and saving it as a csv table
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T, sep="")})} #function to read data

mymergeddata = multmerge("d:/Karen's/PhD/R program/Processed_data/si_addition/") #the actual use of the function, result is a list

merged.data.frame = Reduce(function(...) merge(..., all=T), mymergeddata) #makes a list into a data frame (interactive data table that you can click)

write.table (merged.data.frame, "d:/Karen's/PhD/R program/Processed_data/si_addition/merged.csv", 
             sep=";", col.names=T, row.names=F) #save your data frame into a table
