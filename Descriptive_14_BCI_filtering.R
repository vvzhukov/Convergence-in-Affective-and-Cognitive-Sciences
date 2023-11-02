
setwd("E:/Research/Data_pubmed/processed_data5")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

library(dplyr)
library(ggplot2)
library(scales)
object.size(myfiles)

colnames(myfiles)
myfiles$keywords <- NULL
myfiles$mesh_descriptor <- NULL
myfiles$mesh_descriptor_major <- NULL
myfiles$mesh_qualifier <- NULL
myfiles$mesh_qualifier_major <- NULL


myfiles %>% group_by(year) %>% summarise(n = n()) -> myfiles_yearly
myfiles_yearly <- subset(myfiles_yearly, year %in% c(1967:2019))


# Filter
colnames(myfiles)
remove(myfiles_yearly, p, files)
myfiles$mesh_major_UI <- paste(myfiles$mesh_major_descriptor_UI, myfiles$mesh_descriptor_for_major_qualifier)
myfiles$mesh_UI <- paste(myfiles$mesh_descriptor_UI, myfiles$mesh_qualifier_UI)


mesh_kyes <- 'D038262|D062207|D004567|D058542|D020007|D058117'

data_bci_non_major <- dplyr::filter(myfiles, grepl(mesh_kyes, mesh_UI, ignore.case = TRUE))
data_bci_major <- dplyr::filter(myfiles, grepl(mesh_kyes, mesh_major_UI, ignore.case = TRUE))



write.csv(data_bci_major, "E:/Research/Data_pubmed/bci_data/bci_major_UI.csv")
write.csv(data_bci_non_major, "E:/Research/Data_pubmed/bci_data/bci_non_major_UI.csv")
