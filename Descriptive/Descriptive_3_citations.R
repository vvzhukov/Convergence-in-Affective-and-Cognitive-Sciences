library(dplyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(stringr)

setwd("E:/Research/Data_pubmed/citations_data")

# Get the files names
files = list.files(pattern="*.json")

myData <- new.env()
idx <- as.character(length(myData) + 1)

for (file in files) {
  print(file)
  stream_in(file(file), handler = function(df){ 
    idx <<- as.character(length(myData) + 1)
    myData[[idx]] <<- df[,c(1,8,22,23)] ## filter
  }, pagesize = 10000)
}

myData <- myData %>% as.list() %>% rbindlist()

affective_data <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
affective_cit_ref <- myData[myData$pmid %in% affective_data$pmid]
write.csv(affective_cit_ref, "E:/Research/Data_pubmed/affective_data/affective_major_UI_cit_ref.csv")

affective_data_cr <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_cit_ref.csv")

# NAs in ref and citations
nrow(affective_data_cr[is.na(affective_data_cr$cited_by),]) 
# 45 586 citations
nrow(affective_data_cr[is.na(affective_data_cr$references),]) 
# 49 678 references

# Full records (citations + references)
nrow(affective_data_cr[!is.na(affective_data_cr$references) & !is.na(affective_data_cr$cited_by),]) 
# 237 814 have both citations and references


affective_data_subj <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_subjectareas.csv")

cit_ids <- affective_data_cr$cited_by
cit_ids <- na.omit(cit_ids)
cit_ids <- unlist(sapply(cit_ids,function(x) strsplit(x, split=" ")),recursive = TRUE, use.names = FALSE)
length(cit_ids) #8 552 286
cit_ids <- unique(cit_ids)
length(cit_ids) #2 012 901

ref_ids <- affective_data_cr$references
ref_ids <- na.omit(ref_ids)
ref_ids <- unlist(sapply(ref_ids,function(x) strsplit(x, split=" ")),recursive = TRUE, use.names = FALSE)
length(ref_ids) #8 346 353
ref_ids <- unique(ref_ids)
length(ref_ids) #1 762 975



setwd("E:/Research/Data_pubmed/processed_data5")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
full_data = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

full_data_cit <- full_data[full_data$pmid %in% cit_ids,] # 1 996 249
full_data_ref <- full_data[full_data$pmid %in% ref_ids,] # 1 752 592
write.csv(full_data_cit, "E:/Research/Data_pubmed/affective_data/affective_cit_full_data.csv")
write.csv(full_data_ref, "E:/Research/Data_pubmed/affective_data/affective_ref_full_data.csv")

full_data_cit <- read.csv("E:/Research/Data_pubmed/affective_data/affective_cit_full_data.csv")
full_data_ref <- read.csv("E:/Research/Data_pubmed/affective_data/affective_ref_full_data.csv")

length(unique(full_data_cit$pmid))
length(union(union(full_data_cit$pmid,full_data_ref$pmid),affective_data$pmid))



affective_data_cr <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
affective_data_cr_full <- affective_data_cr[!is.na(affective_data_cr$references) & !is.na(affective_data_cr$cited_by),]
write.csv(affective_data_cr_full,"E:/Research/Data_pubmed/affective_data/affective_major_UI_cit_ref_full.csv")

affective_data <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
affective_data_balanced <- subset(affective_data, pmid %in% affective_data_cr_full$pmid)
affective_data_balanced$X.1 <- NULL
affective_data_balanced$X <- NULL
full_data_cit$X <- NULL
full_data_ref$X <- NULL
affective_data_balanced$mesh_major_UI <- NULL

#----------------------------------
full_data_cit$major <- paste(gsub('.{1}$', '', full_data_cit$mesh_major_descriptor_UI), ", ", 
                              substring(full_data_cit$mesh_descriptor_for_major_qualifier, 2))
full_data_cit$major <- str_replace_all(full_data_cit$major, ",  ]", "]")
full_data_cit$major <- str_replace_all(full_data_cit$major, "\\x5B ,", "[")

full_data_ref$major <- paste(gsub('.{1}$', '', full_data_ref$mesh_major_descriptor_UI), ", ", 
                             substring(full_data_ref$mesh_descriptor_for_major_qualifier, 2))
full_data_ref$major <- str_replace_all(full_data_ref$major, ",  ]", "]")
full_data_ref$major <- str_replace_all(full_data_ref$major, "\\x5B ,", "[")
#----------------------------------

write.csv(full_data_ref, "E:/Research/Data_pubmed/affective_data/balanced/references.csv")
write.csv(full_data_cit, "E:/Research/Data_pubmed/affective_data/balanced/citations.csv")
write.csv(affective_data_balanced, "E:/Research/Data_pubmed/affective_data/balanced/affective.csv")



