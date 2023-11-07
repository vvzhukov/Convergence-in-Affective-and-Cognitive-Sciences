library(stringr)
library(stringi)
library(dplyr)
affective_data <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")

affective_refs <- subset(affective_data, refs != "[]")
refs_ids <- affective_refs[,15]
converted_ids <- c()

for (id in refs_ids){
  temp = stringi::stri_extract_all_regex(id, "(?<=').*?(?=')")[[1]]
  converted_ids <<- c(converted_ids, temp[temp!=", "])
}
write.csv(converted_ids, "E:/Research/Data_pubmed/affective_data/affective_refs.csv")

converted_ids <- read.csv("E:/Research/Data_pubmed/affective_data/affective_refs.csv")

converted_ids <- converted_ids$x[!is.na(converted_ids$x)]
converted_ids <- unique(converted_ids)
converted_ids <- as.integer(converted_ids)


converted_ids_u <- converted_ids[!converted_ids %in% affective_data$pmid]
write.csv(converted_ids_u, "E:/Research/Data_pubmed/affective_data/affective_refs_una.csv")
ref_ids <- read.csv("E:/Research/Data_pubmed/affective_data/affective_refs_una.csv")


setwd("E:/Research/Data_pubmed/processed_data5")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))


ref_full_data <- myfiles[myfiles$pmid %in% ref_ids$x,]
write.csv(ref_full_data, "E:/Research/Data_pubmed/affective_data/affective_refs_dull_data.csv")


refs <- read.csv("E:/Research/Data_pubmed/affective_data/affective_refs_dull_data.csv")
affective <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")

refs$X <- NULL
affective$X <- NULL

colnames(refs)
colnames(affective)
affective$mesh_UI <- NULL
affective$mesh_major_UI <- NULL

aff_ref <- union(refs,affective)
write.csv(aff_ref, "E:/Research/Data_pubmed/affective_data/affective_and_refs.csv")

nrow(affective[affective$refs != "[]",])
