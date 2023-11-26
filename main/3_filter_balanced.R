cog_bal <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_balanced_pmid.csv")
aff_bal <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_balanced_pmid.csv")


cognitive_cit_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_data_cit_ref_data.csv")
data_cognitive_major <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI.csv")

affective_cit_ref <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_data_cit_ref_data.csv")
data_affective_major <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_major_UI.csv")

cognitive_cit_ref <- subset(cognitive_cit_ref, pmid %in% cog_bal$x)
affective_cit_ref <- subset(affective_cit_ref, pmid %in% aff_bal$x)


cog_cit <- unlist(sapply(cognitive_cit_ref$cited_by,function(x) strsplit(x, split=" ")),recursive = TRUE, use.names = FALSE)
cog_ref <- unlist(sapply(cognitive_cit_ref$references,function(x) strsplit(x, split=" ")),recursive = TRUE, use.names = FALSE)

aff_cit <- unlist(sapply(affective_cit_ref$cited_by,function(x) strsplit(x, split=" ")),recursive = TRUE, use.names = FALSE)
aff_ref <- unlist(sapply(affective_cit_ref$references,function(x) strsplit(x, split=" ")),recursive = TRUE, use.names = FALSE)

remove(aff_bal, cog_bal, aff_cit_dat, aff_ref_dat, cog_cit_dat, cog_ref_dat, affective_cit_ref, cognitive_cit_ref, data_affective_major, data_cognitive_major)
gc() # free unused memory

setwd("E:/Research/Data_pubmed/processed_data6")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

colnames(myfiles)
myfiles$keywords <- NULL
myfiles$mesh_descriptor <- NULL
myfiles$mesh_descriptor_major <- NULL
myfiles$mesh_qualifier <- NULL
myfiles$mesh_qualifier_major <- NULL


# Filter
colnames(myfiles)
myfiles$mesh_major_UI <- paste(myfiles$mesh_major_descriptor_UI, myfiles$mesh_descriptor_for_major_qualifier)

#myfiles <- myfiles[,c(1,2,10)]

cog_cit_dat <- subset(myfiles, pmid %in% cog_cit)
cog_ref_dat <- subset(myfiles, pmid %in% cog_ref)

aff_cit_dat <- subset(myfiles, pmid %in% aff_cit)
aff_ref_dat <- subset(myfiles, pmid %in% aff_ref)


write.csv(cog_cit_dat, "E:/Research/Data_pubmed/cognitive_data23/cognitive_cit_bal_full_data.csv")
write.csv(cog_ref_dat, "E:/Research/Data_pubmed/cognitive_data23/cognitive_ref_bal_full_data.csv")
write.csv(aff_cit_dat, "E:/Research/Data_pubmed/affective_data23/affective_cit_bal_full_data.csv")
write.csv(aff_ref_dat, "E:/Research/Data_pubmed/affective_data23/affective_ref_bal_full_data.csv")
