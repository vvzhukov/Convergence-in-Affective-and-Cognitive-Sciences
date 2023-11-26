
setwd("E:/Research/Data_pubmed/processed_data_authors2")

# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

cog_bal <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_balanced_pmid.csv")$x
aff_bal <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_balanced_pmid.csv")$x

aff_authors <- subset(myfiles, pmid %in% aff_bal)
cog_authors <- subset(myfiles, pmid %in% cog_bal)


write.csv(aff_authors, "E:/Research/Data_pubmed/affective_data23/affective_authors.csv")
write.csv(cog_authors, "E:/Research/Data_pubmed/cognitive_data23/cognitive_authors.csv")
