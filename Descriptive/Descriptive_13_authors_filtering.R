
setwd("E:/Research/Data_pubmed/processed_data_authors")

# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))


data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
aff_filter <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/affective.csv")
cog_filter <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_balanced_pmid.csv")
cog_filter_nb <- subset(data_cognitive_major_nb, pmid %in% cog_filter$x)
remove(cog_filter, data_cognitive_major_nb)

aff_filter <- aff_filter$pmid
cog_filter_nb <- cog_filter_nb$pmid

aff_authors <- subset(myfiles, pmid %in% aff_filter)
cog_authors_nb <- subset(myfiles, pmid %in% cog_filter_nb)


write.csv(aff_authors, "E:/Research/Data_pubmed/affective_data/affective_authors.csv")
write.csv(cog_authors_nb, "E:/Research/Data_pubmed/cognitive_data/cognitive_authors_nobrain.csv")
