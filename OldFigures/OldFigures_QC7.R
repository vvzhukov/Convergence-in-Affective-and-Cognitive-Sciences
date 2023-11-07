library(dplyr)

affective_data <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
cognitive_data <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_words.csv")

set.seed(456)

aff_sample <- sample_n(affective_data, 10)[,c("pmid", "mesh_major_descriptor_UI", "mesh_major_qualifier_UI")]
cog_sample <- sample_n(cognitive_data, 10)[,c("pmid", "mesh_major_descriptor_UI", "mesh_major_qualifier_UI")]

aff_SA2[aff_SA2$pmid==9333560,c("pmid","all_mesh_words")]

affective_data[affective_data$pmid==9333560,c("pmid", "mesh_major_descriptor_UI", "mesh_major_qualifier_UI")]
