# review top 5 pubs for each key word

affective_data <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
affective_data_subj <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_subjectareas.csv")

data <- affective_data
data_subj <- affective_data_subj

data$all_mesh_words <- paste(data$mesh_descriptor_major, data$mesh_qualifier_major)
data <- data[,c("pmid","all_mesh_words")]

data$all_mesh_words <- gsub('\', ', '\',', data$all_mesh_words) # remove space between MeSH
data$all_mesh_words <- gsub(', ', '_', data$all_mesh_words)
data$all_mesh_words <- gsub('] ', ']', data$all_mesh_words)
data$all_mesh_words <- gsub(' ', '_', data$all_mesh_words)
data$all_mesh_words <- gsub('&', '', data$all_mesh_words)
data$all_mesh_words <- gsub('__', '_', data$all_mesh_words)

data <- merge(data, data_subj, by = "pmid", all.x=TRUE)

data$SA1 <- as.numeric(data$A + data$B + data$G) #orange
data$SA2 <- as.numeric(data$F) #red 
data$SA3 <- as.numeric(data$C + data$N) #green
data$SA4 <- as.numeric(data$D + data$E + data$J + data$L) #blue
data$SA5 <- as.numeric(data$H + data$I + data$K + data$M) #yellow


aff_SA1 <- subset(data, data$SA1 >0) 
aff_SA2 <- subset(data, data$SA2 >0) 
aff_SA3 <- subset(data, data$SA3 >0) 
aff_SA4 <- subset(data, data$SA4 >0) 
aff_SA5 <- subset(data, data$SA5 >0) 


# red - SA2
set.seed(1)
sample(aff_SA2[grepl('psychology', tolower(aff_SA2$all_mesh_words)),]$pmid,5)
sample(aff_SA2[grepl('stress_psychological', tolower(aff_SA2$all_mesh_words)),]$pmid,5)
sample(aff_SA2[grepl('depression', tolower(aff_SA2$all_mesh_words)),]$pmid,5)


# orange - SA1
sample(aff_SA1[grepl('humans', tolower(aff_SA1$all_mesh_words)),]$pmid,5)
sample(aff_SA1[grepl('metabolism', tolower(aff_SA1$all_mesh_words)),]$pmid,5)
sample(aff_SA1[grepl('animals', tolower(aff_SA1$all_mesh_words)),]$pmid,5)

# yellow - SA5
sample(aff_SA5[grepl('epidemiology', tolower(aff_SA5$all_mesh_words)),]$pmid,5)
sample(aff_SA5[grepl('adult', tolower(aff_SA5$all_mesh_words)),]$pmid,5)
sample(aff_SA5[grepl('middle_aged', tolower(aff_SA5$all_mesh_words)),]$pmid,5)

# green - SA3
sample(aff_SA3[grepl('comorbidity', tolower(aff_SA3$all_mesh_words)),]$pmid,5)
sample(aff_SA3[grepl('sex_factors', tolower(aff_SA3$all_mesh_words)),]$pmid,5)
sample(aff_SA3[grepl('age_factors', tolower(aff_SA3$all_mesh_words)),]$pmid,5)

# blue - SA4
sample(aff_SA4[grepl('drug_therapy', tolower(aff_SA4$all_mesh_words)),]$pmid,5)
sample(aff_SA4[grepl('diagnosis', tolower(aff_SA4$all_mesh_words)),]$pmid,5)
sample(aff_SA4[grepl('methods', tolower(aff_SA4$all_mesh_words)),]$pmid,5)



cognitive_data <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_words.csv")
cognitive_data_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")

data <- cognitive_data
data_subj <- cognitive_data_subj

data$all_mesh_words <- paste(data$mesh_descriptor, data$mesh_descriptor_major, data$mesh_qualifier, data$mesh_qualifier_major)
data <- data[,c("pmid","all_mesh_words")]

data$all_mesh_words <- gsub('\', ', '\',', data$all_mesh_words) # remove space between MeSH
data$all_mesh_words <- gsub(', ', '_', data$all_mesh_words)
data$all_mesh_words <- gsub('] ', ']', data$all_mesh_words)
data$all_mesh_words <- gsub(' ', '_', data$all_mesh_words)
data$all_mesh_words <- gsub('&', '', data$all_mesh_words)
data$all_mesh_words <- gsub('__', '_', data$all_mesh_words)

data <- merge(data, data_subj, by = "pmid", all.x=TRUE)

data$SA1 <- as.numeric(data$A + data$B + data$G) #orange
data$SA2 <- as.numeric(data$F) #red 
data$SA3 <- as.numeric(data$C + data$N) #green
data$SA4 <- as.numeric(data$D + data$E + data$J + data$L) #blue
data$SA5 <- as.numeric(data$H + data$I + data$K + data$M) #yellow


aff_SA1 <- subset(data, data$SA1 >0) 
aff_SA2 <- subset(data, data$SA2 >0) 
aff_SA3 <- subset(data, data$SA3 >0) 
aff_SA4 <- subset(data, data$SA4 >0) 
aff_SA5 <- subset(data, data$SA5 >0) 


# red - SA2
set.seed(1)
sample(aff_SA2[grepl('psychology', tolower(aff_SA2$all_mesh_words)),]$pmid,5)
sample(aff_SA2[grepl('cognition', tolower(aff_SA2$all_mesh_words)),]$pmid,5)
sample(aff_SA2[grepl('attention', tolower(aff_SA2$all_mesh_words)),]$pmid,5)
# orange - SA1
sample(aff_SA1[grepl('metabolism', tolower(aff_SA1$all_mesh_words)),]$pmid,5)
sample(aff_SA1[grepl('brain', tolower(aff_SA1$all_mesh_words)),]$pmid,5)
sample(aff_SA1[grepl('humans', tolower(aff_SA1$all_mesh_words)),]$pmid,5)
# yellow - SA5
sample(aff_SA5[grepl('physiology', tolower(aff_SA5$all_mesh_words)),]$pmid,5)
sample(aff_SA5[grepl('adult', tolower(aff_SA5$all_mesh_words)),]$pmid,5)
sample(aff_SA5[grepl('middle_aged', tolower(aff_SA5$all_mesh_words)),]$pmid,5)
# green - SA3
sample(aff_SA3[grepl('brain_diseases', tolower(aff_SA3$all_mesh_words)),]$pmid,5)
sample(aff_SA3[grepl('brain_neoplasms', tolower(aff_SA3$all_mesh_words)),]$pmid,5)
sample(aff_SA3[grepl('brain_injuries', tolower(aff_SA3$all_mesh_words)),]$pmid,5)
# blue - SA4
sample(aff_SA4[grepl('methods', tolower(aff_SA4$all_mesh_words)),]$pmid,5)
sample(aff_SA4[grepl('diagnostic_imaging', tolower(aff_SA4$all_mesh_words)),]$pmid,5)
sample(aff_SA4[grepl('diagnosis', tolower(aff_SA4$all_mesh_words)),]$pmid,5)

