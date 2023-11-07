library(dplyr)

data_affective <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_plus_venue.csv")
data_affective_mesh <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/affective_subjectareas.csv")

data_affective %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary
summary(data_affective_venue_summary$n)


#Old scheme
#cog_subj$SA1 <- as.numeric(cog_subj$A + cog_subj$B + cog_subj$G)
#cog_subj$SA2 <- as.numeric(cog_subj$F)
#cog_subj$SA3 <- as.numeric(cog_subj$C + cog_subj$N)
#cog_subj$SA4 <- as.numeric(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L)
#cog_subj$SA5 <- as.numeric(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M)
data_affective_mesh$SA1 <- as.numeric(data_affective_mesh$F) #SA2
data_affective_mesh$SA2 <- as.numeric(data_affective_mesh$A + data_affective_mesh$B + data_affective_mesh$G) #SA1
data_affective_mesh$SA3 <- as.numeric(data_affective_mesh$H + data_affective_mesh$I + data_affective_mesh$K + data_affective_mesh$M) #SA5
data_affective_mesh$SA4 <- as.numeric(data_affective_mesh$C + data_affective_mesh$N) #SA3
data_affective_mesh$SA5 <- as.numeric(data_affective_mesh$D + data_affective_mesh$E + data_affective_mesh$J + data_affective_mesh$L) #SA4

data_affective_mesh <- merge(data_affective_mesh, data_affective[,c("pmid", "ISSN_journal")], by = "pmid", all.x = TRUE)
data_affective_mesh <- data_affective_mesh[!is.na(data_affective_mesh$ISSN_journal),]

data_affective_mesh_SA1 <- data_affective_mesh[data_affective_mesh$SA1>0,]
data_affective_mesh_SA2 <- data_affective_mesh[data_affective_mesh$SA2>0,]
data_affective_mesh_SA3 <- data_affective_mesh[data_affective_mesh$SA3>0,]
data_affective_mesh_SA4 <- data_affective_mesh[data_affective_mesh$SA4>0,]
data_affective_mesh_SA5 <- data_affective_mesh[data_affective_mesh$SA5>0,]

data_affective_mesh_SA1 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA1 
data_affective_mesh_SA2 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA2
data_affective_mesh_SA3 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA3
data_affective_mesh_SA4 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA4
data_affective_mesh_SA5 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA5

data_affective_mesh_SA1 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA1 
data_affective_mesh_SA2 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA2
data_affective_mesh_SA3 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA3
data_affective_mesh_SA4 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA4
data_affective_mesh_SA5 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_affective_venue_summary_SA5







data_cognitive <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_plus_venue.csv")
data_cognitive_mesh <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")
data_cognitive_mesh_balanced <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_balanced_pmid.csv")
data_cognitive_mesh <- subset(data_cognitive_mesh, data_cognitive_mesh$pmid %in% data_cognitive_mesh_balanced$x)
remove(data_cognitive_mesh_balanced)

data_cognitive %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_cognitive_venue_summary
summary(data_cognitive_venue_summary$n)

data_cognitive_mesh$SA1 <- as.numeric(data_cognitive_mesh$F) #SA2
data_cognitive_mesh$SA2 <- as.numeric(data_cognitive_mesh$A + data_cognitive_mesh$B + data_cognitive_mesh$G) #SA1
data_cognitive_mesh$SA3 <- as.numeric(data_cognitive_mesh$H + data_cognitive_mesh$I + data_cognitive_mesh$K + data_cognitive_mesh$M) #SA5
data_cognitive_mesh$SA4 <- as.numeric(data_cognitive_mesh$C + data_cognitive_mesh$N) #SA3
data_cognitive_mesh$SA5 <- as.numeric(data_cognitive_mesh$D + data_cognitive_mesh$E + data_cognitive_mesh$J + data_cognitive_mesh$L) #SA4

data_cognitive_mesh <- merge(data_cognitive_mesh, data_cognitive[,c("pmid", "ISSN_journal")], by = "pmid", all.x = TRUE)
data_cognitive_mesh <- data_cognitive_mesh[!is.na(data_cognitive_mesh$ISSN_journal),]

data_cognitive_mesh_SA1 <- data_cognitive_mesh[data_cognitive_mesh$SA1>0,]
data_cognitive_mesh_SA2 <- data_cognitive_mesh[data_cognitive_mesh$SA2>0,]
data_cognitive_mesh_SA3 <- data_cognitive_mesh[data_cognitive_mesh$SA3>0,]
data_cognitive_mesh_SA4 <- data_cognitive_mesh[data_cognitive_mesh$SA4>0,]
data_cognitive_mesh_SA5 <- data_cognitive_mesh[data_cognitive_mesh$SA5>0,]

data_cognitive_mesh_SA1 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA1
data_cognitive_mesh_SA2 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA2
data_cognitive_mesh_SA3 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA3
data_cognitive_mesh_SA4 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA4
data_cognitive_mesh_SA5 %>% group_by(ISSN_journal) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA5

summary(data_cognitive_venue_summary_SA1$n)
summary(data_cognitive_venue_summary_SA2$n)
summary(data_cognitive_venue_summary_SA3$n)
summary(data_cognitive_venue_summary_SA4$n)
summary(data_cognitive_venue_summary_SA5$n)





# IF by ISSN

data_IF <- read.csv("E:/Research/Data_IF/JCR_full_data.csv")
# We might want to filter duplicates
# data_IF <- data_IF[!duplicated(data_IF$Journal_name),]

# trim spaces
data_IF$ISSN <- trimws(data_IF$ISSN, which = c("both"))
data_IF$eISSN <- trimws(data_IF$eISSN, which = c("both"))

get_full_j_info <- function(data_affective_venue_summary_SA1_y, y = FALSE) {
  if (y) {
    data_affective_venue_summary_SA1_y <- merge(data_affective_venue_summary_SA1_y, data_IF, by.x = "ISSN_journal", by.y = "ISSN",  all.x = TRUE)
    
    # for those venues wich were not in ISSN check eISSN
    temp <- merge(data_affective_venue_summary_SA1_y[is.na(data_affective_venue_summary_SA1_y$Journal_name),c(1,2,3)], data_IF, by.x = "ISSN_journal", by.y = "eISSN",  all.x = TRUE)
    colnames(temp) <- colnames(data_affective_venue_summary_SA1_y)
    
    # combine both: eISSN and ISSN
    return(rbind(data_affective_venue_summary_SA1_y[!is.na(data_affective_venue_summary_SA1_y$Journal_name),],
                 temp))
  } else {
  # take top records
  # data_affective_venue_summary_SA1 <- head(data_affective_venue_summary_SA1[order(-data_affective_venue_summary_SA1$n),], 10)
  # merge
  data_affective_venue_summary_SA1_y <- merge(data_affective_venue_summary_SA1_y, data_IF, by.x = "ISSN_journal", by.y = "ISSN",  all.x = TRUE)
  
  # for those venues wich were not in ISSN check eISSN
  temp <- merge(data_affective_venue_summary_SA1_y[is.na(data_affective_venue_summary_SA1_y$Journal_name),c(1,2)], data_IF, by.x = "ISSN_journal", by.y = "eISSN",  all.x = TRUE)
  colnames(temp) <- colnames(data_affective_venue_summary_SA1_y)
  
  # combine both: eISSN and ISSN
  return(rbind(data_affective_venue_summary_SA1_y[!is.na(data_affective_venue_summary_SA1_y$Journal_name),],
        temp))
  }
}


SA1a <- get_full_j_info(data_affective_venue_summary_SA1)
SA2a <- get_full_j_info(data_affective_venue_summary_SA2)
SA3a <- get_full_j_info(data_affective_venue_summary_SA3)
SA4a <- get_full_j_info(data_affective_venue_summary_SA4)
SA5a <- get_full_j_info(data_affective_venue_summary_SA5)
SAaa <- get_full_j_info(data_affective_mesh %>% group_by(ISSN_journal) %>% summarise(n = n()))

SA1c <- get_full_j_info(data_cognitive_venue_summary_SA1)
SA2c <- get_full_j_info(data_cognitive_venue_summary_SA2)
SA3c <- get_full_j_info(data_cognitive_venue_summary_SA3)
SA4c <- get_full_j_info(data_cognitive_venue_summary_SA4)
SA5c <- get_full_j_info(data_cognitive_venue_summary_SA5)
SAac <- get_full_j_info(data_cognitive_mesh %>% group_by(ISSN_journal) %>% summarise(n = n()))


#write.csv(head(SA5c[order(SA5c$n, decreasing = TRUE),],100),"C:/Users/Rubinzone/Desktop/C_SA5.csv")



# Years details

data_affective_mesh_y <- merge(data_affective_mesh, data_affective[,c("pmid","year")], by.x = "pmid", by.y = "pmid", all.x = TRUE)

data_affective_mesh_y$SA1 <- as.numeric(data_affective_mesh_y$F) #SA2
data_affective_mesh_y$SA2 <- as.numeric(data_affective_mesh_y$A + data_affective_mesh_y$B + data_affective_mesh_y$G) #SA1
data_affective_mesh_y$SA3 <- as.numeric(data_affective_mesh_y$H + data_affective_mesh_y$I + data_affective_mesh_y$K + data_affective_mesh_y$M) #SA5
data_affective_mesh_y$SA4 <- as.numeric(data_affective_mesh_y$C + data_affective_mesh_y$N) #SA3
data_affective_mesh_y$SA5 <- as.numeric(data_affective_mesh_y$D + data_affective_mesh_y$E + data_affective_mesh_y$J + data_affective_mesh_y$L) #SA4

data_affective_mesh_y_SA1 <- data_affective_mesh_y[data_affective_mesh_y$SA1>0,]
data_affective_mesh_y_SA2 <- data_affective_mesh_y[data_affective_mesh_y$SA2>0,]
data_affective_mesh_y_SA3 <- data_affective_mesh_y[data_affective_mesh_y$SA3>0,]
data_affective_mesh_y_SA4 <- data_affective_mesh_y[data_affective_mesh_y$SA4>0,]
data_affective_mesh_y_SA5 <- data_affective_mesh_y[data_affective_mesh_y$SA5>0,]

data_affective_mesh_y_SA1 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA1_y 
data_affective_mesh_y_SA2 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA2_y
data_affective_mesh_y_SA3 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA3_y
data_affective_mesh_y_SA4 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA4_y
data_affective_mesh_y_SA5 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA5_y

data_affective_mesh_y_SA1 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA1_y 
data_affective_mesh_y_SA2 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA2_y
data_affective_mesh_y_SA3 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA3_y
data_affective_mesh_y_SA4 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA4_y
data_affective_mesh_y_SA5 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_affective_venue_summary_SA5_y


data_cognitive_mesh_y <- merge(data_cognitive_mesh, data_cognitive[,c("pmid","year")], by.x = "pmid", by.y = "pmid", all.x = TRUE)

data_cognitive_mesh_y$SA1 <- as.numeric(data_cognitive_mesh_y$F) #SA2
data_cognitive_mesh_y$SA2 <- as.numeric(data_cognitive_mesh_y$A + data_cognitive_mesh_y$B + data_cognitive_mesh_y$G) #SA1
data_cognitive_mesh_y$SA3 <- as.numeric(data_cognitive_mesh_y$H + data_cognitive_mesh_y$I + data_cognitive_mesh_y$K + data_cognitive_mesh_y$M) #SA5
data_cognitive_mesh_y$SA4 <- as.numeric(data_cognitive_mesh_y$C + data_cognitive_mesh_y$N) #SA3
data_cognitive_mesh_y$SA5 <- as.numeric(data_cognitive_mesh_y$D + data_cognitive_mesh_y$E + data_cognitive_mesh_y$J + data_cognitive_mesh_y$L) #SA4

data_cognitive_mesh_y_SA1 <- data_cognitive_mesh_y[data_cognitive_mesh_y$SA1>0,]
data_cognitive_mesh_y_SA2 <- data_cognitive_mesh_y[data_cognitive_mesh_y$SA2>0,]
data_cognitive_mesh_y_SA3 <- data_cognitive_mesh_y[data_cognitive_mesh_y$SA3>0,]
data_cognitive_mesh_y_SA4 <- data_cognitive_mesh_y[data_cognitive_mesh_y$SA4>0,]
data_cognitive_mesh_y_SA5 <- data_cognitive_mesh_y[data_cognitive_mesh_y$SA5>0,]

data_cognitive_mesh_y_SA1 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA1_y 
data_cognitive_mesh_y_SA2 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA2_y
data_cognitive_mesh_y_SA3 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA3_y
data_cognitive_mesh_y_SA4 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA4_y
data_cognitive_mesh_y_SA5 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA5_y

data_cognitive_mesh_y_SA1 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA1_y 
data_cognitive_mesh_y_SA2 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA2_y
data_cognitive_mesh_y_SA3 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA3_y
data_cognitive_mesh_y_SA4 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA4_y
data_cognitive_mesh_y_SA5 %>% group_by(ISSN_journal, year) %>% summarise(n = n()) -> data_cognitive_venue_summary_SA5_y


SA1ay <- get_full_j_info(data_affective_venue_summary_SA1_y, y = TRUE)
SA2ay <- get_full_j_info(data_affective_venue_summary_SA2_y, y = TRUE)
SA3ay <- get_full_j_info(data_affective_venue_summary_SA3_y, y = TRUE)
SA4ay <- get_full_j_info(data_affective_venue_summary_SA4_y, y = TRUE)
SA5ay <- get_full_j_info(data_affective_venue_summary_SA5_y, y = TRUE)

SA1cy <- get_full_j_info(data_cognitive_venue_summary_SA1_y, y = TRUE)
SA2cy <- get_full_j_info(data_cognitive_venue_summary_SA2_y, y = TRUE)
SA3cy <- get_full_j_info(data_cognitive_venue_summary_SA3_y, y = TRUE)
SA4cy <- get_full_j_info(data_cognitive_venue_summary_SA4_y, y = TRUE)
SA5cy <- get_full_j_info(data_cognitive_venue_summary_SA5_y, y = TRUE)
