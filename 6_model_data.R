library(dplyr)
library(tidyr)
library(lsa)

# run 3_descriptive_analysis_ui.py
# on cognitive_cit_full_data, cognitive_ref_full_data, affective_cit_full_data, affective_ref_full_data
# to get *_subjectareas files


cog_bal_id <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_balanced_pmid.csv")$x
cog_major <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI.csv")
cog_major_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI_subjectareas.csv")
cog_cit_ref_links <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_data_cit_ref_data.csv")
cog_cit_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_cit_full_data_subjectareas.csv")
cog_ref_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_ref_full_data_subjectareas.csv")

# authors data 
cog_auth <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_authors.csv")


#  Form model data based on updated 2023 files

# get number of authors

cog_auth %>% group_by(pmid) %>% 
  summarise(total_count=n()) -> cog_auth_num

cog_major$X <- NULL
cog_major$mesh_descriptor_UI <- NULL
cog_major$mesh_qualifier_UI <- NULL
cog_major$mesh_major_descriptor_UI <- NULL
cog_major$mesh_descriptor_for_major_qualifier <- NULL
cog_major$mesh_major_UI <- NULL
cog_major$mesh_UI <- NULL
cog_major$mesh_major_qualifier_UI <- NULL
cog_major$refs <- NULL


# filter only balanced recs
cog_major <- subset(cog_major, pmid %in% cog_bal_id)
# add author number
cog_major <- merge(cog_major, cog_auth_num, by.x = "pmid", by.y = "pmid", all.x = T)
colnames(cog_major)[6] <- 'pAuthors_n'


# Subject areas
cog_major_sa$pSA1 <- cog_major_sa$A + cog_major_sa$B + cog_major_sa$G
cog_major_sa$pSA2 <- cog_major_sa$F
cog_major_sa$pSA3 <- cog_major_sa$C + cog_major_sa$N
cog_major_sa$pSA4 <- cog_major_sa$D + cog_major_sa$E + cog_major_sa$J + cog_major_sa$L
cog_major_sa$pSA5 <- cog_major_sa$H + cog_major_sa$I + cog_major_sa$K + cog_major_sa$M

cog_major <- merge(cog_major, cog_major_sa[,c('pmid', 'pSA1', 'pSA2', 'pSA3', 'pSA4', 'pSA5')], by.x = "pmid", by.y = "pmid", all.x = T)

# Cross disciplinarity
cog_major$pX_disc <- as.integer(((cog_major$pSA1>0) + (cog_major$pSA2>0) + (cog_major$pSA3>0) +
                                    (cog_major$pSA4>0) + (cog_major$pSA5>0))>1)


# Citation and references
# citations
cog_cit_link <- cog_cit_ref_links[,c('pmid', 'cited_by')] %>%
  separate_rows(cited_by, sep=" ")

cog_cit_link <- merge(cog_cit_link, cog_cit_sa, by.x = 'cited_by', py.y = 'pmid', all.x = T)

cog_cit_link$cSA1 <- cog_cit_link$A + cog_cit_link$B + cog_cit_link$G
cog_cit_link$cSA2 <- cog_cit_link$F
cog_cit_link$cSA3 <- cog_cit_link$C + cog_cit_link$N
cog_cit_link$cSA4 <- cog_cit_link$D + cog_cit_link$E + cog_cit_link$J + cog_cit_link$L
cog_cit_link$cSA5 <- cog_cit_link$H + cog_cit_link$I + cog_cit_link$K + cog_cit_link$M

cog_cit_link[is.na(cog_cit_link)] <- 0

cog_cit_link %>% group_by(pmid) %>% 
  summarise(cSA1 = sum(cSA1),
            cSA2 = sum(cSA2),
            cSA3 = sum(cSA3),
            cSA4 = sum(cSA4),
            cSA5 = sum(cSA5)) -> cog_cit

cog_major <- merge(cog_major, cog_cit, by.x = 'pmid', by.y = 'pmid', all.x = T)


# same for references
cog_ref_link <- cog_cit_ref_links[,c('pmid', 'references')] %>%
  separate_rows(references, sep=" ")

cog_ref_link <- merge(cog_ref_link, cog_ref_sa, by.x = 'references', py.y = 'pmid', all.x = T)

cog_ref_link$rSA1 <- cog_ref_link$A + cog_ref_link$B + cog_ref_link$G
cog_ref_link$rSA2 <- cog_ref_link$F
cog_ref_link$rSA3 <- cog_ref_link$C + cog_ref_link$N
cog_ref_link$rSA4 <- cog_ref_link$D + cog_ref_link$E + cog_ref_link$J + cog_ref_link$L
cog_ref_link$rSA5 <- cog_ref_link$H + cog_ref_link$I + cog_ref_link$K + cog_ref_link$M

cog_ref_link[is.na(cog_ref_link)] <- 0

cog_ref_link %>% group_by(pmid) %>% 
  summarise(rSA1 = sum(rSA1),
            rSA2 = sum(rSA2),
            rSA3 = sum(rSA3),
            rSA4 = sum(rSA4),
            rSA5 = sum(rSA5),
            reference_count = n()) -> cog_ref

cog_major <- merge(cog_major, cog_ref, by.x = 'pmid', by.y = 'pmid', all.x = T)


# citation count and normalization
cog_major <- merge(cog_major, cog_cit_ref_links[,c('pmid', 'citation_count')], by.x = 'pmid', by.y = 'pmid', all.x = T)

gen_norm_data <- function(aff_cit_m) {
  
  aff_cit_m$citation_count_n <- 0
  
  for (yr in unique(aff_cit_m$year)) {
    # 1. take each 1yrs block
    tmp <- log(subset(aff_cit_m, year == yr)$citation_count +1)
    # 2. calculate mean for the reviewed time-period
    tmp_m <- mean(tmp)
    # 3. calculate sd
    tmp_sd <- sd(tmp)
    # 4. 
    tmp_n <- (tmp-tmp_m)/tmp_sd 
    aff_cit_m[aff_cit_m$year == yr,]$citation_count_n <- tmp_n
  }
  
  return(aff_cit_m)
  
}

cog_major <- gen_norm_data(cog_major)
cog_major$pType <- 'Cognitive'




# now the same for Affective data
aff_bal_id <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_balanced_pmid.csv")$x
aff_major <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_major_UI.csv")
aff_major_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_major_UI_subjectareas.csv")
aff_cit_ref_links <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_data_cit_ref_data.csv")
aff_cit_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_cit_bal_full_data_subjectareas.csv")
aff_ref_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_ref_bal_full_data_subjectareas.csv")

# authors data 
aff_auth <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_authors.csv")


#  Form model data based on updated 2023 files
# pDiversity	cDiversity	rDiversity	cos_mRef_mCit	cos_mRef_pub	cos_mCit_pub

# get number of authors

aff_auth %>% group_by(pmid) %>% 
  summarise(total_count=n()) -> aff_auth_num

aff_major$X <- NULL
aff_major$mesh_descriptor_UI <- NULL
aff_major$mesh_qualifier_UI <- NULL
aff_major$mesh_major_descriptor_UI <- NULL
aff_major$mesh_descriptor_for_major_qualifier <- NULL
aff_major$mesh_major_UI <- NULL
aff_major$mesh_UI <- NULL
aff_major$mesh_major_qualifier_UI <- NULL
aff_major$refs <- NULL


# filter only balanced recs
aff_major <- subset(aff_major, pmid %in% aff_bal_id)
# add author number
aff_major <- merge(aff_major, aff_auth_num, by.x = "pmid", by.y = "pmid", all.x = T)
colnames(aff_major)[6] <- 'pAuthors_n'


# Subject areas
aff_major_sa$pSA1 <- aff_major_sa$A + aff_major_sa$B + aff_major_sa$G
aff_major_sa$pSA2 <- aff_major_sa$F
aff_major_sa$pSA3 <- aff_major_sa$C + aff_major_sa$N
aff_major_sa$pSA4 <- aff_major_sa$D + aff_major_sa$E + aff_major_sa$J + aff_major_sa$L
aff_major_sa$pSA5 <- aff_major_sa$H + aff_major_sa$I + aff_major_sa$K + aff_major_sa$M

aff_major <- merge(aff_major, aff_major_sa[,c('pmid', 'pSA1', 'pSA2', 'pSA3', 'pSA4', 'pSA5')], by.x = "pmid", by.y = "pmid", all.x = T)

# Cross disciplinarity
aff_major$pX_disc <- as.integer(((aff_major$pSA1>0) + (aff_major$pSA2>0) + (aff_major$pSA3>0) +
                                   (aff_major$pSA4>0) + (aff_major$pSA5>0))>1)


# Citation and references
# citations
aff_cit_link <- aff_cit_ref_links[,c('pmid', 'cited_by')] %>%
  separate_rows(cited_by, sep=" ")

aff_cit_link <- merge(aff_cit_link, aff_cit_sa, by.x = 'cited_by', py.y = 'pmid', all.x = T)

aff_cit_link$cSA1 <- aff_cit_link$A + aff_cit_link$B + aff_cit_link$G
aff_cit_link$cSA2 <- aff_cit_link$F
aff_cit_link$cSA3 <- aff_cit_link$C + aff_cit_link$N
aff_cit_link$cSA4 <- aff_cit_link$D + aff_cit_link$E + aff_cit_link$J + aff_cit_link$L
aff_cit_link$cSA5 <- aff_cit_link$H + aff_cit_link$I + aff_cit_link$K + aff_cit_link$M

aff_cit_link[is.na(aff_cit_link)] <- 0

aff_cit_link %>% group_by(pmid) %>% 
  summarise(cSA1 = sum(cSA1),
            cSA2 = sum(cSA2),
            cSA3 = sum(cSA3),
            cSA4 = sum(cSA4),
            cSA5 = sum(cSA5)) -> aff_cit

aff_major <- merge(aff_major, aff_cit, by.x = 'pmid', by.y = 'pmid', all.x = T)


# same for references
aff_ref_link <- aff_cit_ref_links[,c('pmid', 'references')] %>%
  separate_rows(references, sep=" ")

aff_ref_link <- merge(aff_ref_link, aff_ref_sa, by.x = 'references', py.y = 'pmid', all.x = T)

aff_ref_link$rSA1 <- aff_ref_link$A + aff_ref_link$B + aff_ref_link$G
aff_ref_link$rSA2 <- aff_ref_link$F
aff_ref_link$rSA3 <- aff_ref_link$C + aff_ref_link$N
aff_ref_link$rSA4 <- aff_ref_link$D + aff_ref_link$E + aff_ref_link$J + aff_ref_link$L
aff_ref_link$rSA5 <- aff_ref_link$H + aff_ref_link$I + aff_ref_link$K + aff_ref_link$M

aff_ref_link[is.na(aff_ref_link)] <- 0

aff_ref_link %>% group_by(pmid) %>% 
  summarise(rSA1 = sum(rSA1),
            rSA2 = sum(rSA2),
            rSA3 = sum(rSA3),
            rSA4 = sum(rSA4),
            rSA5 = sum(rSA5),
            reference_count = n()) -> aff_ref

aff_major <- merge(aff_major, aff_ref, by.x = 'pmid', by.y = 'pmid', all.x = T)


# citation count and normalization
aff_major <- merge(aff_major, aff_cit_ref_links[,c('pmid', 'citation_count')], by.x = 'pmid', by.y = 'pmid', all.x = T)

aff_major <- gen_norm_data(aff_major)
aff_major$pType <- 'Affective'


model_data <- union(cog_major, aff_major)





# Diversity variables

CD_coocur <- function(V){
  VxV <- outer(V, V) # outer product
  VxV[lower.tri(VxV,diag = F)] <- 0 #  keep only upper diagonal matrix
  Dp <- VxV/sum(VxV)
  f_Dp <- sum(Dp[upper.tri(VxV,diag = F)]) # why not norm!? page 6 of the Brain manuscript
  return(f_Dp)
}


model_data$pDiversity <- apply(model_data[,c("pSA1","pSA2","pSA3","pSA4","pSA5")],1,CD_coocur)
model_data$cDiversity <- apply(model_data[,c("cSA1","cSA2","cSA3","cSA4","cSA5")],1,CD_coocur)
model_data$rDiversity <- apply(model_data[,c("rSA1","rSA2","rSA3","rSA4","rSA5")],1,CD_coocur)

# Similarities

colnames(model_data)[23] <- "pReferences_n"
colnames(model_data)[24] <- "pCitations_n"
colnames(model_data)[25] <- "pCitations_n_norm"

# cos_mRef_mCit	
model_data$cos_mRef_mCit <- 0
for (n in c(1:nrow(model_data))) {
  model_data[n,"cos_mRef_mCit"] <- lsa::cosine(unlist(model_data[n,c("rSA1","rSA2","rSA3","rSA4","rSA5")])/model_data$pReferences_n[n],
                                                   unlist(model_data[n,c("cSA1","cSA2","cSA3","cSA4","cSA5")])/model_data$pCitations_n[n])
  
}

# cos_mRef_pub	
model_data$cos_mRef_pub <- 0
for (n in c(1:nrow(model_data))) {
  model_data[n,"cos_mRef_pub"] <- lsa::cosine(unlist(model_data[n,c("rSA1","rSA2","rSA3","rSA4","rSA5")])/model_data$pReferences_n[n],
                                               unlist(model_data[n,c("pSA1","pSA2","pSA3","pSA4","pSA5")]))
  
}

# cos_mCit_pub
model_data$cos_mCit_pub <- 0
for (n in c(1:nrow(model_data))) {
  model_data[n,"cos_mCit_pub"] <- lsa::cosine(unlist(model_data[n,c("cSA1","cSA2","cSA3","cSA4","cSA5")])/model_data$pCitations_n[n],
                                               unlist(model_data[n,c("pSA1","pSA2","pSA3","pSA4","pSA5")]))
  
}


# pEpoch

colnames(model_data)[2] <- 'pYear'
model_data$pAge <- max(model_data$pYear) - model_data$pYear
model_data$pEpoch <- ifelse(model_data$pYear < 1999, 'early', 
                            ifelse(model_data$pYear > 2009, 'late', 'middle'))

model_data$pEpoch5 <- 0
model_data$pEpoch5[model_data$pYear > 1950] <- 1
model_data$pEpoch5[model_data$pYear > 1955] <- 2
model_data$pEpoch5[model_data$pYear > 1960] <- 3
model_data$pEpoch5[model_data$pYear > 1965] <- 4
model_data$pEpoch5[model_data$pYear > 1970] <- 5
model_data$pEpoch5[model_data$pYear > 1975] <- 6
model_data$pEpoch5[model_data$pYear > 1980] <- 7
model_data$pEpoch5[model_data$pYear > 1985] <- 8
model_data$pEpoch5[model_data$pYear > 1990] <- 9
model_data$pEpoch5[model_data$pYear > 1995] <- 10
model_data$pEpoch5[model_data$pYear > 2000] <- 11
model_data$pEpoch5[model_data$pYear > 2005] <- 12
model_data$pEpoch5[model_data$pYear > 2010] <- 13
model_data$pEpoch5[model_data$pYear > 2015] <- 14



write.csv(model_data, "E:/Research/Data_pubmed/model_data23/model_data.csv")