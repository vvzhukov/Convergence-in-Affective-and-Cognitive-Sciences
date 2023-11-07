library(dplyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(stringr)
library(stringi)
library(superml)
library(lsa)

model_data <- read.csv("E:/Research/Data_pubmed/model_data/model1.csv")
cit_ref_data <- read.csv("E:/Research/Data_pubmed/model_data/model1_cit.csv")
cit_ref_sa_data <- read.csv("E:/Research/Data_pubmed/model_data/model1_cit_ref_sa.csv")

cit_ref_sa_data$SA1 <- as.integer(cit_ref_sa_data$F)
cit_ref_sa_data$SA2 <- as.integer(cit_ref_sa_data$A + cit_ref_sa_data$B + cit_ref_sa_data$G)
cit_ref_sa_data$SA4 <- as.integer(cit_ref_sa_data$C + cit_ref_sa_data$N)
cit_ref_sa_data$SA5 <- as.integer(cit_ref_sa_data$D + cit_ref_sa_data$E + cit_ref_sa_data$J + cit_ref_sa_data$L)
cit_ref_sa_data$SA3 <- as.integer(cit_ref_sa_data$H + cit_ref_sa_data$I + cit_ref_sa_data$K + cit_ref_sa_data$M)
cit_ref_sa_data[,c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","V","Z")] <- NULL
rownames(cit_ref_sa_data) <- NULL
cit_ref_sa_data$X <- NULL

cit_ref_data$cSA1 <- 0
cit_ref_data$cSA2 <- 0
cit_ref_data$cSA3 <- 0
cit_ref_data$cSA4 <- 0
cit_ref_data$cSA5 <- 0

cit_ref_data$rSA1 <- 0
cit_ref_data$rSA2 <- 0
cit_ref_data$rSA3 <- 0
cit_ref_data$rSA4 <- 0
cit_ref_data$rSA5 <- 0


for (row in c(323106:nrow(cit_ref_data))){
  ids <- as.numeric(unlist(strsplit(cit_ref_data[row,c("references")], " ")))
  refs <- colSums(cit_ref_sa_data[cit_ref_sa_data$pmid %in% ids,][,c("SA1","SA2","SA3","SA4","SA5")])
  cit_ref_data[row,c("rSA1","rSA2","rSA3","rSA4","rSA5")] <- refs
  
  ids <- as.numeric(unlist(strsplit(cit_ref_data[row,c("cited_by")], " ")))
  cits <- colSums(cit_ref_sa_data[cit_ref_sa_data$pmid %in% ids,][,c("SA1","SA2","SA3","SA4","SA5")])
  cit_ref_data[row,c("cSA1","cSA2","cSA3","cSA4","cSA5")] <- cits
  
  if(row%%1000==0){
    print(paste("Processed,", row))
  }
}

#write.csv(cit_ref_data, "E:/Research/Data_pubmed/model_data/model1_cit_part.csv")
cit_ref_data <- read.csv("E:/Research/Data_pubmed/model_data/model1_cit_part.csv")




# Model on the data subset

model_data[,c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","V","Z")] <- NULL
model_data_sub <- merge(cit_ref_data, model_data,  by.x = "pmid", by.y = "pmid", all.x=T)

model_data_sub$X.x <- NULL
model_data_sub$X.y <- NULL
model_data_sub$citation_count.y <- NULL


CD_coocur <- function(V){
  VxV <- outer(V, V) # outer product
  VxV[lower.tri(VxV,diag = F)] <- 0 #  keep only upper diagonal matrix
  Dp <- VxV/sum(VxV)
  f_Dp <- sum(Dp[upper.tri(VxV,diag = F)]) # why not norm!? page 6 of the Brain manuscript
  return(f_Dp)
}


model_data_sub$diversity <- apply(model_data_sub[,c("SA1","SA2","SA3","SA4","SA5")],1,CD_coocur)

model_data_sub$dot_C_R <- 0
model_data_sub$references_count <- str_count(model_data_sub$references, "\\S+")

model_data_sub$X.1 <- NULL
colnames(model_data_sub) <- c("pmid", "pCitations_n",  "pCited_by", "pReferences", 
                             "cSA1", "cSA2", "cSA3", "cSA4" , "cSA5", 
                             "rSA1", "rSA2", "rSA3", "rSA4", "rSA5" , 
                             "pYear", "pType",  "pX_disc", "pSA_num",        
                             "pAuthors_n", "pCitation_count_norm",  
                             "pSA1", "pSA2", "pSA3", "pSA4", "pSA5", 
                             "pDiversity", "pReferences_n")

model_data_sub$cos_mRef_mCit <- 0
model_data_sub$cDiversity <- apply(model_data_sub[,c("cSA1","cSA2","cSA3","cSA4","cSA5")],1,CD_coocur)
model_data_sub$rDiversity <- apply(model_data_sub[,c("rSA1","rSA2","rSA3","rSA4","rSA5")],1,CD_coocur)
model_data_sub$cSA_num <- (model_data_sub$cSA1>0)+(model_data_sub$cSA2>0)+(model_data_sub$cSA3>0)+(model_data_sub$cSA4>0)+(model_data_sub$cSA5>0)
model_data_sub$rSA_num <- (model_data_sub$rSA1>0)+(model_data_sub$rSA2>0)+(model_data_sub$rSA3>0)+(model_data_sub$rSA4>0)+(model_data_sub$rSA5>0)

# mapply or other apply will work as well
for (n in c(1:nrow(model_data_sub))) {
  model_data_sub[n,"cos_mRef_mCit"] <- lsa::cosine(unlist(model_data_sub[n,c("rSA1","rSA2","rSA3","rSA4","rSA5")])/model_data_sub$pReferences_n[n],
                                                   unlist(model_data_sub[n,c("cSA1","cSA2","cSA3","cSA4","cSA5")])/model_data_sub$pCitations_n[n])
  
}


col_order <- c("pmid", "pYear", "pType",  "pX_disc", "pAuthors_n",
               "pCitations_n",  "pCitation_count_norm",  "pCited_by",  
               "pReferences_n", "pReferences",
               "pSA_num", "cSA_num", "rSA_num",
               "pDiversity", "cDiversity", "rDiversity",
               "pSA1", "pSA2", "pSA3", "pSA4", "pSA5",
               "cSA1", "cSA2", "cSA3", "cSA4" , "cSA5", 
               "rSA1", "rSA2", "rSA3", "rSA4", "rSA5" , 
               "cos_mRef_mCit"        
               )

model_data_sub <- model_data_sub[, col_order]

write.csv(model_data_sub, "E:/Research/Data_pubmed/model_data/model_data3.csv")
model_data_sub <- read.csv("E:/Research/Data_pubmed/model_data/model_data3.csv")


