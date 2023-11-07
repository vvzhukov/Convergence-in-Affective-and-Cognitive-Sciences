# Pure L1 categories
affective <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/affective.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

cognitive <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")
cog_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")


data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
cog_subj_nb <- subset(cog_subj, pmid %in% data_cognitive_major_nb$pmid)

library(tidyverse)
library(scales) 
library(ggpubr)
library(ggplot2)

aff_subj$SA2 <- as.integer(aff_subj$A + aff_subj$B + aff_subj$G)
aff_subj$SA1 <- as.integer(aff_subj$F)
aff_subj$SA4 <- as.integer(aff_subj$C + aff_subj$N)
aff_subj$SA5 <- as.integer(aff_subj$D + aff_subj$E + aff_subj$J + aff_subj$L)
aff_subj$SA3 <- as.integer(aff_subj$H + aff_subj$I + aff_subj$K + aff_subj$M)

cog_subj$SA1 <- as.integer(cog_subj$F) #SA2
cog_subj$SA2 <- as.integer(cog_subj$A + cog_subj$B + cog_subj$G) #SA1
cog_subj$SA3 <- as.integer(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M) #SA5
cog_subj$SA4 <- as.integer(cog_subj$C + cog_subj$N) #SA3
cog_subj$SA5 <- as.integer(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L) #SA4

cog_subj_nb$SA1 <- as.integer(cog_subj_nb$F) #SA2
cog_subj_nb$SA2 <- as.integer(cog_subj_nb$A + cog_subj_nb$B + cog_subj_nb$G) #SA1
cog_subj_nb$SA3 <- as.integer(cog_subj_nb$H + cog_subj_nb$I + cog_subj_nb$K + cog_subj_nb$M) #SA5
cog_subj_nb$SA4 <- as.integer(cog_subj_nb$C + cog_subj_nb$N) #SA3
cog_subj_nb$SA5 <- as.integer(cog_subj_nb$D + cog_subj_nb$E + cog_subj_nb$J + cog_subj_nb$L) #SA4


CD_coocur <- function(V){
  
  VxV <- outer(V, V) # outer product
  VxV[lower.tri(VxV,diag = F)] <- 0 #  keep only upper diagonal matrix
  Dp <- VxV/sum(VxV)
  f_Dp <- sum(Dp[upper.tri(VxV,diag = F)]) # why not norm!? page 6 of the Brain manuscript
  return(f_Dp)
}


single <- apply(aff_subj[,c("SA1","SA2","SA3","SA4","SA5")],1,CD_coocur)
# adding to dataframe
aff_subj <- cbind(aff_subj, SA_fDp = single)


single <- apply(aff_subj[,c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")],1,CD_coocur)
# adding to dataframe
aff_subj <- cbind(aff_subj, MH_fDp = single)
aff_subj <- aff_subj[,c("pmid", "SA_fDp", "MH_fDp")]


single <- apply(cog_subj[,c("SA1","SA2","SA3","SA4","SA5")],1,CD_coocur)
# adding to dataframe
cog_subj <- cbind(cog_subj, SA_fDp = single)

single <- apply(cog_subj[,c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")],1,CD_coocur)
# adding to dataframe
cog_subj <- cbind(cog_subj, MH_fDp = single)
cog_subj <- cog_subj[,c("pmid", "SA_fDp", "MH_fDp")]



single <- apply(cog_subj_nb[,c("SA1","SA2","SA3","SA4","SA5")],1,CD_coocur)
# adding to dataframe
cog_subj_nb <- cbind(cog_subj_nb, SA_fDp = single)

single <- apply(cog_subj_nb[,c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")],1,CD_coocur)
# adding to dataframe
cog_subj_nb <- cbind(cog_subj_nb, MH_fDp = single)
cog_subj_nb <- cog_subj_nb[,c("pmid", "SA_fDp", "MH_fDp")]


aff_subj_year <- merge(x = aff_subj, y = affective[,c("pmid", "year")], by.x = "pmid", by.y = "pmid")
aff_subj_year %>% group_by(year) %>% summarise(mSA_fDp = mean(SA_fDp, na.rm = T),
                                               mMH_fDp = mean(MH_fDp, na.rm = T),
                                               total = n()) -> aff_subj_year 

cog_subj_year <- merge(x = cog_subj, y = cognitive[,c("pmid", "year")], by.x = "pmid", by.y = "pmid")
cog_subj_year %>% group_by(year) %>% summarise(mSA_fDp = mean(SA_fDp, na.rm = T),
                                               mMH_fDp = mean(MH_fDp, na.rm = T),
                                               total = n()) -> cog_subj_year 

cog_subj_year_nb <- merge(x = cog_subj_nb, y = cognitive[,c("pmid", "year")], by.x = "pmid", by.y = "pmid")
cog_subj_year_nb %>% group_by(year) %>% summarise(mSA_fDp = mean(SA_fDp, na.rm = T),
                                               mMH_fDp = mean(MH_fDp, na.rm = T),
                                               total = n()) -> cog_subj_year_nb 


colnames(aff_subj_year) <- c("year","aff_mSA_fDp","aff_mMH_fDp","aff_total")
colnames(cog_subj_year) <- c("year","cog_mSA_fDp","cog_mMH_fDp","cog_total")
colnames(cog_subj_year_nb) <- c("year","cog_mSA_fDp_nb","cog_mMH_fDp_nb","cog_total_nb")
cog_subj_year <- subset(cog_subj_year, cog_subj_year$year != 1946)
cog_subj_year_nb <- subset(cog_subj_year_nb, cog_subj_year_nb$year != 1946)
subj_year <- merge(aff_subj_year, cog_subj_year, by.x = "year", by.y = "year")
subj_year <- merge(subj_year, cog_subj_year_nb, by.x = "year", by.y = "year")
  
png('C:/Users/Rubinzone/Desktop/F7.png', width = 8, height = 8, units = 'in', res = 300)

ggplot(subj_year, aes(x=year)) + 
  geom_line(aes(y=aff_mSA_fDp, color='Affective'),size=2)  + 
  geom_line(aes(y=cog_mSA_fDp, color='Cognitive'),size=2) +
  geom_line(aes(y=cog_mSA_fDp_nb, color='Cognitive, no brain key'),size=2) +
  ylim(0.25, 0.45) +
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.text = element_text(size=rel(1.2),family="NimbusMon"),
        legend.position = "bottom") +
  scale_color_manual(name = "",       values = c("Affective" = "#949494", 
                                                 "Cognitive" = "black",
                                                 "Cognitive, no brain key" = "purple")) +
  ggtitle("Topical Diversity Index",) 

dev.off()
