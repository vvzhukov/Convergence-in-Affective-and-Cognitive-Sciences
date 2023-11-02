# Pure L1 categories
affective <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")



library(tidyverse)
library(scales) 
library(ggpubr)
library(ggplot2)

aff_subj$SA1 <- as.integer(aff_subj$A + aff_subj$B + aff_subj$G)
aff_subj$SA2 <- as.integer(aff_subj$F)
aff_subj$SA3 <- as.integer(aff_subj$C + aff_subj$N)
aff_subj$SA4 <- as.integer(aff_subj$D + aff_subj$E + aff_subj$J + aff_subj$L)
aff_subj$SA5 <- as.integer(aff_subj$H + aff_subj$I + aff_subj$K + aff_subj$M)

CD_coocur <- function(V){

  VxV <- outer(V, V) # outer product
  VxV[lower.tri(VxV,diag = F)] <- 0 #  keep only upper diagonal matrix
  Dp <- VxV/sum(VxV)
  f_Dp <- sum(Dp[upper.tri(VxV,diag = F)]) # why not norm!? page 6 of the Brain manuscript
  return(f_Dp)
}


single <- apply(aff_subj[,c(18,19,20,21,22)],1,CD_coocur)
# adding to dataframe
aff_subj <- cbind(aff_subj, SA_fDp = single)

single <- apply(aff_subj[,c(2:17)],1,CD_coocur)
# adding to dataframe
aff_subj <- cbind(aff_subj, MH_fDp = single)
aff_subj <- aff_subj[,c(1,23,24)]

colnames(affective)


aff_subj_year <- merge(x = aff_subj, y = affective[,c(2,3)], by.x = "pmid", by.y = "pmid")
aff_subj_year %>% group_by(year) %>% summarise(mSA_fDp = mean(SA_fDp, na.rm = T),
                                               mMH_fDp = mean(MH_fDp, na.rm = T),
                                               total = n()) -> aff_subj_year 

ggplot(aff_subj_year, aes(x=year)) + 
  geom_line(aes(y=mSA_fDp, color='Mean Subject Area f_Dp'),size=1) +
  #geom_line(aes(y=mMH_fDp, color='Mean MeSH Heading f_Dp'),size=1) +
  ylim(0.25, 0.45) +
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none") +
  scale_color_manual(name = "",       values = c("Mean Subject Area f_Dp" = "red", 
                                                 "Mean MeSH Heading f_Dp" = "darkblue")) +
  ggtitle("Trends in cross-topical (SA, L1 MeSH headings) publications in affectivism",) 