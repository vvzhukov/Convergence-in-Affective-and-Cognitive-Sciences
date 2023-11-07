library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(scales)

# IMPORTANT new coding done by colors/labels in legend, SA naming 
# in code/data is incorrect. mapping rule:
# c("SA1","SA2","SA3","SA4","SA5") <- c("SA2", "SA1", "SA4", "SA5", "SA3")
aff_cit <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_cit_ref.csv")
cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_data_cit_ref_data.csv")

data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
cog_cit_nb <- subset(cog_cit, pmid %in% data_cognitive_major_nb$pmid)
remove(data_cognitive_major_nb)

aff_cit <- aff_cit[,c("pmid","citation_count")]
cog_cit <- cog_cit[,c("pmid","citation_count")]
cog_cit_nb <- cog_cit_nb[,c("pmid","citation_count")]


aff_cit_yr <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
cog_cit_yr <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")
aff_cit_yr <- aff_cit_yr[,c("pmid","year")]
cog_cit_yr <- cog_cit_yr[,c("pmid","year")]


aff_cit_m <- merge(aff_cit, aff_cit_yr, all.x = T)
cog_cit_m <- merge(cog_cit, cog_cit_yr, all.x = T)
cog_cit_nb_m <- merge(cog_cit_nb, cog_cit_yr, all.x = T)
remove(aff_cit, aff_cit_yr, cog_cit, cog_cit_yr, cog_cit_nb)

gen_norm_data <- function(aff_cit_m) {
  
  aff_cit_m$citation_count_n <- 0
  
  for (yr in unique(aff_cit_m$year)) {
    # 1. take each 5yrs block
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


aff_cit_m <- gen_norm_data(aff_cit_m)
cog_cit_m <- gen_norm_data(cog_cit_m)
cog_cit_nb_m <- gen_norm_data(cog_cit_nb_m)

aff_cit_m <- aff_cit_m[aff_cit_m$year %in% c(1950:2019),]
cog_cit_m <- cog_cit_m[cog_cit_m$year %in% c(1950:2019),]
cog_cit_nb_m <- cog_cit_nb_m[cog_cit_nb_m$year %in% c(1950:2019),]


aff_cit_m$type <- "aff"
cog_cit_m$type <- "cog"
cog_cit_nb_m$type <- "cog, nb"

model_data <- rbind(aff_cit_m, cog_cit_m, cog_cit_nb_m)
model_data$year <- as.factor(model_data$year)


# Fit the full model 


model.minimal <- lm(citation_count_n ~ type, data = model_data)
summary(model.minimal)


model_data$year <- as.numeric(model_data$year)
model.year <- lm(citation_count_n ~ type + year, data = model_data)
summary(model.year)




