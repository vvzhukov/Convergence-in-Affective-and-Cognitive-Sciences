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

aff_filter <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/affective.csv")
cog_filter <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_balanced_pmid.csv")

aff_cit <- subset(aff_cit, pmid %in% aff_filter$pmid)
cog_cit <- subset(cog_cit, pmid %in% cog_filter$x)

cog_cit_nb <- subset(cog_cit, pmid %in% data_cognitive_major_nb$pmid)

remove(data_cognitive_major_nb, aff_filter, cog_filter)




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

cog_cit_nb_m$type <- "cog nb"
aff_cit_m$type <- "aff"



# SUBJECT AREAS

# add SAs, identify mono/multi and number of SAs
aff_cit_sbj <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_subjectareas.csv")
cog_cit_sbj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")


aff_cit_sbj$SA1 <- aff_cit_sbj$A + aff_cit_sbj$B + aff_cit_sbj$G
aff_cit_sbj$SA2 <- aff_cit_sbj$F
aff_cit_sbj$SA3 <- aff_cit_sbj$C + aff_cit_sbj$N
aff_cit_sbj$SA4 <- aff_cit_sbj$D + aff_cit_sbj$E + aff_cit_sbj$J + aff_cit_sbj$L
aff_cit_sbj$SA5 <- aff_cit_sbj$H + aff_cit_sbj$I + aff_cit_sbj$K + aff_cit_sbj$M

# Cross disciplinar?  
aff_cit_sbj$X_disc <- as.integer(((aff_cit_sbj$SA1>0) + (aff_cit_sbj$SA2>0) + (aff_cit_sbj$SA3>0) +
                                    (aff_cit_sbj$SA4>0) + (aff_cit_sbj$SA5>0))>1)

# Number of SAs
aff_cit_sbj$SA_num <- as.integer(((aff_cit_sbj$SA1>0) + (aff_cit_sbj$SA2>0) + (aff_cit_sbj$SA3>0) +
                                    (aff_cit_sbj$SA4>0) + (aff_cit_sbj$SA5>0)))

aff_cit_m <- merge(aff_cit_m, aff_cit_sbj[,c("pmid","X_disc","SA_num")], by.x = "pmid", by.y = "pmid", all.x  = T)

cog_cit_sbj$SA1 <- cog_cit_sbj$A + cog_cit_sbj$B + cog_cit_sbj$G
cog_cit_sbj$SA2 <- cog_cit_sbj$F
cog_cit_sbj$SA3 <- cog_cit_sbj$C + cog_cit_sbj$N
cog_cit_sbj$SA4 <- cog_cit_sbj$D + cog_cit_sbj$E + cog_cit_sbj$J + cog_cit_sbj$L
cog_cit_sbj$SA5 <- cog_cit_sbj$H + cog_cit_sbj$I + cog_cit_sbj$K + cog_cit_sbj$M

# Cross disciplinar?  
cog_cit_sbj$X_disc <- as.integer(((cog_cit_sbj$SA1>0) + (cog_cit_sbj$SA2>0) + (cog_cit_sbj$SA3>0) +
                                    (cog_cit_sbj$SA4>0) + (cog_cit_sbj$SA5>0))>1)

# Number of SAs
cog_cit_sbj$SA_num <- as.integer(((cog_cit_sbj$SA1>0) + (cog_cit_sbj$SA2>0) + (cog_cit_sbj$SA3>0) +
                                    (cog_cit_sbj$SA4>0) + (cog_cit_sbj$SA5>0)))

cog_cit_nb_m <- merge(cog_cit_nb_m, cog_cit_sbj[,c("pmid","X_disc","SA_num")], by.x = "pmid", by.y = "pmid", all.x  = T)

remove(cog_cit_sbj, aff_cit_sbj)

aff_authors <- read.csv("E:/Research/Data_pubmed/affective_data/affective_authors.csv")
cog_authors_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_authors_nobrain.csv")


aff_authors %>% group_by(pmid) %>% summarise(n_authors = n()) -> aff_authors_summ
cog_authors_nb %>% group_by(pmid) %>% summarise(n_authors = n()) -> cog_authors_nb_summ

aff_cit_m <- merge(aff_cit_m, aff_authors_summ, by = "pmid", all.x = T)
cog_cit_nb_m <- merge(cog_cit_nb_m, cog_authors_nb_summ, by = "pmid", all.x = T)



aff_cog_data <- rbind(aff_cit_m, cog_cit_nb_m)





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


aff_cog_data_norm <- gen_norm_data(aff_cog_data)
aff_cog_data_norm <- aff_cog_data_norm[aff_cog_data_norm$year %in% c(1950:2019),]






# Fit the full model 

aff_cog_data_norm$type <- as.factor(aff_cog_data_norm$type)
aff_cog_data_norm$X_disc <- as.factor(aff_cog_data_norm$X_disc)


model.minimal <- lm(citation_count_n ~ type, data = aff_cog_data_norm)
summary(model.minimal)

par(mfrow=c(2,2))
plot(model.minimal, which =1)
plot(model.minimal, which =2)
plot(model.minimal, which =3)
plot(model.minimal, which =4)


# Mono vs multi discip 
model.xdisc <- lm(citation_count_n ~ type + X_disc, data = aff_cog_data_norm)
summary(model.xdisc)

par(mfrow=c(2,2))
plot(model.xdisc, which =1)
plot(model.xdisc, which =2)
plot(model.xdisc, which =3)
plot(model.xdisc, which =4)


# Number of SAs
model.sanum <- lm(citation_count_n ~ type + SA_num, data = aff_cog_data_norm)
summary(model.sanum)

par(mfrow=c(2,2))
plot(model.sanum, which =1)
plot(model.sanum, which =2)
plot(model.sanum, which =3)
plot(model.sanum, which =4)









# Fit the full model 

aff_cog_data_norm$type <- as.factor(aff_cog_data_norm$type)
aff_cog_data_norm$X_disc <- as.factor(aff_cog_data_norm$X_disc)


model.minimal <- lm(citation_count_n ~ type + n_authors, data = aff_cog_data_norm)
summary(model.minimal)

par(mfrow=c(2,2))
plot(model.minimal, which =1)
plot(model.minimal, which =2)
plot(model.minimal, which =3)
plot(model.minimal, which =4)


# Mono vs multi discip 
model.xdisc <- lm(citation_count_n ~ type + X_disc + n_authors, data = aff_cog_data_norm)
summary(model.xdisc)

par(mfrow=c(2,2))
plot(model.xdisc, which =1)
plot(model.xdisc, which =2)
plot(model.xdisc, which =3)
plot(model.xdisc, which =4)


# Number of SAs
aff_cog_data_norm$SA_num <- as.factor(aff_cog_data_norm$SA_num)
model.sanum <- lm(citation_count_n ~ type + SA_num + n_authors, data = aff_cog_data_norm)
summary(model.sanum)

par(mfrow=c(2,2))
plot(model.sanum, which =1)
plot(model.sanum, which =2)
plot(model.sanum, which =3)
plot(model.sanum, which =4)

