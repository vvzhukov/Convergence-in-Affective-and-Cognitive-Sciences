setwd("E:/Research/Data_pubmed/processed_data5")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(ggpubr)

colnames(myfiles)
myfiles$keywords <- NULL
myfiles$mesh_descriptor <- NULL
myfiles$mesh_descriptor_major <- NULL
myfiles$mesh_qualifier <- NULL
myfiles$mesh_qualifier_major <- NULL


# Filter cognitive
colnames(myfiles)
remove(files)
myfiles$mesh_major_UI <- paste(myfiles$mesh_major_descriptor_UI, myfiles$mesh_descriptor_for_major_qualifier)


# D008568
# D001288
# D010465
# D003657
# D011340
# D007858
# D007802
# D001185
# D003071
# D001519
# D001921


mesh_kyes <- 'D008568|D001288|D010465|D003657|D011340|D007858|D007802|D001185|D003071|D001519|D001921'
data_cognitive_major <- dplyr::filter(myfiles, grepl(mesh_kyes, mesh_major_UI, ignore.case = TRUE))
#write.csv(data_cognitive_major, "E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_words.csv")
write.csv(data_cognitive_major, "E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")
#write.csv(data_cognitive_major, "E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_plus_venue.csv")

data_cognitive_major <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")
remove(myfiles)


# Visualize

data_cognitive <- data_cognitive_major
data_cognitive %>% group_by(year) %>% summarise(n = n()) -> data_cognitive_yearly
data_cognitive_yearly <- subset(data_cognitive_yearly, year %in% c(1967:2019))
q <- ggplot(data = data_cognitive_yearly,
            mapping = aes(x = year, y = n))

q + geom_line() +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(x = NULL, y = "Cognitive publications in PubMed DB",
       title = "Number of records per year",
       subtitle = "Cognitive filter applied, 1967-2019")

# Citations

library(dplyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(stringr)

setwd("E:/Research/Data_pubmed/citations_data")

# Get the files names
files = list.files(pattern="*.json")

myData <- new.env()
idx <- as.character(length(myData) + 1)

for (file in files) {
  print(file)
  stream_in(file(file), handler = function(df){ 
    idx <<- as.character(length(myData) + 1)
    myData[[idx]] <<- df[,c(1,8,22,23)] ## filter
  }, pagesize = 10000)
}



myData2 <- myData %>% as.list() %>% rbindlist()
cognitive_cit_ref <- myData2[myData2$pmid %in% data_cognitive$pmid]
write.csv(cognitive_cit_ref, "E:/Research/Data_pubmed/cognitive_data/cognitive_data_cit_ref_data.csv")


# NAs in ref and citations
nrow(cognitive_cit_ref[!is.na(cognitive_cit_ref$cited_by),]) 
#  525004 citations
nrow(cognitive_cit_ref[!is.na(cognitive_cit_ref$references),]) 
#  519149 references
# Full records (citations + references)
nrow(cognitive_cit_ref[!is.na(cognitive_cit_ref$references) & !is.na(cognitive_cit_ref$cited_by),]) 
#   have both citations and references
length(cognitive_cit_ref[!is.na(cognitive_cit_ref$references) & !is.na(cognitive_cit_ref$cited_by),]$pmid)
write.csv(cognitive_cit_ref[!is.na(cognitive_cit_ref$references) & !is.na(cognitive_cit_ref$cited_by),]$pmid, "E:/Research/Data_pubmed/cognitive_data/cognitive_balanced_pmid.csv")



#---- PART II

cognitive_cit_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_data_cit_ref_data.csv")
data_cognitive_major <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")


data_cognitive_major$major <- paste(gsub('.{1}$', '', data_cognitive_major$mesh_major_descriptor_UI), ", ", 
                              substring(data_cognitive_major$mesh_descriptor_for_major_qualifier, 2))

data_cognitive_major$major <- str_replace_all(data_cognitive_major$major, ",  ]", "]")
data_cognitive_major$major <- str_replace_all(data_cognitive_major$major, "\\x5B ,", "[")

write.csv(data_cognitive_major, "E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")

# run descriptive.py script to add proper flags

data_cognitive_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")


#### DESCRIPTIVE
data_cognitive_subj <- subset(data_cognitive_subj,pmid %in% cognitive_cit_ref[!is.na(cognitive_cit_ref$references) & !is.na(cognitive_cit_ref$cited_by),]$pmid)

data_cognitive_subj$SA2 <- as.integer(data_cognitive_subj$A + data_cognitive_subj$B + data_cognitive_subj$G)
data_cognitive_subj$SA1 <- as.integer(data_cognitive_subj$F)
data_cognitive_subj$SA4 <- as.integer(data_cognitive_subj$C + data_cognitive_subj$N)
data_cognitive_subj$SA5 <- as.integer(data_cognitive_subj$D + data_cognitive_subj$E + data_cognitive_subj$J + data_cognitive_subj$L)
data_cognitive_subj$SA3 <- as.integer(data_cognitive_subj$H + data_cognitive_subj$I + data_cognitive_subj$K + data_cognitive_subj$M)


data_cognitive_subj$SA2b <- data_cognitive_subj$SA2 > 0
data_cognitive_subj$SA1b <- data_cognitive_subj$SA1 > 0
data_cognitive_subj$SA4b <- data_cognitive_subj$SA4 > 0
data_cognitive_subj$SA5b <- data_cognitive_subj$SA5 > 0
data_cognitive_subj$SA3b <- data_cognitive_subj$SA3 > 0 

sum(data_cognitive_subj$SA1b)
sum(data_cognitive_subj$SA2b)
sum(data_cognitive_subj$SA3b)
sum(data_cognitive_subj$SA4b)
sum(data_cognitive_subj$SA5b)


#### END DESCRIPTIVE

aff_subj_bool <- data_cognitive_subj
aff_subj_bool$A <- as.numeric(aff_subj_bool$A >0)
aff_subj_bool$B <- as.numeric(aff_subj_bool$B >0)
aff_subj_bool$C <- as.numeric(aff_subj_bool$C >0)
aff_subj_bool$D <- as.numeric(aff_subj_bool$D >0)
aff_subj_bool$E <- as.numeric(aff_subj_bool$E >0)
aff_subj_bool$F <- as.numeric(aff_subj_bool$F >0)
aff_subj_bool$G <- as.numeric(aff_subj_bool$G >0)
aff_subj_bool$H <- as.numeric(aff_subj_bool$H >0)
aff_subj_bool$I <- as.numeric(aff_subj_bool$I >0)
aff_subj_bool$J <- as.numeric(aff_subj_bool$J >0)
aff_subj_bool$K <- as.numeric(aff_subj_bool$K >0)
aff_subj_bool$L <- as.numeric(aff_subj_bool$L >0)
aff_subj_bool$M <- as.numeric(aff_subj_bool$M >0)
aff_subj_bool$N <- as.numeric(aff_subj_bool$N >0)
aff_subj_bool$V <- as.numeric(aff_subj_bool$V >0)
aff_subj_bool$Z <- as.numeric(aff_subj_bool$Z >0)

aff_subj_bool$SA1 <- as.numeric((aff_subj_bool$A + aff_subj_bool$B + aff_subj_bool$G) > 0)
aff_subj_bool$SA2 <- as.numeric((aff_subj_bool$F) >0)
aff_subj_bool$SA3 <- as.numeric((aff_subj_bool$C + aff_subj_bool$N) >0)
aff_subj_bool$SA4 <- as.numeric((aff_subj_bool$D + aff_subj_bool$E + aff_subj_bool$J + aff_subj_bool$L) >0)
aff_subj_bool$SA5 <- as.numeric((aff_subj_bool$H + aff_subj_bool$I + aff_subj_bool$K + aff_subj_bool$M) >0)

L1_plot <- ggplot(data.frame("L1" = rowSums(aff_subj_bool[,c(2:17)])), aes(x=L1)) + 
  geom_bar(color="black", fill="white") + 
  scale_x_continuous(name="Number of MeSH headings per publication", breaks=c(1:20)) +
  scale_y_continuous(limits = c(0,280000)) +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_blank()) +
  geom_vline(aes(xintercept = mean(L1)), linetype = 2, size = 2)

SA_plot <- ggplot(data.frame("SA" = rowSums(aff_subj_bool[,c(18:22)])), aes(x=SA)) + 
  geom_bar(color="black", fill="white") + 
  scale_x_continuous(name="Number of Subject Areas (SA) per publication", breaks=c(1:20)) +
  scale_y_continuous(limits = c(0,280000)) +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_blank())  +
  geom_vline(aes(xintercept = mean(SA)-0.5), linetype = 2, size = 2)

mean(rowSums(aff_subj_bool[,c(2:17)]))
mean(rowSums(aff_subj_bool[,c(18:22)]))
annotate_figure(ggarrange(L1_plot, SA_plot + rremove("y.text"), 
                          labels = c("A", "B"),
                          ncol = 2, nrow = 1),
                top = text_grob("Cognitive publications, n = 607962", 
                                size = 12))


#---- PART III


cognitive_cit_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_data_cit_ref_data.csv")
data_cognitive_major <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")


cog_cit <- unlist(sapply(cognitive_cit_ref$cited_by,function(x) strsplit(x, split=" ")),recursive = TRUE, use.names = FALSE)
cog_ref <- unlist(sapply(cognitive_cit_ref$references,function(x) strsplit(x, split=" ")),recursive = TRUE, use.names = FALSE)

unique(cog_cit)
unique(cog_ref)

setwd("E:/Research/Data_pubmed/processed_data5")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

colnames(myfiles)
myfiles$keywords <- NULL
myfiles$mesh_descriptor <- NULL
myfiles$mesh_descriptor_major <- NULL
myfiles$mesh_qualifier <- NULL
myfiles$mesh_qualifier_major <- NULL


# Filter cognitive
colnames(myfiles)
myfiles$mesh_major_UI <- paste(myfiles$mesh_major_descriptor_UI, myfiles$mesh_descriptor_for_major_qualifier)
myfiles <- myfiles[,c(1,2,10)]

cog_cit_dat <- subset(myfiles, pmid %in% cog_cit)
cog_ref_dat <- subset(myfiles, pmid %in% cog_ref)

cog_cit_dat$mesh_major_UI <- str_replace_all(cog_cit_dat$mesh_major_UI, ",  ]", "]")
cog_cit_dat$mesh_major_UI <- str_replace_all(cog_cit_dat$mesh_major_UI, "\\x5B ,", "[")
cog_cit_dat$mesh_major_UI <- str_replace_all(cog_cit_dat$mesh_major_UI, "\\x5B]", "")
cog_cit_dat$mesh_major_UI <- str_replace_all(cog_cit_dat$mesh_major_UI, "\\x5B \\x5D", ", ")
cog_cit_dat$mesh_major_UI <- str_replace_all(cog_cit_dat$mesh_major_UI, "\\x5B", "")
cog_cit_dat$mesh_major_UI <- str_replace_all(cog_cit_dat$mesh_major_UI, "\\x5D", "")
cog_cit_dat$mesh_major_UI <- str_replace_all(cog_cit_dat$mesh_major_UI, " ", "")
cog_cit_dat <- cog_cit_dat[cog_cit_dat$mesh_major_UI != "",]

cog_ref_dat$mesh_major_UI <- str_replace_all(cog_ref_dat$mesh_major_UI, ",  ]", "]")
cog_ref_dat$mesh_major_UI <- str_replace_all(cog_ref_dat$mesh_major_UI, "\\x5B ,", "[")
cog_ref_dat$mesh_major_UI <- str_replace_all(cog_ref_dat$mesh_major_UI, "\\x5B]", "")
cog_ref_dat$mesh_major_UI <- str_replace_all(cog_ref_dat$mesh_major_UI, "\\x5B \\x5D", ", ")
cog_ref_dat$mesh_major_UI <- str_replace_all(cog_ref_dat$mesh_major_UI, "\\x5B", "")
cog_ref_dat$mesh_major_UI <- str_replace_all(cog_ref_dat$mesh_major_UI, "\\x5D", "")
cog_ref_dat$mesh_major_UI <- str_replace_all(cog_ref_dat$mesh_major_UI, " ", "")
cog_ref_dat <- cog_ref_dat[cog_ref_dat$mesh_major_UI != "",]

# we had:
# 17 779 022 citations
# 4 393 517 in our data
# 3 882 581 with mesh

# 20 667 508 references
# 3 405 233 in our data
# 3 236 079 with mesh

write.csv(cog_cit_dat, "E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_data.csv")
write.csv(cog_ref_dat, "E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_data.csv")


# run descriptive.py script to add proper flags
cog_cit_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_data_subjectareas.csv")
cog_ref_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_data_subjectareas.csv")

cog_subj_bool <- cog_cit_subj
cog_subj_bool$A <- as.numeric(cog_subj_bool$A >0)
cog_subj_bool$B <- as.numeric(cog_subj_bool$B >0)
cog_subj_bool$C <- as.numeric(cog_subj_bool$C >0)
cog_subj_bool$D <- as.numeric(cog_subj_bool$D >0)
cog_subj_bool$E <- as.numeric(cog_subj_bool$E >0)
cog_subj_bool$F <- as.numeric(cog_subj_bool$F >0)
cog_subj_bool$G <- as.numeric(cog_subj_bool$G >0)
cog_subj_bool$H <- as.numeric(cog_subj_bool$H >0)
cog_subj_bool$I <- as.numeric(cog_subj_bool$I >0)
cog_subj_bool$J <- as.numeric(cog_subj_bool$J >0)
cog_subj_bool$K <- as.numeric(cog_subj_bool$K >0)
cog_subj_bool$L <- as.numeric(cog_subj_bool$L >0)
cog_subj_bool$M <- as.numeric(cog_subj_bool$M >0)
cog_subj_bool$N <- as.numeric(cog_subj_bool$N >0)
cog_subj_bool$V <- as.numeric(cog_subj_bool$V >0)
cog_subj_bool$Z <- as.numeric(cog_subj_bool$Z >0)
cog_subj_bool$SA1 <- as.numeric((cog_subj_bool$A + cog_subj_bool$B + cog_subj_bool$G) > 0)
cog_subj_bool$SA2 <- as.numeric((cog_subj_bool$F) >0)
cog_subj_bool$SA3 <- as.numeric((cog_subj_bool$C + cog_subj_bool$N) >0)
cog_subj_bool$SA4 <- as.numeric((cog_subj_bool$D + cog_subj_bool$E + cog_subj_bool$J + cog_subj_bool$L) >0)
cog_subj_bool$SA5 <- as.numeric((cog_subj_bool$H + cog_subj_bool$I + cog_subj_bool$K + cog_subj_bool$M) >0)

cog_cit_subj <- cog_subj_bool
cog_subj_bool <- cog_ref_subj
#242-262
cog_ref_subj <- cog_subj_bool

remove(cog_cit_bool, cog_ref_bool, cog_subj_bool)
cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_data.csv")
cog_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_data.csv")

cog_cit_subj <- merge(cog_cit[,c(2,3)],cog_cit_subj, by="pmid")
cog_ref_subj <- merge(cog_ref[,c(2,3)],cog_ref_subj, by="pmid")
remove(cog_cit, cog_ref)

cog_cit_subj %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                                    SA2 = sum(SA2, na.rm = T),
                                                    SA3 = sum(SA3, na.rm = T),
                                                    SA4 = sum(SA4, na.rm = T),
                                                    SA5 = sum(SA5, na.rm = T),
                                                    total = n()) -> cog_cit_summ

cog_ref_subj %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                              SA2 = sum(SA2, na.rm = T),
                                              SA3 = sum(SA3, na.rm = T),
                                              SA4 = sum(SA4, na.rm = T),
                                              SA5 = sum(SA5, na.rm = T),
                                              total = n()) -> cog_ref_summ

cog_cit_summ$total_SA_hits <- cog_cit_summ$SA1 + cog_cit_summ$SA2 + cog_cit_summ$SA3 + cog_cit_summ$SA4 + cog_cit_summ$SA5
cog_ref_summ$total_SA_hits <- cog_ref_summ$SA1 + cog_ref_summ$SA2 + cog_ref_summ$SA3 + cog_ref_summ$SA4 + cog_ref_summ$SA5


cit_plot <- ggplot(cog_cit_summ, aes(x=year)) +
  labs(y='Citations, Y') +
  geom_line(aes(y=SA1/total_SA_hits, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2/total_SA_hits, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3/total_SA_hits, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4/total_SA_hits, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5/total_SA_hits, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",       values = c("Biological sciences" = "orange", 
                                                 "Psychological sciences" = "magenta", 
                                                 "Medical sciences" = "red", 
                                                 "Technical methods" = "darkgreen", 
                                                 "Humanities"  = "darkblue"))


ref_plot <- ggplot(cog_ref_summ, aes(x=year)) +
  labs(y='References, Y') +
  geom_line(aes(y=SA1/total_SA_hits, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2/total_SA_hits, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3/total_SA_hits, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4/total_SA_hits, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5/total_SA_hits, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",       values = c("Biological sciences" = "orange", 
                                                 "Psychological sciences" = "magenta", 
                                                 "Medical sciences" = "red", 
                                                 "Technical methods" = "darkgreen", 
                                                 "Humanities"  = "darkblue"))


# get years from data_cog_major and use it for 
cognitive_cit_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_data_cit_ref_data.csv")
cognitive_cit_ref <- cognitive_cit_ref[,c(3,6)]

data_cognitive_major <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")

cognitive_cit_ref <- merge(cognitive_cit_ref,data_cognitive_major[,c(3,4)], by="pmid")

library(tidyr)
cognitive_cit_ref <- separate_rows(cognitive_cit_ref, references)
colnames(cognitive_cit_ref)[3] <- "yearP"

cognitive_cit_ref <- cognitive_cit_ref[,c(2,3)]
colnames(cognitive_cit_ref)[1] <- "pmid"

cog_cit_subj2 <- merge(cog_cit_subj, cognitive_cit_ref, by = "pmid", all.x = TRUE)
# Doubtfull!!!


write.csv(cog_cit_subj, "E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot.csv")
write.csv(cog_cit_subj2, "E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")
write.csv(cog_ref_subj, "E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_plot.csv")


cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")
cog_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_plot.csv")


cog_cit %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                              SA2 = sum(SA2, na.rm = T),
                                              SA3 = sum(SA3, na.rm = T),
                                              SA4 = sum(SA4, na.rm = T),
                                              SA5 = sum(SA5, na.rm = T),
                                              total = n()) -> cog_cit_summ

cog_cit_summ$total_SA_hits <- cog_cit_summ$SA1 + cog_cit_summ$SA2 + cog_cit_summ$SA3 + cog_cit_summ$SA4 + cog_cit_summ$SA5

cog_cit %>% group_by(yearP) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         total = n()) -> cog_cit_summ2

cog_cit_summ2$total_SA_hits <- cog_cit_summ2$SA1 + cog_cit_summ2$SA2 + cog_cit_summ2$SA3 + cog_cit_summ2$SA4 + cog_cit_summ2$SA5


cog_ref %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                              SA2 = sum(SA2, na.rm = T),
                                              SA3 = sum(SA3, na.rm = T),
                                              SA4 = sum(SA4, na.rm = T),
                                              SA5 = sum(SA5, na.rm = T),
                                              total = n()) -> cog_ref_summ

cog_ref_summ$total_SA_hits <- cog_ref_summ$SA1 + cog_ref_summ$SA2 + cog_ref_summ$SA3 + cog_ref_summ$SA4 + cog_ref_summ$SA5


ref_plot <- ggplot(cog_ref_summ, aes(x=year)) +
  labs(y='References') +
  geom_line(aes(y=SA1/total_SA_hits, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2/total_SA_hits, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3/total_SA_hits, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4/total_SA_hits, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5/total_SA_hits, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "top",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",       values = c("Biological sciences" = "orange", 
                                                 "Psychological sciences" = "magenta", 
                                                 "Medical sciences" = "red", 
                                                 "Technical methods" = "darkgreen", 
                                                 "Humanities"  = "darkblue"))



cit_plot1 <- ggplot(cog_cit_summ, aes(x=year)) +
  labs(y='Citations, Y') +
  geom_line(aes(y=SA1/total_SA_hits, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2/total_SA_hits, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3/total_SA_hits, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4/total_SA_hits, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5/total_SA_hits, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",       values = c("Biological sciences" = "orange", 
                                                 "Psychological sciences" = "magenta", 
                                                 "Medical sciences" = "red", 
                                                 "Technical methods" = "darkgreen", 
                                                 "Humanities"  = "darkblue"))

cit_plot2 <- ggplot(cog_cit_summ2, aes(x=yearP)) +
  labs(y='Citations, P') +
  geom_line(aes(y=SA1/total_SA_hits, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2/total_SA_hits, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3/total_SA_hits, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4/total_SA_hits, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5/total_SA_hits, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",       values = c("Biological sciences" = "orange", 
                                                 "Psychological sciences" = "magenta", 
                                                 "Medical sciences" = "red", 
                                                 "Technical methods" = "darkgreen", 
                                                 "Humanities"  = "darkblue"))


ggarrange(ref_plot + rremove("x.text"), cit_plot1 + rremove("x.text"), cit_plot2,
          ncol = 1, nrow = 3, heights = c(1.2,1,1))


# Top investigation
cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")
cog_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_plot.csv")

cog_cit %>% group_by(year, pmid) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                               SA2 = sum(SA2, na.rm = T),
                                               SA3 = sum(SA3, na.rm = T),
                                               SA4 = sum(SA4, na.rm = T),
                                               SA5 = sum(SA5, na.rm = T),
                                               total_cit = n()) -> cog_cit_summ
# c("SA1","SA2","SA3","SA4","SA5") <- c("SA2", "SA1", "SA4", "SA5", "SA3")

for (vyear in c(1999:2019)) {
  slice <- subset(cog_cit_summ, year == vyear)
  print(paste("Year ", vyear))
  print(head(slice[order(-slice$SA2),]$pmid,3))
  
}


# Top investigation
cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")

cog_cit %>% group_by(year, pmid) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                               SA2 = sum(SA2, na.rm = T),
                                               SA3 = sum(SA3, na.rm = T),
                                               SA4 = sum(SA4, na.rm = T),
                                               SA5 = sum(SA5, na.rm = T),
                                               total_cit = n()) -> cog_cit_summ
# c("SA1","SA2","SA3","SA4","SA5") <- c("SA2", "SA1", "SA4", "SA5", "SA3")

for (vyear in c(1999:2019)) {
  slice <- subset(cog_cit_summ, year == vyear)
  print(paste("Year ", vyear))
  print(head(slice[order(-slice$SA2),]$pmid,3))
  
}

# -- go to 2_4_categories.R, and execute top 185 lines

cit_links$SA1_ABG <- cit_links$A + cit_links$B + cit_links$G
cit_links$SA2_F <- cit_links$F
cit_links$SA3_CN <- cit_links$C + cit_links$N
cit_links$SA4_DEJL <- cit_links$D + cit_links$E + cit_links$J + cit_links$L
cit_links$SA5_HIKM <- cit_links$H + cit_links$I + cit_links$K + cit_links$M
cit_links$SA1_ABGp <- cit_links$Ap + cit_links$Bp + cit_links$Gp
cit_links$SA2_Fp <- cit_links$Fp
cit_links$SA3_CNp <- cit_links$Cp + cit_links$Np
cit_links$SA4_DEJLp <- cit_links$Dp + cit_links$Ep + cit_links$Jp + cit_links$Lp
cit_links$SA5_HIKMp <- cit_links$Hp + cit_links$Ip + cit_links$Kp + cit_links$Mp


cit_links %>% group_by(year_published, pmid) %>% summarise(SA1_ABG = sum(SA1_ABG, na.rm = T),
                                               SA2_F = sum(SA2_F, na.rm = T),
                                               SA3_CN = sum(SA3_CN, na.rm = T),
                                               SA4_DEJL = sum(SA4_DEJL, na.rm = T),
                                               SA5_HIKM = sum(SA5_HIKM, na.rm = T),
                                               total_cit = n()) -> aff_cit_summ

for (vyear in c(1999:2019)) {
  slice <- subset(aff_cit_summ, year_published == vyear)
  print(paste("Year ", vyear))
  print(head(slice[order(-slice$SA3_CN),]$pmid,3))
  
}




for (vyear in c(1999:2019)) {
  slice <- subset(cog_cit_summ, year == vyear)
  print(paste("Year ", vyear))
  print(head(slice[order(-slice$SA2),]$pmid,3))
  
}


# Biology old SA #1
for (vyear in c(1999:2019)) {
  slice <- subset(cog_cit_summ, year == vyear)
  print(paste("Year ", vyear))
  print(head(slice[order(-slice$SA1),]$pmid,3))
  
}

# Tech old SA #4
for (vyear in c(1999:2019)) {
  slice <- subset(cog_cit_summ, year == vyear)
  print(paste("Year ", vyear))
  print(head(slice[order(-slice$SA4),]$pmid,3))
  
}

for (vyear in c(1999:2019)) {
  slice <- subset(aff_cit_summ, year_published == vyear)
  print(paste("Year ", vyear))
  print(head(slice[order(-slice$SA1_ABG),]$pmid,3))
  
}

for (vyear in c(1999:2019)) {
  slice <- subset(aff_cit_summ, year_published == vyear)
  print(paste("Year ", vyear))
  print(head(slice[order(-slice$SA4_DEJL),]$pmid,3))
  
}
