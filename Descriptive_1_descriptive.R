
setwd("E:/Research/Data_pubmed/processed_data5")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

library(dplyr)
library(ggplot2)
library(scales)
object.size(myfiles)

colnames(myfiles)
myfiles$keywords <- NULL
myfiles$mesh_descriptor <- NULL
myfiles$mesh_descriptor_major <- NULL
myfiles$mesh_qualifier <- NULL
myfiles$mesh_qualifier_major <- NULL


myfiles %>% group_by(year) %>% summarise(n = n()) -> myfiles_yearly
myfiles_yearly <- subset(myfiles_yearly, year %in% c(1967:2019))

p <- ggplot(data = myfiles_yearly,
            mapping = aes(x = year, y = n))
p + geom_line() +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  labs(x = NULL, y = "Publications in PubMed DB",
       title = "Number of records per year",
       subtitle = "No filters applied, 1967-2019")


# Filter affective
colnames(myfiles)
remove(myfiles_yearly, p, files)
myfiles$mesh_major_UI <- paste(myfiles$mesh_major_descriptor_UI, myfiles$mesh_descriptor_for_major_qualifier)
myfiles$mesh_UI <- paste(myfiles$mesh_descriptor_UI, myfiles$mesh_qualifier_UI)

#myfiles$mesh_all <- paste(myfiles$mesh_descriptor, myfiles$mesh_descriptor_major,
#                          myfiles$mesh_qualifier, myfiles$mesh_qualifier_major)

# Descriptiors:
# Emotional adjustment - D000066498
# Optimism - D000067656
# Pessimism - D000067657
# affective symptoms - D000342
# aggression - D000374
# depression - D003863
# stress, psychological - D013315
# emotions - D004644
# achievement - D000124
# aspirations, psychological - D001240
# anhedonia - D059445
# empathy - D004645
# emotional intelligence - D056348
# negativism - D009341
# expressed emotion - D019260
# trust - D035502
# pain perception - D058748
# pain threshold - D017288
# reward - D012201
# anxiety disorders - D001008
# bipolar and related disorders - D000068105
# mood disorders - D019964
# trauma and stressor related disorders - D000068099
# psychology, positive - D000080032
# anger management therapy - D000067449
# emotion-focused therapy - D000071441
# arousal - D001143



#data_affective <- dplyr::filter(myfiles, grepl('emotional adjustment|optimism|pessimism|affective symptoms|aggression|depression|stress, psychological|
#                             emotions|achievement|aspirations, psychological|anhedonia|empathy|emotional intelligence|negativism|
#                             expressed emotion|trust|pain perception|pain threshold|reward|anxiety disorders|bipolar and related disorders|
#                             trauma and stressor related disorders|psychology, positive|anger management therapy|emotion-focused therapy|
#                             arousal', mesh_all, ignore.case = TRUE))


mesh_kyes <- 'D000066498|D000067656|D000067657|D000342|D000374|D003863|D013315|D004644|D000124|D001240|D059445|D004645|D056348|D009341|D019260|D035502|D058748|D017288|D012201|D001008|D000068105|D019964|D000068099|D000080032|D000067449|D000071441|D001143'

data_affective_non_major <- dplyr::filter(myfiles, grepl(mesh_kyes, mesh_UI, ignore.case = TRUE))
data_affective_major <- dplyr::filter(myfiles, grepl(mesh_kyes, mesh_major_UI, ignore.case = TRUE))

#----------------------------------------------------
library(stringr)
library(dplyr)




data_subs <- subset(data_affective_non_major, !(pmid %in% data_affective_major$pmid))


sample(data_subs$pmid, 30)
sample(data_affective_major$pmid, 30)


test <- subset(data_subs2_aff, pmid == 29988822)
subset(data_subs2_aff, pmid == 29988822)$mesh_major_qualifier_UI
subset(data_subs2_aff, pmid == 29988822)$mesh_major_UI
#----------------------------------------------------

#write.csv(data_affective_major, "E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
#write.csv(data_affective_major, "E:/Research/Data_pubmed/affective_data/affective_major_UI_plus_venue.csv")
data_affective <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")

sample(data_affective$pmid, 30)
sample(data_affective_allMesh$pmid[data_affective_allMesh$pmid %in% data_affective$pmid], 30)

# Visualize
data_affective %>% group_by(year) %>% summarise(n = n()) -> data_affective_yearly
data_affective_yearly <- subset(data_affective_yearly, year %in% c(1967:2019))
q <- ggplot(data = data_affective_yearly,
            mapping = aes(x = year, y = n))

q + geom_line() +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(x = NULL, y = "Affective publications in PubMed DB",
       title = "Number of records per year",
       subtitle = "Affective filter applied, 1967-2019")


data_affective_nkey %>% group_by(year) %>% summarise(n = n()) -> data_affective_nkey_yearly
data_affective_nkey_yearly <- subset(data_affective_nkey_yearly, year %in% c(1950:2021))
r <- ggplot(data = data_affective_nkey_yearly,
            mapping = aes(x = year, y = n))

r + geom_line() +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(x = NULL, y = "Affective publications in PubMed DB",
       title = "Number of records per year",
       subtitle = "Affective filter applied, 1950-2021")


#
write.csv(data_affective_nkey, "E:/Research/Data_pubmed/affective_data/affective_MeSH.csv")
data_affective_nkey$Cluster = "Other"


# 'Behavioral Theory' - 'psychiatry and psychology|anthropology, education, sociology, and social phenomena|humanities'


da_nk_bt <- dplyr::filter(data_affective_nkey, grepl('psychiatry and psychology|anthropology, education, sociology, and social phenomena|humanities',
              mesh_not_key, ignore.case = TRUE))

# 'Biomedical' - 'anatomy|organisms|phenomena and psychology|diseases|drugs and chemicals|health Care'
# 'Techno-Informatics' - 'technology, industry, agriculture|information Science'

