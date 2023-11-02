setwd("E:/Research/Data_pubmed/processed_data_authors2")

# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
summary(myfiles$affiliationinfo)


myfiles2 <- subset(myfiles, !is.na(affiliationinfo))
myfiles2$collectivename <- NULL

write.csv(myfiles2, "E:/Research/Data_pubmed/processed_data_authors2/all_authors.csv")

# add ID to each author
all_authors$uid <- c(1:nrow(all_authors))

write.csv(all_authors, "E:/Research/Data_pubmed/processed_data_authors2/all_authors.csv")
#----------

all_authors <- read.csv("E:/Research/Data_pubmed/processed_data_authors2/all_authors.csv")



# SAMPLE #1

set.seed(133)
library(dplyr)
write.csv(sample_n(all_authors, 1000)[,c("uid", "affiliationinfo")], 
          "E:/Research/Data_pubmed/processed_data_authors2/authors_sample1000.csv", 
          row.names=FALSE)



# Filter Cognitive & Affective
model_data <- read.csv("E:/Research/Data_pubmed/model_data23/model_data.csv")
affective_authors <- subset(all_authors, pmid %in% subset(model_data, pType == "Affective")$pmid)
write.csv(affective_authors, "E:/Research/Data_pubmed/processed_data_authors2/affective_authors.csv")
cognitive_authors <- subset(all_authors, pmid %in% subset(model_data, pType == "Cognitive")$pmid)
write.csv(cognitive_authors, "E:/Research/Data_pubmed/processed_data_authors2/cognitive_authors.csv")


affective_authors <- read.csv("E:/Research/Data_pubmed/processed_data_authors2/affective_authors.csv")
cognitive_authors <- read.csv("E:/Research/Data_pubmed/processed_data_authors2/cognitive_authors.csv")


#affective_authors <- subset(affective_authors, !is.na(affiliationinfo))
#cognitive_authors <- subset(cognitive_authors, !is.na(affiliationinfo))

unique(affective_authors[,c("uid","affiliationinfo")])

affective_u_aff <- data.frame("aid" = c(1:length(unique(affective_authors$affiliationinfo))), "Affiliation" = substr(unique(affective_authors$affiliationinfo),1,50))
cognitive_u_aff <- data.frame("aid" = c(1:length(unique(cognitive_authors$affiliationinfo))), "Affiliation" = substr(unique(cognitive_authors$affiliationinfo),1,50))

write.csv(affective_u_aff, "E:/Research/Data_pubmed/processed_data_authors2/affective_authors_u_aff.csv",row.names = FALSE)
write.csv(cognitive_u_aff, "E:/Research/Data_pubmed/processed_data_authors2/cognitive_authors_u_aff.csv",row.names = FALSE)


#affective_u_aff <- read.csv("E:/Research/Data_pubmed/processed_data_authors2/affective_authors_u_aff.csv")


aff_authors_lbld1 <- read.csv("E:/Research/Data_pubmed/processed_data_authors2/affective_authors_u_aff_labeled_0_1000.csv")
aff_authors_lbld2 <- read.csv("E:/Research/Data_pubmed/processed_data_authors2/affective_authors_u_aff_labeled_1000_10000.csv")


aff_authors_lbld1 <- subset(aff_authors_lbld1, aff_authors_lbld1$Category != "")
aff_authors_lbld2 <- subset(aff_authors_lbld2, aff_authors_lbld2$Category != "")

aff_authors_lbld1$X <- NULL
aff_authors_lbld <- rbind(aff_authors_lbld1, aff_authors_lbld2)

library(data.table)
library(dplyr)


aff_authors_lbld[grepl("Biology", aff_authors_lbld$Category),]$Category <- "Biology"

aff_authors_lbld[grepl("Psychology", aff_authors_lbld$Category),]$Category <- "Psychology"
aff_authors_lbld[grepl("Psychiatry", aff_authors_lbld$Category),]$Category <- "Psychology"
aff_authors_lbld[grepl("Sociology", aff_authors_lbld$Category),]$Category <- "Psychology"
aff_authors_lbld[grepl("Education", aff_authors_lbld$Category),]$Category <- "Psychology"
aff_authors_lbld[grepl("Behavioral Sciences", aff_authors_lbld$Category),]$Category <- "Psychology"
aff_authors_lbld[grepl("Stress and Anxiety Disorders", aff_authors_lbld$Category),]$Category <- "Psychology"

aff_authors_lbld[grepl("Biotechnology&Genetics", aff_authors_lbld$Category),]$Category <- "Biotechnology&Genetics"
aff_authors_lbld[grepl("Biotechnology", aff_authors_lbld$Category),]$Category <- "Biotechnology&Genetics"
aff_authors_lbld[grepl("Genetics", aff_authors_lbld$Category),]$Category <- "Biotechnology&Genetics"

aff_authors_lbld[grepl("Medical", aff_authors_lbld$Category),]$Category <- "Medical"
aff_authors_lbld[grepl("Physiology", aff_authors_lbld$Category),]$Category <- "Medical"
aff_authors_lbld[grepl("Nursing", aff_authors_lbld$Category),]$Category <- "Medical"
aff_authors_lbld[grepl("Pediatrics", aff_authors_lbld$Category),]$Category <- "Medical"
aff_authors_lbld[grepl("Medicine", aff_authors_lbld$Category),]$Category <- "Medical"
aff_authors_lbld[grepl("Epidemiology", aff_authors_lbld$Category),]$Category <- "Medical"
aff_authors_lbld[grepl("Obstetrics and Gynecology", aff_authors_lbld$Category),]$Category <- "Medical"
aff_authors_lbld[grepl("Social Work", aff_authors_lbld$Category),]$Category <- "Medical"
aff_authors_lbld[grepl("Cardiology", aff_authors_lbld$Category),]$Category <- "Medical"

aff_authors_lbld[grepl("Health Sciences", aff_authors_lbld$Category),]$Category <- "Health Sciences"
aff_authors_lbld[grepl("Health", aff_authors_lbld$Category),]$Category <- "Health Sciences"

aff_authors_lbld[grepl("Pathology&Pharmacology", aff_authors_lbld$Category),]$Category <- "Pathology&Pharmacology"
aff_authors_lbld[grepl("Pharmacology", aff_authors_lbld$Category),]$Category <- "Pathology&Pharmacology"
aff_authors_lbld[grepl("Pathology", aff_authors_lbld$Category),]$Category <- "Pathology&Pharmacology"

aff_authors_lbld[grepl("Neuroscience", aff_authors_lbld$Category),]$Category <- "Neuroscience"
aff_authors_lbld[grepl("Neurology", aff_authors_lbld$Category),]$Category <- "Neuroscience"
aff_authors_lbld[grepl("Neuropsychiatry", aff_authors_lbld$Category),]$Category <- "Neuroscience"

aff_authors_lbld[grepl("Engineering&Informatics", aff_authors_lbld$Category),]$Category <- "Engineering&Informatics"
aff_authors_lbld[grepl("Engineering", aff_authors_lbld$Category),]$Category <- "Engineering&Informatics"
aff_authors_lbld[grepl("Informatics", aff_authors_lbld$Category),]$Category <- "Engineering&Informatics"

aff_authors_lbld[grepl("Chemistry&Physics&Math", aff_authors_lbld$Category),]$Category <- "Chemistry&Physics&Math"
aff_authors_lbld[grepl("Chemistry", aff_authors_lbld$Category),]$Category <- "Chemistry&Physics&Math"
aff_authors_lbld[grepl("Physics", aff_authors_lbld$Category),]$Category <- "Chemistry&Physics&Math"
aff_authors_lbld[grepl("Math", aff_authors_lbld$Category),]$Category <- "Chemistry&Physics&Math"



aff_authors_lbld %>% 
  count(Category) %>%
  filter(n>2) %>% 
  filter(Category!='N/A') %>% 
  arrange(desc(n)) -> df

df <- df %>%
  arrange(desc(n)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
head(df, 10)


library(ggplot2)

ggplot(df, aes(x = "", y = n, fill = Category)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  theme_void()






library(data.table)
library(dplyr)


setwd("E:/Research/Data_pubmed/processed_data_authors2/affictive_lbld")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind


authors = do.call(rbind, lapply(files, function(x) fread(x, skip = "Id, Category", fill=TRUE, sep=",")))

# Categories:   Biology, Psychology, Biotechnology&Genetics, 
#               Medical, Health Sciences, Pathology&Pharmacology, 
#               Neuroscience, Engineering&Informatics, Chemistry&Physics&Math

authors[grepl("1", authors$Category),]$Category <- "Biology"
authors[grepl("Biology", authors$Category),]$Category <- "Biology"
authors[grepl("Biostatistics", authors$Category),]$Category <- "Biology"
authors[grepl("Anatomy", authors$Category),]$Category <- "Biology"
authors[grepl("Bioethics", authors$Category),]$Category <- "Biology" #?
authors[grepl("Zoology", authors$Category),]$Category <- "Biology"
authors[grepl("Biological Sciences", authors$Category),]$Category <- "Biology"
authors[grepl("Biomedicine", authors$Category),]$Category <- "Biology"
authors[grepl("Biomedical", authors$Category),]$Category <- "Biology"
authors[grepl("Microbiology", authors$Category),]$Category <- "Biology"
authors[grepl("Animal Science", authors$Category),]$Category <- "Biology"
authors[grepl("Biosciences", authors$Category),]$Category <- "Biology"
authors[grepl("Aging", authors$Category),]$Category <- "Biology"
authors[grepl("Biochemistry", authors$Category),]$Category <- "Biology"

authors[grepl("2", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychology", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychiatry", authors$Category),]$Category <- "Psychology"
authors[grepl("Education", authors$Category),]$Category <- "Psychology"
authors[grepl("Behavioral Sciences", authors$Category),]$Category <- "Psychology"
authors[grepl("Behavioral Science", authors$Category),]$Category <- "Psychology"
authors[grepl("Stress and Anxiety Disorders", authors$Category),]$Category <- "Psychology"
authors[grepl("Suicide Research", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychobiology", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychological Sciences", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychotherapy", authors$Category),]$Category <- "Psychology"
authors[grepl("Child Development", authors$Category),]$Category <- "Psychology"
authors[grepl("Cognitive Science", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychopathology", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychological and Brain Sciences", authors$Category),]$Category <- "Psychology"
authors[grepl("Behavioural Sciences", authors$Category),]$Category <- "Psychology"
authors[grepl("Behavioural Science", authors$Category),]$Category <- "Psychology"
authors[grepl("Family Studies", authors$Category),]$Category <- "Psychology" #?
authors[grepl("Family Practice", authors$Category),]$Category <- "Psychology" #?
authors[grepl("Social Work", authors$Category),]$Category <- NA #?
authors[grepl("Eating Disorders", authors$Category),]$Category <- "Psychology"
authors[grepl("Addictions", authors$Category),]$Category <- "Psychology"
authors[grepl("Substance Abuse", authors$Category),]$Category <- "Psychology"
authors[grepl("Addictions", authors$Category),]$Category <- "Psychology"
authors[grepl("Addiction", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychophysiology", authors$Category),]$Category <- "Psychology"
authors[grepl("Cognition", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychiatric", authors$Category),]$Category <- "Psychology"
authors[grepl("Counseling", authors$Category),]$Category <- "Psychology"

authors[grepl("4", authors$Category),]$Category <- "Medical"
authors[grepl("Medical", authors$Category),]$Category <- "Medical"
authors[grepl("Physiology", authors$Category),]$Category <- "Medical"
authors[grepl("Nursing", authors$Category),]$Category <- "Medical"
authors[grepl("Pediatrics", authors$Category),]$Category <- "Medical"
authors[grepl("Medicine", authors$Category),]$Category <- "Medical"
authors[grepl("Obstetrics and Gynecology", authors$Category),]$Category <- "Medical"
authors[grepl("Cardiology", authors$Category),]$Category <- "Medical"
authors[grepl("Speech and Hearing Sciences", authors$Category),]$Category <- "Medical"
authors[grepl("Rheumatology", authors$Category),]$Category <- "Medical"
authors[grepl("Anesthesiology", authors$Category),]$Category <- "Medical"
authors[grepl("Nutrition", authors$Category),]$Category <- "Medical"
authors[grepl("Radiology", authors$Category),]$Category <- "Medical"
authors[grepl("Ophthalmology", authors$Category),]$Category <- "Medical"
authors[grepl("Oncology", authors$Category),]$Category <- "Medical"
authors[grepl("Dermatology", authors$Category),]$Category <- "Medical"
authors[grepl("Dentistry", authors$Category),]$Category <- "Medical"
authors[grepl("Obstetrics&Gynecology", authors$Category),]$Category <- "Medical"
authors[grepl("General Practice", authors$Category),]$Category <- "Medical"
authors[grepl("Physical Therapy", authors$Category),]$Category <- "Medical"
authors[grepl("Biomedical Sciences", authors$Category),]$Category <- "Medical"
authors[grepl("Surgery", authors$Category),]$Category <- "Medical"
authors[grepl("Endocrinology", authors$Category),]$Category <- "Medical"
authors[grepl("Veterinary", authors$Category),]$Category <- "Medical"
authors[grepl("Gerontology", authors$Category),]$Category <- "Medical"
authors[grepl("Geriatrics", authors$Category),]$Category <- "Medical"
authors[grepl("Gastroenterology", authors$Category),]$Category <- "Medical"
authors[grepl("Urology", authors$Category),]$Category <- "Medical"
authors[grepl("Obstetrics", authors$Category),]$Category <- "Medical"
authors[grepl("Immunology", authors$Category),]$Category <- "Medical"
authors[grepl("Primary Care", authors$Category),]$Category <- "Medical"
authors[grepl("Midwifery", authors$Category),]$Category <- "Medical" #?
authors[grepl("Clinical Sciences", authors$Category),]$Category <- "Medical"
authors[grepl("Paediatrics", authors$Category),]$Category <- "Medical"
authors[grepl("Physiotherapy", authors$Category),]$Category <- "Medical"
authors[grepl("Infectious Diseases", authors$Category),]$Category <- "Medical"
authors[grepl("Palliative Care", authors$Category),]$Category <- "Medical"
authors[grepl("Nephrology", authors$Category),]$Category <- "Medical"
authors[grepl("Otolaryngology", authors$Category),]$Category <- "Medical"
authors[grepl("Anesthesia", authors$Category),]$Category <- "Medical"
authors[grepl("Toxicology", authors$Category),]$Category <- "Medical"
authors[grepl("Otorhinolaryngology", authors$Category),]$Category <- "Medical"
authors[grepl("Anaesthesiology", authors$Category),]$Category <- "Medical"
authors[grepl("Anaesthesia", authors$Category),]$Category <- "Medical"
authors[grepl("Cancer", authors$Category),]$Category <- "Medical"
authors[grepl("Neonatology", authors$Category),]$Category <- "Medical"
authors[grepl("Pathophysiology", authors$Category),]$Category <- "Medical"
authors[grepl("Orthopedics", authors$Category),]$Category <- "Medical"
authors[grepl("Cardiovascular", authors$Category),]$Category <- "Medical"
authors[grepl("Acupuncture", authors$Category),]$Category <- "Medical"
authors[grepl("Clinical", authors$Category),]$Category <- "Medical"
authors[grepl("Orthopaedics", authors$Category),]$Category <- "Medical"
authors[grepl("Gynecology", authors$Category),]$Category <- "Medical"

authors[grepl("5", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Health Sciences", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Health", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Rehabilitation", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Veterans Administration", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Kinesiology", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Social Welfare", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Human Development", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Occupational Therapy", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Veterans Affairs", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Human Science", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Exercise Science", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Social Service", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Epidemiology", authors$Category),]$Category <- "Health Sciences"

authors[grepl("6", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pathology&Pharmacology", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pharmacology", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pathology", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pharmacy", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Psychopharmacology", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pharmaceutical Sciences", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pharmaceutics", authors$Category),]$Category <- "Pathology&Pharmacology"

authors[grepl("7", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuroscience", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuropsychiatry", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuropsychology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurobiology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurosurgery", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurophysiology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuroimaging", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuroendocrinology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuropharmacology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuroradiology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurochemistry", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuropsychopharmacology", authors$Category),]$Category <- "Neuroscience"

authors[grepl("8", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Engineering&Informatics", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Engineering", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Informatics", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Computer Science", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Environmental Sciences", authors$Category),]$Category <- "Engineering&Informatics" #?
authors[grepl("Electronics", authors$Category),]$Category <- "Engineering&Informatics"

authors[grepl("9", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Chemistry&Physics&Math", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Chemistry", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Physics", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Math", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Statistics", authors$Category),]$Category <- "Chemistry&Physics&Math"

authors[grepl("Management", authors$Category),]$Category <- "Humanities"
authors[grepl("Philosophy", authors$Category),]$Category <- "Humanities"
authors[grepl("Research", authors$Category),]$Category <- "Humanities"
authors[grepl("Business", authors$Category),]$Category <- "Humanities"
authors[grepl("Economics", authors$Category),]$Category <- "Humanities"
authors[grepl("Social Change", authors$Category),]$Category <- "Humanities"
authors[grepl("Social Sciences", authors$Category),]$Category <- "Humanities"
authors[grepl("Social Science", authors$Category),]$Category <- "Humanities"
authors[grepl("Life Sciences", authors$Category),]$Category <- "Humanities"
authors[grepl("Life Science", authors$Category),]$Category <- "Humanities"
authors[grepl("Marketing", authors$Category),]$Category <- "Humanities"
authors[grepl("Humanities", authors$Category),]$Category <- "Humanities"
authors[grepl("Geography", authors$Category),]$Category <- "Humanities"
authors[grepl("Law", authors$Category),]$Category <- "Humanities"
authors[grepl("Criminal Justice", authors$Category),]$Category <- "Humanities"
authors[grepl("Political Science", authors$Category),]$Category <- "Humanities"
authors[grepl("Criminology", authors$Category),]$Category <- "Humanities"
authors[grepl("Agriculture", authors$Category),]$Category <- "Humanities"
authors[grepl("Public Policy", authors$Category),]$Category <- "Humanities"
authors[grepl("Sports", authors$Category),]$Category <- "Humanities"
authors[grepl("Public Administration", authors$Category),]$Category <- "Humanities"
authors[grepl("None", authors$Category),]$Category <- "Humanities"
authors[grepl("Music", authors$Category),]$Category <- "Humanities"
authors[grepl("Sociology", authors$Category),]$Category <- "Humanities"
authors[grepl("Linguistics", authors$Category),]$Category <- "Humanities"
authors[grepl("Anthropology", authors$Category),]$Category <- "Humanities"
authors[grepl("Communication", authors$Category),]$Category <- "Humanities"

authors %>% 
  count(Category) %>%
  filter(n>100) %>% 
  filter(Category!='N/A' & Category!='' & Category!='b' & Category!='0' & Category!='d' & Category!='e') %>% 
  arrange(desc(n)) -> df



df <- df %>%
  arrange(desc(n)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
head(df, 10)


library(ggplot2)

ggplot(df, aes(x = "", y = n, fill = Category)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  theme_void()










# Categories:   Biology - 1 , Psychology - 2, Biotechnology&Genetics - 3, 
#               Medical - 4, Health Sciences - 5, Pathology&Pharmacology - 6, 
#               Neuroscience - 7, Engineering&Informatics - 8, Chemistry&Physics&Math - 9
#               Humanities - 0

affective_authors$aff_key <- substr(affective_authors$affiliationinfo,1,50)

authors <- authors[!is.na(authors$Id),]
authors <- authors[!is.na(authors$Category),]

affective_authors$X.2 <- NULL
affective_authors$X.1 <- NULL
affective_authors$X <- NULL

authors$Id <- as.integer(authors$Id)

authors <- authors[authors$Id <= nrow(authors),]

affective_authors_lbld <- merge(affective_u_aff, authors, by.x = 'aid', by.y = 'Id', all.x = T)


affective_authors_lbld$aid <- NULL
affective_authors_lbld <- affective_authors_lbld[!duplicated(affective_authors_lbld),]

affective_authors_lbld %>%
  group_by(Affiliation) %>%
  summarise(Category = Category[[1]]) ->
affective_authors_lbldu

affective_authors_merged <- merge(affective_authors[,c("pmid","aff_key")], 
                                  affective_authors_lbldu[,c("Affiliation","Category")], 
                                  by.x = 'aff_key', by.y = 'Affiliation', all.x = T, all.y = F)


affective_authors_merged$CIP0 <- ifelse(affective_authors_merged$Category == "Humanities", 1, 0)
affective_authors_merged$CIP1 <- ifelse(affective_authors_merged$Category == "Biology", 1, 0)
affective_authors_merged$CIP2 <- ifelse(affective_authors_merged$Category == "Psychology", 1, 0)
affective_authors_merged$CIP3 <- ifelse(affective_authors_merged$Category == "Biotechnology&Genetics", 1, 0)
affective_authors_merged$CIP4 <- ifelse(affective_authors_merged$Category == "Medical", 1, 0)
affective_authors_merged$CIP5 <- ifelse(affective_authors_merged$Category == "Health Sciences", 1, 0)
affective_authors_merged$CIP6 <- ifelse(affective_authors_merged$Category == "Pathology&Pharmacology", 1, 0)
affective_authors_merged$CIP7 <- ifelse(affective_authors_merged$Category == "Neuroscience", 1, 0)
affective_authors_merged$CIP8 <- ifelse(affective_authors_merged$Category == "Engineering&Informatics", 1, 0)
affective_authors_merged$CIP9 <- ifelse(affective_authors_merged$Category == "Chemistry&Physics&Math", 1, 0)



affective_authors_merged[,c("pmid","CIP0","CIP1","CIP2","CIP3","CIP4","CIP5","CIP6","CIP7","CIP8","CIP9")] %>%
  group_by(pmid)  %>% 
  summarise(across(everything(), sum)) -> affective_CIP

aff_model_data <- subset(model_data, pType == "Affective")

aff_model_data <- merge(aff_model_data, 
                        affective_CIP, 
                        by.x = 'pmid', by.y = 'pmid', 
                        all.x = T, all.y = F)


write.csv(aff_model_data, "E:/Research/Data_pubmed/model_data23/model_data_aff_CIP_v2.csv")





# Now same for cognitive




library(data.table)
library(dplyr)


setwd("E:/Research/Data_pubmed/processed_data_authors2/cognitive_lbld")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind


authors = do.call(rbind, lapply(files, function(x) fread(x, skip = "Id, Category", fill=TRUE, sep=",")))
authors <- subset(authors, Id != "")
# Categories:   Biology, Psychology, Biotechnology&Genetics, 
#               Medical, Health Sciences, Pathology&Pharmacology, 
#               Neuroscience, Engineering&Informatics, Chemistry&Physics&Math

authors[grepl("1", authors$Category),]$Category <- "Biology"
authors[grepl("Biology", authors$Category),]$Category <- "Biology"
authors[grepl("Biostatistics", authors$Category),]$Category <- "Biology"
authors[grepl("Anatomy", authors$Category),]$Category <- "Biology"
authors[grepl("Bioethics", authors$Category),]$Category <- "Biology" #?
authors[grepl("Zoology", authors$Category),]$Category <- "Biology"
authors[grepl("Biological Sciences", authors$Category),]$Category <- "Biology"
authors[grepl("Biomedicine", authors$Category),]$Category <- "Biology"
authors[grepl("Biomedical", authors$Category),]$Category <- "Biology"
authors[grepl("Microbiology", authors$Category),]$Category <- "Biology"
authors[grepl("Animal Science", authors$Category),]$Category <- "Biology"
authors[grepl("Biosciences", authors$Category),]$Category <- "Biology"
authors[grepl("Aging", authors$Category),]$Category <- "Biology"
authors[grepl("Biochemistry", authors$Category),]$Category <- "Biology"

authors[grepl("2", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychology", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychiatry", authors$Category),]$Category <- "Psychology"
authors[grepl("Education", authors$Category),]$Category <- "Psychology"
authors[grepl("Behavioral Sciences", authors$Category),]$Category <- "Psychology"
authors[grepl("Behavioral Science", authors$Category),]$Category <- "Psychology"
authors[grepl("Stress and Anxiety Disorders", authors$Category),]$Category <- "Psychology"
authors[grepl("Suicide Research", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychobiology", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychological Sciences", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychotherapy", authors$Category),]$Category <- "Psychology"
authors[grepl("Child Development", authors$Category),]$Category <- "Psychology"
authors[grepl("Cognitive Science", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychopathology", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychological and Brain Sciences", authors$Category),]$Category <- "Psychology"
authors[grepl("Behavioural Sciences", authors$Category),]$Category <- "Psychology"
authors[grepl("Behavioural Science", authors$Category),]$Category <- "Psychology"
authors[grepl("Family Studies", authors$Category),]$Category <- "Psychology" #?
authors[grepl("Family Practice", authors$Category),]$Category <- "Psychology" #?
authors[grepl("Social Work", authors$Category),]$Category <- NA #?
authors[grepl("Eating Disorders", authors$Category),]$Category <- "Psychology"
authors[grepl("Addictions", authors$Category),]$Category <- "Psychology"
authors[grepl("Substance Abuse", authors$Category),]$Category <- "Psychology"
authors[grepl("Addictions", authors$Category),]$Category <- "Psychology"
authors[grepl("Addiction", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychophysiology", authors$Category),]$Category <- "Psychology"
authors[grepl("Cognition", authors$Category),]$Category <- "Psychology"
authors[grepl("Psychiatric", authors$Category),]$Category <- "Psychology"
authors[grepl("Counseling", authors$Category),]$Category <- "Psychology"

authors[grepl("3", authors$Category),]$Category <- "Biotechnology&Genetics"
authors[grepl("Genetics", authors$Category),]$Category <- "Biotechnology&Genetics"

authors[grepl("4", authors$Category),]$Category <- "Medical"
authors[grepl("Medical", authors$Category),]$Category <- "Medical"
authors[grepl("Physiology", authors$Category),]$Category <- "Medical"
authors[grepl("Nursing", authors$Category),]$Category <- "Medical"
authors[grepl("Pediatrics", authors$Category),]$Category <- "Medical"
authors[grepl("Medicine", authors$Category),]$Category <- "Medical"
authors[grepl("Obstetrics and Gynecology", authors$Category),]$Category <- "Medical"
authors[grepl("Cardiology", authors$Category),]$Category <- "Medical"
authors[grepl("Speech and Hearing Sciences", authors$Category),]$Category <- "Medical"
authors[grepl("Rheumatology", authors$Category),]$Category <- "Medical"
authors[grepl("Anesthesiology", authors$Category),]$Category <- "Medical"
authors[grepl("Nutrition", authors$Category),]$Category <- "Medical"
authors[grepl("Radiology", authors$Category),]$Category <- "Medical"
authors[grepl("Ophthalmology", authors$Category),]$Category <- "Medical"
authors[grepl("Oncology", authors$Category),]$Category <- "Medical"
authors[grepl("Dermatology", authors$Category),]$Category <- "Medical"
authors[grepl("Dentistry", authors$Category),]$Category <- "Medical"
authors[grepl("Obstetrics&Gynecology", authors$Category),]$Category <- "Medical"
authors[grepl("General Practice", authors$Category),]$Category <- "Medical"
authors[grepl("Physical Therapy", authors$Category),]$Category <- "Medical"
authors[grepl("Biomedical Sciences", authors$Category),]$Category <- "Medical"
authors[grepl("Surgery", authors$Category),]$Category <- "Medical"
authors[grepl("Endocrinology", authors$Category),]$Category <- "Medical"
authors[grepl("Veterinary", authors$Category),]$Category <- "Medical"
authors[grepl("Gerontology", authors$Category),]$Category <- "Medical"
authors[grepl("Geriatrics", authors$Category),]$Category <- "Medical"
authors[grepl("Gastroenterology", authors$Category),]$Category <- "Medical"
authors[grepl("Urology", authors$Category),]$Category <- "Medical"
authors[grepl("Obstetrics", authors$Category),]$Category <- "Medical"
authors[grepl("Immunology", authors$Category),]$Category <- "Medical"
authors[grepl("Primary Care", authors$Category),]$Category <- "Medical"
authors[grepl("Midwifery", authors$Category),]$Category <- "Medical" #?
authors[grepl("Clinical Sciences", authors$Category),]$Category <- "Medical"
authors[grepl("Paediatrics", authors$Category),]$Category <- "Medical"
authors[grepl("Physiotherapy", authors$Category),]$Category <- "Medical"
authors[grepl("Infectious Diseases", authors$Category),]$Category <- "Medical"
authors[grepl("Palliative Care", authors$Category),]$Category <- "Medical"
authors[grepl("Nephrology", authors$Category),]$Category <- "Medical"
authors[grepl("Otolaryngology", authors$Category),]$Category <- "Medical"
authors[grepl("Anesthesia", authors$Category),]$Category <- "Medical"
authors[grepl("Toxicology", authors$Category),]$Category <- "Medical"
authors[grepl("Otorhinolaryngology", authors$Category),]$Category <- "Medical"
authors[grepl("Anaesthesiology", authors$Category),]$Category <- "Medical"
authors[grepl("Anaesthesia", authors$Category),]$Category <- "Medical"
authors[grepl("Cancer", authors$Category),]$Category <- "Medical"
authors[grepl("Neonatology", authors$Category),]$Category <- "Medical"
authors[grepl("Pathophysiology", authors$Category),]$Category <- "Medical"
authors[grepl("Orthopedics", authors$Category),]$Category <- "Medical"
authors[grepl("Cardiovascular", authors$Category),]$Category <- "Medical"
authors[grepl("Acupuncture", authors$Category),]$Category <- "Medical"
authors[grepl("Clinical", authors$Category),]$Category <- "Medical"
authors[grepl("Orthopaedics", authors$Category),]$Category <- "Medical"
authors[grepl("Gynecology", authors$Category),]$Category <- "Medical"

authors[grepl("5", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Health Sciences", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Health", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Rehabilitation", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Veterans Administration", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Kinesiology", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Social Welfare", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Human Development", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Occupational Therapy", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Veterans Affairs", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Human Science", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Exercise Science", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Social Service", authors$Category),]$Category <- "Health Sciences"
authors[grepl("Epidemiology", authors$Category),]$Category <- "Health Sciences"

authors[grepl("6", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pathology&Pharmacology", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pharmacology", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pathology", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pharmacy", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Psychopharmacology", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pharmaceutical Sciences", authors$Category),]$Category <- "Pathology&Pharmacology"
authors[grepl("Pharmaceutics", authors$Category),]$Category <- "Pathology&Pharmacology"

authors[grepl("7", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuroscience", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuropsychiatry", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuropsychology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurobiology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurosurgery", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurophysiology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuroimaging", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuroendocrinology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuropharmacology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuroradiology", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neurochemistry", authors$Category),]$Category <- "Neuroscience"
authors[grepl("Neuropsychopharmacology", authors$Category),]$Category <- "Neuroscience"

authors[grepl("8", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Engineering&Informatics", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Engineering", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Informatics", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Computer Science", authors$Category),]$Category <- "Engineering&Informatics"
authors[grepl("Environmental Sciences", authors$Category),]$Category <- "Engineering&Informatics" #?
authors[grepl("Electronics", authors$Category),]$Category <- "Engineering&Informatics"

authors[grepl("9", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Chemistry&Physics&Math", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Chemistry", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Physics", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Math", authors$Category),]$Category <- "Chemistry&Physics&Math"
authors[grepl("Statistics", authors$Category),]$Category <- "Chemistry&Physics&Math"

authors[grepl("Management", authors$Category),]$Category <- "Humanities"
authors[grepl("Philosophy", authors$Category),]$Category <- "Humanities"
authors[grepl("Research", authors$Category),]$Category <- "Humanities"
authors[grepl("Business", authors$Category),]$Category <- "Humanities"
authors[grepl("Economics", authors$Category),]$Category <- "Humanities"
authors[grepl("Social Change", authors$Category),]$Category <- "Humanities"
authors[grepl("Social Sciences", authors$Category),]$Category <- "Humanities"
authors[grepl("Social Science", authors$Category),]$Category <- "Humanities"
authors[grepl("Life Sciences", authors$Category),]$Category <- "Humanities"
authors[grepl("Life Science", authors$Category),]$Category <- "Humanities"
authors[grepl("Marketing", authors$Category),]$Category <- "Humanities"
authors[grepl("Humanities", authors$Category),]$Category <- "Humanities"
authors[grepl("Geography", authors$Category),]$Category <- "Humanities"
authors[grepl("Law", authors$Category),]$Category <- "Humanities"
authors[grepl("Criminal Justice", authors$Category),]$Category <- "Humanities"
authors[grepl("Political Science", authors$Category),]$Category <- "Humanities"
authors[grepl("Criminology", authors$Category),]$Category <- "Humanities"
authors[grepl("Agriculture", authors$Category),]$Category <- "Humanities"
authors[grepl("Public Policy", authors$Category),]$Category <- "Humanities"
authors[grepl("Sports", authors$Category),]$Category <- "Humanities"
authors[grepl("Public Administration", authors$Category),]$Category <- "Humanities"
authors[grepl("None", authors$Category),]$Category <- "Humanities"
authors[grepl("Music", authors$Category),]$Category <- "Humanities"
authors[grepl("Sociology", authors$Category),]$Category <- "Humanities"
authors[grepl("Linguistics", authors$Category),]$Category <- "Humanities"
authors[grepl("Anthropology", authors$Category),]$Category <- "Humanities"
authors[grepl("Communication", authors$Category),]$Category <- "Humanities"

authors %>% 
  count(Category) %>%
  filter(n>100) %>% 
  filter(Category!='N/A' & Category!='' & Category!='b' & Category!='0' & Category!='d' & Category!='e') %>% 
  arrange(desc(n)) -> df



df <- df %>%
  arrange(desc(n)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
head(df, 10)


library(ggplot2)

ggplot(subset(df, Category != "Humanities"), aes(x = "", y = n, fill = Category)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  theme_void()










# Categories:   Biology - 1 , Psychology - 2, Biotechnology&Genetics - 3, 
#               Medical - 4, Health Sciences - 5, Pathology&Pharmacology - 6, 
#               Neuroscience - 7, Engineering&Informatics - 8, Chemistry&Physics&Math - 9
#               Humanities - 0
cognitive_authors <- read.csv("E:/Research/Data_pubmed/processed_data_authors2/cognitive_authors.csv")
cognitive_authors$aff_key <- substr(cognitive_authors$affiliationinfo,1,50)

authors <- authors[!is.na(authors$Id),]
authors <- authors[!is.na(authors$Category),]

cognitive_authors$X.2 <- NULL
cognitive_authors$X.1 <- NULL
cognitive_authors$X <- NULL

authors$Id <- as.integer(authors$Id)

authors <- authors[authors$Id <= nrow(authors),]

cognitive_authors_lbld <- merge(cognitive_u_aff, authors, by.x = 'aid', by.y = 'Id', all.x = T)


cognitive_authors_lbld$aid <- NULL
cognitive_authors_lbld <- cognitive_authors_lbld[!duplicated(cognitive_authors_lbld),]

cognitive_authors_lbld %>%
  group_by(Affiliation) %>%
  summarise(Category = Category[[1]]) ->
  cognitive_authors_lbldu

cognitive_authors_merged <- merge(cognitive_authors[,c("pmid","aff_key")], 
                                  cognitive_authors_lbldu[,c("Affiliation","Category")], 
                                  by.x = 'aff_key', by.y = 'Affiliation', all.x = T, all.y = F)


cognitive_authors_merged$CIP0 <- ifelse(cognitive_authors_merged$Category == "Humanities", 1, 0)
cognitive_authors_merged$CIP1 <- ifelse(cognitive_authors_merged$Category == "Biology", 1, 0)
cognitive_authors_merged$CIP2 <- ifelse(cognitive_authors_merged$Category == "Psychology", 1, 0)
cognitive_authors_merged$CIP3 <- ifelse(cognitive_authors_merged$Category == "Biotechnology&Genetics", 1, 0)
cognitive_authors_merged$CIP4 <- ifelse(cognitive_authors_merged$Category == "Medical", 1, 0)
cognitive_authors_merged$CIP5 <- ifelse(cognitive_authors_merged$Category == "Health Sciences", 1, 0)
cognitive_authors_merged$CIP6 <- ifelse(cognitive_authors_merged$Category == "Pathology&Pharmacology", 1, 0)
cognitive_authors_merged$CIP7 <- ifelse(cognitive_authors_merged$Category == "Neuroscience", 1, 0)
cognitive_authors_merged$CIP8 <- ifelse(cognitive_authors_merged$Category == "Engineering&Informatics", 1, 0)
cognitive_authors_merged$CIP9 <- ifelse(cognitive_authors_merged$Category == "Chemistry&Physics&Math", 1, 0)



cognitive_authors_merged[,c("pmid","CIP0","CIP1","CIP2","CIP3","CIP4","CIP5","CIP6","CIP7","CIP8","CIP9")] %>%
  group_by(pmid)  %>% 
  summarise(across(everything(), sum)) -> cognitive_CIP

cog_model_data <- subset(model_data, pType == "Cognitive")

cog_model_data <- merge(cog_model_data, 
                        cognitive_CIP, 
                        by.x = 'pmid', by.y = 'pmid', 
                        all.x = T, all.y = F)


write.csv(cog_model_data, "E:/Research/Data_pubmed/model_data23/model_data_cog_CIP_v2.csv")