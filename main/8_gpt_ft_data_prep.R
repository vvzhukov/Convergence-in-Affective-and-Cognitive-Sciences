aff_authors <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_authors.csv")
cog_authors <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_authors.csv")
model_data <- read.csv("E:/Research/Data_pubmed/model_data23/model_data.csv")

# desired format
#{"prompt": "<prompt text>", "completion": "<ideal generated text>"}
#{"prompt": "<prompt text>", "completion": "<ideal generated text>"}
#{"prompt": "<prompt text>", "completion": "<ideal generated text>"}
#...

# use CLI openai tool to restructure and optimize the data:
#> openai tools fine_tunes.prepare_data -f sample-training.json

# filter only authors with affiliation
aff_authors <- aff_authors[!is.na(aff_authors$affiliationinfo) & !is.na(aff_authors$identifier),]

# keep the longest affiliation for all unique identifiers
library(stringr)
library(dplyr)
aff_authors %>% 
  group_by(identifier) %>% 
  mutate(affiliationinfo = affiliationinfo[which.max(str_length(affiliationinfo))]) %>%
  ungroup -> aff_authors_c

# trim too long affiliations
aff_authors_c <- aff_authors_c[str_length(aff_authors_c$affiliationinfo) < 200,]

aff_authors_c <- merge(aff_authors_c, model_data[,c('pmid', 'pYear', 'pCitations_n')], by.x = 'pmid', by.y = 'pmid', all.x = T)

# add total number of citations per author
aff_authors_c %>% 
  group_by(identifier) %>% 
  summarise(aCitations_n = sum(pCitations_n)) %>%
  ungroup -> aff_authors_cit
aff_authors_c <- merge(aff_authors_c, aff_authors_cit, by.x = 'identifier', by.y='identifier', all.x = T)

# concat all publications years
aff_authors_c %>% 
  group_by(identifier) %>% 
  mutate(pYears = paste0(pYear, collapse = " "),
         pCount = n(),
         pFirstYear = min(pYear)) -> aff_authors_c

aff_authors_c$fullname <- paste(aff_authors_c$forename, 
                                aff_authors_c$initials, 
                                aff_authors_c$lastname, sep = " ")



aff_authors_c$prompt <- paste("How many citations does", aff_authors_c$fullname, 
                              "from" , aff_authors_c$affiliationinfo, 
                              "have?",
                              "They first published in", aff_authors_c$pFirstYear, 
                              "and have", aff_authors_c$pCount,
                              "publciations.",sep = " ")

aff_authors_c$completion <- aff_authors_c$aCitations_n

library(jsonlite)
aff_json_tun <- toJSON(aff_authors_c[,c('prompt', 'completion')])
#cat(aff_json_tun)
write(aff_json_tun, "E:/Research/Data_pubmed/model_data23/train_affective_authros.json")


# Publication focused
# Q: What is the estimate of citations for the publication <title> written by <name> from <affiliation>?
# A: The estimate for the citations for the publication <title> written by <name> from <affiliation> will be <citations_n>.


# Author focused
# Q: What is the 
# A: 

# Affiliation focused
# Q: What are most typical publications keywords for ...
# A:
