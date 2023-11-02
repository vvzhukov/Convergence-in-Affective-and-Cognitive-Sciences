affective_data <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
affective_data_subj <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_subjectareas.csv")

cognitive_data <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_words.csv")
cognitive_data_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")

# choose Affective vs Cognitive
data <- affective_data
data_subj <- affective_data_subj

data <- cognitive_data
data_subj <- cognitive_data_subj

# use new SA encoding

data$all_mesh_words <- paste(data$mesh_descriptor, data$mesh_descriptor_major, data$mesh_qualifier, data$mesh_qualifier_major)
data <- data[,c("pmid","all_mesh_words")]

data <- merge(data, data_subj, by = "pmid", all.x=TRUE)

data$SA1 <- as.numeric(data$A + data$B + data$G) #orange
data$SA2 <- as.numeric(data$F) #red 
data$SA3 <- as.numeric(data$C + data$N) #green
data$SA4 <- as.numeric(data$D + data$E + data$J + data$L) #blue
data$SA5 <- as.numeric(data$H + data$I + data$K + data$M) #yellow


aff_SA1 <- subset(data, data$SA1 >0) 
aff_SA2 <- subset(data, data$SA2 >0) 
aff_SA3 <- subset(data, data$SA3 >0) 
aff_SA4 <- subset(data, data$SA4 >0) 
aff_SA5 <- subset(data, data$SA5 >0) 

# library
library(wordcloud)
library(wordcloud2) 
library(tidyverse)
library(tidytext)
library(tm)
library(ggpubr)
library(webshot)
library(htmlwidgets)




word_df1 <- aff_SA1 %>% unnest_tokens(word, all_mesh_words)
freq_df1 = word_df1 %>% count(word) %>% arrange(desc(n))
# now filter SA words
SA1_words <- read.csv("E:/Research/Data_pubmed/SA1_mesh_words.csv",header = FALSE)
SA1_words <- unique(SA1_words)
freq_df1 = freq_df1[freq_df1$word %in% tolower(SA1_words$V1),]

word_df2 <- aff_SA2 %>% unnest_tokens(word, all_mesh_words)
freq_df2 = word_df2 %>% count(word) %>% arrange(desc(n))
# now filter SA words
SA2_words <- read.csv("E:/Research/Data_pubmed/SA2_mesh_words.csv",header = FALSE)
SA2_words <- unique(SA2_words)
freq_df2 = freq_df2[freq_df2$word %in% tolower(SA2_words$V1),]

word_df3 <- aff_SA3 %>% unnest_tokens(word, all_mesh_words)
freq_df3 = word_df3 %>% count(word) %>% arrange(desc(n))
# now filter SA words
SA3_words <- read.csv("E:/Research/Data_pubmed/SA3_mesh_words.csv",header = FALSE)
SA3_words <- unique(SA3_words)
freq_df3 = freq_df3[freq_df3$word %in% tolower(SA3_words$V1),]

word_df4 <- aff_SA4 %>% unnest_tokens(word, all_mesh_words)
freq_df4 = word_df4 %>% count(word) %>% arrange(desc(n))
# now filter SA words
SA4_words <- read.csv("E:/Research/Data_pubmed/SA4_mesh_words.csv",header = FALSE)
SA4_words <- unique(SA4_words)
freq_df4 = freq_df4[freq_df4$word %in% tolower(SA4_words$V1),]

word_df5 <- aff_SA5 %>% unnest_tokens(word, all_mesh_words)
freq_df5 = word_df5 %>% count(word) %>% arrange(desc(n))
# now filter SA words
SA5_words <- read.csv("E:/Research/Data_pubmed/SA5_mesh_words.csv",header = FALSE)
SA5_words <- unique(SA5_words)
freq_df5 = freq_df5[freq_df5$word %in% tolower(SA5_words$V1),]



webshot::install_phantomjs()

hw <- wordcloud2(head(freq_df1,30),color = "orange",backgroundColor = "black", shape = 'diamond', ellipticity= 0.5, minSize = 0.3, shuffle = F)
saveWidget(hw,"C:/Users/Rubinzone/Desktop/temp.html",selfcontained = F)
webshot::webshot("C:/Users/Rubinzone/Desktop/temp.html","C:/Users/Rubinzone/Desktop/F10_SA1.png",vwidth = 1200, vheight = 1200, delay =10)

hw <- wordcloud2(head(freq_df2,30),color = "red",backgroundColor = "black", shape = 'diamond', ellipticity= 0.5, minSize = 0.3, shuffle = F)
saveWidget(hw,"C:/Users/Rubinzone/Desktop/temp.html",selfcontained = F)
webshot::webshot("C:/Users/Rubinzone/Desktop/temp.html","C:/Users/Rubinzone/Desktop/F10_SA2.png",vwidth = 1200, vheight = 1200, delay =10)

hw <- wordcloud2(freq_df3,color = "green",backgroundColor = "black")
saveWidget(hw,"C:/Users/Rubinzone/Desktop/temp.html",selfcontained = F)
webshot::webshot("C:/Users/Rubinzone/Desktop/temp.html","C:/Users/Rubinzone/Desktop/F10_SA3.png",vwidth = 1200, vheight = 1200, delay =10)

hw <- wordcloud2(freq_df4,color = "blue",backgroundColor = "black")
saveWidget(hw,"C:/Users/Rubinzone/Desktop/temp.html",selfcontained = F)
webshot::webshot("C:/Users/Rubinzone/Desktop/temp.html","C:/Users/Rubinzone/Desktop/F10_SA4.png",vwidth = 1200, vheight = 1200, delay =10)

hw <- wordcloud2(freq_df5,color = "yellow",backgroundColor = "black")
saveWidget(hw,"C:/Users/Rubinzone/Desktop/temp.html",selfcontained = F)
webshot::webshot("C:/Users/Rubinzone/Desktop/temp.html","C:/Users/Rubinzone/Desktop/F10_SA5.png",vwidth = 1200, vheight = 1200, delay =10)
