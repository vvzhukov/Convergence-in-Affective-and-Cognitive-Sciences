library(ggplot2)
library(dplyr)

affective_data <- read.csv("E:/Research/Data_pubmed/affective_data/affective_and_refs.csv")
categories_data <- read.csv("E:/Research/Data_pubmed/affective_data/subjectareas.csv")


propcols=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")
nodes <- categories_data

nodes$Categories=""
nodes$Categories_cnt = 0

for(colname in propcols)({
  coldata=nodes[,colname]
  nodes$Categories[which(coldata==1)]=paste(nodes$Categories[which(coldata==1)],colname)
  nodes$Categories_cnt[which(coldata==1)] = nodes$Categories_cnt[which(coldata==1)] + 1
})

nodes$Cluster = "Other"

nodes$Cluster[nodes$F == 1 | nodes$I == 1 | nodes$K == 1] = "Behavioral Theory"
nodes$Cluster[nodes$A == 1 | nodes$B == 1 | nodes$G == 1 | nodes$C == 1 | nodes$D == 1 | nodes$N == 1] = "Biomedical"
nodes$Cluster[nodes$J == 1 | nodes$L == 1] = "Techno-Informatics"

nodes$BeT = 0
nodes$BeT[nodes$F == 1 | nodes$I == 1 | nodes$K == 1] = 1
nodes$Bio = 0
nodes$Bio[nodes$A == 1 | nodes$B == 1 | nodes$G == 1 | nodes$C == 1 | nodes$D == 1 | nodes$N == 1] = 1
nodes$TI = 0
nodes$TI[nodes$J == 1 | nodes$L == 1] = 1

nodes$Cluster[nodes$BiT + nodes$Bio + nodes$TI > 2] = "Multi"
nodes$Cluster[nodes$BiT + nodes$Bio + nodes$TI == 2] = "Pairs"

nodes$Cluster[nodes$A == 1 | nodes$B == 1 | nodes$G == 1 | nodes$C == 1 | nodes$D == 1 | nodes$N == 1] = "Biomedical"
nodes$Cluster[nodes$J == 1 | nodes$L == 1] = "Techno-Informatics"


table(nodes$Cluster)

# here we dropped 30% of the data as we were not able to classify it 
nodes <- subset(nodes, Cluster != "Other")

nodes_y <- merge(x = nodes, y = affective_data, by = "pmid", all.x = TRUE)

gr1 <- subset(nodes_y, Cluster == "Behavioral Theory")  %>% group_by(year) %>% summarise(n_behav = n())
gr2 <- subset(nodes_y, Cluster == "Biomedical")  %>% group_by(year) %>% summarise(n_bio = n())
gr3 <- subset(nodes_y, Cluster == "Techno-Informatics")  %>% group_by(year) %>% summarise(n_ti = n())

aff_evolution <- merge(x = merge(x = gr1, y = gr2, by = "year"), y = gr3, by = "year")
aff_evolution <- subset(aff_evolution, year %in% c(1950:2020))

ggplot(aff_evolution, aes(x=year)) + 
  geom_line(aes(y=n_bio), color="brown",size=1) +
  geom_line(aes(y=n_behav), color="pink",size=1) +
  geom_line(aes(y=n_ti), color="darkgreen",size=1) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("Affective publications + references (1 level), 1950-2020, n = 867 405")
