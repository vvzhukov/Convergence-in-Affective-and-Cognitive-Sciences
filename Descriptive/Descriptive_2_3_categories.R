library(ggplot2)
library(dplyr)
library(tidyr)
# Here we prep data for sankey using affective L1 and NA without clustering

#part1 <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references.csv")

#part3 <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")

#part1$X <- NULL
#part2$X <- NULL
#part3$X <- NULL

#affective_data <- merge(part1, part2, all=TRUE) 
#affective_data <- merge(affective_data, part3, all=TRUE)
#remove(part1,part2,part3)
affective_data <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")


#part1c <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references_subjectareas.csv")
#part2c <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations_subjectareas.csv")
#part3c <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

#part1c$X <- NULL
#part2c$X <- NULL
#part3c$X <- NULL

#categories_data <- merge(part1c, part2c, all=TRUE) 
#categories_data <- merge(categories_data, part3c, all=TRUE)
#remove(part1c,part2c,part3c)
categories_data <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

cit_ref_data <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_cit_ref.csv")
cit_ref_data <- cit_ref_data[cit_ref_data$pmid %in% affective_data$pmid,]


affective_data$X <- NULL
affective_data$refs <- NULL

cit_data <- cit_ref_data[,c(2,4)]
ref_data <- cit_ref_data[,c(2,5)]

cit_data <- separate_rows(cit_data,2,sep = " ")
length(unique(cit_data$cited_by)) #1980030
length(unique(cit_data$pmid)) #237814

cit_details <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations.csv")
cit_details <- cit_details[,c(2,3)]

colnames(cit_details) <- c("cited_by", "year")


cit_data <- merge(x = cit_data, y = cit_details, by = "cited_by", all.x = TRUE)
summary(cit_data$year)
cit_data <- cit_data[!is.na(cit_data$year),]
colnames(cit_data) <- c("cited_by", "pmid", "year_cited")

ref_data <- separate_rows(ref_data,2,sep = " ")
ref_data <- merge(x = ref_data, y = affective_data[,c(1,2)], by = "pmid", all.x = TRUE)
colnames(ref_data) <- c("pmid", "references", "year_references")
summary(ref_data$year_references)

cit_data$cited_by_affective <- 0
cit_data$cited_by_affective[cit_data$cited_by %in% categories_data$pmid] <- 1
table(cit_data$cited_by_affective) # 0 - 6065819, 1 - 2220319

ref_data$references_affective <- 0
ref_data$references_affective[ref_data$references %in% categories_data$pmid] <- 1


write.csv(cit_data, "E:/Research/Data_pubmed/affective_data/balanced/citations_net_years.csv")
write.csv(ref_data, "E:/Research/Data_pubmed/affective_data/balanced/references_net_years.csv")

cit_data <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations_net_years.csv")
ref_data <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references_net_years.csv")

colnames(categories_data)[1] <- "references"
ref_data <- merge(x = ref_data, y = categories_data, by = "references", all.x = TRUE)

colnames(categories_data)[1] <- "cited_by"
cit_data <- merge(x = cit_data, y = categories_data, by = "cited_by", all.x = TRUE)

ref_data[is.na(ref_data)] = 0
cit_data[is.na(cit_data)] = 0

cit_data %>% group_by(year_cited) %>% summarise(A = sum(A, na.rm = T),
                                                B = sum(B, na.rm = T),
                                                C = sum(C, na.rm = T),
                                                D = sum(D, na.rm = T),
                                                E = sum(E, na.rm = T),
                                                F = sum(F, na.rm = T),
                                                G = sum(G, na.rm = T),
                                                H = sum(H, na.rm = T),
                                                I = sum(I, na.rm = T),
                                                J = sum(J, na.rm = T),
                                                K = sum(K, na.rm = T),
                                                L = sum(L, na.rm = T),
                                                M = sum(M, na.rm = T),
                                                N = sum(N, na.rm = T),
                                                V = sum(V, na.rm = T),
                                                Z = sum(Z, na.rm = T),
                                                total = n(),
                                                cited_by_affective = sum(cited_by_affective, na.rm = T)) -> cit_data_summ

cit_data_summ$cited_by_na <- cit_data_summ$total - 

ref_data %>% group_by(year_references) %>% summarise(A = sum(A, na.rm = T),
                                                    B = sum(B, na.rm = T),
                                                    C = sum(C, na.rm = T),
                                                    D = sum(D, na.rm = T),
                                                    E = sum(E, na.rm = T),
                                                    F = sum(F, na.rm = T),
                                                    G = sum(G, na.rm = T),
                                                    H = sum(H, na.rm = T),
                                                    I = sum(I, na.rm = T),
                                                    J = sum(J, na.rm = T),
                                                    K = sum(K, na.rm = T),
                                                    L = sum(L, na.rm = T),
                                                    M = sum(M, na.rm = T),
                                                    N = sum(N, na.rm = T),
                                                    V = sum(V, na.rm = T),
                                                    Z = sum(Z, na.rm = T)) -> ref_data_summ


library(pals)


ggplot(cit_data_summ, aes(x=year_cited)) + 
  geom_line(aes(y=A), color='black',size=1) +
  geom_line(aes(y=B), color='forestgreen',size=1) +
  geom_line(aes(y=C), color='red2',size=1) +
  geom_line(aes(y=D), color='orange',size=1) +
  geom_line(aes(y=E), color='cornflowerblue',size=1) +
  geom_line(aes(y=F), color='magenta',size=1) +
  geom_line(aes(y=G), color='darkolivegreen4',size=1) +
  geom_line(aes(y=H), color='indianred1',size=1) +
  geom_line(aes(y=I), color='tan4',size=1) +
  geom_line(aes(y=J), color='darkblue',size=1) +
  geom_line(aes(y=K), color='mediumorchid1',size=1) +
  geom_line(aes(y=L), color='firebrick4',size=1) +
  geom_line(aes(y=M), color='yellowgreen',size=1) +
  geom_line(aes(y=N), color='lightsalmon',size=1) +
  geom_line(aes(y=V), color='tan3',size=1) +
  geom_line(aes(y=Z), color='tan1',size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) +
  ggtitle("Citations, n = 8 286 204",)


ggplot(ref_data_summ, aes(x=year_references)) + 
  geom_line(aes(y=A), color='black',size=1) +
  geom_line(aes(y=B), color='forestgreen',size=1) +
  geom_line(aes(y=C), color='red2',size=1) +
  geom_line(aes(y=D), color='orange',size=1) +
  geom_line(aes(y=E), color='cornflowerblue',size=1) +
  geom_line(aes(y=F), color='magenta',size=1) +
  geom_line(aes(y=G), color='darkolivegreen4',size=1) +
  geom_line(aes(y=H), color='indianred1',size=1) +
  geom_line(aes(y=I), color='tan4',size=1) +
  geom_line(aes(y=J), color='darkblue',size=1) +
  geom_line(aes(y=K), color='mediumorchid1',size=1) +
  geom_line(aes(y=L), color='firebrick4',size=1) +
  geom_line(aes(y=M), color='yellowgreen',size=1) +
  geom_line(aes(y=N), color='lightsalmon',size=1) +
  geom_line(aes(y=V), color='tan3',size=1) +
  geom_line(aes(y=Z), color='tan1',size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) +
  ggtitle("References, n = 8 030 045",)
