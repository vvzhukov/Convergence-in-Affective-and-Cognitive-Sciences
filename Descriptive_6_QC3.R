# QC 3

library(dplyr)
library(tidyr)
library(ggplot2)
# Here we prep data for sankey using affective L1 and NA L1


# Pure L1 categories
affective <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")
cit <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/citations.csv")
ref <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/references.csv")

cit_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations_subjectareas.csv")
ref_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references_subjectareas.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

cit_links <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations_net_years.csv")
ref_links <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references_net_years.csv")

colnames(ref_links)
colnames(ref_subj)[1] <- "references"
ref_links <- merge(x=ref_links, y=ref_subj, by = "references", all.x = TRUE)
colnames(aff_subj) <- c("pmid","Ap","Bp","Cp","Dp","Ep","Fp","Gp","Hp","Ip","Jp","Kp","Lp","Mp","Np","Vp","Zp")
ref_links <- merge(x=ref_links, y=aff_subj, by = "pmid", all.x = TRUE)
ref_links <- merge(x=ref_links, y=affective[,c(2,3)], by = "pmid", all.x = TRUE)
colnames(ref_links)[38] <- "year_published"

colnames(cit_links)
colnames(cit_subj)[1] <- "cited_by"
cit_links <- merge(x=cit_links, y=cit_subj, by = "cited_by", all.x = TRUE)
cit_links <- merge(x=cit_links, y=aff_subj, by = "pmid", all.x = TRUE)
cit_links <- merge(x=cit_links, y=affective[,c(2,3)], by = "pmid", all.x = TRUE)
colnames(cit_links)[38] <- "year_published"



cit_links %>% group_by(year_published, pmid) %>% summarise(A = sum(A, na.rm = T),
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
                                                     Ap = sum(Ap, na.rm = T),
                                                     Bp = sum(Bp, na.rm = T),
                                                     Cp = sum(Cp, na.rm = T),
                                                     Dp = sum(Dp, na.rm = T),
                                                     Ep = sum(Ep, na.rm = T),
                                                     Fp = sum(Fp, na.rm = T),
                                                     Gp = sum(Gp, na.rm = T),
                                                     Hp = sum(Hp, na.rm = T),
                                                     Ip = sum(Ip, na.rm = T),
                                                     Jp = sum(Jp, na.rm = T),
                                                     Kp = sum(Kp, na.rm = T),
                                                     Lp = sum(Lp, na.rm = T),
                                                     Mp = sum(Mp, na.rm = T),
                                                     Np = sum(Np, na.rm = T),
                                                     Vp = sum(Vp, na.rm = T),
                                                     Zp = sum(Zp, na.rm = T),
                                                     total = n(),
                                                     cited_by_affective = sum(cited_by_affective, na.rm = T)) -> cit_links_summ_QA

ref_links %>% group_by(year_published, pmid) %>% summarise(A = sum(A, na.rm = T),
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
                                                     Ap = sum(Ap, na.rm = T),
                                                     Bp = sum(Bp, na.rm = T),
                                                     Cp = sum(Cp, na.rm = T),
                                                     Dp = sum(Dp, na.rm = T),
                                                     Ep = sum(Ep, na.rm = T),
                                                     Fp = sum(Fp, na.rm = T),
                                                     Gp = sum(Gp, na.rm = T),
                                                     Hp = sum(Hp, na.rm = T),
                                                     Ip = sum(Ip, na.rm = T),
                                                     Jp = sum(Jp, na.rm = T),
                                                     Kp = sum(Kp, na.rm = T),
                                                     Lp = sum(Lp, na.rm = T),
                                                     Mp = sum(Mp, na.rm = T),
                                                     Np = sum(Np, na.rm = T),
                                                     Vp = sum(Vp, na.rm = T),
                                                     Zp = sum(Zp, na.rm = T),
                                                     total = n(),
                                                     references_affective = sum(references_affective, na.rm = T)) -> ref_links_summ_publ

cit_links_summ_QA$SA1_ABG <- cit_links_summ_QA$A + cit_links_summ_QA$B + cit_links_summ_QA$G
cit_links_summ_QA$SA2_F <- cit_links_summ_QA$F
cit_links_summ_QA$SA3_CN <- cit_links_summ_QA$C + cit_links_summ_QA$N
cit_links_summ_QA$SA4_DEJL <- cit_links_summ_QA$D + cit_links_summ_QA$E + cit_links_summ_QA$J + cit_links_summ_QA$L
cit_links_summ_QA$SA5_HIKM <- cit_links_summ_QA$H + cit_links_summ_QA$I + cit_links_summ_QA$K + cit_links_summ_QA$M
cit_links_summ_QA$SA1_ABGp <- cit_links_summ_QA$Ap + cit_links_summ_QA$Bp + cit_links_summ_QA$Gp
cit_links_summ_QA$SA2_Fp <- cit_links_summ_QA$Fp
cit_links_summ_QA$SA3_CNp <- cit_links_summ_QA$Cp + cit_links_summ_QA$Np
cit_links_summ_QA$SA4_DEJLp <- cit_links_summ_QA$Dp + cit_links_summ_QA$Ep + cit_links_summ_QA$Jp + cit_links_summ_QA$Lp
cit_links_summ_QA$SA5_HIKMp <- cit_links_summ_QA$Hp + cit_links_summ_QA$Ip + cit_links_summ_QA$Kp + cit_links_summ_QA$Mp



cit_links$SA1_ABG <- cit_links$A + cit_links$B + cit_links$G
cit_links$SA2_F <- cit_links$F
cit_links$SA3_CN <- cit_links$C + cit_links$N
cit_links$SA4_DEJL <- cit_links$D + cit_links$E + cit_links$J + cit_links$L
cit_links$SA5_HIKM <- cit_links$H + cit_links$I + cit_links$K + cit_links$M


ref_links$SA1_ABG <- ref_links$A + ref_links$B + ref_links$G
ref_links$SA2_F <- ref_links$F
ref_links$SA3_CN <- ref_links$C + ref_links$N
ref_links$SA4_DEJL <- ref_links$D + ref_links$E + ref_links$J + ref_links$L
ref_links$SA5_HIKM <- ref_links$H + ref_links$I + ref_links$K + ref_links$M

# 1970
QA_1970 <- cit_links_summ_QA[cit_links_summ_QA$year_published == 1970,]
head(QA_1970[order(QA_1970$SA1_ABG,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_1970[order(QA_1970$SA2_F,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_1970[order(QA_1970$SA3_CN,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_1970[order(QA_1970$SA4_DEJL,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_1970[order(QA_1970$SA5_HIKM,decreasing=TRUE),c(1,2,36:41),],4)


# 1983
QA_1983 <- cit_links_summ_QA[cit_links_summ_QA$year_published == 1983,]
head(QA_1983[order(QA_1983$SA1_ABG,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_1983[order(QA_1983$SA2_F,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_1983[order(QA_1983$SA3_CN,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_1983[order(QA_1983$SA4_DEJL,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_1983[order(QA_1983$SA5_HIKM,decreasing=TRUE),c(1,2,36:41),],4)


# 2000
QA_2000 <- cit_links_summ_QA[cit_links_summ_QA$year_published == 2000,]
head(QA_2000[order(QA_2000$SA1_ABG,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_2000[order(QA_2000$SA2_F,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_2000[order(QA_2000$SA3_CN,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_2000[order(QA_2000$SA4_DEJL,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_2000[order(QA_2000$SA5_HIKM,decreasing=TRUE),c(1,2,36:41),],4)

# 2019
QA_2019 <- cit_links_summ_QA[cit_links_summ_QA$year_published == 2019,]
head(QA_2019[order(QA_2019$SA1_ABG,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_2019[order(QA_2019$SA2_F,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_2019[order(QA_2019$SA3_CN,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_2019[order(QA_2019$SA4_DEJL,decreasing=TRUE),c(1,2,36:41),],4)
head(QA_2019[order(QA_2019$SA5_HIKM,decreasing=TRUE),c(1,2,36:41),],4)




# QC3
data[sample(1:nrow(data), 3), ] 
cit_links_summ_QA[sample(1:nrow(cit_links_summ_QA), 15), c(1,2,35,36)]


# Drawn sample:  c(28218050, 1254756, 26808631, 14215077, 19450655, 23441371, 24929958, 32171899, 29078010, 31377452, 27279254, 30027747, 32792683, 538216, 28202502)
x <- 30027747

#cit_links[cit_links$pmid == x,]
nrow(cit_links[cit_links$pmid == x,])
paste(cit_links[cit_links$pmid == x,2], collapse = ", ")
print("---------------------------------")
#ref_links[ref_links$pmid == x,]
nrow(ref_links[ref_links$pmid == x,])
paste(ref_links[ref_links$pmid == x,2], collapse = ", ")


# QC4

cit_links_summ_QA[sample(1:nrow(cit_links_summ_QA), 15), c(1,2,36:41)]
# Drawn sample: 7858278, 9248052, 24359193, 32414727, 30772858, 14693401, 2754228, 14555880, 20223600, 25160884, 29544158, 33990181, 450645, 2625789, 22527082


x <- 7858278
#citations
nrow(cit_links[cit_links$pmid == x,])
cit_links[cit_links$pmid == x,c(1,2,39,40,41,42,43)]
paste(cit_links[cit_links$pmid == x,2], collapse = ", ")
print("---------------------------------")

#references
nrow(ref_links[ref_links$pmid == x,])
ref_links[ref_links$pmid == x,c(1,2,39,40,41,42,43)]
paste(ref_links[ref_links$pmid == x,2], collapse = ", ")
