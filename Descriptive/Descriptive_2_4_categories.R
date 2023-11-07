library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
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

# descriptive for the affective
# number of L1 per publication
aff_subj_bool <- aff_subj
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
  scale_y_continuous(limits = c(0,100000)) +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_blank()) +
  geom_vline(aes(xintercept = mean(L1)), linetype = 2, size = 2)

SA_plot <- ggplot(data.frame("SA" = rowSums(aff_subj_bool[,c(18:22)])), aes(x=SA)) + 
  geom_bar(color="black", fill="white") + 
  scale_x_continuous(name="Number of Subject Areas (SA) per publication", breaks=c(1:20)) +
  scale_y_continuous(limits = c(0,100000)) +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_blank())  +
  geom_vline(aes(xintercept = mean(SA)-0.5), linetype = 2, size = 2)

mean(rowSums(aff_subj_bool[,c(2:17)]))
mean(rowSums(aff_subj_bool[,c(18:22)]))
annotate_figure(ggarrange(L1_plot, SA_plot + rremove("y.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1),
                    top = text_grob("Affective publications, n = 237816", 
                                              size = 12))
      



# descriptive stats for the links
length(cit_links$pmid) # 8 286 138
length(unique(cit_links$pmid)) # 237 495
length(unique(cit_links$cited_by)) # 1 963 257

length(ref_links$pmid) # 8 030 043
length(unique(ref_links$pmid)) # 237 814
length(unique(ref_links$references)) # 1 702 663

# intersection between citations and references
length(intersect(cit_links$cited_by, ref_links$references)) # 786 601
length(intersect(cit_links$cited_by, ref_links$references))*100 / length(unique(cit_links$cited_by)) # 40.06
length(intersect(cit_links$cited_by, ref_links$references))*100 / length(unique(ref_links$references)) # 46.2

# intersection between affective and citations
length(intersect(cit_links$cited_by, cit_links$pmid)) # 215 234
length(intersect(cit_links$cited_by, cit_links$pmid))*100 / length(unique(cit_links$pmid)) # 90.51
length(intersect(cit_links$cited_by, cit_links$pmid))*100 / length(unique(cit_links$cited_by)) # 10.95

# intersection between affective and references
length(intersect(ref_links$references, ref_links$pmid)) # 179 228
length(intersect(ref_links$references, ref_links$pmid))*100 / length(unique(ref_links$pmid)) # 75.36
length(intersect(ref_links$references, ref_links$pmid))*100 / length(unique(ref_links$references)) # 10.53




colnames(ref_subj)[1] <- "references"
ref_links <- merge(x=ref_links, y=ref_subj, by = "references", all.x = TRUE)
colnames(aff_subj) <- c("pmid","Ap","Bp","Cp","Dp","Ep","Fp","Gp","Hp","Ip","Jp","Kp","Lp","Mp","Np","Vp","Zp")
ref_links <- merge(x=ref_links, y=aff_subj, by = "pmid", all.x = TRUE)
ref_links <- merge(x=ref_links, y=affective[,c(2,3)], by = "pmid", all.x = TRUE)
colnames(ref_links)[38] <- "year_published"


colnames(cit_subj)[1] <- "cited_by"
cit_links <- merge(x=cit_links, y=cit_subj, by = "cited_by", all.x = TRUE)
cit_links <- merge(x=cit_links, y=aff_subj, by = "pmid", all.x = TRUE)
cit_links <- merge(x=cit_links, y=affective[,c(2,3)], by = "pmid", all.x = TRUE)
colnames(cit_links)[38] <- "year_published"


cit_links %>% group_by(year_cited) %>% summarise(A = sum(A, na.rm = T),
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
                                                cited_by_affective = sum(cited_by_affective, na.rm = T)) -> cit_links_summ

ref_links %>% group_by(year_references) %>% summarise(A = sum(A, na.rm = T),
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
                                                     references_affective = sum(references_affective, na.rm = T)) -> ref_links_summ


cit_links %>% group_by(year_published) %>% summarise(A = sum(A, na.rm = T),
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
                                                 cited_by_affective = sum(cited_by_affective, na.rm = T)) -> cit_links_summ_publ

ref_links %>% group_by(year_published) %>% summarise(A = sum(A, na.rm = T),
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

remove(cit, cit_links, cit_subj, ref, ref_links, ref_subj, affective, aff_subj)


# SA categories
# OLD before 04/22
#cit_links_summ$SA1_JL <- cit_links_summ$J + cit_links_summ$L
#cit_links_summ$SA2_CN <- cit_links_summ$C + cit_links_summ$N
#cit_links_summ$SA3_AB <- cit_links_summ$A + cit_links_summ$B
#cit_links_summ$SA4_IKMH <- cit_links_summ$I + cit_links_summ$K + cit_links_summ$M + cit_links_summ$H
#cit_links_summ$SA5_D <- cit_links_summ$D
#cit_links_summ$SA6_E <- cit_links_summ$E
#cit_links_summ$SA7_F <- cit_links_summ$F
#cit_links_summ$SA8_G <- cit_links_summ$G
#cit_links_summ$SA1_JLp <- cit_links_summ$Jp + cit_links_summ$Lp
#cit_links_summ$SA2_CNp <- cit_links_summ$Cp + cit_links_summ$Np
#cit_links_summ$SA3_ABp <- cit_links_summ$Ap + cit_links_summ$Bp
#cit_links_summ$SA4_IKMHp <- cit_links_summ$Ip + cit_links_summ$Kp + cit_links_summ$Mp + cit_links_summ$Hp
#cit_links_summ$SA5_Dp <- cit_links_summ$Dp
#cit_links_summ$SA6_Ep <- cit_links_summ$Ep
#cit_links_summ$SA7_Fp <- cit_links_summ$Fp
#cit_links_summ$SA8_Gp <- cit_links_summ$Gp

#ref_links_summ$SA1_JL <- ref_links_summ$J + ref_links_summ$L
#ref_links_summ$SA2_CN <- ref_links_summ$C + ref_links_summ$N
#ref_links_summ$SA3_AB <- ref_links_summ$A + ref_links_summ$B
#ref_links_summ$SA4_IKMH <- ref_links_summ$I + ref_links_summ$K + ref_links_summ$M + ref_links_summ$H
#ref_links_summ$SA5_D <- ref_links_summ$D
#ref_links_summ$SA6_E <- ref_links_summ$E
#ref_links_summ$SA7_F <- ref_links_summ$F
#ref_links_summ$SA8_G <- ref_links_summ$G
#ref_links_summ$SA1_JLp <- ref_links_summ$Jp + ref_links_summ$Lp
#ref_links_summ$SA2_CNp <- ref_links_summ$Cp + ref_links_summ$Np
#ref_links_summ$SA3_ABp <- ref_links_summ$Ap + ref_links_summ$Bp
#ref_links_summ$SA4_IKMHp <- ref_links_summ$Ip + ref_links_summ$Kp + ref_links_summ$Mp + ref_links_summ$Hp
#ref_links_summ$SA5_Dp <- ref_links_summ$Dp
#ref_links_summ$SA6_Ep <- ref_links_summ$Ep
#ref_links_summ$SA7_Fp <- ref_links_summ$Fp
#ref_links_summ$SA8_Gp <- ref_links_summ$Gp

# NEW after 04/22
cit_links_summ$SA1_ABG <- cit_links_summ$A + cit_links_summ$B + cit_links_summ$G
cit_links_summ$SA2_F <- cit_links_summ$F
cit_links_summ$SA3_CN <- cit_links_summ$C + cit_links_summ$N
cit_links_summ$SA4_DEJL <- cit_links_summ$D + cit_links_summ$E + cit_links_summ$J + cit_links_summ$L
cit_links_summ$SA5_HIKM <- cit_links_summ$H + cit_links_summ$I + cit_links_summ$K + cit_links_summ$M
cit_links_summ$SA1_ABGp <- cit_links_summ$Ap + cit_links_summ$Bp + cit_links_summ$Gp
cit_links_summ$SA2_Fp <- cit_links_summ$Fp
cit_links_summ$SA3_CNp <- cit_links_summ$Cp + cit_links_summ$Np
cit_links_summ$SA4_DEJLp <- cit_links_summ$Dp + cit_links_summ$Ep + cit_links_summ$Jp + cit_links_summ$Lp
cit_links_summ$SA5_HIKMp <- cit_links_summ$Hp + cit_links_summ$Ip + cit_links_summ$Kp + cit_links_summ$Mp

ref_links_summ$SA1_ABG <- ref_links_summ$A + ref_links_summ$B + ref_links_summ$G
ref_links_summ$SA2_F <- ref_links_summ$F
ref_links_summ$SA3_CN <- ref_links_summ$C + ref_links_summ$N
ref_links_summ$SA4_DEJL <- ref_links_summ$D + ref_links_summ$E + ref_links_summ$J + ref_links_summ$L
ref_links_summ$SA5_HIKM <- ref_links_summ$H + ref_links_summ$I + ref_links_summ$K + ref_links_summ$M
ref_links_summ$SA1_ABGp <- ref_links_summ$Ap + ref_links_summ$Bp + ref_links_summ$Gp
ref_links_summ$SA2_Fp <- ref_links_summ$Fp
ref_links_summ$SA3_CNp <- ref_links_summ$Cp + ref_links_summ$Np
ref_links_summ$SA4_DEJLp <- ref_links_summ$Dp + ref_links_summ$Ep + ref_links_summ$Jp + ref_links_summ$Lp
ref_links_summ$SA5_HIKMp <- ref_links_summ$Hp + ref_links_summ$Ip + ref_links_summ$Kp + ref_links_summ$Mp


cit_links_summ_publ$SA1_ABG <- cit_links_summ_publ$A + cit_links_summ_publ$B + cit_links_summ_publ$G
cit_links_summ_publ$SA2_F <- cit_links_summ_publ$F
cit_links_summ_publ$SA3_CN <- cit_links_summ_publ$C + cit_links_summ_publ$N
cit_links_summ_publ$SA4_DEJL <- cit_links_summ_publ$D + cit_links_summ_publ$E + cit_links_summ_publ$J + cit_links_summ_publ$L
cit_links_summ_publ$SA5_HIKM <- cit_links_summ_publ$H + cit_links_summ_publ$I + cit_links_summ_publ$K + cit_links_summ_publ$M
cit_links_summ_publ$SA1_ABGp <- cit_links_summ_publ$Ap + cit_links_summ_publ$Bp + cit_links_summ_publ$Gp
cit_links_summ_publ$SA2_Fp <- cit_links_summ_publ$Fp
cit_links_summ_publ$SA3_CNp <- cit_links_summ_publ$Cp + cit_links_summ_publ$Np
cit_links_summ_publ$SA4_DEJLp <- cit_links_summ_publ$Dp + cit_links_summ_publ$Ep + cit_links_summ_publ$Jp + cit_links_summ_publ$Lp
cit_links_summ_publ$SA5_HIKMp <- cit_links_summ_publ$Hp + cit_links_summ_publ$Ip + cit_links_summ_publ$Kp + cit_links_summ_publ$Mp

ref_links_summ_publ$SA1_ABG <- ref_links_summ_publ$A + ref_links_summ_publ$B + ref_links_summ_publ$G
ref_links_summ_publ$SA2_F <- ref_links_summ_publ$F
ref_links_summ_publ$SA3_CN <- ref_links_summ_publ$C + ref_links_summ_publ$N
ref_links_summ_publ$SA4_DEJL <- ref_links_summ_publ$D + ref_links_summ_publ$E + ref_links_summ_publ$J + ref_links_summ_publ$L
ref_links_summ_publ$SA5_HIKM <- ref_links_summ_publ$H + ref_links_summ_publ$I + ref_links_summ_publ$K + ref_links_summ_publ$M
ref_links_summ_publ$SA1_ABGp <- ref_links_summ_publ$Ap + ref_links_summ_publ$Bp + ref_links_summ_publ$Gp
ref_links_summ_publ$SA2_Fp <- ref_links_summ_publ$Fp
ref_links_summ_publ$SA3_CNp <- ref_links_summ_publ$Cp + ref_links_summ_publ$Np
ref_links_summ_publ$SA4_DEJLp <- ref_links_summ_publ$Dp + ref_links_summ_publ$Ep + ref_links_summ_publ$Jp + ref_links_summ_publ$Lp
ref_links_summ_publ$SA5_HIKMp <- ref_links_summ_publ$Hp + ref_links_summ_publ$Ip + ref_links_summ_publ$Kp + ref_links_summ_publ$Mp

write.csv(cit_links_summ, "E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA.csv")
write.csv(ref_links_summ, "E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA.csv")
write.csv(cit_links_summ_publ, "E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA_publ.csv")
write.csv(ref_links_summ_publ, "E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA_publ.csv")



cit_links_summ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA.csv")
ref_links_summ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA.csv")
cit_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA_publ.csv")
ref_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA_publ.csv")


cit_links_summ$total_hits_SA <- cit_links_summ$SA1_ABG +
                                cit_links_summ$SA2_F +
                                cit_links_summ$SA3_CN +
                                cit_links_summ$SA4_DEJL +
                                cit_links_summ$SA5_HIKM

ref_links_summ$total_hits_SA <- ref_links_summ$SA1_ABG +
                                ref_links_summ$SA2_F +
                                ref_links_summ$SA3_CN +
                                ref_links_summ$SA4_DEJL +
                                ref_links_summ$SA5_HIKM

cit_links_summ_publ$total_hits_SA <- cit_links_summ_publ$SA1_ABG +
  cit_links_summ_publ$SA2_F +
  cit_links_summ_publ$SA3_CN +
  cit_links_summ_publ$SA4_DEJL +
  cit_links_summ_publ$SA5_HIKM

ref_links_summ_publ$total_hits_SA <- ref_links_summ_publ$SA1_ABG +
  ref_links_summ_publ$SA2_F +
  ref_links_summ_publ$SA3_CN +
  ref_links_summ_publ$SA4_DEJL +
  ref_links_summ_publ$SA5_HIKM

colnames(ref_links_summ)
library(scales)

cit_plot <- ggplot(cit_links_summ, aes(x=year_cited)) +
  labs(y='Citations, Y') +
  geom_line(aes(y=SA1_ABG/total_hits_SA, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2_F/total_hits_SA, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3_CN/total_hits_SA, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4_DEJL/total_hits_SA, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5_HIKM/total_hits_SA, color='Humanities'),size=1) + 
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

cit_plot_publ <- ggplot(cit_links_summ_publ, aes(x=year_published)) + 
  labs(y='Citations, P') + 
  geom_line(aes(y=SA1_ABG/total_hits_SA, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2_F/total_hits_SA, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3_CN/total_hits_SA, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4_DEJL/total_hits_SA, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5_HIKM/total_hits_SA, color='Humanities'),size=1) + 
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


ref_plot <- ggplot(ref_links_summ, aes(x=year_references)) +
  labs(y='References') + 
  geom_line(aes(y=SA1_ABG/total_hits_SA, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2_F/total_hits_SA, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3_CN/total_hits_SA, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4_DEJL/total_hits_SA, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5_HIKM/total_hits_SA, color='Humanities'),size=1) + 
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


ggarrange(ref_plot + rremove("x.text"), cit_plot + rremove("x.text"), cit_plot_publ,
          ncol = 1, nrow = 3, heights = c(1.2,1,1))




# Stacked
cit_links_summ$X <- NULL
ref_links_summ$X <- NULL
cit_links_summ[,c(1,36,37,38,39,40)] %>% gather("SA", "Hits", -year_cited) -> cit_stacked_data
ref_links_summ[,c(1,36,37,38,39,40)] %>% gather("SA", "Hits", -year_references) -> ref_stacked_data



# Stacked + percent
cit_stacked_plot <- ggplot(cit_stacked_data, aes(fill=SA, y=Hits, x=year_cited)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", values = c("SA1_ABG" = "orange", 
                                                 "SA2_F" = "magenta", 
                                                 "SA3_CN" = "red", 
                                                 "SA4_DEJL" = "darkgreen", 
                                                 "SA5_HIKM" = "darkblue")) + 
  xlim(1967, 2019) +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none") +
        scale_color_hue(labels = c("Biological sciences", 
                                   "Psychological sciences", 
                                   "Medical sciences", 
                                   "Technical methods", 
                                   "Humanities"),
                        labels = c("Biological sciences", 
                                   "Psychological sciences", 
                                   "Medical sciences", 
                                   "Technical methods", 
                                   "Humanities")) + 
        ggtitle("SA hits for citations")

ref_stacked_plot <- ggplot(ref_stacked_data, aes(fill=SA, y=Hits, x=year_references)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", 
                    values = c("SA1_ABG" = "orange", 
                               "SA2_F" = "magenta", 
                               "SA3_CN" = "red", 
                               "SA4_DEJL" = "darkgreen", 
                               "SA5_HIKM" = "darkblue"),
                    labels = c("Biological sciences", 
                               "Psychological sciences", 
                               "Medical sciences", 
                               "Technical methods", 
                               "Humanities")) + 
  xlim(1967, 2019) +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "bottom") +
        ggtitle("SA hits for references")



ggarrange(ref_stacked_plot + rremove("x.text"), cit_stacked_plot, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)









# Published year grouped

cit_plot_publ <- ggplot(cit_links_summ_publ, aes(x=year_published)) + 
  labs(y='Citations') + 
  geom_line(aes(y=SA1_ABG/total_hits_SA, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2_F/total_hits_SA, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3_CN/total_hits_SA, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4_DEJL/total_hits_SA, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5_HIKM/total_hits_SA, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none") +
  scale_color_manual(name = "",       values = c("Biological sciences" = "orange", 
                                                 "Psychological sciences" = "magenta", 
                                                 "Medical sciences" = "red", 
                                                 "Technical methods" = "darkgreen", 
                                                 "Humanities"  = "darkblue"))

ref_plot_publ <- ggplot(ref_links_summ_publ, aes(x=year_published)) + 
  labs(y='References') + 
  geom_line(aes(y=SA1_ABG/total_hits_SA, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2_F/total_hits_SA, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3_CN/total_hits_SA, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4_DEJL/total_hits_SA, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5_HIKM/total_hits_SA, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "bottom") +
  scale_color_manual(name = "",       values = c("Biological sciences" = "orange", 
                                                 "Psychological sciences" = "magenta", 
                                                 "Medical sciences" = "red", 
                                                 "Technical methods" = "darkgreen", 
                                                 "Humanities"  = "darkblue"))

library(ggpubr)
ggarrange(ref_plot_publ + rremove("x.text"), cit_plot_publ, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)




# Stacked
cit_links_summ_publ$X <- NULL
ref_links_summ_publ$X <- NULL
cit_links_summ_publ[,c(1,36,37,38,39,40)] %>% gather("SA", "Hits", -year_published) -> cit_stacked_data_publ
ref_links_summ_publ[,c(1,36,37,38,39,40)] %>% gather("SA", "Hits", -year_published) -> ref_stacked_data_publ



# Stacked + percent
cit_stacked_plot_publ <- ggplot(cit_stacked_data_publ, aes(fill=SA, y=Hits, x=year_published)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", values = c("SA1_ABG" = "orange", 
                                          "SA2_F" = "magenta", 
                                          "SA3_CN" = "red", 
                                          "SA4_DEJL" = "darkgreen", 
                                          "SA5_HIKM" = "darkblue")) + 
  xlim(1967, 2019) +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none") +
  ggtitle("SA hits for citations")

ref_stacked_plot_publ <- ggplot(ref_stacked_data_publ, aes(fill=SA, y=Hits, x=year_published)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", 
                    values = c("SA1_ABG" = "orange", 
                               "SA2_F" = "magenta", 
                               "SA3_CN" = "red", 
                               "SA4_DEJL" = "darkgreen", 
                               "SA5_HIKM" = "darkblue"),
                    labels = c("Biological sciences", 
                               "Psychological sciences", 
                               "Medical sciences", 
                               "Technical methods", 
                               "Humanities")) + 
  xlim(1967, 2019) +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "bottom") +
  ggtitle("SA hits for references")



ggarrange(ref_stacked_plot_publ + rremove("x.text"), cit_stacked_plot_publ, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
