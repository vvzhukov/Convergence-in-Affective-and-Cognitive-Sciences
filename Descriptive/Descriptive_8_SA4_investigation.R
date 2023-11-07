library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

cit_links_summ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA.csv")
ref_links_summ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA.csv")
cit_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA_publ.csv")
ref_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA_publ.csv")

cit_links_summ$total_hits_L1 <- rowSums(cit_links_summ[,c(5,6,11,13)])

ref_links_summ$total_hits_L1 <- rowSums(ref_links_summ[,c(5,6,11,13)])

cit_links_summ_publ$total_hits_L1 <- rowSums(cit_links_summ_publ[,c(5,6,11,13)])

ref_links_summ_publ$total_hits_L1 <- rowSums(ref_links_summ_publ[,c(5,6,11,13)])

colnames(ref_links_summ)

cit_plot <- ggplot(cit_links_summ, aes(x=year_cited)) +
  labs(y='Citations, Y') + 
  geom_line(aes(y=D/total_hits_L1, color='D. Drugs and chemicals'),size=1) +
  geom_line(aes(y=E/total_hits_L1, color='E. Analitical, Diagnostics and Therap...'),size=1) +
  geom_line(aes(y=J/total_hits_L1, color='J. Technology, Industry, Agriculture'),size=1) +
  geom_line(aes(y=L/total_hits_L1, color='L. Information Science'),size=1) +
  xlim(1967, 2019) +
  ylim(0, 0.7) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",       values = c("D. Drugs and chemicals" = "orange", 
                                                 "E. Analitical, Diagnostics and Therap..." = "magenta", 
                                                 "J. Technology, Industry, Agriculture" = "darkgreen", 
                                                 "L. Information Science"  = "darkblue"))

cit_plot_publ <- ggplot(cit_links_summ_publ, aes(x=year_published)) +
  labs(y='Citations, P') +  
  geom_line(aes(y=D/total_hits_L1, color='D. Drugs and chemicals'),size=1) +
  geom_line(aes(y=E/total_hits_L1, color='E. Analitical, Diagnostics and Therap...'),size=1) +
  geom_line(aes(y=J/total_hits_L1, color='J. Technology, Industry, Agriculture'),size=1) +
  geom_line(aes(y=L/total_hits_L1, color='L. Information Science'),size=1) +
  xlim(1967, 2019) +
  ylim(0, 0.7) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",       values = c("D. Drugs and chemicals" = "orange", 
                                                 "E. Analitical, Diagnostics and Therap..." = "magenta", 
                                                 "J. Technology, Industry, Agriculture" = "darkgreen", 
                                                 "L. Information Science"  = "darkblue"))

ref_plot <- ggplot(ref_links_summ, aes(x=year_references)) +
  labs(y='References') + 
  geom_line(aes(y=D/total_hits_L1, color='D. Drugs and chemicals'),size=1) +
  geom_line(aes(y=E/total_hits_L1, color='E. Analitical, Diagnostics'),size=1) +
  geom_line(aes(y=J/total_hits_L1, color='J. Technology, Industry, Agriculture'),size=1) +
  geom_line(aes(y=L/total_hits_L1, color='L. Information Science'),size=1) +
  xlim(1967, 2019) +
  ylim(0, 0.7) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "top",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",      values = c("D. Drugs and chemicals" = "orange", 
                                                "E. Analitical, Diagnostics" = "magenta", 
                                                "J. Technology, Industry, Agriculture" = "darkgreen", 
                                                "L. Information Science"  = "darkblue"))


ggarrange(ref_plot + rremove("x.text"), cit_plot + rremove("x.text"), cit_plot_publ,
          ncol = 1, nrow = 3, heights = c(1.2,1,1))


