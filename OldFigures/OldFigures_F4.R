library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(extrafont)
font_import()
loadfonts(device = "win")

ref_links_summ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA.csv")
cit_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA_publ.csv")

cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")
cog_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_plot.csv")

cog_cit %>% group_by(yearP) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         A = sum(A, na.rm = T),
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
                                         total = n()) -> cog_cit_summ_publ

cog_ref %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         A = sum(A, na.rm = T),
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
                                         total = n()) -> cog_ref_summ

remove(cog_cit, cog_ref)


ref_links_summ$total_hits_L1 <- rowSums(ref_links_summ[,c(6,7,12,14)])
cit_links_summ_publ$total_hits_L1 <- rowSums(cit_links_summ_publ[,c(6,7,12,14)])
cog_ref_summ$total_hits_L1 <- rowSums(cog_ref_summ[,c(10,11,16,18)])
cog_cit_summ_publ$total_hits_L1 <- rowSums(cog_cit_summ_publ[,c(10,11,16,18)])

cit_plot_publ <- ggplot(cit_links_summ_publ, aes(x=year_published)) +
  labs(y='Citations') +  
  geom_line(aes(y=D/total_hits_L1, color='D. Drugs/Chemicals'),size=1) +
  geom_line(aes(y=E/total_hits_L1, color='E. Analitycs/Diagnostics'),size=1) +
  geom_line(aes(y=J/total_hits_L1, color='J. Technology/Industry'),size=1) +
  geom_line(aes(y=L/total_hits_L1, color='L. Information Science'),size=1) +
  xlim(1967, 2019) +
  ylim(0, 0.9) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        #legend.spacing.x = unit(0.7, 'cm'),
        legend.text = element_text(size=rel(1),family="NimbusMon", margin = margin(r = 30, unit = "pt"))) +
  scale_color_manual(name = "",       values = c("D. Drugs/Chemicals" = "lightblue1", 
                                                 "E. Analitycs/Diagnostics" = "steelblue2", 
                                                 "J. Technology/Industry" = "royalblue2", 
                                                 "L. Information Science"  = "navyblue"))+  
  guides(color = guide_legend(override.aes = list(size = 4)))


ref_plot <- ggplot(ref_links_summ, aes(x=year_references)) +
  labs(y='References') +  
  geom_line(aes(y=D/total_hits_L1, color='D. Drugs/Chemicals'),size=1) +
  geom_line(aes(y=E/total_hits_L1, color='E. Analitycs/Diagnostics'),size=1) +
  geom_line(aes(y=J/total_hits_L1, color='J. Technology/Industry'),size=1) +
  geom_line(aes(y=L/total_hits_L1, color='L. Information Science'),size=1) +
  ggtitle("Affective") +
  xlim(1967, 2019) +
  ylim(0, 0.9) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        #legend.spacing.x = unit(0.7, 'cm'),
        legend.text = element_text(size=rel(1),family="NimbusMon", margin = margin(r = 30, unit = "pt"))) +
  scale_color_manual(name = "",       values = c("D. Drugs/Chemicals" = "lightblue1", 
                                                 "E. Analitycs/Diagnostics" = "steelblue2", 
                                                 "J. Technology/Industry" = "royalblue2", 
                                                 "L. Information Science"  = "navyblue"))+    
  guides(color = guide_legend(override.aes = list(size = 4)))

cog_cit_plot_publ <- ggplot(cog_cit_summ_publ, aes(x=yearP)) +
  geom_line(aes(y=D/total_hits_L1, color='D. Drugs/Chemicals'),size=1) +
  geom_line(aes(y=E/total_hits_L1, color='E. Analitycs/Diagnostics'),size=1) +
  geom_line(aes(y=J/total_hits_L1, color='J. Technology/Industry'),size=1) +
  geom_line(aes(y=L/total_hits_L1, color='L. Information Science'),size=1) +
  xlim(1967, 2019) +
  ylim(0, 0.9) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        #legend.spacing.x = unit(0.7, 'cm'),
        legend.text = element_text(size=rel(1),family="NimbusMon", margin = margin(r = 30, unit = "pt"))) +
  scale_color_manual(name = "",       values = c("D. Drugs/Chemicals" = "lightblue1", 
                                                 "E. Analitycs/Diagnostics" = "steelblue2", 
                                                 "J. Technology/Industry" = "royalblue2", 
                                                 "L. Information Science"  = "navyblue"))+  
  guides(color = guide_legend(override.aes = list(size = 4)))


cog_ref_plot <- ggplot(cog_ref_summ, aes(x=year)) +
  geom_line(aes(y=D/total_hits_L1, color='D. Drugs/Chemicals'),size=1) +
  geom_line(aes(y=E/total_hits_L1, color='E. Analitycs/Diagnostics'),size=1) +
  geom_line(aes(y=J/total_hits_L1, color='J. Technology/Industry'),size=1) +
  geom_line(aes(y=L/total_hits_L1, color='L. Information Science'),size=1) +
  ggtitle("Cognitive") +
  xlim(1967, 2019) +
  ylim(0, 0.9) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        #legend.spacing.x = unit(0.7, 'cm'),
        legend.text = element_text(size=rel(1),family="NimbusMon", margin = margin(r = 30, unit = "pt"))) +
  scale_color_manual(name = "",       values = c("D. Drugs/Chemicals" = "lightblue1", 
                                                 "E. Analitycs/Diagnostics" = "steelblue2", 
                                                 "J. Technology/Industry" = "royalblue2", 
                                                 "L. Information Science"  = "navyblue"))+  
  guides(color = guide_legend(override.aes = list(size = 4)))


png('C:/Users/Rubinzone/Desktop/F4_hires.png', width = 10, height = 8, units = 'in', res = 300)

ggarrange(ref_plot + rremove("x.text") , cog_ref_plot + rremove("x.text") + rremove("y.text"),
          cit_plot_publ, cog_cit_plot_publ + rremove("y.text"),
          ncol = 2, nrow = 2,
          heights = c(1.2,1.2,1,1),
          widths = c(1.2,1,1.2,1),
          common.legend = TRUE, legend="bottom")

dev.off()




