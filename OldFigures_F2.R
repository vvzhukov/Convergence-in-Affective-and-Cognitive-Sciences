library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(grid)

# IMPORTANT new coding done by colors/labels in legend, SA naming 
# in code/data is incorrect. mapping rule:
# c("SA1","SA2","SA3","SA4","SA5") <- c("SA2", "SA1", "SA4", "SA5", "SA3")


cit_links_summ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA.csv")
ref_links_summ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA.csv")
cit_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA_publ.csv")
ref_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA_publ.csv")
cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")
cog_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_plot.csv")

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



cog_cit %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         total = n()) -> cog_cit_summ

cog_cit_summ$total_SA_hits <- cog_cit_summ$SA1 + cog_cit_summ$SA2 + cog_cit_summ$SA3 + cog_cit_summ$SA4 + cog_cit_summ$SA5

cog_cit %>% group_by(yearP) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                          SA2 = sum(SA2, na.rm = T),
                                          SA3 = sum(SA3, na.rm = T),
                                          SA4 = sum(SA4, na.rm = T),
                                          SA5 = sum(SA5, na.rm = T),
                                          total = n()) -> cog_cit_summ2

cog_cit_summ2$total_SA_hits <- cog_cit_summ2$SA1 + cog_cit_summ2$SA2 + cog_cit_summ2$SA3 + cog_cit_summ2$SA4 + cog_cit_summ2$SA5


cog_ref %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         total = n()) -> cog_ref_summ

cog_ref_summ$total_SA_hits <- cog_ref_summ$SA1 + cog_ref_summ$SA2 + cog_ref_summ$SA3 + cog_ref_summ$SA4 + cog_ref_summ$SA5



aff_cit_plot <- ggplot(cit_links_summ, aes(x=year_cited)) +
  labs(y='Citations, Y') +
  geom_line(aes(y=SA1_ABG/total_hits_SA, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2_F/total_hits_SA, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3_CN/total_hits_SA, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4_DEJL/total_hits_SA, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5_HIKM/total_hits_SA, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Psychological sciences" = "red",
                                                 "Biological sciences" = "orange", 
                                                 "Humanities"  = "yellow2", 
                                                 "Medical sciences" = "green", 
                                                 "Technical methods" = "blue")) +  
  guides(color = guide_legend(override.aes = list(size = 4)))

aff_cit_plot_publ <- ggplot(cit_links_summ_publ, aes(x=year_published)) + 
  labs(y='Citations') + 
  geom_line(aes(y=SA1_ABG/total_hits_SA, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2_F/total_hits_SA, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3_CN/total_hits_SA, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4_DEJL/total_hits_SA, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5_HIKM/total_hits_SA, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Psychological sciences" = "red",
                                                 "Biological sciences" = "orange", 
                                                 "Humanities"  = "yellow2", 
                                                 "Medical sciences" = "green", 
                                                 "Technical methods" = "blue")) + 
  guides(color = guide_legend(override.aes = list(size = 4)))


aff_ref_plot <- ggplot(ref_links_summ, aes(x=year_references)) +
  labs(y='References') + 
  geom_line(aes(y=SA1_ABG/total_hits_SA, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2_F/total_hits_SA, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3_CN/total_hits_SA, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4_DEJL/total_hits_SA, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5_HIKM/total_hits_SA, color='Humanities'),size=1) + 
  ggtitle("Affective") +
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Psychological sciences" = "red",
                                                 "Biological sciences" = "orange", 
                                                 "Humanities"  = "yellow2", 
                                                 "Medical sciences" = "green", 
                                                 "Technical methods" = "blue")) + 
  guides(color = guide_legend(override.aes = list(size = 4)))


ref_plot <- ggplot(cog_ref_summ, aes(x=year)) +
  labs(y='References') +
  geom_line(aes(y=SA1/total_SA_hits, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2/total_SA_hits, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3/total_SA_hits, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4/total_SA_hits, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5/total_SA_hits, color='Humanities'),size=1) + 
  ggtitle("Cognitive") +
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Psychological sciences" = "red",
                                                 "Biological sciences" = "orange", 
                                                 "Humanities"  = "yellow2", 
                                                 "Medical sciences" = "green", 
                                                 "Technical methods" = "blue")) + 
  guides(color = guide_legend(override.aes = list(size = 4)))



cit_plot1 <- ggplot(cog_cit_summ, aes(x=year)) +
  labs(y='Citations, Y') +
  geom_line(aes(y=SA1/total_SA_hits, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2/total_SA_hits, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3/total_SA_hits, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4/total_SA_hits, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5/total_SA_hits, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Psychological sciences" = "red",
                                                 "Biological sciences" = "orange", 
                                                 "Humanities"  = "yellow2", 
                                                 "Medical sciences" = "green", 
                                                 "Technical methods" = "blue")) + 
  guides(color = guide_legend(override.aes = list(size = 4)))

cit_plot2 <- ggplot(cog_cit_summ2, aes(x=yearP)) +
  labs(y='Citations, P') +
  geom_line(aes(y=SA1/total_SA_hits, color='Biological sciences'),size=1) +
  geom_line(aes(y=SA2/total_SA_hits, color='Psychological sciences'),size=1) +
  geom_line(aes(y=SA3/total_SA_hits, color='Medical sciences'),size=1) +
  geom_line(aes(y=SA4/total_SA_hits, color='Technical methods'),size=1) +
  geom_line(aes(y=SA5/total_SA_hits, color='Humanities'),size=1) + 
  xlim(1967, 2019) +
  ylim(0, 0.5) + 
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Psychological sciences" = "red",
                                                 "Biological sciences" = "orange", 
                                                 "Humanities"  = "yellow2", 
                                                 "Medical sciences" = "green", 
                                                 "Technical methods" = "blue")) + 
  guides(color = guide_legend(override.aes = list(size = 4)))



png('C:/Users/Rubinzone/Desktop/F2_hires.png', width = 10, height = 8, units = 'in', res = 300)

ggarrange(aff_ref_plot + rremove("x.text") , ref_plot + rremove("x.text") + rremove("y.text"),
          aff_cit_plot_publ, cit_plot2 + rremove("y.text"),
          ncol = 2, nrow = 2,
          heights = c(1.2,1.2,1,1),
          widths = c(1.2,1,1.2,1),
          common.legend = TRUE, legend="bottom")

dev.off()