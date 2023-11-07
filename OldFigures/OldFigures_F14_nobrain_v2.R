library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(scales)

# IMPORTANT new coding done by colors/labels in legend, SA naming 
# in code/data is incorrect. mapping rule:
# c("SA1","SA2","SA3","SA4","SA5") <- c("SA2", "SA1", "SA4", "SA5", "SA3")
aff_cit <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_cit_ref.csv")
cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_data_cit_ref_data.csv")

data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
cog_cit_nb <- subset(cog_cit, pmid %in% data_cognitive_major_nb$pmid)
remove(data_cognitive_major_nb)

aff_cit <- aff_cit[,c("pmid","citation_count")]
cog_cit <- cog_cit[,c("pmid","citation_count")]
cog_cit_nb <- cog_cit_nb[,c("pmid","citation_count")]


aff_cit_yr <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI.csv")
cog_cit_yr <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")
aff_cit_yr <- aff_cit_yr[,c("pmid","year")]
cog_cit_yr <- cog_cit_yr[,c("pmid","year")]

aff_cit_m <- merge(aff_cit, aff_cit_yr, all.x = T)
cog_cit_m <- merge(cog_cit, cog_cit_yr, all.x = T)
cog_cit_nb_m <- merge(cog_cit_nb, cog_cit_yr, all.x = T)
remove(aff_cit, aff_cit_yr, cog_cit, cog_cit_yr, cog_cit_nb)


aff_cit_m %>% group_by(year) %>% summarise(total = sum(citation_count, na.rm = T)) -> aff_cit_summ
cog_cit_m %>% group_by(year) %>% summarise(total = sum(citation_count, na.rm = T)) -> cog_cit_summ
cog_cit_nb_m %>% group_by(year) %>% summarise(total = sum(citation_count, na.rm = T)) -> cog_cit_nb_summ

aff_cit_summ <- aff_cit_summ[aff_cit_summ$year %in% c(1950:2019),]
cog_cit_summ <- cog_cit_summ[cog_cit_summ$year %in% c(1950:2019),]
cog_cit_nb_summ <- cog_cit_nb_summ[cog_cit_nb_summ$year %in% c(1950:2019),]

data_plot <- merge(aff_cit_summ,cog_cit_summ, by.x = "year", by.y = "year")
data_plot <- merge(data_plot,cog_cit_nb_summ, by.x = "year", by.y = "year")
colnames(data_plot) <- c("year","total_aff","total_cog","total_cog_nb")


plot_cit <- ggplot(data_plot, aes(x=year)) + 
  labs(y='Citations, all time, groupped by published year \n (x10^5)') + 
  geom_line(aes(y=total_aff/10^5, color='Affective'),size=1.5) +
  geom_line(aes(y=total_cog_nb/10^5, color='Cognitive, no brain key'),size=1.5) +
  geom_line(aes(y=total_cog/10^5, color='Cognitive'),size=1.5) +
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Affective" = "#949494",
                                                 "Cognitive" = "black",
                                                 "Cognitive, no brain key" = "purple")) +
  guides(color = guide_legend(override.aes = list(size = 4)))

data_plot$total_aff_cumm <- cumsum(data_plot$total_aff)
data_plot$total_cog_cumm <- cumsum(data_plot$total_cog)
data_plot$total_cog_nb_cumm <- cumsum(data_plot$total_cog_nb)

plot_cit_cumm <- ggplot(data_plot, aes(x=year)) + 
  labs(y='Citations, all time, cumulative \n (x10^5)') + 
  geom_line(aes(y=total_aff_cumm/10^5, color='Affective'),size=1.5) +
  geom_line(aes(y=total_cog_nb_cumm/10^5, color='Cognitive, no brain key'),size=1.5) +
  geom_line(aes(y=total_cog_cumm/10^5, color='Cognitive'),size=1.5) +
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Affective" = "#949494",
                                                 "Cognitive" = "black",
                                                 "Cognitive, no brain key" = "purple")) +
  guides(color = guide_legend(override.aes = list(size = 4)))


ggarrange(plot_cit,
          plot_cit_cumm,
          common.legend = TRUE, legend="bottom")
