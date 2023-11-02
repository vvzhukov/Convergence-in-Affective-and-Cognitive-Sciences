library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(extrafont)

myfiles_yearly <- read.csv("E:/Research/Data_pubmed/Plots_data/all_recs_summary.csv")
data_affective_yearly <- read.csv("E:/Research/Data_pubmed/Plots_data/aff_recs_summary.csv")
data_cognitive_yearly <- read.csv("E:/Research/Data_pubmed/Plots_data/cog_recs_summary.csv")
data_cognitive_yearly_nb <- read.csv("E:/Research/Data_pubmed/Plots_data/cog_recs_summary_nobrain.csv")


data_affective_yearly <- subset(data_affective_yearly, year %in% c(1967:2019))
data_cognitive_yearly <- subset(data_cognitive_yearly, year %in% c(1967:2019))
data_cognitive_yearly_nb <- subset(data_cognitive_yearly_nb, year %in% c(1967:2019))

all <- ggplot(data = myfiles_yearly, aes(x = year, y = 100*n/sum(myfiles_yearly$n))) + 
  geom_bar(stat="identity", width=0.7, fill = "black") +
  ylim(0,5.7) +
  scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  theme_void() +
  labs(y=as.expression(bquote("Publication volume, %")), title = "PubMed") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

aff <- ggplot(data = data_affective_yearly, aes(x = year, y = 100*n/sum(data_affective_yearly$n))) + 
  geom_bar(stat="identity", width=0.7, fill = "darkviolet") +
  ylim(0,5.7) +
  scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  theme_void() +
  labs(y=as.expression(bquote("Publication volume, %")), title = "Affective") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5), angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

cog_nb <- ggplot(data = data_cognitive_yearly_nb, aes(x = year, y = 100*n/sum(data_cognitive_yearly_nb$n))) + 
  geom_bar(stat="identity", width=0.7, fill = "grey") +
  ylim(0,5.7) +
  scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  theme_void() +
  labs(y=as.expression(bquote("Publication volume, %")), title = "Cognitive") +
  theme(axis.text.x=element_text(size=rel(1.5), angle = 40,family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5), angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

png('C:/Users/Rubinzone/Desktop/F1_nobrain_hires.png', width = 8, height = 9, units = 'in', res = 300)
ggarrange(all, aff, cog_nb,
          ncol = 1, nrow = 3, heights = c(1,1,1.2))
dev.off()
