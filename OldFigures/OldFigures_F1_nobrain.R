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

all <- ggplot(data = myfiles_yearly, aes(x = year, y = n)) + 
  geom_bar(stat="identity", width=0.7, fill = "#ffffff", colour="black") +
  scale_y_continuous(labels = label_number(suffix = " ", accuracy = 0.1, scale = 1e-6), limits=c(0,1500000)) +
  scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  theme_void() +
  labs(y=as.expression(bquote("Publication volume (x" ~ 10^6 ~ ")")), title = "PubMed") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

aff <- ggplot(data = data_affective_yearly, aes(x = year, y = n)) + 
  geom_bar(stat="identity", width=0.7, fill = "#808080") +
  scale_y_continuous(labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3)) +
  scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  theme_void() +
  labs(y=as.expression(bquote("Publication volume (x" ~ 10^3 ~ ")")), title = "Affective") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5), angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))


cog <- ggplot(data = data_cognitive_yearly, aes(x = year, y = n)) + 
  geom_bar(stat="identity", width=0.7, fill = "#000000", colour="black") +
  scale_y_continuous(labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3)) +
  scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  theme_void() +
  labs(y=as.expression(bquote("Publication volume (x" ~ 10^3 ~ ")")), title = "Cognitive") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5), angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

cog_nb <- ggplot(data = data_cognitive_yearly_nb, aes(x = year, y = n)) + 
  geom_bar(stat="identity", width=0.7, fill = "#000000", colour="black") +
  scale_y_continuous(labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3)) +
  scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
  theme_void() +
  labs(y=as.expression(bquote("Publication volume (x" ~ 10^3 ~ ")")), title = "Cognitive, no brain key") +
  theme(axis.text.x=element_text(size=rel(1.5), angle = 40,family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5), angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin=unit(c(1,0.5,0.5,0.5),"cm"))

png('C:/Users/Rubinzone/Desktop/F1_nobrain_hires.png', width = 8, height = 12, units = 'in', res = 300)
ggarrange(all, aff, cog, cog_nb,
          ncol = 1, nrow = 4, heights = c(1,1,1,1.2))
dev.off()
