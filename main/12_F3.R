library(gplots)
library(ggplot2)
library(sjPlot)
library(MASS)
library(car)
library(ggpubr)
library(gtsummary)
library(lme4)
library(ggeffects)
library(ggplot2)

library(dplyr)
library(tidyr)
library(grid)
library(scales)
library(extrafont)

# Latest model data
setwd("E:/Research/Data_pubmed/model_data23")

model_data <- read.csv("model_data4.csv")

cog_major_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI_subjectareas.csv")
aff_major_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_major_UI_subjectareas.csv")

data_major_sa <- union(cog_major_sa, aff_major_sa)


L1_plot_nb <- ggplot(data.frame("L1" = rowSums(data_major_sa[,c(2:17)])), aes(x=L1)) + 
  geom_bar(color="black", fill="white") + 
  scale_x_continuous(name="# of MeSH",breaks=c(1:20), limits=c(0,10)) +
  scale_y_continuous(name=as.expression(bquote("# Publications  (x" ~ 10^3 ~ ")")), 
                     labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3), limits = c(0,200000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_text(size=rel(2),angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin = unit(c(0,0,1,0), 'lines')) + 
  geom_segment(aes(x = mean(L1), y = 0, xend = mean(L1), yend = 200000), linetype = 2, size = 1.2, colour = "red")

# conver hits to logical counters
model_data$pSA1 <- ifelse(model_data$pSA1 > 0, 1, 0)
model_data$pSA2 <- ifelse(model_data$pSA2 > 0, 1, 0)
model_data$pSA3 <- ifelse(model_data$pSA3 > 0, 1, 0)
model_data$pSA4 <- ifelse(model_data$pSA4 > 0, 1, 0)
model_data$pSA5 <- ifelse(model_data$pSA5 > 0, 1, 0)


SA_plot_nb <- ggplot(data.frame("SA" = rowSums(model_data[,c(8:12)])), aes(x=SA)) + 
  geom_bar(color="black", fill="white") + 
  scale_x_continuous(name="# of SA", breaks=c(1:20), limits=c(0,7)) +
  scale_y_continuous(name=as.expression(bquote("Publications with corresponding MeSH number (x" ~ 10^3 ~ ")")), 
                     labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3), limits = c(0,200000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.length = unit(.15, "cm"), axis.ticks = element_line(size = 1),
        plot.margin = unit(c(0,0,1,0), 'lines')) +
  geom_segment(aes(x = mean(SA), y = 0, xend = mean(SA), yend = 200000), linetype = 2, size = 1.2, colour = "red")



png('C:/Users/Rubinzone/Desktop/F3_hires.png', width = 8, height = 4, units = 'in', res = 300)

ggarrange(L1_plot_nb,
          SA_plot_nb,
          ncol = 2, nrow = 1,
          heights = c(1,1),
          widths = c(1.2,1))

dev.off()








# extra files: 13_F9b, 15_F6v2
png('C:/Users/Rubinzone/Desktop/FMd_5all.png', width = 14, height = 9, units = 'in', res = 300)
ggarrange(
ggarrange(
ggarrange(L1_plot_nb,
          SA_plot_nb,
          ncol = 2, nrow = 1,
          heights = c(1,1),
          widths = c(1.2,1)),
ggarrange(perc_diff  + rremove("y.ticks"),
          ncol = 1, nrow = 1),
ncol = 1, nrow = 2,
labels=c("A","B"),
font.label=list(color="black",size=15)
),
annotate_figure(ggarrange(fig2m_diff,
                          ncol = 1, nrow = 1,
                          common.legend = TRUE,
                          legend = "bottom"),
                bottom = text_grob(
                  "SA1 = Biological Sciences    SA2 = Psychological Sciences  \n  S3 = Medical Sciences    SA4 = Technical Methods  \n  SA5 = Humanities",
                  color = "black", size = 12)),
ncol =2, nrow = 1,
labels = c("","C"),
font.label=list(color="black",size=15)
)

dev.off()



