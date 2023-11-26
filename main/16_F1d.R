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

model_data$pType <- as.factor(model_data$pType)
model_data$pX_disc <- as.factor(model_data$pX_disc)



pType <- as.factor(model_data$pType)
pAuthors_n <- model_data$pAuthors_n
logpAuthors_n <- log(pAuthors_n)
pDiversity <- model_data$pDiversity
cDiversity <- model_data$cDiversity
pEpoch5 <- model_data$pEpoch5
citation_count_n <- model_data$citation_count_n


# MeSH L1 data
cog_major_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI_subjectareas.csv")
aff_major_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_major_UI_subjectareas.csv")
data_major_sa <- union(cog_major_sa, aff_major_sa)



# Summary data
df <- data.frame(sas = c("SA1", "SA2", "SA3", "SA4", "SA5"),
                 colour = c("red","orange", "yellow2", "green", "blue"),
                 aff = c(sum(subset(model_data, pType == "Affective")$pSA1), 
                         sum(subset(model_data, pType == "Affective")$pSA2), 
                         sum(subset(model_data, pType == "Affective")$pSA3), 
                         sum(subset(model_data, pType == "Affective")$pSA4), 
                         sum(subset(model_data, pType == "Affective")$pSA5)), #affective
                 names = c("Psychological sciences","Biological sciences", "Humanities", "Medical sciences", "Technical methods"),
                 cog = c(sum(subset(model_data, pType == "Cognitive")$pSA1), 
                         sum(subset(model_data, pType == "Cognitive")$pSA2), 
                         sum(subset(model_data, pType == "Cognitive")$pSA3), 
                         sum(subset(model_data, pType == "Cognitive")$pSA4), 
                         sum(subset(model_data, pType == "Cognitive")$pSA5))) #cognitive, no brain key



# Data descriptive
p_div <- ggplot(model_data, aes(y=pDiversity)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  labs(y="", title = expression(paste(italic("D"["p"])))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x= element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y= element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(1)),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(2,0.5,2.5,0.5), 'lines'))

c_div <- ggplot(model_data, aes(y=cDiversity)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() + 
  labs(y="", title = expression(paste(italic(bar("D")["C"])))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x= element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y= element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(2,0.5,2.5,0.5), 'lines'))

desc_4 <- ggplot(subset(model_data,pType=='Affective'), aes(x=pEpoch5)) +
  geom_histogram(stat="count", fill = "black") +
  theme_bw() +
  labs(y="", title = "# Affective Publciations") +
  scale_x_continuous(breaks=seq(1,14,by=1)) +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=rel(1.5)),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.title.x=element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,2.5,0.5), 'lines'))

desc_5 <- ggplot(subset(model_data,pType=='Cognitive'), aes(x=pEpoch5)) +
  geom_histogram(stat="count", fill = "gray") +
  theme_bw() +
  labs(y="", title = "# Cognitive Publciations") +
  scale_x_continuous(breaks=seq(1,14,by=1)) +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=rel(1.5)),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_blank(),
        axis.title.y=element_text(size=rel(2)),
        axis.title.x=element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,2.5,0.5), 'lines'))

fm2_1n <- ggplot() +
  stat_ecdf(aes(model_data$pCitations_n_norm), geom = "step", linewidth = 1.5, color = "black") +
  labs(y= 'CDF', title=expression(paste(italic("C")[italic("N")]))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_text(size=rel(1.5),angle=90,family="NimbusMon", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(1.5),family="NimbusMon"),
        plot.margin = unit(c(2,0.5,2.5,0.5), 'lines'))

fm2_2n <- ggplot() +
  stat_ecdf(aes(log(model_data$pAuthors_n)), geom = "step", linewidth = 1.5, color = "black") +
  labs(y= 'CDF', title=expression(paste("ln",italic("A")))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_text(size=rel(1.5),angle=90,family="NimbusMon", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(1.5),family="NimbusMon"),
        plot.margin = unit(c(2,0.5,2.5,0.5), 'lines'))





L1_plot_nb <- ggplot(data.frame("L1" = rowSums(data_major_sa[,c(2:17)])), aes(x=L1)) + 
  geom_bar(color="black", fill="white") + 
  #geom_density(color="black", fill="grey") +
  scale_x_continuous(name="# Major MeSH", breaks=c(1:20), limits=c(0,10)) +
  scale_y_continuous(name=as.expression(bquote("PDF")), 
                     breaks=c(0,50000,100000,150000,200000),
                     labels=c("0","0.08","0.16","0.24","0.32"),
                     limits = c(0,200000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3), margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.y=element_text(size=rel(2),angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 5, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin = unit(c(0,0,2.5,3), 'lines')) + 
  geom_segment(aes(x = mean(L1), y = 0, xend = mean(L1), yend = 200000), linetype = 2, size = 1.2, colour = "red")

# conver hits to logical counters
model_data$pbSA1 <- ifelse(model_data$pSA1 > 0, 1, 0)
model_data$pbSA2 <- ifelse(model_data$pSA2 > 0, 1, 0)
model_data$pbSA3 <- ifelse(model_data$pSA3 > 0, 1, 0)
model_data$pbSA4 <- ifelse(model_data$pSA4 > 0, 1, 0)
model_data$pbSA5 <- ifelse(model_data$pSA5 > 0, 1, 0)

SA_plot_nb <- ggplot(data.frame("SA" = rowSums(model_data[,c('pbSA1','pbSA2','pbSA3','pbSA4','pbSA5')])), aes(x=SA)) + 
  geom_bar(color="black", fill="white") + 
  scale_x_continuous(name="# SA", breaks=c(1:20), limits=c(0,7)) +
  scale_y_continuous(name=as.expression(bquote("PDF")), 
                     labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3), limits = c(0,200000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.length = unit(.15, "cm"), axis.ticks = element_line(size = 1),
        plot.margin = unit(c(0,0,2.5,0), 'lines')) +
  geom_segment(aes(x = mean(SA), y = 0, xend = mean(SA), yend = 200000), linetype = 2, size = 1.2, colour = "red")


perc_diff <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "") +
  xlab("Affective [%] - Cognitive [%]") +
  geom_bar(stat = "identity",aes(aff*100/sum(df$aff)-cog*100/sum(df$cog),sas,fill = sas), width = 0.7) +
  #geom_bar(stat = "identity",aes(cog,sas,fill = colour), width = 0.7) +
  scale_x_continuous(labels = comma, limits = c(-10,10)) +
  scale_fill_manual(name = "", values = c("SA1" = "red", 
                                          "SA2" = "orange", 
                                          "SA3" = "yellow2", 
                                          "SA4" = "green", 
                                          "SA5" = "blue"),
                    labels = c("Psychological sciences",
                               "Biological sciences", 
                               "Humanities", 
                               "Medical sciences", 
                               "Technical methods")) +
  geom_vline(xintercept = 0) + 
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(2),family="NimbusMon"),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = rel(2), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        legend.position="none",
        
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Use file 15_F6v2.R to generate fig2m_diff 
source('E:/Research/Scripts_pubmed/15_F6v2.R')






png('C:/Users/Rubinzone/Desktop/F1d.png', width = 22, height = 12, units = 'in', res = 300)
ggarrange(
ggarrange(desc_4, desc_5,
          ggarrange(fm2_1n + rremove("x.title"), fm2_2n + rremove("y.title") + rremove("x.title"), 
                    ncol = 2, nrow = 1, widths = c(1.2,1)),
          ggarrange(p_div, c_div,
                    ncol = 2, nrow = 1, widths = c(1.2,1)),
          ncol = 4, nrow = 1,
          widths = c(1.2,1,1,1),
          labels = c("A", "", "B", "C"),
          font.label=list(color="black",size=30)),
ggarrange(
    ggarrange(L1_plot_nb,
              SA_plot_nb,
              ncol = 2, nrow = 1,
              heights = c(1,1),
              widths = c(1.2,1)),
    perc_diff  + rremove("y.ticks"),
  annotate_figure(ggarrange(fig2m_diff+theme(legend.position = "bottom", legend.direction = "horizontal"),
                            ncol = 1, nrow = 1,
                            common.legend = TRUE,
                            legend = "bottom"),
                  #bottom = text_grob(
                  #  "SA1 = Biological Sciences    SA2 = Psychological Sciences  \n  S3 = Medical Sciences    SA4 = Technical Methods  \n  SA5 = Humanities",
                  #  color = "black", size = 12)
                  ),
  ncol = 3, nrow = 1,
  labels = c("D","E","F"),
  widths = c(1.2,1.2,1),
  font.label=list(color="black",size=30)
),
ncol = 1, 
nrow = 2
)

dev.off()
