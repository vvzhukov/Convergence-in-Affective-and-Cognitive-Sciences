
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(scales)
library(ggpubr)
library(extrafont)

data_cognitive_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
data_cognitive_subj_nb <- subset(data_cognitive_subj, pmid %in% data_cognitive_major_nb$pmid)

cog_subj_bool <- data_cognitive_subj
cog_subj_bool$A <- as.numeric(cog_subj_bool$A >0)
cog_subj_bool$B <- as.numeric(cog_subj_bool$B >0)
cog_subj_bool$C <- as.numeric(cog_subj_bool$C >0)
cog_subj_bool$D <- as.numeric(cog_subj_bool$D >0)
cog_subj_bool$E <- as.numeric(cog_subj_bool$E >0)
cog_subj_bool$F <- as.numeric(cog_subj_bool$F >0)
cog_subj_bool$G <- as.numeric(cog_subj_bool$G >0)
cog_subj_bool$H <- as.numeric(cog_subj_bool$H >0)
cog_subj_bool$I <- as.numeric(cog_subj_bool$I >0)
cog_subj_bool$J <- as.numeric(cog_subj_bool$J >0)
cog_subj_bool$K <- as.numeric(cog_subj_bool$K >0)
cog_subj_bool$L <- as.numeric(cog_subj_bool$L >0)
cog_subj_bool$M <- as.numeric(cog_subj_bool$M >0)
cog_subj_bool$N <- as.numeric(cog_subj_bool$N >0)
cog_subj_bool$V <- as.numeric(cog_subj_bool$V >0)
cog_subj_bool$Z <- as.numeric(cog_subj_bool$Z >0)

cog_subj_bool$SA1 <- as.numeric((cog_subj_bool$A + cog_subj_bool$B + cog_subj_bool$G) > 0)
cog_subj_bool$SA2 <- as.numeric((cog_subj_bool$F) >0)
cog_subj_bool$SA3 <- as.numeric((cog_subj_bool$C + cog_subj_bool$N) >0)
cog_subj_bool$SA4 <- as.numeric((cog_subj_bool$D + cog_subj_bool$E + cog_subj_bool$J + cog_subj_bool$L) >0)
cog_subj_bool$SA5 <- as.numeric((cog_subj_bool$H + cog_subj_bool$I + cog_subj_bool$K + cog_subj_bool$M) >0)


cog_subj_bool_nb <- data_cognitive_subj_nb
cog_subj_bool_nb$A <- as.numeric(cog_subj_bool_nb$A >0)
cog_subj_bool_nb$B <- as.numeric(cog_subj_bool_nb$B >0)
cog_subj_bool_nb$C <- as.numeric(cog_subj_bool_nb$C >0)
cog_subj_bool_nb$D <- as.numeric(cog_subj_bool_nb$D >0)
cog_subj_bool_nb$E <- as.numeric(cog_subj_bool_nb$E >0)
cog_subj_bool_nb$F <- as.numeric(cog_subj_bool_nb$F >0)
cog_subj_bool_nb$G <- as.numeric(cog_subj_bool_nb$G >0)
cog_subj_bool_nb$H <- as.numeric(cog_subj_bool_nb$H >0)
cog_subj_bool_nb$I <- as.numeric(cog_subj_bool_nb$I >0)
cog_subj_bool_nb$J <- as.numeric(cog_subj_bool_nb$J >0)
cog_subj_bool_nb$K <- as.numeric(cog_subj_bool_nb$K >0)
cog_subj_bool_nb$L <- as.numeric(cog_subj_bool_nb$L >0)
cog_subj_bool_nb$M <- as.numeric(cog_subj_bool_nb$M >0)
cog_subj_bool_nb$N <- as.numeric(cog_subj_bool_nb$N >0)
cog_subj_bool_nb$V <- as.numeric(cog_subj_bool_nb$V >0)
cog_subj_bool_nb$Z <- as.numeric(cog_subj_bool_nb$Z >0)

cog_subj_bool_nb$SA1 <- as.numeric((cog_subj_bool_nb$A + cog_subj_bool_nb$B + cog_subj_bool_nb$G) > 0)
cog_subj_bool_nb$SA2 <- as.numeric((cog_subj_bool_nb$F) >0)
cog_subj_bool_nb$SA3 <- as.numeric((cog_subj_bool_nb$C + cog_subj_bool_nb$N) >0)
cog_subj_bool_nb$SA4 <- as.numeric((cog_subj_bool_nb$D + cog_subj_bool_nb$E + cog_subj_bool_nb$J + cog_subj_bool_nb$L) >0)
cog_subj_bool_nb$SA5 <- as.numeric((cog_subj_bool_nb$H + cog_subj_bool_nb$I + cog_subj_bool_nb$K + cog_subj_bool_nb$M) >0)


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


cL1_plot_nb <- ggplot(data.frame("L1" = rowSums(cog_subj_bool_nb[,c(2:17)])), aes(x=L1)) + 
  geom_bar(color="grey", fill="grey") + 
  scale_x_continuous(name="MeSH",breaks=c(1:20), limits=c(0,10)) +
  scale_y_continuous(name=as.expression(bquote("Cognitive pub. (x" ~ 10^3 ~ ")")), labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3), limits = c(0,120000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_text(size=rel(2),angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon")) +
  geom_segment(aes(x = mean(L1), y = 0, xend = mean(L1), yend = 120000), linetype = 2, size = 0.8, colour = "red")


cSA_plot_nb <- ggplot(data.frame("SA" = rowSums(cog_subj_bool_nb[,c(18:22)])), aes(x=SA)) + 
  geom_bar(color="grey", fill="grey") + 
  scale_x_continuous(name="SA", breaks=c(1:20), limits=c(0,10)) +
  scale_y_continuous(name=as.expression(bquote("Publications with corresponding MeSH number (x" ~ 10^3 ~ ")")), labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3), limits = c(0,120000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.length = unit(.15, "cm"), axis.ticks = element_line(size = 1)) +
  geom_segment(aes(x = mean(SA), y = 0, xend = mean(SA), yend = 120000), linetype = 2, size = 0.8, colour = "red")


aL1_plot <- ggplot(data.frame("L1" = rowSums(aff_subj_bool[,c(2:17)])), aes(x=L1)) + 
  geom_bar(color="darkviolet", fill="darkviolet") + 
  scale_x_continuous(breaks=c(1:20), limits=c(0,10)) +
  scale_y_continuous(name=as.expression(bquote("Affective pub. (x" ~ 10^3 ~ ")")), labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3), limits = c(0,120000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3), family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.3), family="NimbusMon"),
        axis.title.y=element_text(size=rel(2),angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon")) +
  geom_segment(aes(x = mean(L1), y = 0, xend = mean(L1), yend = 120000), linetype = 2, size = 0.8, colour = "red")


aSA_plot <- ggplot(data.frame("SA" = rowSums(aff_subj_bool[,c(18:22)])), aes(x=SA)) + 
  geom_bar(color="darkviolet", fill="darkviolet") + 
  scale_x_continuous(name="Subject Areas (SA) per publication", breaks=c(1:20), limits=c(0,10)) +
  scale_y_continuous(name=as.expression(bquote("Cognitive pub. (x" ~ 10^3 ~ ")")), labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3), limits = c(0,120000)) +
  theme_void() +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.length = unit(.15, "cm"), axis.ticks = element_line(size = 1)) +
  geom_segment(aes(x = mean(SA), y = 0, xend = mean(SA), yend = 120000), linetype = 2, size = 0.8, colour = "red")




png('C:/Users/Rubinzone/Desktop/F3_hires.png', width = 8, height = 8, units = 'in', res = 300)

ggarrange(aL1_plot + rremove("x.text"),
          aSA_plot + rremove("y.text") + rremove("x.text"),
          cL1_plot_nb,
          cSA_plot_nb,
          ncol = 2, nrow = 2,
          heights = c(1,1,1.2,1.2),
          widths = c(1.2,1,1.2,1))

dev.off()

