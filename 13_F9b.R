library(ggplot2)
require(scales)
library(ggpubr)
library(extrafont)
font_import()
loadfonts(device = "win")


setwd("E:/Research/Data_pubmed/model_data23")

model_data <- read.csv("model_data4.csv")



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


#---- 





a <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "Affective [%]") +
  geom_bar(stat = "identity",aes(-aff*100/sum(df$aff),sas,fill = sas), width = 0.7) +
  scale_x_continuous(labels = c(40,30,20,10,0), limits = c(-40,0)) +
  scale_fill_manual(name = "", values = c("SA1" = "red", 
                                          "SA2" = "orange", 
                                          "SA3" = "yellow2", 
                                          "SA4" = "green", 
                                          "SA5" = "blue")) +
  #geom_vline(xintercept = 0) + 
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(2),family="NimbusMon"),
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(2), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position="none",
        
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


c <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "Cognitive [%]") +
  geom_bar(stat = "identity",aes(cog*100/sum(df$cog),sas,fill = sas), width = 0.7) +
  scale_x_continuous(labels = comma, limits = c(0,40)) +
  scale_x_continuous(labels = comma, limits = c(0,40)) +
  scale_fill_manual(name = "", values = c("SA1" = "red", 
                                          "SA2" = "orange", 
                                          "SA3" = "yellow2", 
                                          "SA4" = "green", 
                                          "SA5" = "blue")) +
  #geom_vline(xintercept = 0) + 
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(2),family="NimbusMon"),
        axis.ticks.length.y = unit(1, "cm"),
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(2), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0),"cm"),
        legend.position="none",
        
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


png('C:/Users/Rubinzone/Desktop/F9b.png', width = 8, height = 4, units = 'in', res = 300)
ggarrange(a + rremove("y.text") + rremove("y.ticks"),
          c  + rremove("y.ticks"),
          ncol = 2, nrow = 1,
          heights = c(1,1),
          widths = c(1,1.2))
dev.off()




#---- 

df$aff <- -df$aff

raw_diff <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "Affective # - Cognitive #") +
  geom_bar(stat = "identity",aes(aff+cog,sas,fill = sas), width = 0.7) +
  #geom_bar(stat = "identity",aes(cog,sas,fill = colour), width = 0.7) +
  scale_x_continuous(labels = comma, limits = c(-60000,60000)) +
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
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(2), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position="none",
        
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))





perc_diff <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "Affective [%] - Cognitive [%]") +
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
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(2), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position="none",
        
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



png('C:/Users/Rubinzone/Desktop/F9b_diff.png', width = 5, height = 4, units = 'in', res = 300)
ggarrange(perc_diff  + rremove("y.ticks"),
          ncol = 1, nrow = 1)
dev.off()
#---- 