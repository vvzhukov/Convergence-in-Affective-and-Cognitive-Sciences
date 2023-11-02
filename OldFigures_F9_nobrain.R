library(ggplot2)
require(scales)
library(ggpubr)
library(extrafont)
font_import()
loadfonts(device = "win")


df <- data.frame(sas = c("SA1", "SA2", "SA3", "SA4", "SA5"),
          colour = c("red","orange", "yellow2", "green", "blue"),
          aff = c(237816, 89511, 80337, 118501, 105037), #affective
          names = c("Psychological sciences","Biological sciences", "Humanities", "Medical sciences", "Technical methods"),
          cog = c(297537, 373365, 75037, 187267, 315548 ),#cognitive
          ieee = c(755, 955, 389, 354, 1220), #ieee
          cog_nb=c(287576, 178038, 115733, 132951, 195443)) #cognitive, no brain key

df$aff <- df$aff * -1
df$hjust <- 1.3


ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "") +
  geom_bar(stat = "identity",aes(aff,sas,fill = colour), width = 0.7) +
  geom_bar(stat = "identity",aes(cog,sas,fill = colour), width = 0.7) +
  scale_x_continuous(labels = comma, limits = c(-400000,400000)) +
  scale_fill_manual(name = "", values = c("red" = "red", 
                                          "orange" = "orange", 
                                          "yellow2" = "yellow2", 
                                          "green" = "green", 
                                          "blue" = "blue"),
                    labels = c("Psychological sciences",
                               "Biological sciences", 
                               "Humanities", 
                               "Medical sciences", 
                               "Technical methods")) +
  geom_vline(xintercept = 0) + 
  theme(legend.position="bottom")

#---- 


a <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "Cognitive") +
  geom_bar(stat = "identity",aes(cog*100/sum(df$cog),sas,fill = colour), width = 0.7) +
  scale_x_continuous(labels = comma, limits = c(0,40)) +
  scale_fill_manual(name = "", values = c("red" = "red", 
                                          "orange" = "orange", 
                                          "yellow2" = "yellow2", 
                                          "green" = "green", 
                                          "blue" = "blue"),
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
        legend.position="bottom")

a_nb <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "Cognitive, no brain key") +
  geom_bar(stat = "identity",aes(cog_nb*100/sum(df$cog_nb),sas,fill = colour), width = 0.7) +
  scale_x_continuous(labels = comma, limits = c(0,40)) +
  scale_fill_manual(name = "", values = c("red" = "red", 
                                                "orange" = "orange", 
                                                "yellow2" = "yellow2", 
                                                "green" = "green", 
                                                "blue" = "blue"),
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
        legend.position="bottom")

b <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "Affective") +
  geom_bar(stat = "identity",aes(aff*100/sum(df$aff),sas,fill = colour), width = 0.7) +
  scale_x_continuous(labels = comma, limits = c(0,40)) +
  scale_fill_manual(name = "", values = c("red" = "red", 
                                          "orange" = "orange", 
                                          "yellow2" = "yellow2", 
                                          "green" = "green", 
                                          "blue" = "blue"),
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
        legend.position="bottom")


c <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "IEEE") +
  geom_bar(stat = "identity",aes(ieee*100/sum(df$ieee),sas,fill = colour), width = 0.7) +
  scale_x_continuous(labels = comma, limits = c(0,40)) +
  scale_fill_manual(name = "", values = c("red" = "red", 
                                          "orange" = "orange", 
                                          "yellow2" = "yellow2", 
                                          "green" = "green", 
                                          "blue" = "blue"),
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
        legend.position="bottom")

png('C:/Users/Rubinzone/Desktop/F9b.png', width = 16, height = 4, units = 'in', res = 300)

ggarrange(b,
          a + rremove("y.text") + rremove("y.ticks"),
          a_nb + rremove("y.text") + rremove("y.ticks"),
          c + rremove("y.text") + rremove("y.ticks"),
          ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom",
          heights = c(1,1,1),
          widths = c(1.2,1,1))

dev.off()