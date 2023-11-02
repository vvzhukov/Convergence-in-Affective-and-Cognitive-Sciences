library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(ggpubr)
library(TTR)

nodes_d <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")


nodes_e <- nodes_d[,c(5,6,7,8,9,10,12,13,14,15,16)]
nodes_e %>% group_by(Year_target) %>% summarise(cit_other = sum(cit_other, na.rm = T),
                                                cit_bio = sum(cit_bio, na.rm = T),
                                                cit_beh = sum(cit_beh, na.rm = T),
                                                cit_ti = sum(cit_ti, na.rm = T),
                                                cit_na = sum(cit_na, na.rm = T),
                                                ref_other = sum(ref_other, na.rm = T),
                                                ref_bio = sum(ref_bio, na.rm = T),
                                                ref_beh = sum(ref_beh, na.rm = T),
                                                ref_ti = sum(ref_ti, na.rm = T),
                                                ref_na = sum(ref_na, na.rm = T)) -> nodes_e


nodes_e$cit <- nodes_e$cit_bio + nodes_e$cit_beh + nodes_e$cit_ti + nodes_e$cit_na
nodes_e$ref <- nodes_e$ref_bio + nodes_e$ref_beh + nodes_e$ref_ti + nodes_e$ref_na

# Stacked + percent
cit_plot <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=cit_bio), color="brown",size=1) +
  geom_line(aes(y=cit_beh), color="pink",size=1) +
  geom_line(aes(y=cit_ti), color="darkgreen",size=1) +
  geom_line(aes(y=cit_na), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) +
  ggtitle("Citations",) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3), limits = c(0,200000))

cit_plot_l <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=log(cit_bio)), color="brown",size=1) +
  geom_line(aes(y=log(cit_beh)), color="pink",size=1) +
  geom_line(aes(y=log(cit_ti)), color="darkgreen",size=1) +
  geom_line(aes(y=log(cit_na)), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

ref_plot <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=ref_bio), color="brown",size=1) +
  geom_line(aes(y=ref_beh), color="pink",size=1) +
  geom_line(aes(y=ref_ti), color="darkgreen",size=1) +
  geom_line(aes(y=ref_na), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3), limits = c(0,50000)) +
  ggtitle("References")


ref_plot_l <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=log(ref_bio)), color="brown",size=1) +
  geom_line(aes(y=log(ref_beh)), color="pink",size=1) +
  geom_line(aes(y=log(ref_ti)), color="darkgreen",size=1) +
  geom_line(aes(y=log(ref_na)), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

cit_plot2 <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=cit_bio/cit), color='brown',size=1) +
  geom_line(aes(y=cit_beh/cit), color='pink',size=1) +
  geom_line(aes(y=cit_ti/cit), color='darkgreen',size=1) +
  geom_line(aes(y=cit_na/cit), color='blue',size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm"),
        legend.position = "right") +
  scale_y_continuous(labels = label_number(suffix = " ", scale = 1), limits = c(0,1)) +
  ggtitle("")


ref_plot2 <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=ref_bio/ref), color="brown",size=1) +
  geom_line(aes(y=ref_beh/ref), color="pink",size=1) +
  geom_line(aes(y=ref_ti/ref), color="darkgreen",size=1) +
  geom_line(aes(y=ref_na/ref), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) +
  ggtitle("")

cit_plots <- cit_plot + annotation_custom(ggplotGrob(cit_plot_l), xmin = 1950, xmax = 2000, 
                                          ymin = 50000, ymax = 200000)

ref_plots <- ref_plot + annotation_custom(ggplotGrob(ref_plot_l), xmin = 1950, xmax = 2000, 
                                          ymin = 10000, ymax = 52000)

ggarrange(cit_plots, ref_plots, cit_plot2, ref_plot2, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,
          common.legend = TRUE, legend="bottom")



# SMOOTHING
nodes_e$cit_bio_sma <- c(SMA(ts(nodes_e[,c(3)]/nodes_e[,c(12)],start=c(1950,1))[c(1:71)], n =8), NA)
nodes_e$cit_beh_sma <- c(SMA(ts(nodes_e[,c(4)]/nodes_e[,c(12)],start=c(1950,1))[c(1:71)], n =8), NA)
nodes_e$cit_ti_sma <- c(SMA(ts(nodes_e[,c(5)]/nodes_e[,c(12)],start=c(1950,1))[c(1:71)], n =8), NA)
nodes_e$cit_na_sma <- c(SMA(ts(nodes_e[,c(6)]/nodes_e[,c(12)],start=c(1950,1))[c(1:71)], n =8), NA)

nodes_e$ref_bio_sma <- c(NA, NA, NA, NA, SMA(ts(nodes_e[,c(8)]/nodes_e[,c(13)],start=c(1950,1))[c(4:71)], n =8))
nodes_e$ref_beh_sma <- c(NA, NA, NA, NA, SMA(ts(nodes_e[,c(9)]/nodes_e[,c(13)],start=c(1950,1))[c(4:71)], n =8))
nodes_e$ref_ti_sma <- c(NA, NA, NA, NA, SMA(ts(nodes_e[,c(10)]/nodes_e[,c(13)],start=c(1950,1))[c(4:71)], n =8))
nodes_e$ref_na_sma <- c(NA, NA, NA, NA, SMA(ts(nodes_e[,c(11)]/nodes_e[,c(13)],start=c(1950,1))[c(4:71)], n =8))


cit_plot3 <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=cit_bio_sma), color='brown',size=1) +
  geom_line(aes(y=cit_beh_sma), color='pink',size=1) +
  geom_line(aes(y=cit_ti_sma), color='darkgreen',size=1) +
  geom_line(aes(y=cit_na_sma), color='blue',size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        legend.position = "right") +
  scale_y_continuous(labels = label_number(suffix = " ", scale = 1), limits = c(0,1)) +
  ggtitle("Citations")


ref_plot3 <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=ref_bio_sma), color="brown",size=1) +
  geom_line(aes(y=ref_beh_sma), color="pink",size=1) +
  geom_line(aes(y=ref_ti_sma), color="darkgreen",size=1) +
  geom_line(aes(y=ref_na_sma), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        legend.position = "none",
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  scale_y_continuous(labels = label_number(suffix = " ", scale = 1), limits = c(0,1)) +
  ggtitle("References")

ggarrange(cit_plot3, ref_plot3, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend="bottom")