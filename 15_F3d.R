
setwd("E:/Research/Data_pubmed/model_data23")
model_data <- read.csv("model_data4.csv")



library(ggplot2)
library(dplyr)
library(bayestestR)
library(ggh4x)
library(ggpubr)

png('C:/Users/Rubinzone/Desktop/F3d_pdf_Cn.png', width = 20, height = 10, units = 'in', res = 300)

ggarrange(

subset(model_data, pYear %in% c(1950:2022)) %>%
  ggplot(aes(pCitations_n_norm)) +                     # Plot the values
  ggh4x::facet_wrap2(~ pYear, 
             ncol = 10,
             axes = "all", 
             remove_labels = "all") +   # In separate panels
  geom_density(size = 1, col = "lightgreen", fill = "lightgreen") +                         # as density
  stat_function(fun = dnorm, args = list(mean = 0),col="black",linetype = "dashed", size = 1) + # ideal norm
  geom_vline(xintercept = 0, size = 1) +
  xlim(c(-3,3)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.4), oob = scales::oob_keep) +
  ylab(expression("PDF, " ~ italic(P) ~ "(" ~ italic(C)[italic(N)] ~"| {"~ italic(t)~ "})")) +
  xlab(expression("Normalized citation impact" ~ italic(C)[italic(N)])) +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_text(size=rel(2.5),family="NimbusMon", margin = margin(t = 10, unit = "pt")),
        axis.title.y=element_text(size=rel(2.5),family="NimbusMon"),
        plot.title = element_text(size=rel(3),hjust = 0.5, family="NimbusMon"),
        legend.title = element_text(size=rel(1.5), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
        legend.text = element_text(size=rel(1.5),family="NimbusMon"),
        strip.text.x = element_text(size=rel(1.2),family="NimbusMon"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines'))
)

dev.off()
