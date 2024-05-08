library(sjPlot)
library(nnet)
library(ggplot2)
library(ggpubr)
library(ggeffects)

model_data <- read.csv("E:/Research/Data_pubmed/model_data23/model_data2_13_2024.csv")
model_data[model_data$pType=="Intersect",]$pType <- "Mixed"

# MeSH L1 data
cog_major_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI_subjectareas.csv")
aff_major_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_major_UI_subjectareas.csv")
data_major_sa <- rbind(cog_major_sa, aff_major_sa)

# model from Report211.pptx, page #3, right
model1 <- lm(citation_count_n ~ 
                pType + 
                log(pAuthors_n) + 
                pDiversity +
                cDiversity +
                pSA1b + pSA2b + pSA3b + pSA4b + pSA5b, 
              data = model_data)

summary(model1)


png('C:/Users/Rubinzone/Desktop/Model_diag_suppl.png', width = 12, height = 12, units = 'in', res = 300)
par(mfrow=c(2,2))
plot(model1)
dev.off()



dat1 <- ggpredict(model1, "pDiversity [0:1]")
dat1$group <- 'Dp'
dat2 <- ggpredict(model1, "cDiversity [0:1]")
dat2$group <- 'Dc'
dat0 <- rbind(dat1, dat2)

palette("R3")

png('C:/Users/Rubinzone/Desktop/Figure3_v24.png', width = 16, height = 4, units = 'in', res = 300)
ggarrange(
  plot_model(model1, 
             type = "pred", 
             terms = "pType",
             dot.size = 6,
             line.size = 2,
             title = expression(paste(italic("T")))) + 
    theme_bw() +
    aes(color = c("Mixed", "Cognitive", "Affective")) +
    scale_color_manual(labels = c("Mixed", "Cognitive", "Affective"), 
                       values = c("black", "gray","darkviolet")) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(1.5)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(0.5,3,0.5,0.5), 'lines')),
  
  plot_model(model1, 
             type = "pred", 
             terms = "pAuthors_n",
             dot.size = 6,
             line.size = 1.2,
             title = expression(paste("ln",italic("A")))) + 
    theme_bw() +
    xlim(c(0,20)) +
    ylim(c(0,0.8)) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(2)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')
    ),
  
  
  
  ggplot(dat0, aes(x = x, y = predicted, colour = group, fill = group),
         title = expression(paste(italic("D")))) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, colour = NA) +
    scale_x_continuous(breaks=seq(0,1,by=0.5)) +
    geom_line(size = 1.2) + 
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=rel(2)),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=rel(2.5)),
      axis.text.x = element_text(size=rel(2.5)),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      legend.position=c(.8,.2),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(2.8,0.5,0.5,0.5), 'lines')),
  
  
  ncol = 3, nrow = 1,
  widths = c(1.1,1,1)
)


dev.off()




library(ggplot2)
library(ggeffects)
library(gplots)
library(ggpubr)
library(scales)


a_fig <- ggplot(model_data, aes(x=pEpoch5, fill=pType)) +
  geom_histogram(position="stack", stat="count") +
  theme_bw() +
  labs(y="", title = "# of Publciations") +
  scale_x_continuous(breaks=seq(1,14,by=1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=rel(1.5)),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.title.x=element_blank(),
        legend.position = c(0.2,0.4),
        plot.margin = unit(c(0.5,0.5,2.5,0.5), 'lines'))


a_fig <- set_palette(a_fig,palette = c("black","grey","purple"))
a_fig


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



b_fig <- ggarrange(fm2_1n + rremove("x.title"), 
                   fm2_2n + rremove("y.title") + rremove("x.title"), 
                   ncol = 2, nrow = 1, widths = c(1.2,1))


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

c_fig <- ggarrange(p_div, c_div,
          ncol = 2, nrow = 1, widths = c(1.2,1))


L1_plot_nb <- ggplot(data.frame("L1" = rowSums(data_major_sa[,c(2:17)])), aes(x=L1)) + 
  geom_bar(color="black", fill="white") + 
  scale_x_continuous(name="# Major MeSH", breaks=c(1:20), limits=c(0,10)) +
  scale_y_continuous(name=as.expression(bquote("PDF")), 
                     breaks=c(0,50000,100000,150000,200000,250000),
                     labels=c("0","0.08","0.16","0.24","0.32", "0.40"),
                     limits = c(0,250000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3), margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.y=element_text(size=rel(2),angle=90,family="NimbusMon", face="bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 5, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"),
        plot.margin = unit(c(0,0,2.5,3), 'lines')) + 
  geom_segment(aes(x = mean(L1), y = 0, xend = mean(L1), yend = 250000), linetype = 2, size = 1.2, colour = "red")

SA_plot_nb <- ggplot(data.frame("SA" = rowSums(model_data[,c('pSA1b','pSA2b','pSA3b','pSA4b','pSA5b')])), aes(x=SA)) + 
  geom_bar(color="black", fill="white") + 
  scale_x_continuous(name="# SA", breaks=c(1:20), limits=c(0,7)) +
  scale_y_continuous(name=as.expression(bquote("PDF")), 
                     labels = label_number(suffix = " ", accuracy = 1, scale = 1e-3), limits = c(0,250000)) +
  theme_void() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.length = unit(.15, "cm"), axis.ticks = element_line(size = 1),
        plot.margin = unit(c(0,0,2.5,0), 'lines')) +
  geom_segment(aes(x = mean(SA), y = 0, xend = mean(SA), yend = 250000), linetype = 2, size = 1.2, colour = "red")


d_fig <- ggarrange(L1_plot_nb,
                   SA_plot_nb,
                   ncol = 2, nrow = 1,
                   widths = c(1.2,1))


# 
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
                         sum(subset(model_data, pType == "Cognitive")$pSA5)),#cognitive, no brain key
                 mxd = c(sum(subset(model_data, pType == "Mixed")$pSA1), 
                         sum(subset(model_data, pType == "Mixed")$pSA2), 
                         sum(subset(model_data, pType == "Mixed")$pSA3), 
                         sum(subset(model_data, pType == "Mixed")$pSA4), 
                         sum(subset(model_data, pType == "Mixed")$pSA5))) #mixed


perc_diff_1 <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "") +
  xlab("Affective [%] - Mixed [%]") +
  geom_bar(stat = "identity",aes(aff*100/sum(df$aff)-mxd*100/sum(df$mxd),sas,fill = sas), width = 0.7) +
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

perc_diff_2 <- ggplot(df, aes(sas,label=sas)) +
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

perc_diff_3 <- ggplot(df, aes(sas,label=sas)) +
  theme_bw() +
  labs(x="", y = "", title = "") +
  xlab("Cognitive [%] - Mixed [%]") +
  geom_bar(stat = "identity",aes(cog*100/sum(df$cog)-mxd*100/sum(df$mxd),sas,fill = sas), width = 0.7) +
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

png('C:/Users/Rubinzone/Desktop/F1_v24.png', width = 18, height = 12, units = 'in', res = 300)
ggarrange(
  ggarrange(ggarrange(a_fig, b_fig, nrow = 1,
                      labels = c("A","B"),
                      font.label=list(color="black",size=30)),
            ggarrange(c_fig, d_fig, ncol = 1,
                      labels = c("C","D"),
                      font.label=list(color="black",size=30)),
            ncol = 2, nrow = 1,
            widths = c(1,0.5)
  ),
  ggarrange(perc_diff_1  + rremove("y.ticks"), 
            perc_diff_2  + rremove("y.ticks") + rremove("y.text"),
            perc_diff_3  + rremove("y.ticks") + rremove("y.text"),
            nrow = 1,
            widths = c(1.2,1,1),
            labels = c("E","F","G"),
            font.label=list(color="black",size=30)),
nrow = 2, ncol = 1)

dev.off()




# multinom model
# Multinomial model on pType
model3 <- multinom(pType ~ pDiversity + cDiversity, data = model_data) 

coeff <- summary(model3)$coefficients
std.errs <- summary(model3)$standard.errors

# calculate z-statistics of coefficients
z_stats <- summary(model3)$coefficients/
  summary(model3)$standard.errors

# convert to p-values
p_values <- (1 - pnorm(abs(z_stats)))*2

# Produce model results
summary(model3)
plot_model(model3, type = "pred", terms="pDiversity [all]")
plot_model(model3, type = "pred", terms="cDiversity [all]")


# Vis for multinom model


plot_model(model3, type = "pred", terms="pDiversity [all]")
plot_model(model3, type = "pred", terms="cDiversity [all]")



dat01 <- ggemmeans(model3, terms = "pDiversity [all]")
dat01$group <- 'Dp'
dat02 <- ggemmeans(model3, terms = "cDiversity [all]")
dat02$group <- 'Dc'
dat00 <- rbind(dat01, dat02)


png('C:/Users/Rubinzone/Desktop/Figure3_v24_add.png', width = 8, height = 4, units = 'in', res = 300)

ggarrange(
  ggplot(dat00[dat00$response.level=='Affective',], aes(x = x, y = predicted, colour = group, fill = group),
         title = expression(paste(italic("D")))) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, colour = NA) +
    scale_x_continuous(breaks=seq(0,1,by=0.5)) +
    ylim(0.3,0.6) +
    geom_line(size = 1.2) + 
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=rel(2)),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=rel(2.5)),
      axis.text.x = element_text(size=rel(2.5)),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      legend.position=c(.8,.2),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(2.8,0.5,0.5,0.5), 'lines')),
  
  ggplot(dat00[dat00$response.level=='Cognitive',], aes(x = x, y = predicted, colour = group, fill = group),
         title = expression(paste(italic("D")))) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, colour = NA) +
    scale_x_continuous(breaks=seq(0,1,by=0.5)) +
    ylim(0.3,0.6) +
    geom_line(size = 1.2) + 
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=rel(2)),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=rel(2.5)),
      axis.text.x = element_text(size=rel(2.5)),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      legend.position=c(.8,.2),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(2.8,0.5,0.5,0.5), 'lines')),
  ncol = 2, nrow = 1,
  widths = c(1,1)
  )
dev.off()