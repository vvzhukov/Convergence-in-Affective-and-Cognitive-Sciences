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

# Latest model data
setwd("E:/Research/Data_pubmed/model_data23")

model_data <- read.csv("model_data4.csv")

model_data$pType <- as.factor(model_data$pType)
model_data$pX_disc <- as.factor(model_data$pX_disc)



pType <- as.factor(model_data$pType)
#table(pType)
pAuthors_n <- model_data$pAuthors_n
#table(pAuthors_n)
#summary(pAuthors_n)
logpAuthors_n <- log(pAuthors_n)
#table(logpAuthors_n)
#summary(logpAuthors_n)
pDiversity <- model_data$pDiversity
#table(pDiversity)
#summary(pDiversity)
cDiversity <- model_data$cDiversity
#table(cDiversity)
#summary(cDiversity)
pEpoch5 <- model_data$pEpoch5
#table(pEpoch5)
#summary(pEpoch5)
citation_count_n <- model_data$citation_count_n


# First model (Normalized citations ~ publication type + number of authors + 
#               publication diversity + mean citations diversity +
#               5 years epoch number
model1 <- lm(citation_count_n ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               #+pEpoch5 +
               cDiversity, 
               data = model_data)
summary(model1)




model2 <- glm(formula = pType ~ cDiversity + 
                logpAuthors_n + pDiversity + 
                #pEpoch5 + 
                citation_count_n, 
              family = "binomial")
summary(model2)


# check model1 and model2 without 2020, 2021, 2022, 2023


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
        plot.margin = unit(c(2,0.5,0.5,0.5), 'lines'))

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
        plot.margin = unit(c(2,0.5,0.5,0.5), 'lines'))


png('C:/Users/Rubinzone/Desktop/FMd_3.png', width = 8, height = 4, units = 'in', res = 300)
ggarrange(p_div, c_div, 
          ncol = 2, nrow = 1,
          widths = c(1.2,1))
dev.off()



desc_1 <- ggplot(model_data, aes(x=pType, fill=pType)) +
  geom_histogram(stat="count") +
  theme_bw() +
  labs(y="", title = "# Publications") +
  scale_fill_manual(labels = c("Affective", "Cognitive"), values = c("black", "gray")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=rel(2)),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.title.x=element_blank(),
        legend.position = "none")

desc_2 <- ggplot(model_data, aes(y=citation_count_n)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  labs(y="", title = expression(paste(italic("C")[italic("N")]))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(1.5,0.5,0.5,0.5), 'lines'))


desc_3 <- ggplot(model_data, aes(y=log(pAuthors_n))) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  labs(y="", title = expression(paste("ln",italic("A")))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(1.5,0.5,0.5,0.5), 'lines'))

png('C:/Users/Rubinzone/Desktop/FMd_2.png', width = 12, height = 4, units = 'in', res = 300)
ggarrange(desc_1, desc_2, desc_3, 
          ncol = 3, nrow = 1,
          widths = c(1.2,1,1))
dev.off()



# extra plot for the non normalized citations

ggplot(model_data, aes(y=pCitations_n)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  ylim(c(0,200)) +
  labs(y="", title = expression(paste(italic("C")))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(1.5,0.5,0.5,0.5), 'lines'))




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
        legend.position = "none")

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
        legend.position = "none")



png('C:/Users/Rubinzone/Desktop/FMd_1.png', width = 10, height = 4, units = 'in', res = 300)
ggarrange(desc_4, desc_5, 
          ncol = 2, nrow = 1,
          widths = c(1.2,1))
dev.off()






# fm* may be found at 12_extra_plots.R

png('C:/Users/Rubinzone/Desktop/FMd_4all.png', width = 12, height = 8, units = 'in', res = 300)
ggarrange(desc_4, desc_5,
          ggarrange(fm2_1n + rremove("x.title"), fm2_2n + rremove("y.title") + rremove("x.title"), 
                    ncol = 2, nrow = 1, widths = c(1.2,1)),
          ggarrange(p_div, c_div,
                    ncol = 2, nrow = 1, widths = c(1.2,1)),
          ncol = 2, nrow = 2,
          widths = c(1.2,1,1,1),
          labels = c("A", "", "B", "C"))
dev.off()







# tables
# https://www.danieldsjoberg.com/gtsummary/
# install.packages("gtsummary")

model_data$pAuthors_n_log <- log(model_data$pAuthors_n)

table <- 
  tbl_summary(
    model_data[c('citation_count_n','pType','pAuthors_n','pAuthors_n_log', 'cDiversity', 'pDiversity', 'pCitations_n')],
    statistic = list(
      #all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    by = pType, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

table

table2 <- 
  tbl_summary(
    model_data[c('citation_count_n','pType','pAuthors_n','pAuthors_n_log', 'cDiversity', 'pDiversity', 'pCitations_n')],
    statistic = list(
      #all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

table2





# Marginal effect plot
png('C:/Users/Rubinzone/Desktop/FMm_1.png', width = 16, height = 4, units = 'in', res = 300)

ggarrange(
  plot_model(model1, 
             type = "pred", 
             terms = "pType",
             dot.size = 6,
             line.size = 2,
             title = "Publication type") + 
    theme_bw() +
    aes(color = c("Affective", "Cognitive")) +
    scale_color_manual(labels = c("Affective", "Cognitive"), values = c("darkviolet", "gray")) +
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
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(1.5)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5)
    ),

  
  plot_model(model1, 
             type = "pred", 
             terms = "pDiversity",
             dot.size = 6,
             line.size = 1.2,
             title = expression(paste(italic("D"["p"])))) + 
    theme_bw() +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=rel(2)),
      axis.text.x = element_text(size=rel(1.5)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(0.5,2,0.5,0.5), 'lines')),
  
  plot_model(model1, 
             type = "pred", 
             terms = "cDiversity",
             dot.size = 6,
             line.size = 1.2,
             title = expression(paste(italic(bar("D")["C"])))) + 
    theme_bw() +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=rel(2)),
      axis.text.x = element_text(size=rel(1.5)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(0.5,2,0.5,0.5), 'lines')),
  
  
    plot_model(model1, 
             type = "pred", 
             terms = "pEpoch5",
             dot.size = 6,
             line.size = 1.2,
             title = "Epoch") + 
    theme_bw() +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(1.5)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5)
    ),
  
  ncol = 5, nrow = 1,
  widths = c(1.2,1,1,1,1,1)
)


dev.off()




dat1 <- ggpredict(model1, "pDiversity [0:1]")
dat1$group <- 'Dp'
dat2 <- ggpredict(model1, "cDiversity [0:1]")
dat2$group <- 'Dc'
dat0 <- rbind(dat1, dat2)




png('C:/Users/Rubinzone/Desktop/FMm_1v2.png', width = 16, height = 4, units = 'in', res = 300)

ggarrange(
  plot_model(model1, 
             type = "pred", 
             terms = "pType",
             dot.size = 6,
             line.size = 2,
             dodge = -1,
             title = expression(paste(italic("T")))) + 
    theme_bw() +
    aes(color = c("Affective", "Cognitive")) +
    scale_color_manual(labels = c("Affective", "Cognitive"), values = c("darkviolet", "gray")) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(2)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')),
  
  plot_model(model1, 
             type = "pred", 
             terms = "pAuthors_n",
             dot.size = 6,
             line.size = 1.2,
             title = expression(paste("ln",italic("A")))) + 
    theme_bw() +
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







dat1 <- ggpredict(model2, "pDiversity [0:1]")
dat1$group <- 'Dp'
dat2 <- ggpredict(model2, "cDiversity [0:1]")
dat2$group <- 'Dc'
dat <- rbind(dat1, dat2)









png('C:/Users/Rubinzone/Desktop/FMm_2.png', width = 12, height = 4, units = 'in', res = 300)

ggarrange(
  plot_model(model2, 
             type = "pred", 
             terms = "citation_count_n",
             dot.size = 6,
             line.size = 2,
             title = expression(paste(italic("C")[italic("N")]))) + 
    theme_bw() +
    scale_y_continuous(limits = c(0.30,0.70), labels = scales::percent) +
    #ylim(.4, .6) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(2)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5)
    ),
  
  plot_model(model2, 
             type = "pred", 
             terms = "logpAuthors_n",
             dot.size = 6,
             line.size = 1.2,
             title = expression(paste("ln",italic("A")))) + 
    theme_bw() +
    scale_y_continuous(limits = c(0.30,0.70), labels = scales::percent) +
    #ylim(.4, .6) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(2)),
      axis.text.y = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(0.8,0.5,0.5,0.5), 'lines')),
  
  
  ggplot(dat, aes(x = x, y = predicted, colour = group, fill = group),
         title = expression(paste(italic("D")))) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, colour = NA) +
    geom_line(size = 1.2) + 
    theme_bw() +
    scale_y_continuous(limits = c(0.3,0.7), labels = scales::percent) + 
    scale_x_continuous(breaks=seq(0,1,by=0.5)) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=rel(2)),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size=rel(2.5)),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      legend.position=c(.2,.2),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(3,0.5,0.5,0.5), 'lines')),
  
    
  ncol = 3, nrow = 1
)


dev.off()








png('C:/Users/Rubinzone/Desktop/F1m.png', width = 12, height = 8, units = 'in', res = 300)


ggarrange(
  plot_model(model1, 
             type = "pred", 
             terms = "pType",
             dot.size = 6,
             line.size = 2,
             dodge = -1,
             title = expression(paste(italic("T")))) + 
    theme_bw() +
    aes(color = c("Affective", "Cognitive")) +
    scale_color_manual(labels = c("Affective", "Cognitive"), values = c("darkviolet", "gray")) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(2)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')),
  
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
  
  plot_model(model2, 
             type = "pred", 
             terms = "citation_count_n",
             dot.size = 6,
             line.size = 2,
             title = expression(paste(italic("C")[italic("N")]))) + 
    theme_bw() +
    scale_y_continuous(limits = c(0.30,0.70), labels = scales::percent) +
    #ylim(.4, .6) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(2)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(1,0.5,0.5,0.5), 'lines')
    ),
  
  plot_model(model2, 
             type = "pred", 
             terms = "logpAuthors_n",
             dot.size = 6,
             line.size = 1.2,
             title = expression(paste("ln",italic("A")))) + 
    theme_bw() +
    xlim(c(0,20)) +
    scale_y_continuous(limits = c(0.30,0.70), labels = scales::percent) +
    #ylim(.4, .6) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(2)),
      axis.text.y = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(1.2,0.5,0.5,0.5), 'lines')),
  
  
  ggplot(dat, aes(x = x, y = predicted, colour = group, fill = group),
         title = expression(paste(italic("D")))) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, colour = NA) +
    geom_line(size = 1.2) + 
    theme_bw() +
    scale_y_continuous(limits = c(0.3,0.7), labels = scales::percent) + 
    scale_x_continuous(breaks=seq(0,1,by=0.5)) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=rel(2)),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size=rel(2.5)),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      legend.position=c(.2,.2),
      plot.title = element_text(size=rel(2.5), hjust = 0.5, vjust = 0.5),
      plot.margin = unit(c(3.4,0.5,0.5,0.5), 'lines')),
  
  ncol = 3, nrow = 2,
  widths = c(1.1,1,1),
  labels = c("A","","","B"),
  font.label=list(color="black",size=30)
)

dev.off()
