library(dplyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(stringr)
library(stringi)
library(superml)
library(lsa)
library(GGally)
library(corpcor)
library(mctest)
library(olsrr)
library(sjPlot)
library(glmmTMB)
library(effects)
library(ggpubr)

model_data <- read.csv("E:/Research/Data_pubmed/model_data23/model_data.csv")

# 6/21 correction: old dates trimmed
model_data <- subset(model_data, pYear >= 1950)

# Factorizing variables
model_data$pType <- as.factor(model_data$pType)
model_data$pX_disc <- as.factor(model_data$pX_disc)


# Checking multicolinearity
#X<-model_data[,c("pAuthors_n",
#                 "pDiversity", "cDiversity", "rDiversity",
#                 "cos_mRef_mCit")]
#ggpairs(drop_na(X))
#cor2pcor(cov(drop_na(X)), )




model15 <- lm(pCitations_n_norm ~ 
                pType + 
                log(pAuthors_n) + 
                pDiversity +
                cDiversity + 
                rDiversity +
                cos_mRef_mCit +
                cos_mCit_pub +
                pEpoch5 +
                #rDiversity * cos_mRef_mCit +
                pDiversity * cos_mRef_mCit + #temp
                pDiversity * cos_mCit_pub, 
              data = model_data)

summary(model15)

model18 <- lm(pCitations_n_norm ~ 
                pType + 
                log(pAuthors_n) + 
                #pDiversity +
                cDiversity + 
                rDiversity +
                cos_mRef_mCit +
                cos_mCit_pub +
                pEpoch5,
                #rDiversity * cos_mRef_mCit +
                #pDiversity * cos_mRef_mCit + #temp
                #pDiversity * cos_mCit_pub, 
              data = model_data)

summary(model18)

model16 <- lm(pCitations_n_norm ~ 
                #pType + 
                #log(pAuthors_n) + 
                #pDiversity +
                #cDiversity + 
                #rDiversity +
                #cos_mRef_mCit +
                #cos_mCit_pub +
                #pEpoch5 +
                pType * pEpoch5,
              #pDiversity * cos_mRef_mCit + #temp
              #pDiversity * cos_mCit_pub, 
              data = model_data)
summary(model16)


# Citations Diversity as a response
model17 <- lm(cDiversity ~
                pCitations_n_norm + 
                pType + 
                log(pAuthors_n) + 
                # pDiversity +
                #rDiversity +
                #cos_mRef_mCit +
                #cos_mCit_pub +
                pDiversity * cos_mRef_pub +
                pCitations_n_norm * cos_mRef_pub +   
                pEpoch5,
              #pType * pEpoch5,
              #pDiversity * cos_mRef_mCit + #temp
              #pDiversity * cos_mCit_pub, 
              data = model_data)
summary(model17)



png('C:/Users/Rubinzone/Desktop/F15_nb_model_hires.png', width = 14, height = 14, units = 'in', res = 300)

ggarrange(
  plot_model(model15, 
             type = "pred", 
             terms = "pType",
             dot.size = 6,
             line.size = 2,
             title = "Publication type") + 
    theme_bw() +
    aes(color = c("aff", "cog nb")) +
    scale_color_manual(labels = c("aff", "cog nb"), values = c("darkviolet", "gray")) +
    theme(
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(1.5)),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5)
    ),
  
  plot_model(model15, 
             type = "pred", 
             terms = "pAuthors_n",
             dot.size = 6,
             line.size = 1.2,
             title = "Number of authors") + 
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
  
  plot_model(model15, 
             type = "pred", 
             terms = "pEpoch5",
             dot.size = 6,
             line.size = 1.2,
             title = "5 years epoch") + 
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
  
  plot_model(model15, 
             type = "pred", 
             terms = "pDiversity",
             dot.size = 6,
             line.size = 1.2,
             title = "Publciation diversity") + 
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
  
  plot_model(model15, 
             type = "pred", 
             terms = "cDiversity",
             dot.size = 6,
             line.size = 1.2,
             title = "c̄ diversity") + 
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
  
  plot_model(model15, 
             type = "pred", 
             terms = "rDiversity",
             dot.size = 6,
             line.size = 1.2,
             title = "r̄ diversity") + 
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
  
  plot_model(model15, 
             type = "pred", 
             terms = "cos_mRef_mCit",
             dot.size = 6,
             line.size = 1.2,
             title = "Cosine similarity r̄ • c̄") + 
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
  
  
  plot_model(model15, 
             type = "pred", 
             terms = "cos_mCit_pub",
             dot.size = 6,
             line.size = 1.2,
             title = "Cosine similarity c̄ • p") + 
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
  
  NULL,
  
  plot_model(model15, 
             type = "int",
             #mdrt.values = "meansd",
             dot.size = 6, 
             line.size = 1.2,
             title = "Interaction pDiversity * r̄ • c̄")[[1]] +  # pDiversity * cos_mRef_mCit
    labs(color='r̄ • c̄')  +
    theme_bw() +
    theme(
      legend.title = element_text(size=rel(1.5)),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(1.5)),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5),
      legend.position = c(0.87, 0.5)
    ),
  
  plot_model(model15, 
             type = "int",
             #mdrt.values = "meansd",
             dot.size = 6, 
             line.size = 1.2,
             title = "Interaction pDiversity * c̄ • p")[[2]] +  # pDiversity * cos_mRef_mCit
    labs(color='c̄ • p')  +
    theme_bw() +
    theme(
      legend.title = element_text(size=rel(1.5)),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(1.5)),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5),
      legend.position = c(0.87, 0.6)
    ),
  
  plot_model(model16, 
             type = "int",
             #mdrt.values = "meansd",
             dot.size = 6, 
             line.size = 1.2,
             title = "Interaction pType*pEpoch") + 
    labs(color='pEpoch')  +
    theme_bw() +
    theme(
      legend.title = element_text(size=rel(1.5)),
      axis.title.y = element_blank(),
      axis.text = element_text(size=rel(1.5)),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5),
      legend.position = c(0.6, 0.4)
    ),
  ncol = 3, nrow = 4
)


dev.off()







plot_model(model17, 
           type = "int",
           dot.size = 6, 
           line.size = 1.2,
           title = "Interaction ")[[1]] +  # pDiversity * cos_mRef_mCit
  labs(color='r̄ • c̄')  +
  theme_bw() +
  theme(
    legend.title = element_text(size=rel(1.5)),
    axis.title.y = element_blank(),
    axis.text = element_text(size=rel(1.5)),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5),
    legend.position = c(0.87, 0.5)
  )







# variables descriptive


p_div <- ggplot(model_data, aes(y=pDiversity)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  labs(y="", title = "Publication diversity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x= element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y= element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank())

c_div <- ggplot(model_data, aes(y=cDiversity)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() + 
  labs(y="", title = "c̄ diversity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x= element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y= element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank())

r_div <- ggplot(model_data, aes(y=rDiversity)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() + 
  labs(y="", title = "r̄ diversity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y= element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(1)),
        axis.ticks.x = element_blank())

png('C:/Users/Rubinzone/Desktop/F15_1.png', width = 12, height = 4, units = 'in', res = 300)
ggarrange(r_div, p_div, c_div, 
          ncol = 3, nrow = 1,
          widths = c(1.2,1,1))
dev.off()



cos_1 <- ggplot(model_data, aes(y=cos_mCit_pub)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  labs(y="", title = "Cosine similarity: pub vs c̄") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(1)),
        axis.ticks.x = element_blank())

cos_2 <- ggplot(model_data, aes(y=cos_mRef_mCit)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  labs(y="", title = "Cosine similarity: r̄ vs c̄") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank())

png('C:/Users/Rubinzone/Desktop/F15_2.png', width = 8, height = 4, units = 'in', res = 300)
ggarrange(cos_1, cos_2, 
          ncol = 2, nrow = 1,
          widths = c(1.2,1))
dev.off()



desc_1 <- ggplot(model_data, aes(x=pType)) +
  geom_histogram(stat="count") +
  theme_bw() +
  labs(y="", title = "Publication type") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=rel(2)),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.title.x=element_blank())

desc_2 <- ggplot(model_data, aes(y=pCitations_n_norm)) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  labs(y="", title = "Normalized citations") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.ticks.x = element_blank())


desc_3 <- ggplot(model_data, aes(y=log(pAuthors_n))) +
  geom_boxplot(outlier.shape = 1, outlier.size=4) +
  theme_bw() +
  labs(y="", title = "Log authors count") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(size=rel(1.5), hjust = 0.5),
        axis.text.y=element_text(size=rel(2)),
        axis.title.y=element_text(size=rel(2)),
        axis.ticks.x = element_blank())

png('C:/Users/Rubinzone/Desktop/F15_3.png', width = 12, height = 4, units = 'in', res = 300)
ggarrange(desc_1, desc_2, desc_3, 
          ncol = 3, nrow = 1,
          widths = c(1.2,1,1))
dev.off()

# tables
# https://www.danieldsjoberg.com/gtsummary/
# install.packages("gtsummary")
library(gtsummary)

table <- 
  tbl_summary(
    model_data,
    by = pType, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  #add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

table

model6 <- lm(pCitations_n_norm ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity + 
               cos_mRef_mCit +
               cos_mCit_pub, 
             data = model_data)

summary(model6)
tbl_regression(model6)

model_data$ISSN_journal <- NULL
model_data$ISSN_linking <- NULL

ggpairs_out <- ggpairs(drop_na(model_data))
ggpairs_out2 <- ggpairs(drop_na(model_data), ggplot2::aes(colour=pType))

png('C:/Users/Rubinzone/Desktop/F15_4.png', width = 12, height = 8, units = 'in', res = 300)
ggpairs_out2
dev.off()

#install.packages("modelsummary")
library(modelsummary)

b <- list(geom_vline(xintercept = 0, color = 'orange'),
          annotate("rect", alpha = .1,
                   xmin = -.5, xmax = .5, 
                   ymin = -Inf, ymax = Inf),
          geom_point(aes(y = term, x = estimate), alpha = .3, 
                     size = 10, color = 'red'))

png('C:/Users/Rubinzone/Desktop/F15_5.png', width = 8, height = 4, units = 'in', res = 300)
modelplot(model6, background = b)
dev.off()




# Some extra QC
nrow(subset(model_data, pType == "Cognitive" & pYear > 2019))
data_c <- subset(model_data, pType == "Cognitive" & pYear > 2019)
data_a <- subset(model_data, pType == "Affective" & pYear > 2019)


boxplot(data_a$pCitations_n, ylim=c(0,50))
boxplot(data_c$pCitations_n, ylim=c(0,50))

head(data_a[order(data_a$pCitations_n, decreasing = TRUE), ], 5)$pCitations_n
head(data_c[order(data_c$pCitations_n, decreasing = TRUE), ], 5)$pCitations_n

sum(data_a$pCitations_n)
sum(data_c$pCitations_n)

