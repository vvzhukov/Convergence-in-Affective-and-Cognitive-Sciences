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
library(ggplot2)
library(ggpubr)
library(gtsummary)
library(modelsummary)

model_data <- read.csv("E:/Research/Data_pubmed/model_data/model_data3.csv")
model_data <- model_data[,c("pCitation_count_norm", "pType", "pAuthors_n", 
                            "pDiversity",
                            "cDiversity", 
                            "rDiversity", 
                            "cos_mRef_mCit",
                            "cos_mCit_pub")]


# Factorizing variables
model_data$pType <- as.factor(model_data$pType)



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

desc_2 <- ggplot(model_data, aes(y=pCitation_count_norm)) +
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

model6 <- lm(pCitation_count_norm ~ 
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

ggpairs_out <- ggpairs(drop_na(model_data))
ggpairs_out2 <- ggpairs(drop_na(model_data), ggplot2::aes(colour=pType))

png('C:/Users/Rubinzone/Desktop/F15_4.png', width = 12, height = 8, units = 'in', res = 300)
ggpairs_out2
dev.off()

install.packages("modelsummary")


b <- list(geom_vline(xintercept = 0, color = 'orange'),
          annotate("rect", alpha = .1,
                   xmin = -.5, xmax = .5, 
                   ymin = -Inf, ymax = Inf),
          geom_point(aes(y = term, x = estimate), alpha = .3, 
                     size = 10, color = 'red'))

png('C:/Users/Rubinzone/Desktop/F15_5.png', width = 8, height = 4, units = 'in', res = 300)
modelplot(model6, background = b)
dev.off()

model6$coefficients
model6$

ggplot(data = df,
       aes(x = explanatory,
           y = coef,
           ymin = lower,
           ymax = upper,
           fill = resp.fill,
           col = resp.col)) +
  geom_pointrange(position = position_dodge(width = 0.4), size = 2,
                  shape = 21) +
  geom_hline(yintercept = 0) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_bw()
