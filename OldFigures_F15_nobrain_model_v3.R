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

model_data <- read.csv("E:/Research/Data_pubmed/model_data/model_data3.csv")

# Factorizing variables
model_data$pType <- as.factor(model_data$pType)
model_data$pX_disc <- as.factor(model_data$pX_disc)


# Checking multicolinearity
X<-model_data[,c("pAuthors_n",
              "pSA_num", "cSA_num", "rSA_num",
              "pDiversity", "cDiversity", "rDiversity",
              "cos_mRef_mCit")]
ggpairs(drop_na(X))


cor2pcor(cov(drop_na(X)), )

model1 <- lm(pCitation_count_norm ~ pType + log(pAuthors_n) + 
               pX_disc + pSA_num + cSA_num + rSA_num + pDiversity +
               cDiversity + rDiversity + cos_mRef_mCit, 
             data = model_data)
# Diagnostics 1
omcdiag(model1)
imcdiag(model1)
library(ppcor)
pcor(drop_na(X), method = "pearson")

summary(model1)


par(mfrow=c(2,2))
plot(model1, which =1)
plot(model1, which =2)
plot(model1, which =3)
plot(model1, which =4)




model2 <- lm(pCitation_count_norm ~ pType + log(pAuthors_n) + 
               pX_disc + pDiversity +
               cDiversity + rDiversity + cos_mRef_mCit, 
             data = model_data)

omcdiag(model2)
imcdiag(model2)


model3 <- lm(pCitation_count_norm ~ pType + log(pAuthors_n) + 
               pDiversity +
               cDiversity + rDiversity + cos_mRef_mCit, 
             data = model_data)

omcdiag(model3)

summary(model3)



par(mfrow=c(2,2))
plot(model3, which =1)
plot(model3, which =2)
plot(model3, which =3)
plot(model3, which =4)





# 3 new models: SA_num + 3cosine / SA_num^2 + 3cosine (skipped, no changes) / Diversity + 3cosine

model4 <- lm(pCitation_count_norm ~ 
               pType + 
               log(pAuthors_n) + 
               pSA_num +
               cSA_num + 
               rSA_num + 
               cos_mRef_mCit +
               cos_mRef_pub +
               cos_mCit_pub, 
             data = model_data)

omcdiag(model4)
summary(model4)

par(mfrow=c(2,2))
plot(model4, which =1)
plot(model4, which =2)
plot(model4, which =3)
plot(model4, which =4)






model5 <- lm(pCitation_count_norm ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity + 
               cos_mRef_mCit +
               cos_mRef_pub +
               cos_mCit_pub, 
             data = model_data)

omcdiag(model5)
imcdiag(model2)
summary(model5)

par(mfrow=c(2,2))
plot(model5, which =1)
plot(model5, which =2)
plot(model5, which =3)
plot(model5, which =4)



model6 <- lm(pCitation_count_norm ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity + 
               cos_mRef_mCit +
               cos_mCit_pub, 
             data = model_data)

omcdiag(model6)
imcdiag(model6)
summary(model6)


# Checking multicolinearity
model6_v<-model_data[,c("pAuthors_n",
                 "pDiversity", 
                 "cDiversity", 
                 "rDiversity",
                 "cos_mRef_mCit", 
                 "cos_mCit_pub")]
ggpairs(drop_na(model6_v))

par(mfrow=c(2,2))
plot(model6, which =1)
plot(model6, which =2)
plot(model6, which =3)
plot(model6, which =4)





model7 <- lm(pCitation_count_norm ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity, 
             data = model_data)

omcdiag(model7)
imcdiag(model7)
summary(model7)




model8 <- lm(pCitation_count_norm ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity +
               cos_mRef_mCit +
               cos_mCit_pub +
               cDiversity * cos_mRef_mCit +
               cDiversity * cos_mCit_pub +
               rDiversity * cos_mRef_mCit +
               pDiversity * cos_mCit_pub, 
             data = model_data)

omcdiag(model8)
imcdiag(model8)
summary(model8)





model9 <- lm(pCitation_count_norm ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity +
               cos_mRef_mCit +
               cos_mCit_pub +
               rDiversity * cos_mRef_mCit +
               pDiversity * cos_mCit_pub, 
             data = model_data)

omcdiag(model9)
imcdiag(model9)
summary(model9)





model10 <- lm(pCitation_count_norm ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity +
               cos_mRef_mCit +
               cos_mCit_pub +
               #rDiversity * cos_mRef_mCit +
               pDiversity * cos_mRef_mCit + #temp
               pDiversity * cos_mCit_pub, 
             data = model_data)

omcdiag(model10)
imcdiag(model10)
summary(model10)









model11 <- lm(pCitation_count_norm ~ 
                pType + 
                log(pAuthors_n) + 
                pDiversity +
                cDiversity + 
                rDiversity +
                pDiversity * cDiversity +
                rDiversity * cDiversity, 
              data = model_data)

omcdiag(model11)
imcdiag(model11)
summary(model11)



model12 <- lm(pCitation_count_norm ~ 
                pType + 
                log(pAuthors_n) + 
                pDiversity +
                cDiversity + 
                rDiversity +
                pDiversity * cDiversity +
                rDiversity * cDiversity, 
              data = model_data)

omcdiag(model12)
imcdiag(model12)
summary(model12)





model13 <- lm(pCitation_count_norm ~ 
                pType + 
                log(pAuthors_n) + 
                pDiversity +
                cDiversity + 
                rDiversity +
                cos_mRef_mCit +
                cos_mCit_pub +
                rDiversity * cos_mRef_mCit +
                pDiversity * cos_mCit_pub, 
              data = model_data[model_data$pCitations_n<=25,])

omcdiag(model13)
imcdiag(model13)
summary(model13)


model_all <- lm(pCitation_count_norm ~ 
               pType + 
               pX_disc +
               log(pAuthors_n) + 
               pSA_num +
               cSA_num +
               rSA_num +
               pDiversity +
               cDiversity + 
               rDiversity + 
               cos_mRef_mCit +
               cos_mRef_pub +
               cos_mCit_pub, 
             data = model_data)

ols_step_all_possible(model_all)


ols_step_best_subset(model_all)

ols_step_forward_p(model_all)
ols_step_backward_p(model_all)
ols_step_both_p(model_all)


ols_step_forward_aic(model_all)
ols_step_backward_aic(model_all)
ols_step_both_aic(model_all)





# models including time


model_data$pAge <- max(model_data$pYear) - model_data$pYear
model_data$pEpoch <- ifelse(model_data$pYear < 1999, 'early', 
                            ifelse(model_data$pYear > 2009, 'late', 'middle'))

model_data$pEpoch5 <- NA
model_data$pEpoch5[model_data$pYear > 1950] <- 1
model_data$pEpoch5[model_data$pYear > 1955] <- 2
model_data$pEpoch5[model_data$pYear > 1960] <- 3
model_data$pEpoch5[model_data$pYear > 1965] <- 4
model_data$pEpoch5[model_data$pYear > 1970] <- 5
model_data$pEpoch5[model_data$pYear > 1975] <- 6
model_data$pEpoch5[model_data$pYear > 1980] <- 7
model_data$pEpoch5[model_data$pYear > 1985] <- 8
model_data$pEpoch5[model_data$pYear > 1990] <- 9
model_data$pEpoch5[model_data$pYear > 1995] <- 10
model_data$pEpoch5[model_data$pYear > 2000] <- 11
model_data$pEpoch5[model_data$pYear > 2005] <- 12
model_data$pEpoch5[model_data$pYear > 2010] <- 13
model_data$pEpoch5[model_data$pYear > 2015] <- 14


barplot(table(model_data$pEpoch)[c(1,3,2)])
barplot(table(model_data[model_data$pType=='cog nb',]$pEpoch5))


model15 <- lm(pCitation_count_norm ~ 
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


model16 <- lm(pCitation_count_norm ~ 
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
              pCitation_count_norm + 
              pType + 
              log(pAuthors_n) + 
              # pDiversity +
              #rDiversity +
              #cos_mRef_mCit +
              #cos_mCit_pub +
              pDiversity * cos_mRef_pub +
              pCitation_count_norm * cos_mRef_pub +   
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
      legend.position = c(0.8, 0.4)
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
