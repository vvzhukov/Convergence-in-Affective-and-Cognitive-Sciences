### In this code we model the citations

### This is the list of libraries loaded and needed for the code to run
library(gplots)
library(sjPlot)
library(MASS)
library(car)

### Level of significance for tests
alpha<-0.05

### This is the path where the data are
#setwd(".........")

# Latest model data
model_data <- read.csv("model_data4.csv")

model_data$pType <- as.factor(model_data$pType)
model_data$pX_disc <- as.factor(model_data$pX_disc)


pType <- as.factor(model_data$pType)
table(pType)
pAuthors_n <- model_data$pAuthors_n
table(pAuthors_n)
summary(pAuthors_n)
logpAuthors_n <- log(pAuthors_n)
table(logpAuthors_n)
summary(logpAuthors_n)
pDiversity <- model_data$pDiversity
table(pDiversity)
summary(pDiversity)
cDiversity <- model_data$cDiversity
table(cDiversity)
summary(cDiversity)
pEpoch5 <- model_data$pEpoch5
table(pEpoch5)
summary(pEpoch5)

citation_count_n <- model_data$citation_count_n
table(citation_count_n)
summary(citation_count_n)

### Here is the response variable
Y<-citation_count_n

D<-data.frame(Y,pType,logpAuthors_n,pDiversity,cDiversity,pEpoch5)

VarNames<-c("Citation_count_n","pType","logpAuthors_n","pDiversity","cDiversity","pEpoch5")
            
hist(citation_count_n)

Y<-sqrt(citation_count_n-min(citation_count_n,na.rm=T)+1)
hist(Y)

D<-data.frame(Y,pType,logpAuthors_n,pDiversity,cDiversity,pEpoch5)


fm00 <- lm(Y ~ pType + logpAuthors_n + pDiversity + cDiversity + pEpoch5)

summary(fm00)
anova(fm00)


plot_model(fm00,"est",sort=FALSE, show.values = TRUE, value.offset = .3,xlab="")+font_size(labels.x=15,labels.y=15,title = 0,axis_title.x =0)
plot_model(fm00, type = "pred")$pType
plot_model(fm00, type = "pred")$logpAuthors_n
plot_model(fm00, type = "pred")$pDiversity
plot_model(fm00, type = "pred")$cDiversity
plot_model(fm00, type = "pred")$pEpoch5






############################################
### Here we study as response the cDiversity
############################################

hist(cDiversity)

FM <- lm(cDiversity ~ pType + logpAuthors_n + pDiversity + pEpoch5 + citation_count_n)

summary(FM)
anova(FM)


plot_model(FM,"est",sort=FALSE, show.values = TRUE, value.offset = .3,xlab="")+font_size(labels.x=15,labels.y=15,title = 0,axis_title.x =0)
plot_model(FM, type = "pred")$pType
plot_model(FM, type = "pred")$logpAuthors_n
plot_model(FM, type = "pred")$pDiversity
plot_model(FM, type = "pred")$citation_count_n
plot_model(FM, type = "pred")$pEpoch5





#######################################
### Here we study as response the pType
#######################################
FM<-glm(formula = pType ~ cDiversity + logpAuthors_n + pDiversity + pEpoch5 + citation_count_n, family = "binomial")

### Here is the model's output
summary(FM)

## Table of odds ratios and 95% CI
tab<-exp(cbind(OR = coef(FM), confint(FM)))
tab

### Transform from odds to probabilities
(tab[,1])/(1+tab[,1])


plot_model(FM,"est",transform = "plogis",sort=TRUE,ci_method="wald", show.values = TRUE, value.offset = .3) ### put labels
#plot_model(FM,"pred") ### Predicted values (marginal effects) for each of model terms
plot_model(FM,"pred")$cDiversity
plot_model(FM,"pred")$logpAuthors_n
plot_model(FM,"pred")$pDiversity
plot_model(FM,"pred")$pEpoch5
plot_model(FM,"pred")$citation_count_n




