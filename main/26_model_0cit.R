model_data <- read.csv("E:/Research/Data_pubmed/model_data24/model_data.csv")
model_data_old <- read.csv("model_data2_13_2024.csv")


# Re-normalize citations;
# - five years epoch
# - aff + cog together

gen_norm_data <- function(data) {
  
  data$citation_count_n <- 0
  
  for (yr in unique(data$pYear)) {
    # 1. take each 5yrs block
    tmp <- log(subset(data, pYear == yr)$pCitations_n +1)
    # 2. calculate mean for the reviewed time-period
    tmp_m <- mean(tmp)
    # 3. calculate sd
    tmp_sd <- sd(tmp)
    # 4. 
    tmp_n <- (tmp-tmp_m)/tmp_sd 
    data[data$pYear == yr,]$citation_count_n <- tmp_n
  }
  return(data)
}

# For uncited papers, define \overline{D}_c(p_i) = 0
model_data$UnCited <- model_data$pCitations_n == 0
model_data[model_data$pCitations_n == 0,]$cDiversity <- 0

# renormalized
# Change eq. to be c+1 instead of c; recompute the average and std deviation of the logs.
model_data$pCitations_n <- model_data$pCitations_n + 1
model_data_v2 <- gen_norm_data(model_data)

# Boolean flags
model_data_v2$pSA1b <- model_data_v2$pSA1 > 0
model_data_v2$pSA2b <- model_data_v2$pSA2 > 0
model_data_v2$pSA3b <- model_data_v2$pSA3 > 0
model_data_v2$pSA4b <- model_data_v2$pSA4 > 0
model_data_v2$pSA5b <- model_data_v2$pSA5 > 0

# Assumption set NA authors to 1
model_data_v2[is.na(model_data_v2$pAuthors_n),]$pAuthors_n <- 1

# Relevel
model_data_v2$pType <- as.factor(model_data_v2$pType)
model_data_v2 <- within(model_data_v2, pType <- relevel(pType, ref = 'Mixed'))



# MeSH L1 data
cog_major_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI_subjectareas.csv")
aff_major_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_major_UI_subjectareas.csv")
data_major_sa <- rbind(cog_major_sa, aff_major_sa)


# model from Report211.pptx, page #3, right
model_data_v2$pType <- as.factor(model_data_v2$pType)
model_data_v2$pSA1b <- as.factor(model_data_v2$pSA1b)
model_data_v2$pSA2b <- as.factor(model_data_v2$pSA2b)
model_data_v2$pSA3b <- as.factor(model_data_v2$pSA3b)
model_data_v2$pSA4b <- as.factor(model_data_v2$pSA4b)
model_data_v2$pSA5b <- as.factor(model_data_v2$pSA5b)
model1 <- lm(citation_count_n ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity +
               pSA1b + pSA2b + pSA3b + pSA4b + pSA5b, 
             data = model_data_v2)

summary(model1)


png('C:/Users/Rubinzone/Desktop/Model_diag_suppl.png', width = 12, height = 12, units = 'in', res = 300)
par(mfrow=c(2,2))
plot(model1)
dev.off()

library(sjPlot)
library(nnet)
library(ggplot2)
library(ggpubr)
library(ggeffects)

dat1 <- ggpredict(model1, "pDiversity [0:1]")
dat1$group <- 'Dp'
dat2 <- ggpredict(model1, "cDiversity [0:1]")
dat2$group <- 'Dc'
dat0 <- rbind(dat1, dat2)

palette("R3")

png('C:/Users/Rubinzone/Desktop/Figure3_v24.png', width = 10, height = 4, units = 'in', res = 300)
ggarrange(
  plot_model(model1, 
             type = "pred", 
             terms = "pType",
             dot.size = 6,
             line.size = 2,
             title = expression(paste(italic("T")))) + 
    theme_bw() +
    ylim(0.08, .25) +
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
  
  ggplot(dat0, aes(x = x, y = predicted, colour = group, fill = group),
         title = expression(paste(italic("D")))) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, colour = NA) +
    scale_x_continuous(breaks=seq(0,1,by=0.5)) +
    scale_y_continuous(breaks=seq(-0.5,1.5,by=0.5)) +
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
  widths = c(1.1,1)
)


dev.off()






# fro verification - old model
model_data_old$pType <- as.factor(model_data_old$pType)
model_data_old <- within(model_data_old, pType <- relevel(pType, ref = 'Intersect'))

model_old <- lm(citation_count_n ~ 
                  pType + 
                  log(pAuthors_n) + 
                  pDiversity +
                  cDiversity +
                  pSA1b + pSA2b + pSA3b + pSA4b + pSA5b, 
                data = model_data_old)

summary(model_old)





# multinom model
# Multinomial model on pType
model3 <- multinom(pType ~ pDiversity + cDiversity, data = model_data_v2) 

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










# Data descriptive
p_div <- ggplot(model_data_v2, aes(y=pDiversity)) +
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

c_div <- ggplot(model_data_v2, aes(y=cDiversity)) +
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



library(data.table)
df <- data.table(df)
df_new <- df[sample(.N, 39972)]