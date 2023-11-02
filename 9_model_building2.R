model_data_new <- read.csv("E:/Research/Data_pubmed/model_data23/model_data.csv")
model_data_old <- read.csv("E:/Research/Data_pubmed/model_data/model_data3.csv")


model_data_new <- subset(model_data_new, pYear >= 1950)

# Factorizing variables
model_data_new$pType <- as.factor(model_data_new$pType)
model_data_new$pX_disc <- as.factor(model_data_new$pX_disc)


model <- lm(pCitations_n_norm ~ 
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
              data = model_data_new)

summary(model)


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

model_data_new_v2 <- gen_norm_data(model_data_new)
model_data_new_v2 <- subset(model_data_new_v2, pYear < 2023)


model_data_new_v2 <- within(model_data_new_v2, pType <- relevel(pType, ref = 'Cognitive'))

model2 <- lm(citation_count_n ~ 
              pType + 
              log(pAuthors_n) + 
              pDiversity +
              cDiversity + 
              #rDiversity +
              #cos_mRef_mCit +
              #cos_mCit_pub +
              pEpoch5,
              #cDiversity * pType,
              #pDiversity * cos_mRef_mCit + #temp
              #pDiversity * cos_mCit_pub, 
            data = model_data_new_v2)


model2 <- lm(citation_count_n ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               #rDiversity +
               #cos_mRef_mCit +
               #cos_mCit_pub +
               pEpoch5 +
               cDiversity * pType,
             #pDiversity * cos_mRef_mCit + #temp
             #pDiversity * cos_mCit_pub, 
             data = model_data_new_v2)

model2 <- lm(citation_count_n ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity +
               #cos_mRef_mCit +
               #cos_mCit_pub +
               pEpoch5,
             #cDiversity * pType,
             #pDiversity * cos_mRef_mCit + #temp
             #pDiversity * cos_mCit_pub, 
             data = model_data_new_v2)

summary(model2)

# for plotting in file #7



library(ggeffects)
ggpredict(model2, terms = "pType")


png('C:/Users/Rubinzone/Desktop/F16_nb_model_hires.png', width = 5, height = 5, units = 'in', res = 300)
plot_model(model2, vline.color = "black", show.values = TRUE, value.offset = .3)
dev.off()

png('C:/Users/Rubinzone/Desktop/F15_nb_model_hires.png', width = 14, height = 14, units = 'in', res = 300)

ggarrange(
  plot_model(model2, 
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
      plot.title = element_text(size=rel(2), hjust = 0.5, vjust = 0.5)
    ),
  
  plot_model(model2, 
             type = "pred", 
             terms = "pAuthors_n",
             dot.size = 6,
             line.size = 1.2,
             title = "Number of authors, log, Mean ± SD",
             #axis.lim = list(c(NaN,NaN),
            #            c(mean(log(model_data_new_v2$pAuthors_n), na.rm =T)-sd(log(model_data_new_v2$pAuthors_n), na.rm =T),
            #              mean(log(model_data_new_v2$pAuthors_n), na.rm =T)+sd(log(model_data_new_v2$pAuthors_n), na.rm =T)))
            ) + 
            xlim(c(0,100)
            ) +
    geom_vline(xintercept = mean(log(model_data_new_v2$pAuthors_n), na.rm =T), color = 'red', lwd = 1) +
    geom_vline(xintercept = mean(log(model_data_new_v2$pAuthors_n), na.rm =T)-sd(log(model_data_new_v2$pAuthors_n), na.rm =T), color = 'red',linetype=3, lwd = 1) +
    geom_vline(xintercept = mean(log(model_data_new_v2$pAuthors_n), na.rm =T)+sd(log(model_data_new_v2$pAuthors_n), na.rm =T), color = 'red',linetype=3, lwd = 1) +
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
  
  plot_model(model2, 
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
  
  plot_model(model2, 
             type = "pred", 
             terms = "pDiversity",
             dot.size = 6,
             line.size = 1.2,
             title = "Pub. diversity, Mean ± SD") + 
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity), color = 'red', lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity)-sd(model_data_new_v2$pDiversity, na.rm =T), color = 'red',linetype=3, lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity)+sd(model_data_new_v2$pDiversity, na.rm =T), color = 'red',linetype=3, lwd = 1) +
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
  plot_model(model2, 
             type = "pred", 
             terms = "cDiversity",
             dot.size = 6,
             line.size = 1.2,
             title = "c̄ diversity, Mean ± SD") +
    geom_vline(xintercept = mean(model_data_new_v2$cDiversity, na.rm=T), color = 'red', lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$cDiversity, na.rm=T)-sd(model_data_new_v2$cDiversity, na.rm =T), color = 'red',linetype=3, lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$cDiversity, na.rm=T)+sd(model_data_new_v2$cDiversity, na.rm =T), color = 'red',linetype=3, lwd = 1) +
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
  plot_model(model2, 
             type = "pred", 
             terms = "rDiversity",
             dot.size = 6,
             line.size = 1.2,
             title = "r̄ diversity, Mean ± SD") + 
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity), color = 'red', lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity)-sd(model_data_new_v2$pDiversity, na.rm =T), color = 'red',linetype=3, lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity)+sd(model_data_new_v2$pDiversity, na.rm =T), color = 'red',linetype=3, lwd = 1) +
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
  
  
  ncol = 2, nrow = 3
)
  

dev.off()  
  
  
















  
  plot_model(model2, 
             type = "int",
             #mdrt.values = "meansd",
             dot.size = 6, 
             line.size = 1.2,
             title = "Interaction pType*cDiversity") + 
    labs(color='cDiversity')  +
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
    
  
  plot_model(model15, 
             type = "int",
             mdrt.values = "meansd",
             dot.size = 6, 
             line.size = 1.2,
             title = "Interaction pDiversity * r̄ • c̄")[[1]] +  # pDiversity * cos_mRef_mCit
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity, na.rm=T), color = 'black', lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity, na.rm=T)-sd(model_data_new_v2$pDiversity, na.rm =T), color = 'black',linetype=3, lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity, na.rm=T)+sd(model_data_new_v2$pDiversity, na.rm =T), color = 'black',linetype=3, lwd = 1) +
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
             mdrt.values = "meansd",
             dot.size = 6, 
             line.size = 1.2,
             title = "Interaction pDiversity * c̄ • p")[[2]] +  # pDiversity * cos_mRef_mCit
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity, na.rm=T), color = 'black', lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity, na.rm=T)-sd(model_data_new_v2$pDiversity, na.rm =T), color = 'black',linetype=3, lwd = 1) +
    geom_vline(xintercept = mean(model_data_new_v2$pDiversity, na.rm=T)+sd(model_data_new_v2$pDiversity, na.rm =T), color = 'black',linetype=3, lwd = 1) +
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


# Sampling hi/lo

# lo/lo
df <- subset(model_data_new_v2, pDiversity < mean(model_data_new_v2$pDiversity, na.rm=T)-sd(model_data_new_v2$pDiversity, na.rm =T) & 
         cos_mCit_pub < mean(model_data_new_v2$cos_mCit_pub, na.rm=T)-sd(model_data_new_v2$cos_mCit_pub, na.rm =T))


df[order(df$citation_count_n, decreasing = T),]


# hi/lo
df <- subset(model_data_new_v2, pDiversity > mean(model_data_new_v2$pDiversity, na.rm=T)+sd(model_data_new_v2$pDiversity, na.rm =T) & 
               cos_mCit_pub < mean(model_data_new_v2$cos_mCit_pub, na.rm=T)-sd(model_data_new_v2$cos_mCit_pub, na.rm =T))


df[order(df$citation_count_n, decreasing = T),]



# hi/hi
df <- subset(model_data_new_v2, pDiversity > mean(model_data_new_v2$pDiversity, na.rm=T)+sd(model_data_new_v2$pDiversity, na.rm =T) & 
               cos_mCit_pub > mean(model_data_new_v2$cos_mCit_pub, na.rm=T)+sd(model_data_new_v2$cos_mCit_pub, na.rm =T))


df[order(df$citation_count_n, decreasing = T),]


# lo/hi
df <- subset(model_data_new_v2, pDiversity < mean(model_data_new_v2$pDiversity, na.rm=T)-sd(model_data_new_v2$pDiversity, na.rm =T) & 
               cos_mCit_pub > mean(model_data_new_v2$cos_mCit_pub, na.rm=T)+sd(model_data_new_v2$cos_mCit_pub, na.rm =T))


df[order(df$citation_count_n, decreasing = T),]









#multicolinearity check
library(car)

model2.0 <- lm(citation_count_n ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity + 
               rDiversity +
               cos_mRef_mCit +
               cos_mCit_pub +
               pEpoch5, 
             data = model_data_new_v2)

vif(model2.0)

X<-model_data_new_v2[,c('citation_count_n','pType','pAuthors_n','pDiversity',
                        'cDiversity','rDiversity','cos_mRef_mCit','cos_mCit_pub','pEpoch5')]
library(GGally)
ggpairs(X)









model_data_old$pEpoch5 <- 0
model_data_old$pEpoch5[model_data_old$pYear > 1950] <- 1
model_data_old$pEpoch5[model_data_old$pYear > 1955] <- 2
model_data_old$pEpoch5[model_data_old$pYear > 1960] <- 3
model_data_old$pEpoch5[model_data_old$pYear > 1965] <- 4
model_data_old$pEpoch5[model_data_old$pYear > 1970] <- 5
model_data_old$pEpoch5[model_data_old$pYear > 1975] <- 6
model_data_old$pEpoch5[model_data_old$pYear > 1980] <- 7
model_data_old$pEpoch5[model_data_old$pYear > 1985] <- 8
model_data_old$pEpoch5[model_data_old$pYear > 1990] <- 9
model_data_old$pEpoch5[model_data_old$pYear > 1995] <- 10
model_data_old$pEpoch5[model_data_old$pYear > 2000] <- 11
model_data_old$pEpoch5[model_data_old$pYear > 2005] <- 12
model_data_old$pEpoch5[model_data_old$pYear > 2010] <- 13
model_data_old$pEpoch5[model_data_old$pYear > 2015] <- 14

model_data_old <- subset(model_data_old, pYear >= 1950)
model_data_old <- subset(model_data_old, pYear < 2020)




model2_old <- lm(pCitation_count_norm ~ 
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
                 data = model_data_old)

summary(model2_old)

model_data_old %>% group_by(pYear, pType) %>% 
  summarise(Cit = sum(pCitations_n)) -> data_gr_old

model_data %>% group_by(pYear, pType) %>% 
  summarise(Cit = sum(pCitations_n)) -> data_gr_new

data_gr_old$pType <- as.factor(data_gr_old$pType)
levels(data_gr_old$pType) <- c('Old Affective', 'Old Cognitive')


data_gr <- rbind(data_gr_old, data_gr_new) 


ggplot(data_gr, aes(x=pYear, y=Cit, group=pType, color=pType)) +
geom_line(aes(linetype=pType), size=1.2) +
  scale_color_manual(values=c('lightblue', 'grey', 'blue', 'black')) +
  scale_linetype_manual(values=c('solid','solid','dashed','dashed'))


boxplot()
boxplot()

data <- data.frame( Aff_old = sort(subset(model_data_old, type ='aff')$pCitations_n, decreasing=T)[c(1:50)],
                    Aff_new = sort(subset(model_data, type ='Affective')$pCitations_n, decreasing=T)[c(1:50)],
                    Cog_old = sort(subset(model_data_old, type ='cog nb')$pCitations_n, decreasing=T)[c(1:50)],
                    Cog_new = sort(subset(model_data, type ='Cognitive')$pCitations_n, decreasing=T)[c(1:50)]
)

# Applying boxplot function
boxplot(data)


