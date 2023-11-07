library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(scales)

# IMPORTANT new coding done by colors/labels in legend, SA naming 
# in code/data is incorrect. mapping rule:
# c("SA1","SA2","SA3","SA4","SA5") <- c("SA2", "SA1", "SA4", "SA5", "SA3")
aff_cit <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/citations.csv")
cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")

data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
cog_cit_nb <- subset(cog_cit, pmid %in% data_cognitive_major_nb$pmid)
remove(data_cognitive_major_nb)

cog_cit$total <- cog_cit$A + cog_cit$B + cog_cit$C + cog_cit$D + cog_cit$E +
                  cog_cit$F + cog_cit$G + cog_cit$H + cog_cit$I + cog_cit$J +
                    cog_cit$K + cog_cit$L + cog_cit$M + cog_cit$N + cog_cit$V +
                      cog_cit$Z

cog_cit_nb$total <- cog_cit_nb$A + cog_cit_nb$B + cog_cit_nb$C + cog_cit_nb$D + cog_cit_nb$E +
  cog_cit_nb$F + cog_cit_nb$G + cog_cit_nb$H + cog_cit_nb$I + cog_cit_nb$J +
  cog_cit_nb$K + cog_cit_nb$L + cog_cit_nb$M + cog_cit_nb$N + cog_cit_nb$V +
  cog_cit_nb$Z

aff_cit$total <- aff_cit$A + aff_cit$B + aff_cit$C + aff_cit$D + aff_cit$E +
  aff_cit$F + aff_cit$G + aff_cit$H + aff_cit$I + aff_cit$J +
  aff_cit$K + aff_cit$L + aff_cit$M + aff_cit$N + aff_cit$V +
  aff_cit$Z

aff_cit <- aff_cit[c(2,19,20)]
cog_cit_nb <- cog_cit_nb[c(2,25,26)]
cog_cit <- cog_cit[c(2,25,26)]



aff_cit %>% group_by(year) %>% summarise(total = sum(total, na.rm = T)) -> aff_cit_summ
cog_cit %>% group_by(yearP) %>% summarise(total = sum(total, na.rm = T)) -> cog_cit_summ
cog_cit_nb %>% group_by(yearP) %>% summarise(total = sum(total, na.rm = T)) -> cog_cit_nb_summ

aff_cit_summ <- aff_cit_summ[aff_cit_summ$year %in% c(1950:2019),]
cog_cit_summ <- cog_cit_summ[cog_cit_summ$yearP %in% c(1950:2019),]
cog_cit_nb_summ <- cog_cit_nb_summ[cog_cit_nb_summ$yearP %in% c(1950:2019),]

data_norm <- merge(aff_cit_summ,cog_cit_summ, by.x = "year", by.y = "yearP")
data_norm <- merge(data_norm,cog_cit_nb_summ, by.x = "year", by.y = "yearP")
colnames(data_norm) <- c("yearP","total_aff","total_cog","total_cog_nb")


ggplot(data_norm, aes(x=yearP)) + 
  labs(y='Citations \n (x10^5)') + 
  geom_line(aes(y=total_aff/10^5, color='Affective'),size=1.5) +
  geom_line(aes(y=total_cog_nb/10^5, color='Cognitive, no brain key'),size=1.5) +
  geom_line(aes(y=total_cog/10^5, color='Cognitive'),size=1.5) +
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Affective" = "#949494",
                                                 "Cognitive" = "black",
                                                 "Cognitive, no brain key" = "purple")) +
  guides(color = guide_legend(override.aes = list(size = 4)))




# NORMALIZATION

dev.off()
par(mfrow = c(7, 2))
aff_cit$total_n <- 0

for (stage in c(1:14)) {
  high_bar = stage * 5
  low_bar = high_bar - 4
  print(paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ))
  
  tmp <- subset(aff_cit, year %in% c((1949+low_bar):(1949+high_bar)))$total
  tmp_m <- mean(tmp)
  tmp_sd <- sd(tmp)
  tmp <- (tmp-tmp_m)/tmp_sd 
  
  aff_cit[aff_cit$year %in% c((1949+low_bar):(1949+high_bar)),]$total_n <- tmp
  
  x <- density(tmp, bw = 1)
  plot(x,
       col = "#949494",
       lwd = 3,
       main = paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ),
       xlim = c(-5,5),
       ylim = c(0,0.4),
       ylab = "")
  polygon(x, col="#dfdfdf")
  abline(v = 0, lwd=3, lty=2)
}


dev.off()
par(mfrow = c(7, 2))
cog_cit$total_n <- 0

for (stage in c(1:14)) {
  high_bar = stage * 5
  low_bar = high_bar - 4
  print(paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ))
  
  tmp <- subset(cog_cit, yearP %in% c((1949+low_bar):(1949+high_bar)))$total
  tmp_m <- mean(tmp)
  tmp_sd <- sd(tmp)
  tmp <- (tmp-tmp_m)/tmp_sd 

  cog_cit[cog_cit$yearP %in% c((1949+low_bar):(1949+high_bar)),]$total_n <- tmp
  
  x <- density(tmp, bw = 1)
  plot(x,
       col = "black",
       lwd = 3,
       main = paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ),
       xlim = c(-5,5),
       ylim = c(0,0.4),
       ylab = "")
  polygon(x, col="#949494")
  abline(v = 0, lwd=3, lty=2)
}


dev.off()
par(mfrow = c(7, 2))
cog_cit_nb$total_n <- 0

for (stage in c(1:14)) { # 5yrs intervals
  high_bar = stage * 5
  low_bar = high_bar - 4
  print(paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ))
  
  # 1. take each 5yrs block
  tmp <- subset(cog_cit_nb, yearP %in% c((1949+low_bar):(1949+high_bar)))$total
  # 2. calculate mean for the reviewed time-period
  tmp_m <- mean(tmp)
  # 3. calculate sd
  tmp_sd <- sd(tmp)
  # 4. 
  tmp <- (tmp-tmp_m)/tmp_sd 
  
  cog_cit_nb[cog_cit_nb$yearP %in% c((1949+low_bar):(1949+high_bar)),]$total_n <- tmp
  
  x <- density(tmp, bw = 1)
  plot(x,
       col = "purple3",
       lwd = 3,
       main = paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ),
       xlim = c(-5,5),
       ylim = c(0,0.4),
       ylab = "")
  polygon(x, col="purple")
  abline(v = 0, lwd=3, lty=2)
}




dev.off()




aff_cit %>% group_by(year) %>% summarise(total = sum(total_n, na.rm = T)) -> aff_cit_summ
cog_cit %>% group_by(yearP) %>% summarise(total = sum(total_n, na.rm = T)) -> cog_cit_summ
cog_cit_nb %>% group_by(yearP) %>% summarise(total = sum(total_n, na.rm = T)) -> cog_cit_nb_summ

aff_cit_summ <- aff_cit_summ[aff_cit_summ$year %in% c(1950:2019),]
cog_cit_summ <- cog_cit_summ[cog_cit_summ$yearP %in% c(1950:2019),]
cog_cit_nb_summ <- cog_cit_nb_summ[cog_cit_nb_summ$yearP %in% c(1950:2019),]

data_norm <- merge(aff_cit_summ,cog_cit_summ, by.x = "year", by.y = "yearP")
data_norm <- merge(data_norm,cog_cit_nb_summ, by.x = "year", by.y = "yearP")
colnames(data_norm) <- c("yearP","total_aff","total_cog","total_cog_nb")

ggplot(data_norm, aes(x=yearP)) + 
  labs(y='Citations \n (x10^5)') + 
  geom_line(aes(y=total_aff/10^5, color='Affective'),size=1.5) +
  geom_line(aes(y=total_cog_nb/10^5, color='Cognitive, no brain key'),size=1.5) +
  geom_line(aes(y=total_cog/10^5, color='Cognitive'),size=1.5) +
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Affective" = "#949494",
                                                 "Cognitive" = "black",
                                                 "Cognitive, no brain key" = "purple")) +
  guides(color = guide_legend(override.aes = list(size = 4)))
