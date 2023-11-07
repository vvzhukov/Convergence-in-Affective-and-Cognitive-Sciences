library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(scales)

# IMPORTANT new coding done by colors/labels in legend, SA naming 
# in code/data is incorrect. mapping rule:
# c("SA1","SA2","SA3","SA4","SA5") <- c("SA2", "SA1", "SA4", "SA5", "SA3")


cit_links_summ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA.csv")
cit_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA_publ.csv")
cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")

data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
cog_cit_nb <- subset(cog_cit, pmid %in% data_cognitive_major_nb$pmid)

remove(data_cognitive_major_nb)

cit_links_summ$total_hits_SA <- cit_links_summ$SA1_ABG +
  cit_links_summ$SA2_F +
  cit_links_summ$SA3_CN +
  cit_links_summ$SA4_DEJL +
  cit_links_summ$SA5_HIKM

cit_links_summ_publ$total_hits_SA <- cit_links_summ_publ$SA1_ABG +
  cit_links_summ_publ$SA2_F +
  cit_links_summ_publ$SA3_CN +
  cit_links_summ_publ$SA4_DEJL +
  cit_links_summ_publ$SA5_HIKM

cog_cit %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         total = n()) -> cog_cit_summ

cog_cit_summ$total_SA_hits <- cog_cit_summ$SA1 + cog_cit_summ$SA2 + cog_cit_summ$SA3 + cog_cit_summ$SA4 + cog_cit_summ$SA5

cog_cit %>% group_by(yearP) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                          SA2 = sum(SA2, na.rm = T),
                                          SA3 = sum(SA3, na.rm = T),
                                          SA4 = sum(SA4, na.rm = T),
                                          SA5 = sum(SA5, na.rm = T),
                                          total = n()) -> cog_cit_summ2

cog_cit_summ2$total_SA_hits <- cog_cit_summ2$SA1 + cog_cit_summ2$SA2 + cog_cit_summ2$SA3 + cog_cit_summ2$SA4 + cog_cit_summ2$SA5

cog_cit_nb %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                            SA2 = sum(SA2, na.rm = T),
                                            SA3 = sum(SA3, na.rm = T),
                                            SA4 = sum(SA4, na.rm = T),
                                            SA5 = sum(SA5, na.rm = T),
                                            total = n()) -> cog_cit_summ_nb

cog_cit_summ_nb$total_SA_hits <- cog_cit_summ_nb$SA1 + cog_cit_summ_nb$SA2 + cog_cit_summ_nb$SA3 + cog_cit_summ_nb$SA4 + cog_cit_summ_nb$SA5

cog_cit_nb %>% group_by(yearP) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                             SA2 = sum(SA2, na.rm = T),
                                             SA3 = sum(SA3, na.rm = T),
                                             SA4 = sum(SA4, na.rm = T),
                                             SA5 = sum(SA5, na.rm = T),
                                             total = n()) -> cog_cit_summ2_nb

cog_cit_summ2_nb$total_SA_hits <- cog_cit_summ2_nb$SA1 + cog_cit_summ2_nb$SA2 + cog_cit_summ2_nb$SA3 + cog_cit_summ2_nb$SA4 + cog_cit_summ2_nb$SA5



summ_data <- merge(cit_links_summ_publ[,c(2,35)], 
              cog_cit_summ2_nb[,c(1,7)], by.x = "year_published", by.y = "yearP")
summ_data <- merge(summ_data, 
                   cog_cit_summ2[,c(1,7)], by.x = "year_published", by.y = "yearP")

colnames(summ_data) <- c("yearP", "total_aff", "total_cog_nb", "total_cog")


summ_data_norm <- summ_data[c(3:72),] # 1949 - 2019
summ_data_norm$total_aff <- log(summ_data_norm$total_aff +1)
summ_data_norm$total_cog <- log(summ_data_norm$total_cog +1)
summ_data_norm$total_cog_nb <- log(summ_data_norm$total_cog_nb +1)

summ_data_norm_5yr <- summ_data_norm

for (stage in c(1:14)) {
  # 5 yrs intervals, no sliding window
  
  high_bar =  stage * 5
  low_bar = high_bar - 4
  # print(paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ))
  
  # (value - mean5yr) / sd5yr
  total_aff_tmp <- (summ_data_norm[c(low_bar:high_bar),]$total_aff - mean(summ_data_norm[c(low_bar:high_bar),]$total_aff)) / sd(summ_data_norm[c(low_bar:high_bar),]$total_aff)
  total_cog_tmp <- (summ_data_norm[c(low_bar:high_bar),]$total_cog - mean(summ_data_norm[c(low_bar:high_bar),]$total_cog)) / sd(summ_data_norm[c(low_bar:high_bar),]$total_cog)
  total_cog_nb_tmp <- (summ_data_norm[c(low_bar:high_bar),]$total_cog_nb - mean(summ_data_norm[c(low_bar:high_bar),]$total_cog_nb)) / sd(summ_data_norm[c(low_bar:high_bar),]$total_cog_nb)
  
  summ_data_norm_5yr[c(low_bar:high_bar),]$total_aff <- total_aff_tmp
  summ_data_norm_5yr[c(low_bar:high_bar),]$total_cog <- total_cog_tmp
  summ_data_norm_5yr[c(low_bar:high_bar),]$total_cog_nb <- total_cog_nb_tmp
}



summ_data_norm_5yr_sliding <- summ_data_norm

for (stage in c(1:66)) {
  # sliding window

  low_bar = stage
  high_bar =  stage+4
  print(paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ))
  
  # (value - mean5yr) / sd5yr
  total_aff_tmp <- (summ_data_norm[c(low_bar:high_bar),]$total_aff - mean(summ_data_norm[c(low_bar:high_bar),]$total_aff)) / sd(summ_data_norm[c(low_bar:high_bar),]$total_aff)
  total_cog_tmp <- (summ_data_norm[c(low_bar:high_bar),]$total_cog - mean(summ_data_norm[c(low_bar:high_bar),]$total_cog)) / sd(summ_data_norm[c(low_bar:high_bar),]$total_cog)
  total_cog_nb_tmp <- (summ_data_norm[c(low_bar:high_bar),]$total_cog_nb - mean(summ_data_norm[c(low_bar:high_bar),]$total_cog_nb)) / sd(summ_data_norm[c(low_bar:high_bar),]$total_cog_nb)
  
  summ_data_norm_5yr_sliding[c(low_bar:high_bar),]$total_aff <- total_aff_tmp
  summ_data_norm_5yr_sliding[c(low_bar:high_bar),]$total_cog <- total_cog_tmp
  summ_data_norm_5yr_sliding[c(low_bar:high_bar),]$total_cog_nb <- total_cog_nb_tmp
}



a <- round(summ_data$total_aff/10^5 - summ_data$total_cog_nb/10^5,3)
b <- round(summ_data_norm_5yr$total_aff - summ_data_norm_5yr$total_cog_nb,3)
c <- round(summ_data_norm_5yr_sliding$total_aff - summ_data_norm_5yr_sliding$total_cog_nb,3)
n <- 5

bar_data <- data.frame(date = c("1950 - 1954",
                                "1955 - 1959",
                                "1960 - 1964",
                                "1965 - 1969",
                                "1970 - 1974",
                                "1975 - 1979",
                                "1980 - 1984",
                                "1985 - 1989",
                                "1990 - 1994",
                                "1995 - 1999",
                                "2000 - 2004",
                                "2005 - 2009",
                                "2010 - 2014",
                                "2015 - 2019"),
                       raw = (tibble::tibble(a) %>%
                                group_by(group = gl(length(a)/n, n)) %>%
                                summarise(mean_val = mean(a)))$mean_val[1:14],
                       norm = round((tibble::tibble(b) %>%
                                 group_by(group = gl(length(b)/n, n)) %>%
                                 summarise(mean_val = mean(b)))$mean_val,3),
                       norm_slide = (tibble::tibble(c) %>%
                                       group_by(group = gl(length(c)/n, n)) %>%
                                       summarise(mean_val = mean(c)))$mean_val
)


plot1 <- ggplot(summ_data, aes(x=yearP)) + 
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


plot2 <- ggplot(summ_data_norm_5yr, aes(x=yearP)) + 
  labs(y='Citations, normalised, \n 5 yrs fixed') + 
  geom_line(aes(y=total_aff, color='Affective'),size=1.5) +
  geom_line(aes(y=total_cog_nb, color='Cognitive, no brain key'),size=1.5) +
  geom_line(aes(y=total_cog, color='Cognitive'),size=1.5) +
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

plot3 <- ggplot(summ_data_norm_5yr_sliding, aes(x=yearP)) + 
  labs(y='Citations, normalised, \n 5 yrs sliding window') + 
  geom_line(aes(y=total_aff, color='Affective'),size=1.5) +
  geom_line(aes(y=total_cog_nb, color='Cognitive, no brain key'),size=1.5) +
  geom_line(aes(y=total_cog, color='Cognitive'),size=1.5) +
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


png('C:/Users/Rubinzone/Desktop/F11_hires.png', width = 8, height = 4, units = 'in', res = 300)

ggarrange(plot1,
          #plot2 + rremove("x.text"),
          #plot3,
          ncol = 1, nrow = 1,
          heights = c(1),
          widths = c(1),
          common.legend = TRUE, legend="bottom")

dev.off()




plot1_2 <- ggplot(summ_data, aes(x=yearP)) + 
  labs(y='Citations \n (x10^5)') + 
  geom_line(aes(y=total_aff/10^5 - total_cog_nb/10^5, color='Affective - Cognitive, no brain key'),size=1.5) +
  xlim(1967, 2019) +
  ylim(-2,8) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  scale_color_manual(name = "",       values = c("Affective - Cognitive, no brain key" = "black")) +
  guides(color = guide_legend(override.aes = list(size = 4)))

plot2_2 <- ggplot(summ_data_norm_5yr, aes(x=yearP)) + 
  labs(y='Citations, normalised, \n 5 yrs fixed') + 
  geom_line(aes(y=total_aff - total_cog_nb, color='Affective - Cognitive, no brain key'),size=1.5) +
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
  scale_color_manual(name = "",       values = c('Affective - Cognitive, no brain key' = "black")) +
  guides(color = guide_legend(override.aes = list(size = 4)))

plot3_2 <- ggplot(summ_data_norm_5yr_sliding, aes(x=yearP)) + 
  labs(y='Citations, normalised, \n 5 yrs sliding window') + 
  geom_line(aes(y=total_aff - total_cog_nb, color='Affective - Cognitive, no brain key'),size=1.5) +
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
  scale_color_manual(name = "",       values = c('Affective - Cognitive, no brain key' = "black")) +
  guides(color = guide_legend(override.aes = list(size = 4)))


png('C:/Users/Rubinzone/Desktop/F11_hires_2.png', width = 8, height = 4, units = 'in', res = 300)

ggarrange(plot1_2,
          #plot2_2 + rremove("x.text"),
          #plot3_2,
          ncol = 1, nrow = 1,
          heights = c(1),
          widths = c(1),
          common.legend = TRUE, legend="bottom")

dev.off()




plot1_3 <- ggplot(bar_data, aes(date,raw)) +
  geom_col() +
  labs(y='Citations: Affective - Cognitive, no brain key \n mean 5 yr, (x10^5)') + 
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45,hjust=1,size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  guides(color = guide_legend(override.aes = list(size = 4)))


plot2_3 <- ggplot(bar_data, aes(date,norm)) +
  geom_col() +
  labs(y='Citations normalized: Affective - Cognitive, no brain key \n mean 5 yr, (x10^5)') + 
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45,hjust=1,size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  guides(color = guide_legend(override.aes = list(size = 4)))

plot3_3 <- ggplot(bar_data, aes(date,norm_slide)) +
  geom_col() +
  labs(y='Citations normalized: Affective - Cognitive, no brain key \n sliding window, mean 5 yr, (x10^5)') + 
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45,hjust=1,size=rel(1.5),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1),family="NimbusMon")) +
  guides(color = guide_legend(override.aes = list(size = 4)))


png('C:/Users/Rubinzone/Desktop/F11_hires_3.png', width = 8, height = 6, units = 'in', res = 300)
ggarrange(plot1_3,
          #plot3_3,
          ncol = 1, nrow = 1,
          heights = c(1,1.2),
          widths = c(1),
          common.legend = TRUE, legend="bottom")
dev.off()





dev.off()
par(mfrow = c(7, 2))

for (stage in c(1:14)) {
  high_bar =  stage * 5
  low_bar = high_bar - 4
  
  print(summ_data_norm_5yr[c(low_bar:high_bar),])
  
  plot(density(summ_data_norm_5yr[c(low_bar:high_bar),]$total_aff),
       main = paste(as.character(1949+low_bar), "-", as.character(1949+high_bar) ),
       xlim = c(-5,5),
       ylim = c(0,0.5))
}





x <- rnorm(1000)
plot(density(summ_data_norm_5yr[c(low_bar:high_bar),]$total_aff))
hist(x)
