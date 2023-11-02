affective <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")
affective_data_cr <- read.csv("E:/Research/Data_pubmed/affective_data/affective_major_UI_cit_ref.csv")

library(tidyverse)
library(dplyr)
library(scales) 
library(ggpubr)

aff_subj$Cat_cnt <- aff_subj$A + aff_subj$B + aff_subj$C + aff_subj$D + aff_subj$E + aff_subj$F + aff_subj$G + aff_subj$H +
                    aff_subj$I + aff_subj$J + aff_subj$K + aff_subj$L + aff_subj$M + aff_subj$N + aff_subj$V + aff_subj$Z

aff_subj$SA1 <- as.numeric(aff_subj$A + aff_subj$B + aff_subj$G)
aff_subj$SA2 <- as.numeric(aff_subj$F)
aff_subj$SA3 <- as.numeric(aff_subj$C + aff_subj$N)
aff_subj$SA4 <- as.numeric(aff_subj$D + aff_subj$E + aff_subj$J + aff_subj$L)
aff_subj$SA5 <- as.numeric(aff_subj$H + aff_subj$I + aff_subj$K + aff_subj$M)
aff_subj$SA_cnt <- as.numeric(aff_subj$SA1 > 0) + as.numeric(aff_subj$SA2 > 0) + as.numeric(aff_subj$SA3 > 0) + 
                    as.numeric(aff_subj$SA4 > 0) + as.numeric(aff_subj$SA5 > 0)


aff_aggregated <- merge(affective[,c(2,3)], aff_subj[,c(1,24)], by = "pmid")
aff_aggregated$mono <- as.numeric(aff_aggregated$SA_cnt < 2) 
aff_aggregated$x1 <- as.numeric(aff_aggregated$SA_cnt == 1) 
aff_aggregated$x2 <- as.numeric(aff_aggregated$SA_cnt == 2) 
aff_aggregated$x3 <- as.numeric(aff_aggregated$SA_cnt == 3) 
aff_aggregated$x4 <- as.numeric(aff_aggregated$SA_cnt == 4) 
aff_aggregated$x5 <- as.numeric(aff_aggregated$SA_cnt == 5) 

affective_data_cr <- merge(affective_data_cr, aff_aggregated[,c(1,2)], by = "pmid")

affective_data_cr %>% group_by(year) %>% summarise(avg_cit = mean(citation_count)) -> affective_data_cr_g
aff_aggregated <- merge(aff_aggregated,affective_data_cr[,c(1,3)], by = "pmid")
aff_aggregated <- merge(aff_aggregated,affective_data_cr_g, by = "year")

aff_aggregated_more <- aff_aggregated[aff_aggregated$citation_count > aff_aggregated$avg_cit,]
aff_aggregated_less <- aff_aggregated[aff_aggregated$citation_count <= aff_aggregated$avg_cit,]


aff_aggregated_more %>% group_by(year) %>% summarise(x1_more = sum(x1, na.rm = T),
                                                x2_more = sum(x2, na.rm = T),
                                                x3_more = sum(x3, na.rm = T),
                                                x4_more = sum(x4, na.rm = T),
                                                x5_more = sum(x5, na.rm = T),
                                                total_more = n()) -> aff_final_more

aff_aggregated_less %>% group_by(year) %>% summarise(x1_less = sum(x1, na.rm = T),
                                                x2_less = sum(x2, na.rm = T),
                                                x3_less = sum(x3, na.rm = T),
                                                x4_less = sum(x4, na.rm = T),
                                                x5_less = sum(x5, na.rm = T),
                                                total_less = n()) -> aff_final_less


aff_final <- merge(aff_final_more, aff_final_less, by = "year")
aff_final$total <- aff_final$total_more + aff_final$total_less

a <- ggplot(aff_final, aes(x=year)) +
  labs(y='Thousands P, featuring cross-domain comb.') +
  geom_line(aes(y=x1_more, color='One'),size=1) +
  geom_line(aes(y=x2_more, color='Two'),size=1) +
  geom_line(aes(y=x3_more, color='Three'),size=1) +
  geom_line(aes(y=x4_more, color='Four'),size=1) +
  geom_line(aes(y=x5_more, color='Five'),size=1) + 
  geom_line(aes(y=x1_less, color='One'), linetype = "dashed",size=1) +
  geom_line(aes(y=x2_less, color='Two'), linetype = "dashed",size=1) +
  geom_line(aes(y=x3_less, color='Three'), linetype = "dashed",size=1) +
  geom_line(aes(y=x4_less, color='Four'), linetype = "dashed",size=1) +
  geom_line(aes(y=x5_less, color='Five'), linetype = "dashed",size=1) + 
  xlim(1967, 2019) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1)),
        plot.title = element_text(size = 9, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none",
        legend.text = element_text(size=rel(1))) +
  scale_color_manual(name = "",       values = c("One" = "black", 
                                                 "Two" = "grey", 
                                                 "Three" = "light blue", 
                                                 "Four" = "blue", 
                                                 "Five"  = "dark blue")) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e-3))


b <- ggplot(aff_final, aes(x=year)) +
  labs(y='Fraction of P, featuring cross-domain comb.') +
  geom_line(aes(y=x1_more/total_more, color='1 (M) SA'),size=1) +
  geom_line(aes(y=x2_more/total_more, color='2 (X) SA'),size=1) +
  geom_line(aes(y=x3_more/total_more, color='3 (X) SA'),size=1) +
  geom_line(aes(y=x4_more/total_more, color='4 (X) SA'),size=1) +
  geom_line(aes(y=x5_more/total_more, color='5 (X) SA'),size=1) + 
  geom_line(aes(y=x1_less/total_less, color='1 (M) SA'), linetype = "dashed",size=1) +
  geom_line(aes(y=x2_less/total_less, color='2 (X) SA'), linetype = "dashed",size=1) +
  geom_line(aes(y=x3_less/total_less, color='3 (X) SA'), linetype = "dashed",size=1) +
  geom_line(aes(y=x4_less/total_less, color='4 (X) SA'), linetype = "dashed",size=1) +
  geom_line(aes(y=x5_less/total_less, color='5 (X) SA'), linetype = "dashed",size=1) + 
  xlim(1967, 2019) +
  ylim(0,0.65) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.5)),
        axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1)),
        plot.title = element_text(size = 9, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "bottom",
        legend.text = element_text(size=rel(1.3))) +
  scale_color_manual(name = "",       values = c("1 (M) SA" = "black", 
                                                 "2 (X) SA" = "grey", 
                                                 "3 (X) SA" = "light blue", 
                                                 "4 (X) SA" = "blue", 
                                                 "5 (X) SA"  = "dark blue"))







ggarrange(a + rremove("x.text"), b,
          ncol = 1, nrow = 2, heights = c(1,1.2))