library(dplyr)
library(ggplot2)

nodes_d <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")

# For citations
# Top 5 mos cited pubs
# 3yrs before/after

#1962
top_ids <- function(years, flag='cit'){
  for (yearV in years) {
    if (flag == 'ref') {
      temp <- subset(nodes_d, Year_target == yearV)
      temp$ref <- temp$ref_bio + temp$ref_beh + temp$ref_ti + temp$ref_na
      print(paste("Year: ", yearV))
      print(temp[order(temp$ref),]$ID[c(1:30)])
    } else {
      temp <- subset(nodes_d, Year_target == yearV)
      temp$cit <- temp$cit_bio + temp$cit_beh + temp$cit_ti + temp$cit_na      
      print(paste("Year: ", yearV))
      print(temp[order(temp$cit),]$ID[c(1:30)])
    }
  }
}

top_ids(c(1962:1968))
top_ids(c(1982:1988),flag='ref')

subset(nodes_d, ID == 4158664)



# PART 2
# updated logic
library(dplyr)

# upload data
nodes_c <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/sa-nodes5.csv")
edges <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/sa-edges5.csv")

# CITATIONS (what from non affective was cited the most)
cit_plot2 +
  geom_vline(xintercept=1965) +
  geom_vline(xintercept=1971)


# selecting all non affective
edges_t_na <- subset(edges, Target %in% nodes_c[nodes_c$Cluster == 'Non affective',]$ID)
years <- c(1965)
yearV < 1965
# df to store stats
avg_5_cit <- data_frame("Year"=c(), "mean_top5_cit"=c(), "median_top5_cit"=c())

for (yearV in years) {
  print(yearV)
  temp <- subset(edges_t_na, Year == yearV)
  temp %>% 
    group_by(Target) %>% 
    summarise(n = n()) -> temp2
  
  # ordering, selecting top records
  temp5 <- temp2[order(temp2$n,decreasing = TRUE),][c(1:5),]
  print(temp5)
  print(subset(temp, Target %in% temp5$Target))
  
  #collecting stats for the vis:
  #avg_5_cit <<- rbind(avg_5_cit, c(yearV, mean(temp5$n), median(temp5$n)))
}

colnames(avg_5_cit) <- c("year", "mean_cit", "median_cit")

ggplot(avg_5_cit, aes(x=year)) +
  geom_line(aes(y=mean_cit), cex = 1.5) + 
  geom_line(aes(y=median_cit), linetype = "dashed") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())





# REFERENCES (who were citing the most)
yearV <- 1962
temp <- subset(edges, Year == yearV)
temp2 <- subset(edges, Target %in% temp$Source)

temp2 %>% 
  group_by(Source) %>% 
  summarise(n = n()) -> temp3
temp3[order(temp3$n,decreasing = TRUE),][c(1:5),]


years <- c(1982:1988)

for (yearV in years) {
  print(yearV)
  temp <- subset(edges, Year == yearV)
  temp2 <- subset(edges, Target %in% temp$Source)
  
  temp2 %>% 
    group_by(Source) %>% 
    summarise(n = n()) -> temp3
  
  # ordering, selecting top records
  temp3 <- temp3[order(temp3$n,decreasing = TRUE),][c(1:5),]
  print(temp3)
  #print(subset(temp, Source %in% temp5$Source))
}






# PART 3
# descriptive stats
library(dplyr)
library(ggplot2)


#SE fundtion
std <- function(x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x) / length(x))
}


# upload data
nodes_c <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/sa-nodes5.csv")
edges <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/sa-edges5.csv")

# n numbers for each year
years <- c(1950:2020)
stats_edges <- data_frame("Year"=c(), "n" =c() ,"mean_cit"=c(), "median_cit"=c(), "se_cit" = c())
stats_values <- list()
stats_values_df <- data_frame("Data"=c(), "Year"=c())

for (year in years) {
  temp <- subset(edges, Year == year)
  temp %>% 
    group_by(Target) %>% 
    summarise(n = n()) -> temp2
  stats_edges <<- rbind(stats_edges, c(year, nrow(temp),mean(temp2$n),std(temp2$n)))
  stats_values <- append(stats_values, list(temp2$n))
  
  temp_df <- data_frame("Data" = temp2$n)
  temp_df$Year <- year
  stats_values_df <<- union_all(stats_values_df, temp_df)
}

colnames(stats_edges) <- c("Year", "n", "mean_cit", "se_cit")
stats_edges$mean_cit <- round(stats_edges$mean_cit, 4)
stats_edges$se_cit <- round(stats_edges$se_cit, 4)
stats_edges$Details <- NULL

stats_values_df$Year <- factor(stats_values_df$Year)

ggplot(stats_values_df, aes(x=Year, y=Data)) +
  geom_boxplot() +
  xlab("") +
  ylab("Citations number") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

# stats without 1
stats_values_df_f1 <- subset(stats_values_df, Data > 1)



ggplot(subset(stats_values_df_f1, Year %in% c(1950:2000)), aes(x=Year, y=Data)) +
  geom_boxplot() +
  xlab("") +
  ylab("Citations number") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1)) + 
  scale_y_continuous(breaks= c(0:27))

ggplot(subset(stats_values_df_f1, Year %in% c(2001:2021)), aes(x=Year, y=Data)) +
  geom_boxplot() +
  xlab("") +
  ylab("Citations number") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

# stats without 5
stats_values_df_f2 <- subset(stats_values_df, Data > 5)

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

ggplot(stats_values_df_f2, aes(x=Year, y=Data)) +
  geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text", fun = median, 
               position = position_dodge(width = 0.75),
               size = 4) +
  stat_summary(
               aes(label=after_stat(y), y = stage(Data, after_stat = 4)), 
               fun.data = give.n, geom = "text", fun = give.n, 
               position = position_dodge(width = 0.75),
               size = 4
               ) +
  #stat_summary(fun=mean, geom="point", shape=18, size=4, col="white", 
  #             position = position_dodge(0.9)) +
  xlab("") +
  ylab("Citations number") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1, size = 20)) +
  ylim(c(3,20))

# Create an aggregate of median & count
cts <- merge(aggregate(Data ~ Year, stats_values_df_f2, length), 
               aggregate(Data ~ Year, stats_values_df_f2, median), 
               by=c("Year"))
# Rename the col names to fit with the original dataset..
names(cts) <- c("Year", "count", "Data")
cts$Data <- 3

ggplot(stats_values_df_f2, aes(x=Year, y=Data)) + 
  geom_boxplot(position = position_dodge(width=1.0)) + 
  geom_text(data = cts, aes(label=count), 
            position=position_dodge(width=1.0),
            angle = 60) + 
  xlab("") +
  ylab("Citations number") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1, size = 20)) +
  ylim(c(3,20))


# Trace back top publ
temp %>%
  group_by(Target) %>% 
  summarise(n = n()) -> temp2020

temp_s1965 <- subset(edges, Year < 1990)
selection <- unique(subset(temp_s1965, Target %in% head(temp2020[order(temp2020$n,decreasing = TRUE),], 1500)$Target)$Target)

subset(subset(nodes_c, ID %in% selection), Cluster != "Non affective")

subset(temp2020, Target %in% c(14044222, 6059863, 5349366, 5146491, 847061, 843571, 7154893, 6392204, 3616518, 28136248))
selection2 <- c(14044222, 6059863, 5349366, 5146491, 847061, 843571, 7154893, 6392204, 3616518, 28136248)


nodes_d <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")
nodes_d <- subset(nodes_d, ID %in% selection2)

nodes_e <- nodes_d[,c(5,6,7,8,9,10,12,13,14,15,16)]
nodes_e %>% group_by(Year_target) %>% summarise(cit_other = sum(cit_other, na.rm = T),
                                                cit_bio = sum(cit_bio, na.rm = T),
                                                cit_beh = sum(cit_beh, na.rm = T),
                                                cit_ti = sum(cit_ti, na.rm = T),
                                                cit_na = sum(cit_na, na.rm = T),
                                                ref_other = sum(ref_other, na.rm = T),
                                                ref_bio = sum(ref_bio, na.rm = T),
                                                ref_beh = sum(ref_beh, na.rm = T),
                                                ref_ti = sum(ref_ti, na.rm = T),
                                                ref_na = sum(ref_na, na.rm = T)) -> nodes_e

# Remove 'other' from citations (same as na) and from references
nodes_e$cit <- nodes_e$cit_bio + nodes_e$cit_beh + nodes_e$cit_ti + nodes_e$cit_na
nodes_e$ref <- nodes_e$ref_bio + nodes_e$ref_beh + nodes_e$ref_ti + nodes_e$ref_na
nodes_e$cit_other <- NULL
nodes_e$ref_other <- NULL

nodes_e$cit <- nodes_e$cit_bio + nodes_e$cit_beh + nodes_e$cit_ti + nodes_e$cit_na
nodes_e$ref <- nodes_e$ref_bio + nodes_e$ref_beh + nodes_e$ref_ti + nodes_e$ref_na

cit_na_prop <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=cit_bio/cit), color="brown",size=1) +
  geom_line(aes(y=cit_beh/cit), color="pink",size=1) +
  geom_line(aes(y=cit_ti/cit), color="darkgreen",size=1) +
  geom_line(aes(y=cit_na/cit), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) +
  scale_y_continuous(labels = label_number(suffix = " ", scale = 1), limits = c(0,1)) +
  ggtitle("")


cit_na_abs <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=cit_bio), color="brown",size=1) +
  geom_line(aes(y=cit_beh), color="pink",size=1) +
  geom_line(aes(y=cit_ti), color="darkgreen",size=1) +
  geom_line(aes(y=cit_na), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) +
  ggtitle("")


cit_na_abs_scale <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=cit_bio), color="brown",size=1) +
  geom_line(aes(y=cit_beh), color="pink",size=1) +
  geom_line(aes(y=cit_ti), color="darkgreen",size=1) +
  geom_line(aes(y=cit_na), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) +
  xlim(1965, 2005) +
  ylim(0, 100) + 
  ggtitle("")

cit_na_abs <- cit_na_abs + annotation_custom(ggplotGrob(cit_na_abs_scale), xmin = 1970, xmax = 2000, 
                                          ymin = 100, ymax = 500)

ggarrange(cit_na_abs, cit_na_prop,  
          labels = c("A", "B"),
          ncol = 1, nrow = 2,
          common.legend = TRUE, legend="bottom")



# same for affect

subset(temp2020, Target %in% c(13638508, 14399272, 13688369, 14221692, 6080235, 1202204, 444788, 472086, 6399758, 3901065))
selection3 <- c(13638508, 14399272, 13688369, 14221692, 6080235, 1202204, 444788, 472086, 6399758, 3901065)


nodes_d <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")
nodes_d <- subset(nodes_d, ID %in% selection3)