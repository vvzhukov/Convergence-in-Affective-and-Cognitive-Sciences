library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(ggpubr)

nodes_d <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")

# create a dataset
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
nodes_e <- nodes_e[1:71,]
write.csv(nodes_e, "C:/Users/Rubinzone/Desktop/citations_references_trends.csv")

nodes_ref <- melt(nodes_e[,c(1,8,9,10,11)], id.vars = "Year_target")
nodes_cit <- melt(nodes_e[,c(1,3,4,5,6)], id.vars = "Year_target")
nodes_ref[218,3] <- 0

# Stacked + percent
ggplot(nodes_ref, aes(fill=variable, y=value, x=Year_target)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual("legend", values = c("ref_bio" = "brown", "ref_beh" = "pink", "ref_ti" = "darkgreen", "ref_na" = "blue")) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggplot(nodes_cit, aes(fill=variable, y=value, x=Year_target)) + 
  geom_bar(position="fill", stat="identity")  + 
  scale_fill_manual("legend", values = c("cit_bio" = "brown", "cit_beh" = "pink", "cit_ti" = "darkgreen", "cit_na" = "blue")) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

nodes_ref <- melt(nodes_e[,c(1,8,9,10)], id.vars = "Year_target")
nodes_cit <- melt(nodes_e[,c(1,3,4,5)], id.vars = "Year_target")


ggplot(nodes_ref, aes(fill=variable, y=value, x=Year_target)) + 
  geom_bar(position="fill", stat="identity") + 
    scale_fill_manual("legend", values = c("ref_bio" = "brown", "ref_beh" = "pink", "ref_ti" = "darkgreen")) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())

ggplot(nodes_cit, aes(fill=variable, y=value, x=Year_target)) + 
  geom_bar(position="fill", stat="identity")  + 
    scale_fill_manual("legend", values = c("cit_bio" = "brown", "cit_beh" = "pink", "cit_ti" = "darkgreen")) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())


# Data for the Scheme with a single node
nodes_d <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/network_details2.csv")
nodes_c <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/sa-nodes5.csv")
edges <- read.csv("C:/Users/Rubinzone/Desktop/Network_visualisations/data-network/sa-edges5.csv")

temp <- subset(nodes_d, !is.na(ref_other) & !is.na(cit_other))
subset(temp, ID == 28068404)
subset(edges, Target == 28068404)$Source
subset(nodes_c, ID %in% subset(edges, Target == 28068404)$Source)
subset(nodes_c, ID %in% subset(edges, Source == 28068404)$Target)





# Fractions II

# create a dataset
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



# Normalization for citations
nodes_d$cit_other <- NULL
nodes_d$ref_other <- NULL

nodes_d$cit_bio[is.na(nodes_d$cit_bio)] <- 0
nodes_d$cit_beh[is.na(nodes_d$cit_beh)] <- 0
nodes_d$cit_ti[is.na(nodes_d$cit_ti)] <- 0
nodes_d$cit_na[is.na(nodes_d$cit_na)] <- 0

nodes_d$cit <- nodes_d$cit_bio + nodes_d$cit_beh + nodes_d$cit_ti + nodes_d$cit_na

# n
nodes_d %>% group_by(Year_target) %>% 
  summarise(n = n()) -> nodes_all

# cit
nodes_d %>% group_by(Year_target) %>% 
  summarise(cit = sum(cit)) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

# cit mean
nodes_d %>% group_by(Year_target) %>% 
  summarise(cit_mean = mean(cit)) -> nodes_d_temp

nodes_d <- left_join(nodes_d, nodes_d_temp)
nodes_all <- left_join(nodes_all, nodes_d_temp)


# n above
nodes_d %>% 
  filter(
    cit >= cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_above = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

# n below
nodes_d %>% 
  filter(
    cit < cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_below = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)


# bio above

nodes_d %>% 
  filter(
    cit_bio >= cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_above_bio = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

# beh above

nodes_d %>% 
  filter(
    cit_beh >= cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_above_beh = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

# ti above

nodes_d %>% 
  filter(
    cit_ti >= cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_above_ti = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

# na above

nodes_d %>% 
  filter(
    cit_na >= cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_above_na = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)


# bio below

nodes_d %>% 
  filter(
    cit_bio < cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_below_bio = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

# beh below

nodes_d %>% 
  filter(
    cit_beh < cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_below_beh = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

# ti below

nodes_d %>% 
  filter(
    cit_ti < cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_below_ti = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

# na below

nodes_d %>% 
  filter(
    cit_na < cit_mean
  ) %>%
  group_by(Year_target) %>%
  summarise(
    n_below_na = n()
  ) -> nodes_d_temp

nodes_all <- left_join(nodes_all, nodes_d_temp)

remove(nodes_d_temp)
nodes_all$n_below[is.na(nodes_all$n_below)] <- 0
nodes_all$n_below_bio[is.na(nodes_all$n_below_bio)] <- 0
nodes_all$n_below_beh[is.na(nodes_all$n_below_beh)] <- 0
nodes_all$n_below_ti[is.na(nodes_all$n_below_ti)] <- 0
nodes_all$n_below_na[is.na(nodes_all$n_below_na)] <- 0

nodes_all$n_above[is.na(nodes_all$n_above)] <- 0
nodes_all$n_above_bio[is.na(nodes_all$n_above_bio)] <- 0
nodes_all$n_above_beh[is.na(nodes_all$n_above_beh)] <- 0
nodes_all$n_above_ti[is.na(nodes_all$n_above_ti)] <- 0
nodes_all$n_above_na[is.na(nodes_all$n_above_na)] <- 0


cit_plotn <- ggplot(data = nodes_all, aes(x=Year_target)) +
  geom_line(aes(y=n_above_bio/n_above), color = "brown",size=1) +
  #geom_line(aes(Year_target, n_below_bio/n_below), color = "brown", linetype = "dashed",size=1) +
  geom_line(aes(y=n_above_beh/n_above), color = "pink",size=1) +
  #geom_line(aes(Year_target, n_below_beh/n_below), color = "pink", linetype = "dashed",size=1) +
  geom_line(aes(y=n_above_ti/n_above), color = "darkgreen",size=1) +
  #geom_line(aes(Year_target, n_below_ti/n_below), color = "darkgreen", linetype = "dashed",size=1) +
  geom_line(aes(y=n_above_na/n_above), color = "blue",size=1) +
  #geom_line(aes(Year_target, n_below_na/n_below), color = "blue", linetype = "dashed",size=1) +
  ylim(0, 1) + 
  xlab("") + 
  ylab("")

nodes_all <- nodes_all[,c(1,2,13,14,3:12)]


nodes_e$cit <- nodes_e$cit_bio + nodes_e$cit_beh + nodes_e$cit_ti + nodes_e$cit_na
nodes_e$ref <- nodes_e$ref_bio + nodes_e$ref_beh + nodes_e$ref_ti + nodes_e$ref_na

#nodes_ref <- melt(nodes_e[,c(1,8,9,10,11)], id.vars = "Year_target")
#nodes_cit <- melt(nodes_e[,c(1,3,4,5,6)], id.vars = "Year_target")
#nodes_ref[218,3] <- 0

# Stacked + percent
cit_plot <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=cit_bio), color="brown",size=1) +
  geom_line(aes(y=cit_beh), color="pink",size=1) +
  geom_line(aes(y=cit_ti), color="darkgreen",size=1) +
  geom_line(aes(y=cit_na), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
  ggtitle("Citations",)

cit_plot_l <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=log(cit_bio)), color="brown",size=1) +
  geom_line(aes(y=log(cit_beh)), color="pink",size=1) +
  geom_line(aes(y=log(cit_ti)), color="darkgreen",size=1) +
  geom_line(aes(y=log(cit_na)), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

ref_plot <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=ref_bio), color="brown",size=1) +
  geom_line(aes(y=ref_beh), color="pink",size=1) +
  geom_line(aes(y=ref_ti), color="darkgreen",size=1) +
  geom_line(aes(y=ref_na), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3), limits = c(0,50000)) +
  ggtitle("References")


ref_plot_l <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=log(ref_bio)), color="brown",size=1) +
  geom_line(aes(y=log(ref_beh)), color="pink",size=1) +
  geom_line(aes(y=log(ref_ti)), color="darkgreen",size=1) +
  geom_line(aes(y=log(ref_na)), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

cit_plot2 <- ggplot(nodes_e, aes(x=Year_target)) + 
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

ref_plot2 <- ggplot(nodes_e, aes(x=Year_target)) + 
  geom_line(aes(y=ref_bio/ref), color="brown",size=1) +
  geom_line(aes(y=ref_beh/ref), color="pink",size=1) +
  geom_line(aes(y=ref_ti/ref), color="darkgreen",size=1) +
  geom_line(aes(y=ref_na/ref), color="blue",size=1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) +
  ggtitle("")

cit_plots <- cit_plot + annotation_custom(ggplotGrob(cit_plot_l), xmin = 1950, xmax = 2000, 
                                        ymin = 50000, ymax = 200000)

ref_plots <- ref_plot + annotation_custom(ggplotGrob(ref_plot_l), xmin = 1950, xmax = 2000, 
                                          ymin = 10000, ymax = 52000)
  
ggarrange(cit_plots, ref_plots, cit_plot2, ref_plot2, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,
          common.legend = TRUE, legend="bottom")



#-----------
# TIME SERIES
# without normalization

ggarrange(cit_plot2, ref_plot2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# conver to fractions:
nodes_e$cit_bio <- nodes_e$cit_bio/nodes_e$cit
nodes_e$ref_bio <- nodes_e$ref_bio/nodes_e$ref

library("TTR")
# convert to time series
cit_timeseries <- ts(nodes_e[c(1:71),c(2)],start=c(1950,1))
ref_timeseries <- ts(nodes_e[c(1:71),c(6)],start=c(1950,1))
plot.ts(cit_timeseries)
plot.ts(ref_timeseries)

# we can try to estimate the trend component of this time series by smoothing using a simple moving average (order 3)
SMA3 <- SMA(cit_timeseries, n = 3)
plot.ts(SMA3)

# still a lot of random fluctuations, let's try sma8
SMA8 <- SMA(cit_timeseries, n = 8)
plot.ts(SMA8)

# DECOMPOSITION
# Not working the time series has no or less than 2 periods
plot.ts(decompose(cit_timeseries))
plot.ts(decompose(ref_timeseries))

# EXPONENTIAL SMOOTHING
#  fit a simple exponential smoothing predictive model
cit_forecast <- HoltWinters(cit_timeseries, beta=FALSE, gamma=FALSE)
cit_forecast3 <- HoltWinters(cit_timeseries, gamma=FALSE)
plot(cit_forecast)
plot(cit_forecast3)

library("forecast")
library("stats")
cit_forecast2 <- forecast:::forecast.HoltWinters(cit_forecast, h =8)
forecast:::plot.forecast(cit_forecast2)


# ARIMA
forecast:::plot.forecast( forecast:::forecast.Arima(arima(cit_timeseries, order=c(0,1,1)), h = 8))
