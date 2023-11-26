

model_data <- read.csv("E:/Research/Data_pubmed/model_data23/model_data.csv")


library(ggplot2)
library(ggpubr)

ggplot(data = subset(model_data,pType == "Affective"), mapping = aes(x = pDiversity, y = cDiversity)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

ggplot(data = subset(model_data,pType == "Cognitive"), mapping = aes(x = pDiversity, y = cDiversity)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")


fm2_1 <- ggplot() +
  stat_ecdf(aes(model_data$pCitations_n), geom = "step", linewidth = 1.5, color = "black") +
  labs(y= 'CDF', x=expression(paste(italic("C"))))+#[italic("N")]))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_text(size=rel(1.5),angle=90,family="NimbusMon", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"))

fm2_2 <- ggplot() +
  stat_ecdf(aes(model_data$pAuthors_n), geom = "step", linewidth = 1.5, color = "black") +
  labs(y= 'CDF', x=expression(paste(italic("A"))))+#[italic("N")]))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_text(size=rel(1.5),angle=90,family="NimbusMon", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(2.5),family="NimbusMon"))


png('C:/Users/Rubinzone/Desktop/FMd_2.png', width = 12, height = 6, units = 'in', res = 300)


ggarrange(
  fm2_1, fm2_2,
  ncol = 2, nrow = 1)
dev.off()



fm2_1n <- ggplot() +
  stat_ecdf(aes(model_data$pCitations_n_norm), geom = "step", linewidth = 1.5, color = "black") +
  labs(y= 'CDF', title=expression(paste(italic("C")[italic("N")]))) +
  #scale_x_log10(
  #  breaks = scales::trans_breaks("log10", function(x) 10^x),
  #  labels = scales::trans_format("log10", scales::math_format(10^.x))
  #) +
  #scale_y_log10(
  #  breaks = scales::trans_breaks("log10", function(x) 10^x),
  #  labels = scales::trans_format("log10", scales::math_format(10^.x))
  #) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_text(size=rel(1.5),angle=90,family="NimbusMon", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(1.5),family="NimbusMon"),
        plot.margin = unit(c(2,0.5,0.5,0.5), 'lines'))

fm2_2n <- ggplot() +
  stat_ecdf(aes(log(model_data$pAuthors_n)), geom = "step", linewidth = 1.5, color = "black") +
  labs(y= 'CDF', title=expression(paste("ln",italic("A")))) +
  #scale_x_log10(
  #  breaks = scales::trans_breaks("log10", function(x) 10^x),
  #  labels = scales::trans_format("log10", scales::math_format(10^.x))
  #) +
  #scale_y_log10(
  #  breaks = scales::trans_breaks("log10", function(x) 10^x),
  #  labels = scales::trans_format("log10", scales::math_format(10^.x))
  #) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.3)),
        axis.title.y=element_text(size=rel(1.5),angle=90,family="NimbusMon", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(size=rel(2),family="NimbusMon", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = 0.45,size=rel(1.5),family="NimbusMon"),
        plot.margin = unit(c(2,0.5,0.5,0.5), 'lines'))


png('C:/Users/Rubinzone/Desktop/FMd_2n.png', width = 12, height = 6, units = 'in', res = 300)

ggarrange(fm2_1n, fm2_2n,
          ncol = 2, nrow = 1)
  
dev.off()


ggarrange(
  ggplot(data = subset(model_data,pType == "Affective"), aes(x = pDiversity, y = cDiversity), 
         title = "Affective") +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)),
  ggplot(data = subset(model_data,pType == "Cognitive"), aes(x = pDiversity, y = cDiversity), 
         title = "Cognitive") +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)),
  ncol = 2, nrow = 1, common.legend = T
  )




library(reshape2) # For melt function
library(MASS)
library(scales)

geyser1 <- subset(model_data_clean,pType == "Affective")
geyser2 <- subset(model_data_clean,pType == "Cognitive")
# waiting = cDiversity
# duration = pDiversity

# Calculate the common x and y range for geyser1 and geyser2
xrng = range(c(geyser1$pDiversity, geyser2$pDiversity))
yrng = range(c(geyser1$cDiversity, geyser2$cDiversity))

# Calculate the 2d density estimate over the common range
d1 = kde2d(geyser1$pDiversity, geyser1$cDiversity, lims=c(xrng, yrng), n=2000)
d2 = kde2d(geyser2$pDiversity, geyser2$cDiversity, lims=c(xrng, yrng), n=2000)

# Confirm that the grid points for each density estimate are identical
identical(d1$x, d2$x) # TRUE
identical(d1$y, d2$y) # TRUE

# Calculate the difference between the 2d density estimates
diff12 = d1 
diff12$z = d2$z - d1$z

## Melt data into long format
# First, add row and column names (x and y grid values) to the z-value matrix
rownames(diff12$z) = diff12$x
colnames(diff12$z) = diff12$y

# Now melt it to long format
diff12.m = melt(diff12$z, id.var=rownames(diff12))
names(diff12.m) = c("pDiversity","cDiversity","z")

# Plot difference between geyser2 and geyser1 density
ggplot(diff12.m, aes(pDiversity, cDiversity, z=z, fill=z)) +
  geom_tile() +
  stat_contour(aes(colour=..level..), binwidth=0.001) +
  scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0) +
  scale_colour_gradient2(low=muted("red"), mid="white", high=muted("blue"), midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE)







summary(model_data$pCitations_n_norm)
hist(model_data$pCitations_n_norm)

model_data$pCitations_n_lvl <- ifelse(model_data$pCitations_n_norm < -2, 'x < -2σ',
                                      ifelse(model_data$pCitations_n_norm < -1, 'x < -σ',
                                             ifelse(model_data$pCitations_n_norm < 1, 'x < ±σ',
                                                    ifelse(model_data$pCitations_n_norm < 2, 'σ < x < 2σ', 'x > 2σ'))))

table(model_data$pCitations_n_lvl)

model_data_clean <- subset(model_data, !is.na(pDiversity) & !is.na(cDiversity) & ! is.na(pCitations_n_lvl))

ggplot(data = model_data_clean, aes(x = pDiversity, y = cDiversity)) +
  stat_density_2d(aes(fill = pCitations_n_lvl), geom = "polygon")


# change order of levels
model_data$pCitations_n_lvl <- factor(model_data$pCitations_n_lvl,
                levels = c('x < -2σ', 'x < -σ', 'x < ±σ', 'σ < x < 2σ', 'x > 2σ'))


ggplot(data = model_data_clean, aes(x = pDiversity, y = cDiversity)) +
  stat_density_2d_filled() +
  facet_wrap(vars(pCitations_n_lvl))


ggarrange(
  ggplot(data =  subset(model_data_clean,pType == "Affective"), 
         aes(x = pDiversity, y = cDiversity)) +
    stat_density_2d_filled() +
    facet_wrap(vars(pCitations_n_lvl)),
  
  ggplot(data =  subset(model_data_clean,pType == "Cognitive"), 
         aes(x = pDiversity, y = cDiversity)) +
    stat_density_2d_filled() +
    facet_wrap(vars(pCitations_n_lvl)),
  
  ncol = 2, nrow = 1, common.legend = T
)



# scaled by the number of observations in each group NOT USED
ggplot(data = model_data_clean, aes(x = pDiversity, y = cDiversity)) +
  stat_density_2d_filled(contour_var = "count") +
  facet_wrap(vars(pCitations_n_lvl))



hist(model_data$cDiversity)


ggplot(data = model_data, mapping = aes(x = pEpoch, y = cDiversity)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")




model_data$pEpoch <- ordered(model_data$pEpoch, levels = c("early", "middle", "late"))
sizes <- ordered(sizes, levels = c("small", "medium", "large"))

ggplot(model_data, aes(x=cDiversity)) +
  geom_histogram(position="dodge") +
  geom_vline(xintercept=mean(model_data$cDiversity, na.rm = T), lwd=1, linetype=2, color="red") +
  facet_grid(~pEpoch5)+theme_bw()

mean(subset(model_data, pEpoch5 == 9)$cDiversity, na.rm = T)

library(dplyr)
model_data %>% group_by(pEpoch5) %>% 
  summarise(mean_cDiversity=mean(cDiversity, na.rm = T)) %>%
  ggplot()

