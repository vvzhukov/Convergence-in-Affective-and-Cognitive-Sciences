# Pure L1 categories
cognitive <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")
cog_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")
cog_subj <- merge(cognitive[,c(3,4)], cog_subj, by = "pmid")

library(tidyverse)
library(scales) 
library(ggpubr)


heatmap_func <- function(cog_subj, title_prefix, in_labels, in_size = 4, rem_x_text = FALSE){
  output <- data.frame(matrix(ncol = 14, nrow = 14))
  colnames(output) <- colnames(cog_subj[2:15])
  rownames(output) <- colnames(cog_subj[2:15])
  
  for (group in colnames(output)) {
    for (subgroup in rownames(output)) {
      if (group != subgroup){
        output[[group,subgroup]] <- sum(cog_subj[cog_subj[[group]] > 0,][[subgroup]])
      } else {
        output[[group,subgroup]] <- sum(cog_subj[cog_subj[[group]] > 0 & rowSums(cog_subj[,2:17]) - cog_subj[[group]] == 0,][[subgroup]])
        
      }
    }
  }
  output[lower.tri(output, diag = F)] <- 0
  
  # Now same for SAs
  cog_subj$SA1 <- as.numeric(cog_subj$A + cog_subj$B + cog_subj$G)
  cog_subj$SA2 <- as.numeric(cog_subj$F)
  cog_subj$SA3 <- as.numeric(cog_subj$C + cog_subj$N)
  cog_subj$SA4 <- as.numeric(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L)
  cog_subj$SA5 <- as.numeric(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M)
  
  output2 <- data.frame(matrix(ncol = 5, nrow = 5))
  colnames(output2) <- c("SA1", "SA2", "SA3", "SA4", "SA5")
  rownames(output2) <- c("SA1", "SA2", "SA3", "SA4", "SA5")
  
  for (group in colnames(output2)) {
    for (subgroup in rownames(output2)) {
      if (group != subgroup){
        output2[[group,subgroup]] <- sum(cog_subj[cog_subj[[group]] > 0,][[subgroup]])
      } else {
        output2[[group,subgroup]] <- sum(cog_subj[cog_subj[[group]] > 0 & rowSums(cog_subj[,2:17]) - cog_subj[[group]] == 0,][[subgroup]])
      }
    }
  }
  output2[lower.tri(output2, diag = F)] <- NA
  
  # HEATMAP
  
  dt3 <- output2 %>%
    rownames_to_column() %>%
    gather(colname, hits, -rowname)
  head(dt3)
  
  SA_heatmap <- ggplot(dt3, aes(x = rowname, y = colname, fill = hits)) +
    geom_tile() + 
    ggtitle(in_labels) + 
    geom_text(aes(label = hits), size=in_size) + 
    scale_fill_gradient(name = "",
                        low = "#FFFFFF",
                        high = "#52f9ff",
                        na.value = "#FFFFFF",
                        labels = comma) +
    theme(axis.text.x=element_text(size=rel(1.5)),
          axis.text.y=element_text(size=rel(1.5)),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5),
          legend.text = element_text(size=rel(1.5)))
  
  # should be 1400 x 550
  if (rem_x_text) {
    heatmap <- ggarrange(SA_heatmap + rremove("x.text"), 
                         labels = title_prefix,
                         ncol = 1, nrow = 1)
  } else {
    heatmap <- ggarrange(SA_heatmap, 
                         labels = title_prefix,
                         ncol = 1, nrow = 1)
  }  
  
  
  
  heatmap_labelled <- annotate_figure(heatmap, top = text_grob(
    title_prefix,
    color = "black", face = "bold", size = 14))
  #return(heatmap_labelled)
  return(heatmap)
}



fig0 <- heatmap_func(subset(cog_subj, year %in% c(1967:2020)), "", "1967 - 2019", 6, rem_x_text = FALSE)

annotate_figure(fig0, top = text_grob(
  "SA1 - Biological sciences, SA2 - Psychological sciences, SA3 - Medical sciences, \n SA4 - Technical methods, SA5 - Humanities",
  color = "black", size = 12))


fig1 <- heatmap_func(subset(cog_subj, year %in% c(1967:1990)), "", "1967 - 1990", 6, rem_x_text = TRUE)
fig2 <- heatmap_func(subset(cog_subj, year %in% c(1991:2000)), "", "1991 - 2000", 6, rem_x_text = TRUE)
fig3 <- heatmap_func(subset(cog_subj, year %in% c(2001:2010)), "", "2001 - 2010", 6, rem_x_text = TRUE)
fig4 <- heatmap_func(subset(cog_subj, year %in% c(2011:2019)), "", "2011 - 2019", 6, rem_x_text = FALSE)

fig5 <- ggarrange(fig1, fig2, fig3, fig4, 
                  ncol = 1, nrow = 4)

annotate_figure(fig5, top = text_grob(
  "SA1 - Biological sciences, SA2 - Psychological sciences, SA3 - Medical sciences, \n SA4 - Technical methods, SA5 - Humanities",
  color = "black", size = 12))
