# Pure L1 categories

cognitive <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")
cog_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")
cog_subj <- merge(cognitive[,c(3,4)], cog_subj, by = "pmid")

data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
cog_subj_nb <- subset(cog_subj, pmid %in% data_cognitive_major_nb$pmid)

#affective <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

library(tidyverse)
library(scales) 
library(ggpubr)
library(extrafont)
library(scales)
font_import()
loadfonts(device = "win")

# Check

heatmap_func <- function(cog_subj, in_labels, in_size = 4, high_col, leg_title = "", side_lab = FALSE, leg_pos = "bottom", legbarheight = 25){
  output <- data.frame(matrix(ncol = 16, nrow = 16))
  
  L1_v <- c("A",    "B",    "C",    "D",    "E",   "F",    "G",    "H",    "I",    "J",    "K",    "L",    "M",    "N",    "V",    "Z")
  SA_v <- c("SA1", "SA2", "SA3", "SA4", "SA5")
  colnames(output) <- L1_v
  rownames(output) <- L1_v
  
  for (group in colnames(output)) {
    for (subgroup in rownames(output)) {
      if (group != subgroup){
        output[[group,subgroup]] <- (sum(cog_subj[cog_subj[[group]] > 0,][[subgroup]]))
      } else {
        output[[group,subgroup]] <- (sum(cog_subj[cog_subj[[group]] > 0 & rowSums(cog_subj[,L1_v]) - cog_subj[[group]] == 0,][[subgroup]]))
        
      }
    }
  }
  output[lower.tri(output, diag = F)] <- 0
  
  # Now same for SAs
  #Old scheme
  #cog_subj$SA1 <- as.numeric(cog_subj$A + cog_subj$B + cog_subj$G)
  #cog_subj$SA2 <- as.numeric(cog_subj$F)
  #cog_subj$SA3 <- as.numeric(cog_subj$C + cog_subj$N)
  #cog_subj$SA4 <- as.numeric(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L)
  #cog_subj$SA5 <- as.numeric(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M)
  cog_subj$SA1 <- as.numeric(cog_subj$F) #SA2
  cog_subj$SA2 <- as.numeric(cog_subj$A + cog_subj$B + cog_subj$G) #SA1
  cog_subj$SA3 <- as.numeric(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M) #SA5
  cog_subj$SA4 <- as.numeric(cog_subj$C + cog_subj$N) #SA3
  cog_subj$SA5 <- as.numeric(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L) #SA4
  
  
  output2 <- data.frame(matrix(ncol = 5, nrow = 5))
  colnames(output2) <- SA_v
  rownames(output2) <- SA_v
  
  for (group in colnames(output2)) {
    for (subgroup in rownames(output2)) {
      if (group != subgroup){
        output2[[group,subgroup]] <- round((sum(cog_subj[cog_subj[[group]] > 0,][[subgroup]])),1)
      } else {
        output2[[group,subgroup]] <- round((sum(cog_subj[cog_subj[[group]] > 0 & rowSums(cog_subj[,SA_v]) - cog_subj[[group]] == 0,][[subgroup]])),1)
      }
    }
  }
  output2[lower.tri(output2, diag = F)] <- NA
  
  # HEATMAP
  
  dt3 <- output2 %>%
    rownames_to_column() %>%
    gather(colname, hits, -rowname)
  head(dt3)
  
  if (!side_lab){ 
    SA_heatmap <- ggplot(dt3, aes(x = rowname, y = colname, fill = hits)) +
      geom_tile() + 
      ggtitle(in_labels) + 
      geom_text(aes(label = hits), size=in_size) + 
      scale_fill_gradient(name = leg_title,
                          low = "#FFFFFF",
                          high = high_col,
                          na.value = "#FFFFFF",
                          labels = comma,
                          guide=guide_colorbar(barwidth=0.7, barheight = legbarheight)) +
      theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
            axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, size = 25,family="NimbusMon"),
            legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
            legend.text = element_text(size=rel(1.2),family="NimbusMon"),
            legend.position = leg_pos)
  } else {
    SA_heatmap <- ggplot(dt3, aes(x = rowname, y = colname, fill = hits)) +
      geom_tile() + 
      labs(y=in_labels) + 
      geom_text(aes(label = hits), size=in_size) + 
      scale_fill_gradient(name = leg_title,
                          low = "#FFFFFF",
                          high = high_col,
                          na.value = "#FFFFFF",
                          labels = comma,
                          guide=guide_colorbar(barwidth=0.7, barheight = legbarheight)) +
      theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
            axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
            plot.title = element_blank(),
            legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
            legend.text = element_text(size=rel(1.2),family="NimbusMon"),
            legend.position = leg_pos)
    
  }
  return(SA_heatmap)
}



cog_subj[c(146),c(3,6)] <- c(2,3)
cog_subj[,c(18:23)]


heatmap_func(cog_subj[c(146),], "Test case 1 SA(0,1,0,0,0)", 5, high_col = "#FF7852", leg_title = "", leg_pos = "left")

heatmap_func(cog_subj[c(1),], "Test case 2 SA(0,1,0,0,1)", 5, high_col = "#FF7852", leg_title = "", leg_pos = "left")

heatmap_func(cog_subj[c(68),], "Test case 3 SA(0,3,0,1,1)", 5, high_col = "#FF7852", leg_title = "", leg_pos = "left")


heatmap_func(cog_subj[c(1:3),], "Test case 4", 5, high_col = "#FF7852", leg_title = "", leg_pos = "left")

heatmap_func(cog_subj[c(86:91),], "Test case 5", 5, high_col = "#FF7852", leg_title = "", leg_pos = "left")

heatmap_func(cog_subj[c(1:10),], "Test case 6", 5, high_col = "#FF7852", leg_title = "", leg_pos = "left")
