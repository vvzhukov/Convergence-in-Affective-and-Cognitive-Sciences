# Pure L1 categories

setwd("E:/Research/Data_pubmed/model_data23")
model_data <- read.csv("model_data4.csv")
cog_major_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI_subjectareas.csv")
aff_major_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_major_UI_subjectareas.csv")

cog_cit_sa <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_cit_full_data_subjectareas.csv")
aff_cit_sa <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_cit_bal_full_data_subjectareas.csv")

cog_cit_sa$year <- 0
aff_cit_sa$year <- 0
cog_major_sa$year <- 0
aff_major_sa$year <- 0

cog_cit_sa <- cog_cit_sa[, c("pmid", "year", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")]
aff_cit_sa <- aff_cit_sa[, c("pmid", "year", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")]
cog_major_sa <- cog_major_sa[, c("pmid", "year", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")]
aff_major_sa <- aff_major_sa[, c("pmid", "year", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "V", "Z")]


library(tidyverse)
library(scales) 
library(ggpubr)
library(ggnewscale)
library(extrafont)
library(scales)
font_import()
loadfonts(device = "win")


output_ext <- list()

heatmap_func <- function(cog_subj, in_labels, in_size = 4, leg_title = "", side_lab = FALSE, leg_pos = "bottom", 
                         legbarheight = 0.7, legbarwidth=15, leglimits = c(0,25), leglimits2 = c(0,25)
                         , both = FALSE, alttitle = "", percent = TRUE){
  
  SA_v <- c("SA1", "SA2", "SA3", "SA4", "SA5")
  
  cog_subj$SA2 <- as.numeric(cog_subj$F) #SA2
  cog_subj$SA1 <- as.numeric(cog_subj$A + cog_subj$B + cog_subj$G) #SA1
  cog_subj$SA5 <- as.numeric(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M) #SA5
  cog_subj$SA3 <- as.numeric(cog_subj$C + cog_subj$N) #SA3
  cog_subj$SA4 <- as.numeric(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L) #SA4
  
  #cog_subj$SA1 <- as.numeric(cog_subj$F) #SA2
  #cog_subj$SA2 <- as.numeric(cog_subj$A + cog_subj$B + cog_subj$G) #SA1
  #cog_subj$SA3 <- as.numeric(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M) #SA5
  #cog_subj$SA4 <- as.numeric(cog_subj$C + cog_subj$N) #SA3
  #cog_subj$SA5 <- as.numeric(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L) #SA4
  
  
  output2 <- data.frame(matrix(ncol = 5, nrow = 5))
  colnames(output2) <- SA_v
  rownames(output2) <- SA_v
  output2[is.na(output2)] <- 0
  
  upper_bound <- nrow(cog_subj)
  for (row in c(1:upper_bound)) {
    # for each publication get SA vector V
    V = cog_subj[row,][,SA_v]
    # calculate outer product VV
    VV = outer(as.numeric(unlist(V)),as.numeric(unlist(V)))
    # take only upper triangle and diagonal
    VV[lower.tri(VV,diag = F)] <- 0
    # normalize (divide by summ)
    if (sum(VV) == 0) {
      VV = 0
    } else {
      VV = VV/sum(VV)
    }
    # add result to the output
    output2 <- output2 + VV
  }
  
  output_ext[[length(output_ext)+1]] <<- output2
  
  
  if (percent){
    output2 <- round(100 * output2 / length(cog_subj$pmid),1)  
  } else {
    output2 <- round(output2,1)  
  }
  output2[lower.tri(output2, diag = F)] <- NA
  
  # HEATMAP
  
  dt3 <- output2 %>%
    rownames_to_column() %>%
    gather(colname, hits, -rowname)
  head(dt3)
  
  
  
  if (!side_lab){ 
    SA_heatmap <- ggplot(dt3, aes(x = rowname, y = colname)) +
      geom_tile(aes(fill = hits)) + 
      ggtitle(in_labels) + 
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits,
                           low = "#FFFFFF", 
                           mid = "#FFFF00", 
                           high = "#FF5F00",
                           na.value = "#FFFFFF",
                           labels = comma,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      new_scale("fill") +
      geom_tile(aes(fill = hits), data = dt3[dt3$rowname == dt3$colname,]) +
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits2,
                           low = "#FFFFFF",
                           high = "404040",
                           na.value = "#FFFFFF",
                           labels = comma,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      geom_text(aes(label = hits), size=in_size) +
      
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
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits,
                           low = "#FFFFFF", 
                           mid = "#FFFF00", 
                           high = "#FF5F00",
                           na.value = "#FFFFFF",
                           labels = comma,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      new_scale("fill") +
      geom_tile(aes(fill = hits), data = dt3[dt3$rowname == dt3$colname,]) +
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits2,
                           low = "#FFFFFF",
                           high = "404040",
                           na.value = "#FFFFFF",
                           labels = comma,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      geom_text(aes(label = hits), size=in_size) +
      theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
            axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
            plot.title = element_blank(),
            legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
            legend.text = element_text(size=rel(1.2),family="NimbusMon"),
            legend.position = leg_pos)
    
  }
  
  if (both){ 
    SA_heatmap <- ggplot(dt3, aes(x = rowname, y = colname, fill = hits)) +
      geom_tile() + 
      ggtitle(alttitle) + 
      labs(y=in_labels) + 
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits,
                           low = "#FFFFFF", 
                           mid = "#FFFF00", 
                           high = "#FF5F00",
                           na.value = "#FFFFFF",
                           labels = comma,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      new_scale("fill") +
      geom_tile(aes(fill = hits), data = dt3[dt3$rowname == dt3$colname,]) +
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits2,
                           low = "#FFFFFF",
                           high = "404040",
                           na.value = "#FFFFFF",
                           labels = comma,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      geom_text(aes(label = hits), size=in_size) +
      theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
            axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
            plot.title = element_text(hjust = 0.5, size = 25,family="NimbusMon"),
            legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
            legend.text = element_text(size=rel(1.2),family="NimbusMon"),
            legend.position = leg_pos)
  }
  return(SA_heatmap)
}

# PERCENTS

fig0 <- heatmap_func(aff_cit_sa, "Affective Citations", 5, 
                     leg_title = "%", leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,30))

fig1 <- heatmap_func(cog_cit_sa, "Cognitive Citations", 5, 
                     leg_title = "%", leg_pos = "right", leglimits = c(0,15), leglimits2 = c(0,30))

png('C:/Users/Rubinzone/Desktop/F6p.png', width = 11, height = 6, units = 'in', res = 300)
annotate_figure(ggarrange(fig0, fig1+ rremove("y.text") + rremove("y.ticks"),
                          ncol = 2, nrow = 1,
                          heights = c(1.2,1.2),
                          widths = c(1.1,1),
                          common.legend = TRUE,
                          legend = "bottom"),
                bottom = text_grob(
                  "SA1 = Biological sciences    SA2 = Psychological sciences    S3 = Medical sciences    SA4 = Technical methods    SA5 = Humanities",
                  color = "black", size = 12))

dev.off()
