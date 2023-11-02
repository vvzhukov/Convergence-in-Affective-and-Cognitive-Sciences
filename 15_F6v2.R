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

heatmap_func <- function(cog_subj, in_labels, in_size = 4, leg_title = "", side_lab = FALSE, 
                         leg_pos = "bottom", 
                         leg_dir = "horizontal",
                         legbarheight = 0.7, legbarwidth=15, 
                         leglimits = c(0,25), leglimits2 = c(0,25), 
                         both = FALSE, 
                         alttitle = "", 
                         percent = TRUE, 
                         mesh = TRUE,
                         direct_input = FALSE, 
                         leg1_low = "#FFFFFF", 
                         leg1_mid = "#FFFF00", 
                         leg1_high = "#FF5F00",
                         leg1_na.value = "#FFFFFF",
                         leg2_low = "#FFFFFF",
                         leg2_mid = "#808080",
                         leg2_high = "#404040",
                         leg2_na.value = "#FFFFFF"){
  
  SA_v <- c("SA1", "SA2", "SA3", "SA4", "SA5")
  
  if (!direct_input){
    
    if (mesh){
      cog_subj$SA2 <- as.numeric(cog_subj$F) #SA2
      cog_subj$SA1 <- as.numeric(cog_subj$A + cog_subj$B + cog_subj$G) #SA1
      cog_subj$SA5 <- as.numeric(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M) #SA5
      cog_subj$SA3 <- as.numeric(cog_subj$C + cog_subj$N) #SA3
      cog_subj$SA4 <- as.numeric(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L) #SA4
    } else {
      # For the mean citation
      cog_subj$SA2 <- cog_subj$cSA2
      cog_subj$SA1 <- cog_subj$cSA1
      cog_subj$SA5 <- cog_subj$cSA5
      cog_subj$SA3 <- cog_subj$cSA3
      cog_subj$SA4 <- cog_subj$cSA4
    }  
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
    
    
    if (percent){
      output2 <- round(100 * output2 / sum(colSums(output2)),1)  
    } else {
      output2 <- round(output2,1)  
    }
    output2[lower.tri(output2, diag = F)] <- NA
    
    output_ext[[length(output_ext)+1]] <<- output2
    
  } else {
    output2 <- round(cog_subj,1)
  }
  
  
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
                           low = leg1_low, 
                           mid = leg1_mid, 
                           high = leg1_high,
                           na.value = leg1_na.value,
                           labels = comma,
                           position = leg_pos,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      new_scale("fill") +
      geom_tile(aes(fill = hits), data = dt3[dt3$rowname == dt3$colname,]) +
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits2,
                           low = leg2_low,
                           mid = leg2_mid,
                           high = leg2_high,
                           na.value = leg2_na.value,
                           labels = comma,
                           position = leg_pos,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      geom_text(aes(label = hits), size=in_size) +
      
      theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
            axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, size = 25,family="NimbusMon"),
            legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
            legend.text = element_text(size=rel(1.2),family="NimbusMon"),
            legend.position = leg_pos,
            legend.direction = leg_dir)
  } else {
    SA_heatmap <- ggplot(dt3, aes(x = rowname, y = colname, fill = hits)) +
      geom_tile() + 
      labs(y=in_labels) + 
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits,
                           low = leg1_low, 
                           mid = leg1_mid, 
                           high = leg1_high,
                           na.value = leg1_na.value,
                           labels = comma,
                           position = leg_pos,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      new_scale("fill") +
      geom_tile(aes(fill = hits), data = dt3[dt3$rowname == dt3$colname,]) +
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits2,
                           low = leg2_low,
                           mid = leg2_mid,
                           high = leg2_high,
                           na.value = leg2_na.value,
                           labels = comma,
                           position = leg_pos,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      geom_text(aes(label = hits), size=in_size) +
      theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
            axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
            plot.title = element_blank(),
            legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
            legend.text = element_text(size=rel(1.2),family="NimbusMon"),
            legend.position = leg_pos,
            legend.direction = leg_dir)
    
  }
  
  if (both){ 
    SA_heatmap <- ggplot(dt3, aes(x = rowname, y = colname, fill = hits)) +
      geom_tile() + 
      ggtitle(alttitle) + 
      labs(y=in_labels) + 
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits,
                           low = leg1_low, 
                           mid = leg1_mid, 
                           high = leg1_high,
                           na.value = leg1_na.value,
                           labels = comma,
                           position = leg_pos,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      new_scale("fill") +
      geom_tile(aes(fill = hits), data = dt3[dt3$rowname == dt3$colname,]) +
      scale_fill_gradient2(name = leg_title,
                           limits = leglimits2,
                           low = leg2_low,
                           mid = leg2_mid,
                           high = leg2_high,
                           na.value = leg2_na.value,
                           labels = comma,
                           position = leg_pos,
                           guide=guide_colorbar(barwidth=legbarwidth, barheight = legbarheight)) +
      
      geom_text(aes(label = hits), size=in_size) +
      theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
            axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=rel(1.5),family="NimbusMon"),
            plot.title = element_text(hjust = 0.5, size = 25,family="NimbusMon"),
            legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
            legend.text = element_text(size=rel(1.2),family="NimbusMon"),
            legend.position = leg_pos,
            legend.direction = leg_dir)
  }
  return(SA_heatmap)
}

# PERCENTS

fig0 <- heatmap_func(aff_cit_sa, "Affective Citations", 5, 
                     leg_title = "%", leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,30))

fig1 <- heatmap_func(cog_cit_sa, "Cognitive Citations", 5, 
                     leg_title = "%", leg_pos = "right", leglimits = c(0,15), leglimits2 = c(0,30))

#png('C:/Users/Rubinzone/Desktop/F6p.png', width = 11, height = 6, units = 'in', res = 300)
#annotate_figure(ggarrange(fig0, fig1+ rremove("y.text") + rremove("y.ticks"),
#                          ncol = 2, nrow = 1,
#                          heights = c(1.2,1.2),
#                          widths = c(1.1,1),
#                          common.legend = TRUE,
#                          legend = "bottom"),
#                bottom = text_grob(
#                  "SA1 = Biological sciences    SA2 = Psychological sciences    S3 = Medical sciences    SA4 = Technical methods    SA5 = Humanities",
#                  color = "black", size = 12))
#dev.off()






fig0m <- heatmap_func(subset(model_data, pType == "Affective")[, c("pmid", "pYear", "cSA1", "cSA2", "cSA3", "cSA4", "cSA5")], 
                     "Affective C̄", 5, 
                     leg_title = "%", leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,30),
                     mesh = FALSE)

fig1m <- heatmap_func(subset(model_data, pType == "Cognitive")[, c("pmid", "pYear", "cSA1", "cSA2", "cSA3", "cSA4", "cSA5")], 
                     "Cognitive C̄", 5, 
                     leg_title = "%", leg_pos = "right", leglimits = c(0,15), leglimits2 = c(0,30),
                     mesh = FALSE)



#png('C:/Users/Rubinzone/Desktop/F6p_mean_cit.png', width = 11, height = 6, units = 'in', res = 300)
#annotate_figure(ggarrange(fig0m, fig1m+ rremove("y.text") + rremove("y.ticks"),
#                          ncol = 2, nrow = 1,
#                          heights = c(1.2,1.2),
#                          widths = c(1.1,1),
#                          common.legend = TRUE,
#                          legend = "bottom"),
#                bottom = text_grob(
#                  "SA1 = Biological sciences    SA2 = Psychological sciences    S3 = Medical sciences    SA4 = Technical methods    SA5 = Humanities",
#                  color = "black", size = 12))
#dev.off()



# DIFFERENCES

fig2_diff <- heatmap_func(data.frame(output_ext[1]) - data.frame(output_ext[2]), "Affective C - Cognitive C", 5, 
                          leg_title = "Δ%", leg_pos = "none", leglimits = c(-5,3.5), leglimits2 = c(-5,3.5), direct_input = TRUE, 
                          leg1_low = "#0000FF", 
                          leg1_mid = "#FFFFFF", 
                          leg1_high = "#ff0000",
                          leg1_na.value = "#FFFFFF",
                          leg2_low = "#0000FF", 
                          leg2_mid = "#FFFFFF", 
                          leg2_high = "#ff0000",
                          leg2_na.value = "#FFFFFF")



fig2m_diff <- heatmap_func(data.frame(output_ext[3]) - data.frame(output_ext[4]), bquote("Δ" ~ bold(D)[italic(N)]^italic(u) ~ "× 100%"), 5,# ~ "(Affective) -" ~ bold(D)[italic(N)]^italic(u) ~ "(Cognitive)"), 5, #"Affective C̄ - Cognitive C̄"
                          leg_title = "Δ%", leg_pos = "bottom", leg_dir="horizontal", leglimits = c(-5,3.5), leglimits2 = c(-5,3.5), direct_input = TRUE, 
                          leg1_low = "#0000FF", 
                          leg1_mid = "#FFFFFF", 
                          leg1_high = "#ff0000",
                          leg1_na.value = "#FFFFFF",
                          leg2_low = "#0000FF", 
                          leg2_mid = "#FFFFFF", 
                          leg2_high = "#ff0000",
                          leg2_na.value = "#FFFFFF")


#png('C:/Users/Rubinzone/Desktop/F6p_diff.png', width = 11, height = 6, units = 'in', res = 300)
#annotate_figure(ggarrange(fig2_diff, fig2m_diff+ rremove("y.text") + rremove("y.ticks"),
#                          ncol = 2, nrow = 1,
#                          heights = c(1.2,1.2),
#                          widths = c(1.1,1),
#                          common.legend = TRUE,
#                          legend = "bottom"),
#                bottom = text_grob(
#                  "SA1 = Biological sciences    SA2 = Psychological sciences    S3 = Medical sciences    SA4 = Technical methods    SA5 = Humanities",
#                  color = "black", size = 12))
#dev.off()



#png('C:/Users/Rubinzone/Desktop/F6p_diff_mean_only.png', width = 7, height = 8, units = 'in', res = 300)
#annotate_figure(ggarrange(fig2m_diff,
#                          ncol = 1, nrow = 1,
#                          common.legend = TRUE,
#                          legend = "bottom"),
#                bottom = text_grob(
#                  "SA1 = Biological Sciences    SA2 = Psychological Sciences  \n  S3 = Medical Sciences    SA4 = Technical Methods  \n  SA5 = Humanities",
#                  color = "black", size = 12))
#dev.off()