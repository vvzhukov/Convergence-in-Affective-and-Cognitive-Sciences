# Pure L1 categories
cognitive <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI.csv")
cog_subj <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_subjectareas_major_UI.csv")
cog_subj <- merge(cognitive[,c(3,4)], cog_subj, by = "pmid")

affective <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

balanced_ids <-  read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_balanced_pmid.csv")
cognitive <- subset(cognitive,pmid %in% balanced_ids$x)
cog_subj <- subset(cog_subj,pmid %in% balanced_ids$x)
cog_subj <- cog_subj[!duplicated(cog_subj$pmid), ]


library(tidyverse)
library(scales) 
library(ggpubr)
library(extrafont)
library(scales)
font_import()
loadfonts(device = "win")

# Check

output_ext <- list()

heatmap_func <- function(cog_subj, in_labels, in_size = 4, leg_title = "", side_lab = FALSE, leg_pos = "bottom", legbarheight = 0.7, legbarwidth=15, leglimits = c(0,25), leglimits2 = c(0,25)
                         , both = FALSE, alttitle = "", percent = TRUE){
 
  SA_v <- c("SA1", "SA2", "SA3", "SA4", "SA5")

  cog_subj$SA1 <- as.numeric(cog_subj$F) #SA2
  cog_subj$SA2 <- as.numeric(cog_subj$A + cog_subj$B + cog_subj$G) #SA1
  cog_subj$SA3 <- as.numeric(cog_subj$H + cog_subj$I + cog_subj$K + cog_subj$M) #SA5
  cog_subj$SA4 <- as.numeric(cog_subj$C + cog_subj$N) #SA3
  cog_subj$SA5 <- as.numeric(cog_subj$D + cog_subj$E + cog_subj$J + cog_subj$L) #SA4
  
  
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
    VV = VV/sum(VV)
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

fig0 <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:2020),2],], "Affective", 5, leg_title = "%", leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,30))
fig1 <- heatmap_func(subset(cog_subj, year %in% c(1967:2020)), "Cognitive", 5, leg_title = "%", leg_pos = "right", leglimits = c(0,15), leglimits2 = c(0,30))
png('C:/Users/Rubinzone/Desktop/F5p.png', width = 11, height = 6, units = 'in', res = 300)
annotate_figure(ggarrange(fig0, fig1+ rremove("y.text") + rremove("y.ticks"),
          ncol = 2, nrow = 1,
          heights = c(1.2,1.2),
          widths = c(1.1,1),
          common.legend = TRUE,
          legend = "bottom"),
          bottom = text_grob(
            "SA1 = Psychological sciences    SA2 = Biological sciences    SA3 = Humanities    S4 = Medical sciences    SA5 = Technical methods",
            color = "black", size = 12))

dev.off()

# PURE NUMBERS
fig0c <- heatmap_func(aff_subj, "Affective", 5, leg_title = "%", leg_pos = "none", 
                     leglimits = c(0,nrow(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:2020),2],])), percent = FALSE)
fig1c <- heatmap_func(cog_subj, "Cognitive", 5, leg_title = "%", leg_pos = "right", 
                     leglimits = c(0,nrow(subset(cog_subj, year %in% c(1967:2020)))), percent = FALSE)
png('C:/Users/Rubinzone/Desktop/F5n.png', width = 12, height = 6, units = 'in', res = 300)
annotate_figure(ggarrange(fig0c, fig1c + rremove("y.text") + rremove("y.ticks"),
                          ncol = 2, nrow = 1,
                          heights = c(1.2,1.2),
                          widths = c(1,1.1),
                          common.legend = TRUE,
                          legend = "bottom"),
                bottom = text_grob(
                  "SA1 = Psychological sciences    SA2 = Biological sciences    SA3 = Humanities    S4 = Medical sciences    SA5 = Technical methods",
                  color = "black", size = 12))
dev.off()

# NO CELL VALUES
fig0b <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:2020),2],], "Affective", 0, leg_title = "%", leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,30))
fig1b <- heatmap_func(subset(cog_subj, year %in% c(1967:2020)), "Cognitive", 0, leg_title = "%", leg_pos = "right", leglimits = c(0,15), leglimits2 = c(0,30))

png('C:/Users/Rubinzone/Desktop/F5.png', width = 11, height = 6, units = 'in', res = 300)
annotate_figure(ggarrange(fig0b, fig1b+ rremove("y.text") + rremove("y.ticks"),
                          ncol = 2, nrow = 1,
                          heights = c(1.2,1.2),
                          widths = c(1.1,1),
                          common.legend = TRUE,
                          legend = "bottom"),
                bottom = text_grob(
                  "SA1 = Psychological sciences    SA2 = Biological sciences    SA3 = Humanities    S4 = Medical sciences    SA5 = Technical methods",
                  color = "black", size = 12))
dev.off()


# PERCENTS
fig1aa <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:1968),2],], "1967 - 1990", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,20), leglimits2 = c(0,50), both = TRUE, alttitle = "Affective")
fig2aa <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1991:2000),2],], "1991 - 2000", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,20), leglimits2 = c(0,50),)
fig3aa <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2001:2010),2],], "2001 - 2010", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,20), leglimits2 = c(0,50),)
fig4aa <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2011:2019),2],], "2011 - 2019", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,20), leglimits2 = c(0,50),)

fig1ca <- heatmap_func(subset(cog_subj, year %in% c(1967:1990)), "1967 - 1990", 5, leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,20), leglimits2 = c(0,50), both = TRUE, alttitle = "Cognitive")
fig2ca <- heatmap_func(subset(cog_subj, year %in% c(1991:2000)), "1991 - 2000", 5, leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,20), leglimits2 = c(0,50),)
fig3ca <- heatmap_func(subset(cog_subj, year %in% c(2001:2010)), "2001 - 2010", 5, leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,20), leglimits2 = c(0,50),)
fig4ca <- heatmap_func(subset(cog_subj, year %in% c(2011:2019)), "2011 - 2019", 5, leg_title = "%", side_lab = TRUE, leg_pos = "bottom", leglimits = c(0,20), leglimits2 = c(0,50),)

png('C:/Users/Rubinzone/Desktop/F6p.png', width = 8, height = 14, units = 'in', res = 300)

annotate_figure(
ggarrange(fig1aa + rremove("x.text"), 
          fig1ca + rremove("y.text") + rremove("x.text") + rremove("y.title"),
          fig2aa + rremove("x.text"), 
          fig2ca + rremove("y.text") + rremove("x.text") + rremove("y.title"),
          fig3aa + rremove("x.text"), 
          fig3ca + rremove("y.text") + rremove("x.text") + rremove("y.title"),
          fig4aa, 
          fig4ca + rremove("y.text") + rremove("y.title"),
          ncol = 2, nrow = 4,
          heights = c(1.2,1,1,1.1,1,1,1,1),
          widths = c(1.2,1,1,1,1,1,1,1),
          common.legend = TRUE, legend="bottom"),
bottom = text_grob(
  "SA1 = Psychological sciences    SA2 = Biological sciences    SA3 = Humanities   \n S4 = Medical sciences    SA5 = Technical methods",
  color = "black", size = 12))

dev.off()

# PURE NUMBERS
fig1ab <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:1968),2],], "1967 - 1990", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", 
                      leglimits = c(0,nrow(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:1968),2],])), both = TRUE, alttitle = "Affective", percent = FALSE)
fig2ab <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1991:2000),2],], "1991 - 2000", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", 
                      leglimits = c(0,nrow(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1991:2000),2],])), percent = FALSE)
fig3ab <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2001:2010),2],], "2001 - 2010", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", 
                      leglimits = c(0,nrow(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2001:2010),2],])), percent = FALSE)
fig4ab <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2011:2019),2],], "2011 - 2019", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", 
                      leglimits = c(0,nrow(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2011:2019),2],])), percent = FALSE)

fig1cb <- heatmap_func(subset(cog_subj, year %in% c(1967:1990)), "1967 - 1990", 5 ,leg_title = "", side_lab = TRUE, leg_pos = "none", 
                       leglimits = c(0,nrow(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:1968),2],])), both = TRUE, alttitle = "Cognitive", percent = FALSE)
fig2cb <- heatmap_func(subset(cog_subj, year %in% c(1991:2000)), "1991 - 2000", 5, leg_title = "", side_lab = TRUE, leg_pos = "none", 
                       leglimits = c(0,nrow(subset(cog_subj, year %in% c(1991:2000)))))
fig3cb <- heatmap_func(subset(cog_subj, year %in% c(2001:2010)), "2001 - 2010", 5, leg_title = "", side_lab = TRUE, leg_pos = "none", 
                       leglimits = c(0,nrow(subset(cog_subj, year %in% c(2001:2010)))))
fig4cb <- heatmap_func(subset(cog_subj, year %in% c(2011:2019)), "2011 - 2019", 5, leg_title = "%", side_lab = TRUE, leg_pos = "bottom", 
                       leglimits = c(0,nrow(subset(cog_subj, year %in% c(2011:2019)))))

png('C:/Users/Rubinzone/Desktop/F6n.png', width = 8, height = 14, units = 'in', res = 300)

annotate_figure(
  ggarrange(fig1ab + rremove("x.text"), 
            fig1cb + rremove("y.text") + rremove("x.text") + rremove("y.title"),
            fig2ab + rremove("x.text"), 
            fig2cb + rremove("y.text") + rremove("x.text") + rremove("y.title"),
            fig3ab + rremove("x.text"), 
            fig3cb + rremove("y.text") + rremove("x.text") + rremove("y.title"),
            fig4ab, 
            fig4cb + rremove("y.text") + rremove("y.title"),
            ncol = 2, nrow = 4,
            heights = c(1.2,1,1,1.1,1,1,1,1),
            widths = c(1.2,1,1,1,1,1,1,1),
            common.legend = TRUE, legend="bottom"),
  bottom = text_grob(
    "SA1 = Psychological sciences    SA2 = Biological sciences    SA3 = Humanities    S4 = Medical sciences    SA5 = Technical methods",
    color = "black", size = 12))

dev.off()


# NO CELL VALUES
fig1a <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:1990),2],], "1967 - 1990", 0 ,leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,50), both = TRUE, alttitle = "Affective")
fig2a <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1991:2000),2],], "1991 - 2000", 0 ,leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,50),)
fig3a <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2001:2010),2],], "2001 - 2010", 0 ,leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,50),)
fig4a <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2011:2019),2],], "2011 - 2019", 0 ,leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,50),)

fig1c <- heatmap_func(subset(cog_subj, year %in% c(1967:1990)), "1967 - 1990", 0, leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,50), both = TRUE, alttitle = "Cognitive")
fig2c <- heatmap_func(subset(cog_subj, year %in% c(1991:2000)), "1991 - 2000", 0, leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,50),)
fig3c <- heatmap_func(subset(cog_subj, year %in% c(2001:2010)), "2001 - 2010", 0, leg_title = "", side_lab = TRUE, leg_pos = "none", leglimits = c(0,15), leglimits2 = c(0,50),)
fig4c <- heatmap_func(subset(cog_subj, year %in% c(2011:2019)), "2011 - 2019", 0, leg_title = "%", side_lab = TRUE, leg_pos = "bottom", leglimits = c(0,15), leglimits2 = c(0,50),)

png('C:/Users/Rubinzone/Desktop/F6p_hires_b.png', width = 8, height = 14, units = 'in', res = 300)

annotate_figure(
  ggarrange(fig1a + rremove("x.text"), 
            fig1c + rremove("y.text") + rremove("x.text") + rremove("y.title"),
            fig2a + rremove("x.text"), 
            fig2c + rremove("y.text") + rremove("x.text") + rremove("y.title"),
            fig3a + rremove("x.text"), 
            fig3c + rremove("y.text") + rremove("x.text") + rremove("y.title"),
            fig4a, 
            fig4c + rremove("y.text") + rremove("y.title"),
            ncol = 2, nrow = 4,
            heights = c(1.2,1,1,1.1,1,1,1,1),
            widths = c(1.2,1,1,1,1,1,1,1),
            common.legend = TRUE, legend="bottom"),
  bottom = text_grob(
    "SA1 = Psychological sciences    SA2 = Biological sciences    SA3 = Humanities    S4 = Medical sciences    SA5 = Technical methods",
    color = "black", size = 12))

dev.off()
