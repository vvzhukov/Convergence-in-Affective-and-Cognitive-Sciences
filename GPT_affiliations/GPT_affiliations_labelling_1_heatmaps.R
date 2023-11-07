
affective <- read.csv("E:/Research/Data_pubmed/model_data23/model_data_aff_CIP_v2.csv")
cognitive <- read.csv("E:/Research/Data_pubmed/model_data23/model_data_cog_CIP_v2.csv")

library(tidyverse)
library(tidyr)
library(scales) 
library(ggpubr)
library(ggnewscale)
library(extrafont)
library(scales)
font_import()
loadfonts(device = "win")

output_ext <- list()


heatmap_func <- function(data, in_labels, in_size = 4, leg_title = "", 
                         leg_pos = "bottom", 
                         legbarheight = 0.7, legbarwidth=15, 
                         leglimits = c(0,25), 
                         alttitle = "", percent = T){
  
  data %>% drop_na(pSA1,pSA2,pSA3,pSA4,pSA5,
                   CIP0,CIP1,CIP2,CIP3,CIP4,CIP5,CIP6,CIP7,CIP8,CIP9) -> data
  
  SA_v <- c("pSA1", "pSA2", "pSA3", "pSA4", "pSA5")
  CIP_v <- c("CIP0","CIP1","CIP2","CIP3","CIP4","CIP5","CIP6","CIP7","CIP8","CIP9")

  output2 <- data.frame(matrix(ncol = 5, nrow = 10))
  colnames(output2) <- SA_v
  rownames(output2) <- CIP_v
  output2[is.na(output2)] <- 0
  
  upper_bound <- nrow(data)
  for (row in c(1:upper_bound)) {
    
    # for each publication get SA vector V
    V = data[row,][,SA_v]
    
    # for each publication get CIP vector W
    W = data[row,][,CIP_v]
    
    
    
    # calculate outer product VW
    VW = outer(as.numeric(unlist(V)),as.numeric(unlist(W)))
    
    # normalize (divide by summ)
    if (sum(VW) > 0){
      VW = VW/sum(VW)
    }
    
    # add result to the output
    output2 <- output2 + VW
  }
  
  output_ext[[length(output_ext)+1]] <<- output2

  
  if (percent){
    output2 <- round(100 * output2 / nrow(data),1)  
  } else {
    output2 <- round(output2,1)  
  }
  
  # HEATMAP
  
  dt3 <- output2 %>%
    rownames_to_column() %>%
    gather(colname, hits, -rowname)
  head(dt3)
  
  
  
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
      
      geom_text(aes(label = hits), size=in_size) +
      
      theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
            axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, size = 25,family="NimbusMon"),
            legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
            legend.text = element_text(size=rel(1.2),family="NimbusMon"),
            legend.position = leg_pos)
  
  return(SA_heatmap)
}




output_ext_cip <- list()

heatmap_cip_func <- function(data, in_labels, in_size = 4, leg_title = "", 
                         leg_pos = "bottom", 
                         legbarheight = 0.7, legbarwidth=15, 
                         leglimits = c(0,25), 
                         leglimits2 = c(0,50),
                         alttitle = "", percent = T){
  
  data %>% drop_na(CIP0,CIP1,CIP2,CIP3,CIP4,CIP5,CIP6,CIP7,CIP8,CIP9) -> data

  CIP_v <- c("CIP0","CIP1","CIP2","CIP3","CIP4","CIP5","CIP6","CIP7","CIP8","CIP9")
  
  output2 <- data.frame(matrix(ncol = 10, nrow = 10))
  colnames(output2) <- CIP_v
  rownames(output2) <- CIP_v
  output2[is.na(output2)] <- 0
  
  upper_bound <- nrow(data)
  for (row in c(1:upper_bound)) {
    # for each publication get SA vector V
    V = data[row,][,CIP_v]
    # calculate outer product VV
    VV = outer(as.numeric(unlist(V)),as.numeric(unlist(V)))
    # take only upper triangle and diagonal
    VV[lower.tri(VV,diag = F)] <- 0
    # normalize (divide by summ)
    if (sum(VV) > 0){
      VV = VV/sum(VV)
    }
    # add result to the output
    output2 <- output2 + VV
  }
  
  output_ext_cip[[length(output_ext)+1]] <<- output2
  
  
  if (percent){
    output2 <- round(100 * output2 / nrow(data),1)  
  } else {
    output2 <- round(output2,1)  
  }
  output2[lower.tri(output2, diag = F)] <- NA
  
  # HEATMAP
  
  dt3 <- output2 %>%
    rownames_to_column() %>%
    gather(colname, hits, -rowname)
  head(dt3)
  
  
  
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
  
  return(SA_heatmap)
}



output_ext_fd <- list()


heatmap_fd_func <- function(data, in_labels, in_size = 4, leg_title = "", 
                         leg_pos = "bottom", 
                         legbarheight = 0.7, legbarwidth=15, 
                         leglimits = c(0,25), 
                         alttitle = "", percent = T){
  
  data %>% drop_na(pSA1,pSA2,pSA3,pSA4,pSA5,
                   CIP0,CIP1,CIP2,CIP3,CIP4,CIP5,CIP6,CIP7,CIP8,CIP9) -> data
  
  SA_vec <- c("pSA1", "pSA2", "pSA3", "pSA4", "pSA5")
  CIP_vec <- c("CIP0","CIP1","CIP2","CIP3","CIP4","CIP5","CIP6","CIP7","CIP8","CIP9")
  
  SA_v <- c('SA-0.0-0.19','SA-0.2-0.39','SA-0.4-0.59',
            'SA-0.6-0.79','SA-0.8-1.00')
  CIP_v <- c('CIP-0.0-0.19','CIP-0.2-0.39','CIP-0.4-0.59',
             'CIP-0.6-0.79','CIP-0.8-1.00')
  
  output2 <- data.frame(matrix(ncol = 5, nrow = 5))
  colnames(output2) <- SA_v
  rownames(output2) <- CIP_v
  output2[is.na(output2)] <- 0
  
  upper_bound <- nrow(data)
  
  for (row in c(1:upper_bound)) {
    # for each publication get SA vector V
    V = data[row,][,SA_vec]
    # calculate outer product VV
    VV = outer(as.numeric(unlist(V)),as.numeric(unlist(V)))
    # take only upper triangle and diagonal
    VV[lower.tri(VV,diag = F)] <- 0
    # calculate convergence
    
    V_fd = 0
    if (sum(VV) > 0) {
      V_fd = 1 - (sum(diag(VV))/sum(VV))  
    }
    
    # normalize (divide by summ)
    if (sum(VV) > 0){
      VV = VV/sum(VV)
    }
    # add result to the output
    
    # for each publication get CIP vector W
    W = data[row,][,CIP_vec]
    # calculate outer product WW
    WW = outer(as.numeric(unlist(W)),as.numeric(unlist(W)))
    # take only upper triangle and diagonal
    WW[lower.tri(WW,diag = F)] <- 0
    # calculate convergence
    
    W_fd = 0
    if (sum(WW) > 0){
      W_fd = 1 - (sum(diag(WW))/sum(WW))
    }
    
    
    # normalize (divide by summ)
    if (sum(WW) > 0){
      WW = WW/sum(WW)
    }
    # add result to the output
    
    if (V_fd < 0.2) {
      if (W_fd < 0.2) {
        output2['CIP-0.0-0.19','SA-0.0-0.19'] = output2['CIP-0.0-0.19','SA-0.0-0.19'] + 1
      } else if (0.2 <= W_fd & W_fd < 0.4) {
        output2['CIP-0.2-0.39','SA-0.0-0.19'] = output2['CIP-0.2-0.39','SA-0.0-0.19'] + 1
      } else if (0.4 <= W_fd & W_fd < 0.6) {
        output2['CIP-0.4-0.59','SA-0.0-0.19'] = output2['CIP-0.4-0.59','SA-0.0-0.19'] + 1
      } else if (0.6 <= W_fd & W_fd < 0.8) {
        output2['CIP-0.6-0.79','SA-0.0-0.19'] = output2['CIP-0.6-0.79','SA-0.0-0.19'] + 1
      } else if (0.8 <= W_fd & W_fd < 1.0) {
        output2['CIP-0.8-1.00','SA-0.0-0.19'] = output2['CIP-0.8-1.00','SA-0.0-0.19'] + 1
      }  
    } else if (0.2 <= V_fd & V_fd < 0.4) {
      if (W_fd < 0.2) {
        output2['CIP-0.0-0.19','SA-0.2-0.39'] = output2['CIP-0.0-0.19','SA-0.2-0.39'] + 1
      } else if (0.2 <= W_fd & W_fd < 0.4) {
        output2['CIP-0.2-0.39','SA-0.2-0.39'] = output2['CIP-0.2-0.39','SA-0.2-0.39'] + 1
      } else if (0.4 <= W_fd & W_fd < 0.6) {
        output2['CIP-0.4-0.59','SA-0.2-0.39'] = output2['CIP-0.4-0.59','SA-0.2-0.39'] + 1
      } else if (0.6 <= W_fd & W_fd < 0.8) {
        output2['CIP-0.6-0.79','SA-0.2-0.39'] = output2['CIP-0.6-0.79','SA-0.2-0.39'] + 1
      } else if (0.8 <= W_fd & W_fd < 1.0) {
        output2['CIP-0.8-1.00','SA-0.2-0.39'] = output2['CIP-0.8-1.00','SA-0.2-0.39'] + 1
      }  
    } else if (0.4 <= V_fd & V_fd < 0.6) {
      if (W_fd < 0.2) {
        output2['CIP-0.0-0.19','SA-0.4-0.59'] = output2['CIP-0.0-0.19','SA-0.4-0.59'] + 1
      } else if (0.2 <= W_fd & W_fd < 0.4) {
        output2['CIP-0.2-0.39','SA-0.4-0.59'] = output2['CIP-0.2-0.39','SA-0.4-0.59'] + 1
      } else if (0.4 <= W_fd & W_fd < 0.6) {
        output2['CIP-0.4-0.59','SA-0.4-0.59'] = output2['CIP-0.4-0.59','SA-0.4-0.59'] + 1
      } else if (0.6 <= W_fd & W_fd < 0.8) {
        output2['CIP-0.6-0.79','SA-0.4-0.59'] = output2['CIP-0.6-0.79','SA-0.4-0.59'] + 1
      } else if (0.8 <= W_fd & W_fd < 1.0) {
        output2['CIP-0.8-1.00','SA-0.4-0.59'] = output2['CIP-0.8-1.00','SA-0.4-0.59'] + 1
      }
    } else if (0.6 <= V_fd & V_fd < 0.8) {
      if (W_fd < 0.2) {
        output2['CIP-0.0-0.19','SA-0.6-0.79'] = output2['CIP-0.0-0.19','SA-0.6-0.79'] + 1
      } else if (0.2 <= W_fd & W_fd < 0.4) {
        output2['CIP-0.2-0.39','SA-0.6-0.79'] = output2['CIP-0.2-0.39','SA-0.6-0.79'] + 1
      } else if (0.4 <= W_fd & W_fd < 0.6) {
        output2['CIP-0.4-0.59','SA-0.6-0.79'] = output2['CIP-0.4-0.59','SA-0.6-0.79'] + 1
      } else if (0.6 <= W_fd & W_fd < 0.8) {
        output2['CIP-0.6-0.79','SA-0.6-0.79'] = output2['CIP-0.6-0.79','SA-0.6-0.79'] + 1
      } else if (0.8 <= W_fd & W_fd < 1.0) {
        output2['CIP-0.8-1.00','SA-0.6-0.79'] = output2['CIP-0.8-1.00','SA-0.2-0.39'] + 1
      }
    } else if (0.8 <= V_fd & V_fd < 1.0) {
      if (W_fd < 0.2) {
        output2['CIP-0.0-0.19','SA-0.8-1.00'] = output2['CIP-0.0-0.19','SA-0.8-1.00'] + 1
      } else if (0.2 <= W_fd & W_fd < 0.4) {
        output2['CIP-0.2-0.39','SA-0.8-1.00'] = output2['CIP-0.2-0.39','SA-0.8-1.00'] + 1
      } else if (0.4 <= W_fd & W_fd < 0.6) {
        output2['CIP-0.4-0.59','SA-0.8-1.00'] = output2['CIP-0.4-0.59','SA-0.8-1.00'] + 1
      } else if (0.6 <= W_fd & W_fd < 0.8) {
        output2['CIP-0.6-0.79','SA-0.8-1.00'] = output2['CIP-0.6-0.79','SA-0.8-1.00'] + 1
      } else if (0.8 <= W_fd & W_fd < 1.0) {
        output2['CIP-0.8-1.00','SA-0.8-1.00'] = output2['CIP-0.8-1.00','SA-0.8-1.00'] + 1
      }
    }
    
  }
  
  output_ext_fd[[length(output_ext_fd)+1]] <<- output2
  
  
  if (percent){
    output2 <- round(100 * output2 / nrow(data),1)  
  } else {
    output2 <- round(output2,1)  
  }
  
  # HEATMAP
  
  
  dt3 <- output2[c(1:4),c(1:4)] %>%
    rownames_to_column() %>%
    gather(colname, hits, -rowname)
  head(dt3)
  
  
  
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
    
    geom_text(aes(label = hits), size=in_size) +
    
    theme(axis.text.x=element_text(size=rel(1.5),family="NimbusMon"),
          axis.text.y=element_text(size=rel(1.5),family="NimbusMon"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 25,family="NimbusMon"),
          legend.title = element_text(size=rel(1.2), margin = margin(b = 10, unit = "pt"), family="NimbusMon"),
          legend.text = element_text(size=rel(1.2),family="NimbusMon"),
          legend.position = leg_pos)
  
  return(SA_heatmap)
}




# PERCENTS FOR CIP vs SA
fig0 <- heatmap_func(affective, "Affective data", 5, leg_title = "%", leg_pos = "none", leglimits = c(0,25))

png('C:/Users/Rubinzone/Desktop/FCIP1.png', width = 7, height = 6, units = 'in', res = 300)
annotate_figure(ggarrange(fig0, 
          legend = "bottom"),
          bottom = text_grob(
            "SA1 = Psychological sciences, SA2 = Biological sciences, SA3 = Humanities, \n S4 = Medical sciences, SA5 = Technical methods. \n
            CIP0 = Humanities, CIP1 = Biology, CIP2 = Psychology, CIP3 = Biotechnology&Genetics,
            CIP4 = Medical, CIP5 = Health Sciences, CIP6 = Pathology&Pharmacology,
            CIP7 = Neuroscience, CIP8 = Engineering&Informatics, CIP9 = Chemistry&Physics&Math",
            
            color = "black", size = 12))

dev.off()

# COGNITIVE
fig0c <- heatmap_func(cognitive, "Cognitive data", 5, leg_title = "%", leg_pos = "none", leglimits = c(0,25))

png('C:/Users/Rubinzone/Desktop/FCIP1c.png', width = 7, height = 6, units = 'in', res = 300)
annotate_figure(ggarrange(fig0c, 
                          legend = "bottom"),
                bottom = text_grob(
                  "SA1 = Psychological sciences, SA2 = Biological sciences, SA3 = Humanities, \n S4 = Medical sciences, SA5 = Technical methods. \n
            CIP0 = Humanities, CIP1 = Biology, CIP2 = Psychology, CIP3 = Biotechnology&Genetics,
            CIP4 = Medical, CIP5 = Health Sciences, CIP6 = Pathology&Pharmacology,
            CIP7 = Neuroscience, CIP8 = Engineering&Informatics, CIP9 = Chemistry&Physics&Math",
                  
                  color = "black", size = 12))

dev.off()




# PERCENTS FOR CIP vs SA & TIME FRAMES
fig1aa <- heatmap_func(affective[affective$pYear %in% c(1968:1990),], "1968 - 1990", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,30))
fig2aa <- heatmap_func(affective[affective$pYear %in% c(1991:2000),], "1991 - 2000", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,30))
fig3aa <- heatmap_func(affective[affective$pYear %in% c(2001:2010),], "2001 - 2010", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,30))
fig4aa <- heatmap_func(affective[affective$pYear %in% c(2011:2022),], "2011 - 2022", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,30))


png('C:/Users/Rubinzone/Desktop/FCIP2.png', width = 7, height = 24, units = 'in', res = 300)

annotate_figure(
ggarrange(fig1aa + rremove("x.text"), 
          fig2aa + rremove("x.text"), 
          fig3aa + rremove("x.text"), 
          fig4aa, 
          ncol = 1, nrow = 4,
          common.legend = TRUE, legend="bottom"),
bottom = text_grob(
  "SA1 = Psychological sciences    SA2 = Biological sciences    SA3 = Humanities   \n S4 = Medical sciences    SA5 = Technical methods",
  color = "black", size = 12))

dev.off()


# COGNITIVE
fig1aa <- heatmap_func(cognitive[cognitive$pYear %in% c(1968:1990),], "1968 - 1990", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,30))
fig2aa <- heatmap_func(cognitive[cognitive$pYear %in% c(1991:2000),], "1991 - 2000", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,30))
fig3aa <- heatmap_func(cognitive[cognitive$pYear %in% c(2001:2010),], "2001 - 2010", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,30))
fig4aa <- heatmap_func(cognitive[cognitive$pYear %in% c(2011:2022),], "2011 - 2022", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,30))


png('C:/Users/Rubinzone/Desktop/FCIP2c.png', width = 7, height = 24, units = 'in', res = 300)

annotate_figure(
  ggarrange(fig1aa + rremove("x.text"), 
            fig2aa + rremove("x.text"), 
            fig3aa + rremove("x.text"), 
            fig4aa, 
            ncol = 1, nrow = 4,
            common.legend = TRUE, legend="bottom"),
  bottom = text_grob(
    "SA1 = Psychological sciences    SA2 = Biological sciences    SA3 = Humanities   \n S4 = Medical sciences    SA5 = Technical methods",
    color = "black", size = 12))

dev.off()





# PERCENTS FOR CIP vs CIP
fig1 <- heatmap_cip_func(affective, "Affective data", 5, leg_title = "%", 
                         leg_pos = "none", 
                         leglimits = c(0,1), 
                         leglimits2 = c(0,50))

png('C:/Users/Rubinzone/Desktop/FCIP3.png', width = 8, height = 7.5, units = 'in', res = 300)
annotate_figure(ggarrange(fig1, 
                          legend = "bottom"),
                bottom = text_grob(
                  "CIP0 = Humanities, CIP1 = Biology, CIP2 = Psychology, CIP3 = Biotechnology&Genetics,
                  CIP4 = Medical, CIP5 = Health Sciences, CIP6 = Pathology&Pharmacology,
                  CIP7 = Neuroscience, CIP8 = Engineering&Informatics, CIP9 = Chemistry&Physics&Math",
                  
                  color = "black", size = 12))

dev.off()


# COGNITIVE
fig1c <- heatmap_cip_func(cognitive, "Cognitive data", 5, leg_title = "%", 
                         leg_pos = "none", 
                         leglimits = c(0,1), 
                         leglimits2 = c(0,50))

png('C:/Users/Rubinzone/Desktop/FCIP3c.png', width = 8, height = 7.5, units = 'in', res = 300)
annotate_figure(ggarrange(fig1c, 
                          legend = "bottom"),
                bottom = text_grob(
                  "CIP0 = Humanities, CIP1 = Biology, CIP2 = Psychology, CIP3 = Biotechnology&Genetics,
                  CIP4 = Medical, CIP5 = Health Sciences, CIP6 = Pathology&Pharmacology,
                  CIP7 = Neuroscience, CIP8 = Engineering&Informatics, CIP9 = Chemistry&Physics&Math",
                  
                  color = "black", size = 12))

dev.off()





# PERCENTS FOR fD SA vs fD CIP
fig1fd <- heatmap_fd_func(affective, "Affective data", 5, leg_title = "%", 
                         leg_pos = "none", 
                         leglimits = c(0,50))


png('C:/Users/Rubinzone/Desktop/FCIP4fd.png', width = 8, height = 7.5, units = 'in', res = 300)
annotate_figure(ggarrange(fig1fd, 
                          legend = "bottom"),
                bottom = text_grob(
                  "",
                  
                  color = "black", size = 12))

dev.off()

# COGNITIVE
fig1fdc <- heatmap_fd_func(cognitive, "Cognitive data", 5, leg_title = "%", 
                          leg_pos = "none", 
                          leglimits = c(0,50))


png('C:/Users/Rubinzone/Desktop/FCIP4fdc.png', width = 8, height = 7.5, units = 'in', res = 300)
annotate_figure(ggarrange(fig1fdc, 
                          legend = "bottom"),
                bottom = text_grob(
                  "",
                  
                  color = "black", size = 12))

dev.off()



# PERCENTS FOR fD SA vs fD CIP & TIME FRAMES
fig1aa <- heatmap_fd_func(affective[affective$pYear %in% c(1968:1990),], "1968 - 1990", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,50))
fig2aa <- heatmap_fd_func(affective[affective$pYear %in% c(1991:2000),], "1991 - 2000", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,50))
fig3aa <- heatmap_fd_func(affective[affective$pYear %in% c(2001:2010),], "2001 - 2010", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,50))
fig4aa <- heatmap_fd_func(affective[affective$pYear %in% c(2011:2022),], "2011 - 2022", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,50))

png('C:/Users/Rubinzone/Desktop/FCIP4fd_evo.png', width = 11, height = 10, units = 'in', res = 300)

annotate_figure(
  ggarrange(fig1aa + rremove("x.text"), 
            fig2aa + rremove("x.text") + rremove("y.text"), 
            fig3aa, 
            fig4aa + rremove("y.text"), 
            ncol = 2, nrow = 2,
            widths = c(1.2,1,1.2,1),
            common.legend = TRUE, legend="bottom"),
  bottom = text_grob(
    "Evolution over time",
    color = "black", size = 12))

dev.off()


# COGNITIVE
# NOT NEEDED?


# PERCENTS FOR fD SA vs fD CIP & ABOVE/BELOW
fig1aa <- heatmap_fd_func(affective[affective$pCitations_n_norm > 0,], "Above mean Cn", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,50))
fig2aa <- heatmap_fd_func(affective[affective$pCitations_n_norm <= 0,], "Below mean Cn", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,50))

png('C:/Users/Rubinzone/Desktop/FCIP5fd.png', width = 11, height = 5, units = 'in', res = 300)

annotate_figure(
  ggarrange(fig1aa, 
            fig2aa + rremove("y.text"), 
            ncol = 2, nrow = 1,
            widths = c(1.2,1),
            common.legend = TRUE, legend="bottom"),
  bottom = text_grob(
    "",
    color = "black", size = 12))

dev.off()

# COGNITIVE
fig1aac <- heatmap_fd_func(cognitive[cognitive$pCitations_n_norm > 0,], "Above mean Cn", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,50))
fig2aac <- heatmap_fd_func(cognitive[cognitive$pCitations_n_norm <= 0,], "Below mean Cn", 5 ,leg_title = "", leg_pos = "none", leglimits = c(0,50))

png('C:/Users/Rubinzone/Desktop/FCIP5fdc.png', width = 11, height = 5, units = 'in', res = 300)

annotate_figure(
  ggarrange(fig1aac, 
            fig2aac + rremove("y.text"), 
            ncol = 2, nrow = 1,
            widths = c(1.2,1),
            common.legend = TRUE, legend="bottom"),
  bottom = text_grob(
    "",
    color = "black", size = 12))

dev.off()
