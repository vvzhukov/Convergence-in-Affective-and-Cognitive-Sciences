# Pure L1 categories
affective <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")
cit <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/citations.csv")
ref <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/references.csv")

cit_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations_subjectareas.csv")
ref_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references_subjectareas.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

cit_links <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations_net_years.csv")
ref_links <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references_net_years.csv")

# SA1
nrow(aff_subj[(aff_subj$A != 0) | (aff_subj$B != 0) | (aff_subj$G != 0) ,]) # 89 511
sample(aff_subj[(aff_subj$A != 0) | (aff_subj$B != 0) | (aff_subj$G != 0), 1], 30)

# SA2
nrow(aff_subj[(aff_subj$F != 0),]) # 237 816
sample(aff_subj[(aff_subj$F != 0),1], 30)

# SA3
nrow(aff_subj[(aff_subj$C != 0) | (aff_subj$N != 0),]) # 118 501
sample(aff_subj[(aff_subj$C != 0) | (aff_subj$N != 0),1], 30)

# SA4
nrow(aff_subj[(aff_subj$D != 0) | (aff_subj$E != 0) | (aff_subj$J != 0) | (aff_subj$L != 0),]) # 105 037
sample(aff_subj[(aff_subj$D != 0) | (aff_subj$E != 0) | (aff_subj$J != 0) | (aff_subj$L != 0),1], 30)


# SA5
nrow(aff_subj[(aff_subj$H != 0) | (aff_subj$I != 0) | (aff_subj$K != 0) | (aff_subj$M != 0),]) #80 337
sample(aff_subj[(aff_subj$H != 0) | (aff_subj$I != 0) | (aff_subj$K != 0) | (aff_subj$M != 0),1], 30)

remove(cit, cit_links, cit_subj, ref, ref_links, ref_subj)



library(tidyverse)
library(scales) 
library(ggpubr)


heatmap_func <- function(aff_subj, title_prefix, in_labels, rem_x_text = FALSE){
  output <- data.frame(matrix(ncol = 14, nrow = 14))
  colnames(output) <- colnames(aff_subj[2:15])
  rownames(output) <- colnames(aff_subj[2:15])
  
  for (group in colnames(output)) {
    for (subgroup in rownames(output)) {
      if (group != subgroup){
        output[[group,subgroup]] <- sum(aff_subj[aff_subj[[group]] > 0,][[subgroup]])
      } else {
        output[[group,subgroup]] <- sum(aff_subj[aff_subj[[group]] > 0 & rowSums(aff_subj[,2:17]) - aff_subj[[group]] == 0,][[subgroup]])
        
      }
    }
  }
  output[lower.tri(output, diag = F)] <- 0
  
  # Now same for SAs
  aff_subj$SA1 <- as.numeric(aff_subj$A + aff_subj$B + aff_subj$G)
  aff_subj$SA2 <- as.numeric(aff_subj$F)
  aff_subj$SA3 <- as.numeric(aff_subj$C + aff_subj$N)
  aff_subj$SA4 <- as.numeric(aff_subj$D + aff_subj$E + aff_subj$J + aff_subj$L)
  aff_subj$SA5 <- as.numeric(aff_subj$H + aff_subj$I + aff_subj$K + aff_subj$M)
  
  output2 <- data.frame(matrix(ncol = 5, nrow = 5))
  colnames(output2) <- c("SA1", "SA2", "SA3", "SA4", "SA5")
  rownames(output2) <- c("SA1", "SA2", "SA3", "SA4", "SA5")
  
  for (group in colnames(output2)) {
    for (subgroup in rownames(output2)) {
      if (group != subgroup){
        output2[[group,subgroup]] <- sum(aff_subj[aff_subj[[group]] > 0,][[subgroup]])
      } else {
        output2[[group,subgroup]] <- sum(aff_subj[aff_subj[[group]] > 0 & rowSums(aff_subj[,2:17]) - aff_subj[[group]] == 0,][[subgroup]])
      }
    }
  }
  output2[lower.tri(output2, diag = F)] <- NA
  
  # HEATMAP

  
  dt2 <- output %>%
    rownames_to_column() %>%
    gather(colname, hits, -rowname)
  head(dt2)
  
  L1_heatmap <- ggplot(dt2, aes(x = rowname, y = colname, fill = hits)) +
    geom_tile() + 
    scale_fill_gradient(name = "hits",
                        low = "#FFFFFF",
                        high = "#012345") +
    theme(axis.text.x=element_text(size=rel(1.5)),
          axis.text.y=element_text(size=rel(1.5)),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5))
  
  
  dt3 <- output2 %>%
    rownames_to_column() %>%
    gather(colname, hits, -rowname)
  head(dt3)
  
  SA_heatmap <- ggplot(dt3, aes(x = rowname, y = colname, fill = hits)) +
    geom_tile() + 
    geom_text(aes(label = hits), size=4) + 
    scale_fill_gradient(name = "hits",
                        low = "#FFFFFF",
                        high = "#FF7852",
                        na.value = "#FFFFFF",
                        labels = comma) +
    theme(axis.text.x=element_text(size=rel(1.5)),
          axis.text.y=element_text(size=rel(1.5)),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5))
  
  # should be 1400 x 550
  if (rem_x_text) {
    heatmap <- ggarrange(L1_heatmap + rremove("x.text"), SA_heatmap + rremove("x.text"), 
                         labels = in_labels,
                         ncol = 2, nrow = 1)
  } else {
    heatmap <- ggarrange(L1_heatmap, SA_heatmap, 
                         labels = in_labels,
                         ncol = 2, nrow = 1)
  }  
  
  
  
  heatmap_labelled <- annotate_figure(heatmap, top = text_grob(
                                        title_prefix,
                                        color = "black", face = "bold", size = 14))
  return(heatmap_labelled)
}

#aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1950:1965),2],]

fig0 <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:2020),2],], "1967 - 2020", c("A", "B"))
annotate_figure(fig0, bottom = text_grob(
  "SA1 - Biological sciences, SA2 - Psychological sciences, SA3 - Medical sciences, SA4 - Technical methods, SA5 - Humanities",
  color = "black", size = 10))


fig1 <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1967:1990),2],], "1967 - 1990", c("A", "B"))
fig2 <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(1991:2000),2],], "1991 - 2000", c("E", "F"))
fig3 <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2001:2010),2],], "2001 - 2010", c("G", "H"))
fig4 <- heatmap_func(aff_subj[aff_subj$pmid %in% affective[affective$year %in% c(2011:2020),2],], "2011 - 2020", c("I", "J"))

fig5 <- ggarrange(fig1, fig2, fig3, fig4, 
                  ncol = 1, nrow = 4)
annotate_figure(fig5, bottom = text_grob(
  "SA1 - Biological sciences, SA2 - Psychological sciences, SA3 - Medical sciences, SA4 - Technical methods, SA5 - Humanities",
  color = "black", size = 10))
