library(plyr)
library(readr)
library(tidyr)
library(dplyr)
library(networkD3)
library(htmlwidgets)
library(webshot)


# Pure L1 categories
affective <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective.csv")
cit <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/citations.csv")
ref <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/references.csv")

cit_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations_subjectareas.csv")
ref_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references_subjectareas.csv")
aff_subj <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/affective_subjectareas.csv")

cit_links <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/citations_net_years.csv")
ref_links <- read.csv("E:/Research/Data_pubmed/affective_data/balanced/references_net_years.csv")

colnames(ref_links)
colnames(ref_subj)[1] <- "references"
ref_links <- merge(x=ref_links, y=ref_subj, by = "references", all.x = TRUE)
colnames(aff_subj) <- c("pmid","Ap","Bp","Cp","Dp","Ep","Fp","Gp","Hp","Ip","Jp","Kp","Lp","Mp","Np","Vp","Zp")
ref_links <- merge(x=ref_links, y=aff_subj, by = "pmid", all.x = TRUE)
ref_links <- merge(x=ref_links, y=affective[,c(2,3)], by = "pmid", all.x = TRUE)
colnames(ref_links)[38] <- "year_published"

colnames(cit_links)
colnames(cit_subj)[1] <- "cited_by"
cit_links <- merge(x=cit_links, y=cit_subj, by = "cited_by", all.x = TRUE)
cit_links <- merge(x=cit_links, y=aff_subj, by = "pmid", all.x = TRUE)
cit_links <- merge(x=cit_links, y=affective[,c(2,3)], by = "pmid", all.x = TRUE)
colnames(cit_links)[38] <- "year_published"


remove(affective, aff_subj, cit, cit_subj, ref, ref_subj)



sankey <- function(var_cit_links, var_ref_links, png_file_prefix){
  
  var_cit_links$SA1_ABG <- as.numeric(var_cit_links$A + var_cit_links$B + var_cit_links$G)
  var_cit_links$SA2_F <- as.numeric(var_cit_links$F)
  var_cit_links$SA3_CN <- as.numeric(var_cit_links$C + var_cit_links$N)
  var_cit_links$SA4_DEJL <- as.numeric(var_cit_links$D + var_cit_links$E + var_cit_links$J + var_cit_links$L)
  var_cit_links$SA5_HIKM <- as.numeric(var_cit_links$H + var_cit_links$I + var_cit_links$K + var_cit_links$M)
  var_cit_links$SA1_ABGp <- as.numeric(var_cit_links$Ap + var_cit_links$Bp + var_cit_links$Gp)
  var_cit_links$SA2_Fp <- as.numeric(var_cit_links$Fp)
  var_cit_links$SA3_CNp <- as.numeric(var_cit_links$Cp + var_cit_links$Np)
  var_cit_links$SA4_DEJLp <- as.numeric(var_cit_links$Dp + var_cit_links$Ep + var_cit_links$Jp + var_cit_links$Lp)
  var_cit_links$SA5_HIKMp <- as.numeric(var_cit_links$Hp + var_cit_links$Ip + var_cit_links$Kp + var_cit_links$Mp)
  
  
  var_ref_links$SA1_ABG <- as.numeric(var_ref_links$A + var_ref_links$B + var_ref_links$G)
  var_ref_links$SA2_F <- as.numeric(var_ref_links$F)
  var_ref_links$SA3_CN <- as.numeric(var_ref_links$C + var_ref_links$N)
  var_ref_links$SA4_DEJL <- as.numeric(var_ref_links$D + var_ref_links$E + var_ref_links$J + var_ref_links$L)
  var_ref_links$SA5_HIKM <- as.numeric(var_ref_links$H + var_ref_links$I + var_ref_links$K + var_ref_links$M)
  var_ref_links$SA1_ABGp <- as.numeric(var_ref_links$Ap + var_ref_links$Bp + var_ref_links$Gp)
  var_ref_links$SA2_Fp <- as.numeric(var_ref_links$Fp)
  var_ref_links$SA3_CNp <- as.numeric(var_ref_links$Cp + var_ref_links$Np)
  var_ref_links$SA4_DEJLp <- as.numeric(var_ref_links$Dp + var_ref_links$Ep + var_ref_links$Jp + var_ref_links$Lp)
  var_ref_links$SA5_HIKMp <- as.numeric(var_ref_links$Hp + var_ref_links$Ip + var_ref_links$Kp + var_ref_links$Mp)
  
  
  
  var_cit_links[var_cit_links$SA1_ABGp >= 1,] %>% group_by(year_published) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_cit_links_SA1p
  var_cit_links_SA1p$cluster <- "SA1p"
  
  var_cit_links[var_cit_links$SA2_Fp >= 1,] %>% group_by(year_published) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_cit_links_SA2p
  var_cit_links_SA2p$cluster <- "SA2p"
  
  var_cit_links[var_cit_links$SA3_CNp >= 1,] %>% group_by(year_published) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_cit_links_SA3p
  var_cit_links_SA3p$cluster <- "SA3p"
  
  var_cit_links[var_cit_links$SA4_DEJLp >= 1,] %>% group_by(year_published) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_cit_links_SA4p
  var_cit_links_SA4p$cluster <- "SA4p"
  
  var_cit_links[var_cit_links$SA5_HIKMp >= 1,] %>% group_by(year_published) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_cit_links_SA5p
  var_cit_links_SA5p$cluster <- "SA5p"
  
  var_ref_links[var_ref_links$SA1_ABGp >= 1,] %>% group_by(year_references) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_ref_links_SA1p
  var_ref_links_SA1p$cluster <- "SA1p"
  
  var_ref_links[var_ref_links$SA2_Fp >= 1,] %>% group_by(year_references) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_ref_links_SA2p
  var_ref_links_SA2p$cluster <- "SA2p"
  
  var_ref_links[var_ref_links$SA3_CNp >= 1,] %>% group_by(year_references) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_ref_links_SA3p
  var_ref_links_SA3p$cluster <- "SA3p"
  
  var_ref_links[var_ref_links$SA4_DEJLp >= 1,] %>% group_by(year_references) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_ref_links_SA4p
  var_ref_links_SA4p$cluster <- "SA4p"
  
  var_ref_links[var_ref_links$SA5_HIKMp >= 1,] %>% group_by(year_references) %>% summarise(
    SA1_ABG = sum(SA1_ABG, na.rm = T),
    SA2_F = sum(SA2_F, na.rm = T),
    SA3_CN = sum(SA3_CN, na.rm = T),
    SA4_DEJL = sum(SA4_DEJL, na.rm = T),
    SA5_HIKM = sum(SA5_HIKM, na.rm = T),
    total_records = n(),
    total_hits = sum(SA1_ABG + SA2_F + SA3_CN + SA4_DEJL + SA5_HIKM)) -> var_ref_links_SA5p
  var_ref_links_SA5p$cluster <- "SA5p"
  
  
  
  ### 1. CIT FULL
  
  ## create a dataframe with 10 nodes
  nodes = data.frame("name" = c("SA1", "SA2", "SA3", "SA4", "SA5", 
                                "SA1p", "SA2p", "SA3p", "SA4p", "SA5p"))
  
  
  ## create edges with weights
  #sum_cit = sum(var_cit_links$SA1_ABG, var_cit_links$SA2_F, var_cit_links$SA3_CN, var_cit_links$SA4_DEJL, var_cit_links$SA5_HIKM, na.rm = T)
  links = as.data.frame(matrix(c(0, 5, sum(var_cit_links_SA1p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA1p$total_hits), # SA1 - SA1p                                 
                                 1, 5, sum(var_cit_links_SA1p$SA2_F, na.rm = T) / sum(var_cit_links_SA1p$total_hits), #                                
                                 2, 5, sum(var_cit_links_SA1p$SA3_CN, na.rm = T) / sum(var_cit_links_SA1p$total_hits), # 
                                 3, 5, sum(var_cit_links_SA1p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA1p$total_hits), #
                                 4, 5, sum(var_cit_links_SA1p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA1p$total_hits), #
                                 
                                 0, 6, sum(var_cit_links_SA2p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA2p$total_hits), # SA1 - SA2p                                 
                                 1, 6, sum(var_cit_links_SA2p$SA2_F, na.rm = T) / sum(var_cit_links_SA2p$total_hits), #                                
                                 2, 6, sum(var_cit_links_SA2p$SA3_CN, na.rm = T) / sum(var_cit_links_SA2p$total_hits), # 
                                 3, 6, sum(var_cit_links_SA2p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA2p$total_hits), #
                                 4, 6, sum(var_cit_links_SA2p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA2p$total_hits), #
                                 
                                 0, 7, sum(var_cit_links_SA3p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA3p$total_hits), # SA1 - SA3p                                 
                                 1, 7, sum(var_cit_links_SA3p$SA2_F, na.rm = T) / sum(var_cit_links_SA3p$total_hits), #                                
                                 2, 7, sum(var_cit_links_SA3p$SA3_CN, na.rm = T) / sum(var_cit_links_SA3p$total_hits), # 
                                 3, 7, sum(var_cit_links_SA3p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA3p$total_hits), #
                                 4, 7, sum(var_cit_links_SA3p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA3p$total_hits), #
                                 
                                 0, 8, sum(var_cit_links_SA4p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA4p$total_hits), # SA1 - SA4p                                 
                                 1, 8, sum(var_cit_links_SA4p$SA2_F, na.rm = T) / sum(var_cit_links_SA4p$total_hits), #                                
                                 2, 8, sum(var_cit_links_SA4p$SA3_CN, na.rm = T) / sum(var_cit_links_SA4p$total_hits), # 
                                 3, 8, sum(var_cit_links_SA4p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA4p$total_hits), #
                                 4, 8, sum(var_cit_links_SA4p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA4p$total_hits), #
                                 
                                 0, 9, sum(var_cit_links_SA5p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA5p$total_hits), # SA1 - SA5p                                 
                                 1, 9, sum(var_cit_links_SA5p$SA2_F, na.rm = T) / sum(var_cit_links_SA5p$total_hits), #                                
                                 2, 9, sum(var_cit_links_SA5p$SA3_CN, na.rm = T) / sum(var_cit_links_SA5p$total_hits), # 
                                 3, 9, sum(var_cit_links_SA5p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA5p$total_hits), #
                                 4, 9, sum(var_cit_links_SA5p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA5p$total_hits) #
  ), byrow = TRUE, ncol = 3))
  
  ## set column names for links
  names(links) = c("source", "target", "value")
  
  ## add edge types for coloring purpose
  links$group = c("type_0", 
                  "type_1", 
                  "type_2",
                  "type_3", 
                  "type_4",
                  
                  "type_0", 
                  "type_1", 
                  "type_2",
                  "type_3",
                  "type_4", 
                  
                  "type_0", 
                  "type_1", 
                  "type_2",
                  "type_3",
                  "type_4",
                  
                  "type_0", 
                  "type_1", 
                  "type_2",
                  "type_3",
                  "type_4",
                  
                  "type_0", 
                  "type_1", 
                  "type_2",
                  "type_3",
                  "type_4")
  
  ## Create custom color list using d3 for each node
  node_color <- 'd3.scaleOrdinal() .domain([
  "SA1", "SA2", "SA3", "SA4", "SA5", 
  "SA1p", "SA2p", "SA3p", "SA4p", "SA5p", 
  "type_0", "type_1", "type_2", "type_3", "type_4"]) .range([
  "orange", "magenta", "red" , "darkgreen", "darkblue", 
  "orange", "magenta", "red" , "darkgreen", "darkblue",
  "orange", "magenta", "red" , "darkgreen", "darkblue",])'

  
  ## Draw Sankey Diagram
  r = sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize = 0, nodeWidth = 40,
                    colourScale = node_color,
                    LinkGroup="group")
  
  #r
  
  
  ### 2. REF FULL
  
  ## create a dataframe with 10 nodes
  nodes = data.frame("name" = c("SA1", "SA2", "SA3", "SA4", "SA5", 
                                "SA1p", "SA2p", "SA3p", "SA4p", "SA5p"))
  
  
  ## create edges with weights
  sum_cit = sum(var_ref_links$SA1_ABG, var_ref_links$SA2_F, var_ref_links$SA3_CN, var_ref_links$SA4_DEJL, var_ref_links$SA5_HIKM, na.rm = T)
  links = as.data.frame(matrix(c(0, 5, sum(var_ref_links_SA1p$SA1_ABG, na.rm = T) /  sum(var_cit_links_SA1p$total_hits), # SA1 - SA1p                                 
                                 1, 5, sum(var_ref_links_SA1p$SA2_F, na.rm = T) /  sum(var_cit_links_SA1p$total_hits), #                                
                                 2, 5, sum(var_ref_links_SA1p$SA3_CN, na.rm = T) /  sum(var_cit_links_SA1p$total_hits), # 
                                 3, 5, sum(var_ref_links_SA1p$SA4_DEJL, na.rm = T) /  sum(var_cit_links_SA1p$total_hits), #
                                 4, 5, sum(var_ref_links_SA1p$SA5_HIKM, na.rm = T) /  sum(var_cit_links_SA1p$total_hits), #
                                 
                                 0, 6, sum(var_ref_links_SA2p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA2p$total_hits), # SA1 - SA1p                                 
                                 1, 6, sum(var_ref_links_SA2p$SA2_F, na.rm = T) / sum(var_cit_links_SA2p$total_hits), #                                
                                 2, 6, sum(var_ref_links_SA2p$SA3_CN, na.rm = T) / sum(var_cit_links_SA2p$total_hits), # 
                                 3, 6, sum(var_ref_links_SA2p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA2p$total_hits), #
                                 4, 6, sum(var_ref_links_SA2p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA2p$total_hits), #
                                 
                                 0, 7, sum(var_ref_links_SA3p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA3p$total_hits), # SA1 - SA1p                                 
                                 1, 7, sum(var_ref_links_SA3p$SA2_F, na.rm = T) / sum(var_cit_links_SA3p$total_hits), #                                
                                 2, 7, sum(var_ref_links_SA3p$SA3_CN, na.rm = T) / sum(var_cit_links_SA3p$total_hits), # 
                                 3, 7, sum(var_ref_links_SA3p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA3p$total_hits), #
                                 4, 7, sum(var_ref_links_SA3p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA3p$total_hits), #
                                 
                                 0, 8, sum(var_ref_links_SA4p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA4p$total_hits), # SA1 - SA1p                                 
                                 1, 8, sum(var_ref_links_SA4p$SA2_F, na.rm = T) / sum(var_cit_links_SA4p$total_hits), #                                
                                 2, 8, sum(var_ref_links_SA4p$SA3_CN, na.rm = T) / sum(var_cit_links_SA4p$total_hits), # 
                                 3, 8, sum(var_ref_links_SA4p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA4p$total_hits), #
                                 4, 8, sum(var_ref_links_SA4p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA4p$total_hits), #
                                 
                                 0, 9, sum(var_ref_links_SA5p$SA1_ABG, na.rm = T) / sum(var_cit_links_SA5p$total_hits), # SA1 - SA1p                                 
                                 1, 9, sum(var_ref_links_SA5p$SA2_F, na.rm = T) / sum(var_cit_links_SA5p$total_hits), #                                
                                 2, 9, sum(var_ref_links_SA5p$SA3_CN, na.rm = T) / sum(var_cit_links_SA5p$total_hits), # 
                                 3, 9, sum(var_ref_links_SA5p$SA4_DEJL, na.rm = T) / sum(var_cit_links_SA5p$total_hits), #
                                 4, 9, sum(var_ref_links_SA5p$SA5_HIKM, na.rm = T) / sum(var_cit_links_SA5p$total_hits) #
  ), byrow = TRUE, ncol = 3))
  
  ## set column names for links
  names(links) = c("source", "target", "value")
  
  ## add edge types for coloring purpose
  links$group = c("type_0", 
                  "type_1", 
                  "type_2",
                  "type_3", 
                  "type_4",
                  
                  "type_0", 
                  "type_1", 
                  "type_2",
                  "type_3",
                  "type_4", 
                  
                  "type_0", 
                  "type_1", 
                  "type_2",
                  "type_3",
                  "type_4",
                  
                  "type_0", 
                  "type_1", 
                  "type_2",
                  "type_3",
                  "type_4",
                  
                  "type_0", 
                  "type_1", 
                  "type_2",
                  "type_3",
                  "type_4")
  
  ## Create custom color list using d3 for each node
  node_color <- 'd3.scaleOrdinal() .domain([
  "SA1", "SA2", "SA3", "SA4", "SA5", 
  "SA1p", "SA2p", "SA3p", "SA4p", "SA5p", 
  "type_0", "type_1", "type_2", "type_3", "type_4"]) .range([
  "orange", "magenta", "red" , "darkgreen", "darkblue", 
  "orange", "magenta", "red" , "darkgreen", "darkblue",
  "orange", "magenta", "red" , "darkgreen", "darkblue",])'
  
  
  ## Draw Sankey Diagram
  p = sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",
                    fontSize = 0, nodeWidth = 40,
                    colourScale = node_color,
                    LinkGroup="group")
  
  
  ## Save both p and r
  saveWidget(p, "temp1.html")
  webshot("temp1.html", file = paste("ref_", png_file_prefix, ".png", sep =""))
  file.remove("temp1.html")
  saveWidget(r, "temp2.html")
  webshot("temp2.html", file = paste("cit_", png_file_prefix, ".png", sep =""))
  file.remove("temp2.html")
}


setwd("E:/Research/Data_pubmed/Descriptive/")

sankey(cit_links, ref_links, "full")
sankey(subset(cit_links, year_published <= 1965), 
       subset(ref_links, year_references <= 1965),
       "1950_1965")
sankey(subset(cit_links, year_published > 1965 & year_published <= 1990),
       subset(ref_links, year_references > 1965 & year_references <= 1990),
       "1966_1990")
sankey(subset(cit_links, year_published >= 1991 & year_published <= 2000),
       subset(ref_links, year_references >= 1991 & year_references <= 2000),
       "1991_2000")
sankey(subset(cit_links, year_published >= 2001 & year_published <= 2010),
       subset(ref_links, year_references >= 2001 & year_references <= 2010), 
       "2001_2010")
sankey(subset(cit_links, year_published >= 2011 & year_published <= 2020),
       subset(ref_links, year_references >= 2011 & year_references <= 2020), 
       "2011_2020")
