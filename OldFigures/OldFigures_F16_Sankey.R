library(dplyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(stringr)
library(stringi)
library(superml)
library(lsa)
library(GGally)
library(corpcor)
library(mctest)
library(olsrr)
library(sjPlot)
library(glmmTMB)
library(effects)
library(ggpubr)
library(networkD3)
library(htmlwidgets)
library(webshot)

model_data <- read.csv("E:/Research/Data_pubmed/model_data/model_data3.csv")


## Sankey References - Publication

## create a dataframe with 10 nodes
nodes = data.frame("name" = c("SA1r", "SA2r", "SA3r", "SA4r", "SA5r", 
                              "SA1p", "SA2p", "SA3p", "SA4p", "SA5p"))


model_data <- model_data[model_data$pType=="cog nb",]

## create edges with weights
links = as.data.frame(matrix(c(0, 5, sum(model_data[model_data$pSA1 > 0,c("rSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), # SA1r - SA1p                                 
                               1, 5, sum(model_data[model_data$pSA1 > 0,c("rSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), #                                
                               2, 5, sum(model_data[model_data$pSA1 > 0,c("rSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), # 
                               3, 5, sum(model_data[model_data$pSA1 > 0,c("rSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), #
                               4, 5, sum(model_data[model_data$pSA1 > 0,c("rSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), #
                               
                               0, 6, sum(model_data[model_data$pSA2 > 0,c("rSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), # SA1r - SA2p                                 
                               1, 6, sum(model_data[model_data$pSA2 > 0,c("rSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),                                
                               2, 6, sum(model_data[model_data$pSA2 > 0,c("rSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               3, 6, sum(model_data[model_data$pSA2 > 0,c("rSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               4, 6, sum(model_data[model_data$pSA2 > 0,c("rSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               
                               0, 7, sum(model_data[model_data$pSA3 > 0,c("rSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), # SA1r - SA3p                                 
                               1, 7, sum(model_data[model_data$pSA3 > 0,c("rSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),                        
                               2, 7, sum(model_data[model_data$pSA3 > 0,c("rSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               3, 7, sum(model_data[model_data$pSA3 > 0,c("rSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               4, 7, sum(model_data[model_data$pSA3 > 0,c("rSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               
                               0, 8, sum(model_data[model_data$pSA4 > 0,c("rSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), # SA1r - SA4p                                 
                               1, 8, sum(model_data[model_data$pSA4 > 0,c("rSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),                               
                               2, 8, sum(model_data[model_data$pSA4 > 0,c("rSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               3, 8, sum(model_data[model_data$pSA4 > 0,c("rSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               4, 8, sum(model_data[model_data$pSA4 > 0,c("rSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               
                               0, 9, sum(model_data[model_data$pSA5 > 0,c("rSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])), # SA1r - SA5p                                 
                               1, 9, sum(model_data[model_data$pSA5 > 0,c("rSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),                                
                               2, 9, sum(model_data[model_data$pSA5 > 0,c("rSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               3, 9, sum(model_data[model_data$pSA5 > 0,c("rSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")])),
                               4, 9, sum(model_data[model_data$pSA5 > 0,c("rSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("rSA1","rSA2","rSA3","rSA4","rSA5")]))
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
  "SA1r", "SA2r", "SA3r", "SA4r", "SA5r", 
  "SA1p", "SA2p", "SA3p", "SA4p", "SA5p", 
  "type_0", "type_1", "type_2", "type_3", "type_4"]) .range([
  "red", "orange", "yellow" , "green", "blue", 
  "red", "orange", "yellow" , "green", "blue",
  "red", "orange", "yellow" , "green", "blue",])'


## Draw Sankey Diagram
r = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize = 0, nodeWidth = 40,
                  colourScale = node_color,
                  LinkGroup="group")





## Sankey Citations - Publication

## create a dataframe with 10 nodes
nodes = data.frame("name" = c("SA1c", "SA2c", "SA3c", "SA4c", "SA5c", 
                              "SA1p", "SA2p", "SA3p", "SA4p", "SA5p"))


## create edges with weights
links = as.data.frame(matrix(c(0, 5, sum(model_data[model_data$pSA1 > 0,c("cSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), # SA1c - SA1p                                 
                               1, 5, sum(model_data[model_data$pSA1 > 0,c("cSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), #                                
                               2, 5, sum(model_data[model_data$pSA1 > 0,c("cSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), # 
                               3, 5, sum(model_data[model_data$pSA1 > 0,c("cSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), #
                               4, 5, sum(model_data[model_data$pSA1 > 0,c("cSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA1 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), #
                               
                               0, 6, sum(model_data[model_data$pSA2 > 0,c("cSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), # SA1c - SA2p                                 
                               1, 6, sum(model_data[model_data$pSA2 > 0,c("cSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),                                
                               2, 6, sum(model_data[model_data$pSA2 > 0,c("cSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               3, 6, sum(model_data[model_data$pSA2 > 0,c("cSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               4, 6, sum(model_data[model_data$pSA2 > 0,c("cSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA2 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               
                               0, 7, sum(model_data[model_data$pSA3 > 0,c("cSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), # SA1c - SA3p                                 
                               1, 7, sum(model_data[model_data$pSA3 > 0,c("cSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),                        
                               2, 7, sum(model_data[model_data$pSA3 > 0,c("cSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               3, 7, sum(model_data[model_data$pSA3 > 0,c("cSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               4, 7, sum(model_data[model_data$pSA3 > 0,c("cSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA3 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               
                               0, 8, sum(model_data[model_data$pSA4 > 0,c("cSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), # SA1c - SA4p                                 
                               1, 8, sum(model_data[model_data$pSA4 > 0,c("cSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),                               
                               2, 8, sum(model_data[model_data$pSA4 > 0,c("cSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               3, 8, sum(model_data[model_data$pSA4 > 0,c("cSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               4, 8, sum(model_data[model_data$pSA4 > 0,c("cSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA4 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               
                               0, 9, sum(model_data[model_data$pSA5 > 0,c("cSA1")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])), # SA1r - SA5p                                 
                               1, 9, sum(model_data[model_data$pSA5 > 0,c("cSA2")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),                                
                               2, 9, sum(model_data[model_data$pSA5 > 0,c("cSA3")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               3, 9, sum(model_data[model_data$pSA5 > 0,c("cSA4")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")])),
                               4, 9, sum(model_data[model_data$pSA5 > 0,c("cSA5")], na.rm = T) / sum(colSums(model_data[model_data$pSA5 > 0,c("cSA1","cSA2","cSA3","cSA4","cSA5")]))
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
  "SA1c", "SA2c", "SA3c", "SA4c", "SA5c", 
  "SA1p", "SA2p", "SA3p", "SA4p", "SA5p", 
  "type_0", "type_1", "type_2", "type_3", "type_4"]) .range([
  "red", "orange", "yellow" , "green", "blue", 
  "red", "orange", "yellow" , "green", "blue",
  "red", "orange", "yellow" , "green", "blue",])'


## Draw Sankey Diagram
c = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize = 0, nodeWidth = 40,
                  colourScale = node_color,
                  LinkGroup="group")






setwd("C:/Users/Rubinzone/Desktop/")

## Save both p and r
saveWidget(r, "temp1.html")
webshot("temp1.html", file = paste("ref_", "1", ".png", sep =""))
file.remove("temp1.html")
saveWidget(c, "temp2.html")
webshot("temp2.html", file = paste("cit_", "1", ".png", sep =""))
file.remove("temp2.html")
