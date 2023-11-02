library(dplyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(stringr)



cog_bal <- read.csv("E:/Research/Data_ieee/data/web_parser_L1_uid.csv")

setwd("E:/Research/Data_pubmed/citations_data23")
# Get the files names
files = list.files(pattern="*.json")
myData <- new.env()
idx <- as.character(length(myData) + 1)

for (file in files) {
  print(file)
  stream_in(file(file), handler = function(df){ 
    idx <<- as.character(length(myData) + 1)
    myData[[idx]] <<- df[,c(1,2,8,22,23)] ## filter
  }, pagesize = 10000)
}

myData2 <- myData %>% as.list() %>% rbindlist()


length(subset(myData2, doi %in% cog_bal$doi))
