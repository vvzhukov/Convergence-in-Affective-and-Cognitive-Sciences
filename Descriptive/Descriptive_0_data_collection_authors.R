library(xml2)
library(XML)
library(plyr)

# SLOW! Use python instead!
# Call: & 'C:\Program Files\R\R-4.0.3\bin\Rscript.exe' --vanilla .\0_data_collection.R 1 1 10
args = commandArgs(trailingOnly=TRUE)

if (length(args)<3) {
  stop("At least three argument must be supplied (worker_name, in_file_starg, in_file_end).n", call.=FALSE)
} else if (length(args)==3) {
  worker = strtoi(args[1])
  range_low = strtoi(args[2])
  range_high = strtoi(args[3])
}


data_folder = 'E:/Research/Data_pubmed/baseline/'
processed_data = 'processed_data_authors/'


output <- data.frame("id"=integer(),
                     "LastName"=character(),
                     "ForeName"=character(),
                     "Initials"=character(),
                     "Suffix"=character(),
                     "CollectiveName"=character(),
                     "Identifier"=character(),
                     "AffiliationInfo"=character())


for (file in list.files(path = data_folder, pattern = ".xml")[range_low:range_high]){
  
  data = read_xml(paste(data_folder, file, sep =''))
  n_records <- xml_length(data)
  print(paste(Sys.time(), "Worker ", worker, " started. Range to process: ", range_low, " to ", range_high))
  print(paste(Sys.time(), "New file started:", file, " Records: ", n_records))
  
  for (record in c(1:n_records)) {
    tryCatch({
      parsed_child <- xmlParse(xml_child(data,record)) ##SLOW! FIX!!!
      
      tryCatch({
        rec <- xmlToDataFrame(node=getNodeSet(parsed_child, "//Author"))
        rec$id <- xmlToDataFrame(node=getNodeSet(parsed_child, "//PMID"))[1,1]

        output <<- rbind.fill(output, rec)
      }, 
      error = function(error_condition) {
      })
    })
  }
  write.csv(output, paste(data_folder, processed_data, '_', n_records, file, sep=''))
  output <- output[0,]
}
