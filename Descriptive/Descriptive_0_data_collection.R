library(xml2)
library(XML)

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
processed_data = 'processed_data/'
files_to_process = range_high - range_low 

output <- data.frame("id"=integer(),"year"=integer(),
                     "month"=integer(),"refs"=character(),
                     "major_mesh"=character(),"mesh"=character(),
                     "keywords"=character())


for (file in list.files(path = data_folder, pattern = ".xml")[range_low:range_high]){
  data = read_xml(paste(data_folder, file, sep =''))
  n_records <- xml_length(data)
  print(paste(Sys.time(), "Worker ", worker, " started. Range to process: ", range_low, " to ", range_high))
  print(paste(Sys.time(), "New file started:", file, " Records: ", n_records))
  
  for (record in c(1:n_records)) {
    tryCatch({
      parsed_child <- xmlParse(xml_child(data,record)) ##SLOW! FIX!!!
      
      rec <- data.frame("id" = xmlToDataFrame(node=getNodeSet(parsed_child, "//PMID"))[1],
                        "year" = xmlToDataFrame(node=getNodeSet(parsed_child, "//PubDate"))[1],
                        "month" = xmlToDataFrame(node=getNodeSet(parsed_child, "//PubDate"))[2],
                        "refs" = NA,
                        "major_mesh" = NA,
                        "mesh" = NA,
                        "keywords" = NA)
      
      colnames(rec) <- c("id","year", "month", "refs", "major_mesh", "mesh", "keywords")
      
      tryCatch({
        rec$keywords <- paste(xmlToDataFrame(node=getNodeSet(parsed_child, "//KeywordList")), collapse="; ")
      }, 
      error = function(error_condition) {
      })
      tryCatch({
        rec$mesh <- paste(xmlToDataFrame(node=getNodeSet(parsed_child, '//*[@MajorTopicYN="N"]'))[,1], collapse="; ")
      }, 
      error = function(error_condition) {
      })
      tryCatch({
        rec$major_mesh <- paste(xmlToDataFrame(node=getNodeSet(parsed_child, '//*[@MajorTopicYN="Y"]'))[,1], collapse="; ")
      }, 
      error = function(error_condition) {
      })
      tryCatch({
        rec$refs <- paste(xmlToDataFrame(node=getNodeSet(parsed_child, "//Reference"))[,2], collapse=" ")
      }, 
      error = function(error_condition) {
      })
      output <<- rbind(output, rec)
      }, 
      error = function(error_condition) {
      })
  }
  write.csv(output, paste(data_folder, processed_data, "worker_", worker, "_", file, sep=''))
  output <- output[0,]
}
