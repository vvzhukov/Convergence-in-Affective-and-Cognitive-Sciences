
## TESTING FIELDS
object.size(data)
object.size(output[c(1:6000),])

file = "pubmed22n1004.xml"
record = 500

library(rbenchmark)
benchmark("Get child" = {xml_child(data,record)},
          "Read data" = {read_xml(paste(data_folder, file, sep =''))},
          replications = 10,
          columns = c("test", "replications", "elapsed",
                      "relative", "user.self", "sys.self"))


system.time({xml_child(data,record)})
system.time({xmlParse(xml_child(data,record))})
system.time({xmlToDataFrame(node=getNodeSet(parsed_child, "//PMID"))[1]})
system.time({xmlToDataFrame(node=getNodeSet(parsed_child, "//PubDate"))[1]})
system.time({xmlToDataFrame(node=getNodeSet(parsed_child, "//PubDate"))[2]})
system.time({xmlToDataFrame(node=getNodeSet(parsed_child, '//*[@MajorTopicYN="N"]'))[,1]})
system.time({xmlToDataFrame(node=getNodeSet(parsed_child, '//*[@MajorTopicYN="Y"]'))[,1]})
system.time({xmlToDataFrame(node=getNodeSet(parsed_child, "//Reference"))[,2]})

obj.size(data)
