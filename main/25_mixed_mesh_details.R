library(dplyr)
mixed_data_sa <- read.csv("E:/Research/Data_pubmed/model_data23/mixed_data_sa_5_17_2024.csv")

tbl <- mixed_data_sa[,c(2,43:58)]

tbl_str <- data.frame(ID=tbl$pmid, flag=(apply(tbl[,-1], 1, function(x) paste0(names(x)[which(x==1)], collapse=','))))

tbl_str %>%
  distinct() -> test

tbl_str %>%
  distinct() %>%
  group_by(flag) %>%
  tally() %>%
  arrange(desc(n)) -> tbl_str_grpd 


mesh1 <- read.csv("E:/Research/Data_pubmed/affective_data23/affective_cit_bal_full_data.csv")
mesh2 <- read.csv("E:/Research/Data_pubmed/cognitive_data23/cognitive_cit_bal_full_data.csv")
mesh <- rbind(mesh1, mesh2)
remove(mesh1, mesh2)

mesh <- subset(mesh, pmid %in% mixed_data_sa$pmid)
mesh <- mesh[,c('pmid','mesh_major_UI')]

mesh$mesh_major_UI <- gsub("\\[", "", mesh$mesh_major_UI)
mesh$mesh_major_UI <- gsub("\\]", "", mesh$mesh_major_UI)
mesh$mesh_major_UI <- gsub(" ", "", mesh$mesh_major_UI)
mesh$mesh_major_UI <- gsub("'", "", mesh$mesh_major_UI)
mesh$mesh_major_UI <- gsub(",", "", mesh$mesh_major_UI)
mesh$mesh_major_UI <- gsub("D", ",D", mesh$mesh_major_UI)
mesh$mesh_major_UI <- substring(mesh$mesh_major_UI,2)


mesh$mesh_major_UI <- unname(sapply(mesh$mesh_major_UI, function(x) {
                              paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse=',')} ))

mesh_d <- distinct(mesh)

mesh_kyes_aff <- c("D000066498","D000067656","D000067657","D000342","D000374","D003863","D013315","D004644","D000124","D001240","D059445","D004645","D056348","D009341","D019260","D035502","D058748","D017288","D012201","D001008","D000068105","D019964","D000068099","D000080032","D000067449","D000071441","D001143")
mesh_kyes_cog <- c("D008568","D001288","D010465","D003657","D011340","D007858","D007802","D001185","D003071","D001519")

out <- data.frame('aff'=c(),
                  'cog'=c())

for (i in 1:nrow(mesh_d)){
  tmp_aff <- c()
  tmp_cog <- c()
  for (j in strsplit(mesh_d$mesh_major[i], split=',')[[1]]){
    if (j %in% mesh_kyes_aff){
      tmp_aff <- c(tmp_aff, j)
    } else if (j %in% mesh_kyes_cog){
      tmp_cog <- c(tmp_cog, j)
    }
    out <<- rbind(out,expand.grid('aff'=tmp_aff, 'cog'=tmp_cog))
  }
}


out$pair <- paste(out$aff, ", ", out$cog)

out %>%
  group_by(pair) %>%
  tally() %>%
  arrange(desc(n)) -> mesh_pairs_grpd 


mesh %>%
  distinct() %>%
  group_by(mesh_major_UI) %>%
  tally() %>%
  arrange(desc(n)) -> mesh_grpd 

sum(grepl("D001143", mesh_d$mesh_major_UI, fixed=TRUE))



write.csv(head(mesh_grpd, 100), "C:/Users/Rubinzone/Desktop/mixed_majorMesh_top100.csv")

write.csv(head(mesh_pairs_grpd, 100), "C:/Users/Rubinzone/Desktop/mixed_majorMesh_pairs_top100.csv")