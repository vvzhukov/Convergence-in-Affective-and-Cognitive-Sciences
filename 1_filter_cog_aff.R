
setwd("E:/Research/Data_pubmed/processed_data6")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

colnames(myfiles)
myfiles$keywords <- NULL
myfiles$mesh_descriptor <- NULL
myfiles$mesh_descriptor_major <- NULL
myfiles$mesh_qualifier <- NULL
myfiles$mesh_qualifier_major <- NULL

# Filter affective
myfiles$mesh_major_UI <- paste(myfiles$mesh_major_descriptor_UI, myfiles$mesh_descriptor_for_major_qualifier)
myfiles$mesh_UI <- paste(myfiles$mesh_descriptor_UI, myfiles$mesh_qualifier_UI)

# Descriptiors:

# Affective
# emotional adjustment - D000066498
# optimism - D000067656
# pessimism - D000067657
# affective symptoms - D000342
# aggression - D000374
# depression - D003863
# stress, psychological - D013315
# emotions - D004644
# achievement - D000124
# aspirations, psychological - D001240
# anhedonia - D059445
# empathy - D004645
# emotional intelligence - D056348
# negativism - D009341
# expressed emotion - D019260
# trust - D035502
# pain perception - D058748
# pain threshold - D017288
# reward - D012201
# anxiety disorders - D001008
# bipolar and related disorders - D000068105
# mood disorders - D019964
# trauma and stressor related disorders - D000068099
# psychology, positive - D000080032
# anger management therapy - D000067449
# emotion-focused therapy - D000071441
# arousal - D001143

# Cognitive
# Memory - D008568
# Attention - D001288
# Perception - D010465
# Decision Making - D003657
# Problem Solving - D011340
# Learning - D007858
# Language - D007802
# Artificial Intelligence - D001185
# Cognition - D003071
# Behavior - D001519
# Brain - D001921 (terminated)



mesh_kyes_aff <- 'D000066498|D000067656|D000067657|D000342|D000374|D003863|D013315|D004644|D000124|D001240|D059445|D004645|D056348|D009341|D019260|D035502|D058748|D017288|D012201|D001008|D000068105|D019964|D000068099|D000080032|D000067449|D000071441|D001143'
mesh_kyes_cog <- 'D008568|D001288|D010465|D003657|D011340|D007858|D007802|D001185|D003071|D001519'

data_affective_major <- dplyr::filter(myfiles, grepl(mesh_kyes_aff, mesh_major_UI, ignore.case = TRUE))
data_cognitive_major <- dplyr::filter(myfiles, grepl(mesh_kyes_cog, mesh_major_UI, ignore.case = TRUE))

write.csv(data_affective_major, "E:/Research/Data_pubmed/affective_data23/affective_major_UI.csv")
write.csv(data_cognitive_major, "E:/Research/Data_pubmed/cognitive_data23/cognitive_major_UI.csv")

