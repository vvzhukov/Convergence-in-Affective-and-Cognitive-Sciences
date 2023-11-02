

cit_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/citations_L1_SA_publ.csv")
ref_links_summ_publ <- read.csv("E:/Research/Data_pubmed/affective_data/balanced_including_na/descriptive/references_L1_SA_publ.csv")

cog_cit <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_cit_full_plot_ext.csv")
cog_ref <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_ref_full_plot.csv")


data_cognitive_major_nb <- read.csv("E:/Research/Data_pubmed/cognitive_data/cognitive_major_UI_nobrain.csv")
cog_cit_nb <- subset(cog_cit, pmid %in% data_cognitive_major_nb$pmid)
cog_ref_nb <- subset(cog_ref, pmid %in% data_cognitive_major_nb$pmid)


cog_cit %>% group_by(yearP) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         total = n()) -> cog_cit

cog_ref %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         total = n()) -> cog_ref

cog_cit_nb %>% group_by(yearP) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                          SA2 = sum(SA2, na.rm = T),
                                          SA3 = sum(SA3, na.rm = T),
                                          SA4 = sum(SA4, na.rm = T),
                                          SA5 = sum(SA5, na.rm = T),
                                          total = n()) -> cog_cit_nb

cog_ref_nb %>% group_by(year) %>% summarise(SA1 = sum(SA1, na.rm = T),
                                         SA2 = sum(SA2, na.rm = T),
                                         SA3 = sum(SA3, na.rm = T),
                                         SA4 = sum(SA4, na.rm = T),
                                         SA5 = sum(SA5, na.rm = T),
                                         total = n()) -> cog_ref_nb

colnames(cit_links_summ_publ)[c(37,38,39,40,41)] <- c("SA2", "SA1", "SA4", "SA5", "SA3")
colnames(ref_links_summ_publ)[c(37,38,39,40,41)] <- c("SA2", "SA1", "SA4", "SA5", "SA3")

cit_links_summ_publ$X <- NULL
ref_links_summ_publ$X <- NULL
cit_links_summ_publ[,c(1,36,37,38,39,40)] %>% gather("SA", "Hits", -year_published) -> cit_stacked_data_publ
ref_links_summ_publ[,c(1,36,37,38,39,40)] %>% gather("SA", "Hits", -year_published) -> ref_stacked_data_publ


colnames(cog_cit)[c(2,3,4,5,6)] <- c("SA2", "SA1", "SA4", "SA5", "SA3")
colnames(cog_ref)[c(2,3,4,5,6)] <- c("SA2", "SA1", "SA4", "SA5", "SA3")
cog_cit$X <- NULL
cog_ref$X <- NULL
cog_cit[,c(1,2,3,4,5,6)] %>% gather("SA", "Hits", -yearP) -> cog_cit_stacked
cog_ref[,c(1,2,3,4,5,6)] %>% gather("SA", "Hits", -year) -> cog_ref_stacked

colnames(cog_cit_nb)[c(2,3,4,5,6)] <- c("SA2", "SA1", "SA4", "SA5", "SA3")
colnames(cog_ref_nb)[c(2,3,4,5,6)] <- c("SA2", "SA1", "SA4", "SA5", "SA3")
cog_cit_nb$X <- NULL
cog_ref_nb$X <- NULL
cog_cit_nb[,c(1,2,3,4,5,6)] %>% gather("SA", "Hits", -yearP) -> cog_cit_stacked_nb
cog_ref_nb[,c(1,2,3,4,5,6)] %>% gather("SA", "Hits", -year) -> cog_ref_stacked_nb

remove(cit_links_summ_publ, ref_links_summ_publ, cog_cit, cog_ref, cog_cit_nb, cog_ref_nb)



# Stacked + percent
acit_stacked_plot_publ <- ggplot(cit_stacked_data_publ, aes(fill=SA, y=Hits, x=year_published)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", values = c("SA1" = "red", 
                                          "SA2" = "orange", 
                                          "SA3" = "yellow2", 
                                          "SA4" = "green", 
                                          "SA5" = "blue"),
                    labels = c("Psychological sciences",
                               "Biological sciences", 
                               "Humanities", 
                               "Medical sciences", 
                               "Technical methods")) + 
  xlim(1967, 2019) +
  labs(y="Citations") + 
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.2),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none")

ccit_stacked_plot_publ <- ggplot(cog_cit_stacked, aes(fill=SA, y=Hits, x=yearP)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", values = c("SA1" = "red", 
                                          "SA2" = "orange", 
                                          "SA3" = "yellow2", 
                                          "SA4" = "green", 
                                          "SA5" = "blue"),
                    labels = c("Psychological sciences",
                               "Biological sciences", 
                               "Humanities", 
                               "Medical sciences", 
                               "Technical methods")) + 
  xlim(1967, 2019) +
  labs(y="Citations") + 
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.2),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none")

ccit_stacked_plot_publ_nb <- ggplot(cog_cit_stacked_nb, aes(fill=SA, y=Hits, x=yearP)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", values = c("SA1" = "red", 
                                          "SA2" = "orange", 
                                          "SA3" = "yellow2", 
                                          "SA4" = "green", 
                                          "SA5" = "blue"),
                    labels = c("Psychological sciences",
                               "Biological sciences", 
                               "Humanities", 
                               "Medical sciences", 
                               "Technical methods")) + 
  xlim(1967, 2019) +
  labs(y="Citations") + 
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.2),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "none")

aref_stacked_plot_publ <- ggplot(ref_stacked_data_publ, aes(fill=SA, y=Hits, x=year_published)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", 
                    values = c("SA1" = "red", 
                               "SA2" = "orange", 
                               "SA3" = "yellow2", 
                               "SA4" = "green", 
                               "SA5" = "blue"),
                    labels = c("Psychological sciences",
                               "Biological sciences", 
                               "Humanities", 
                               "Medical sciences", 
                               "Technical methods")) + 
  xlim(1967, 2019) +
  labs(y="References") +
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.2),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(2), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "bottom") +
  ggtitle("Affective")


cref_stacked_plot_publ <- ggplot(cog_ref_stacked, aes(fill=SA, y=Hits, x=year)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", 
                    values = c("SA1" = "red", 
                               "SA2" = "orange", 
                               "SA3" = "yellow2", 
                               "SA4" = "green", 
                               "SA5" = "blue"),
                    labels = c("Psychological sciences",
                               "Biological sciences", 
                               "Humanities", 
                               "Medical sciences", 
                               "Technical methods")) + 
  xlim(1967, 2019) +
  labs(y="References") +
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.2),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(2), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "bottom") +
  ggtitle("Cognitive")


cref_stacked_plot_publ_nb <- ggplot(cog_ref_stacked_nb, aes(fill=SA, y=Hits, x=year)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(name = "", 
                    values = c("SA1" = "red", 
                               "SA2" = "orange", 
                               "SA3" = "yellow2", 
                               "SA4" = "green", 
                               "SA5" = "blue"),
                    labels = c("Psychological sciences",
                               "Biological sciences", 
                               "Humanities", 
                               "Medical sciences", 
                               "Technical methods")) + 
  xlim(1967, 2019) +
  labs(y="References") +
  theme(axis.text.x=element_text(size=rel(1.2),family="NimbusMon"),
        axis.text.y=element_text(size=rel(1.2),family="NimbusMon"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = rel(1.5), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.title = element_text(size = rel(2), hjust = 0.5, vjust = 2.5,family="NimbusMon"),
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
        legend.position = "bottom") +
  ggtitle("Cognitive, no brain key")

png('C:/Users/Rubinzone/Desktop/F8.png', width = 12, height = 8, units = 'in', res = 300)

ggarrange(aref_stacked_plot_publ + rremove("x.text"),
          cref_stacked_plot_publ + rremove("x.text") + rremove("y.text") + rremove("y.title"), 
          cref_stacked_plot_publ_nb + rremove("x.text") + rremove("y.text") + rremove("y.title"), 
          acit_stacked_plot_publ,
          ccit_stacked_plot_publ + rremove("y.text") + rremove("y.title"),
          ccit_stacked_plot_publ_nb + rremove("y.text") + rremove("y.title"),
          ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom",
          heights = c(1.1,1,1,1),
          widths = c(1.1,1,1,1))

dev.off()