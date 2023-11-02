library(wordcloud)


#lines 1-79 from F10.r

ffreq_df1 <- freq_df1[!freq_df1$word %in% c(freq_df2$word, freq_df3$word, freq_df4$word, freq_df5$word),]
ffreq_df2 <- freq_df2[!freq_df2$word %in% c(freq_df1$word, freq_df3$word, freq_df4$word, freq_df5$word),]
ffreq_df3 <- freq_df3[!freq_df3$word %in% c(freq_df2$word, freq_df1$word, freq_df4$word, freq_df5$word),]
ffreq_df4 <- freq_df4[!freq_df4$word %in% c(freq_df2$word, freq_df3$word, freq_df1$word, freq_df5$word),]
ffreq_df5 <- freq_df5[!freq_df5$word %in% c(freq_df2$word, freq_df3$word, freq_df4$word, freq_df1$word),]


png('C:/Users/Rubinzone/Desktop/F11_1.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df1$word,40) , head(freq_df1$n,40), 
          scale = c(7,1),
          color = "orange",
          rot.per=0 )
dev.off()

png('C:/Users/Rubinzone/Desktop/F11_2.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df2$word,40) , head(freq_df2$n,40), 
                 scale = c(8,1),
                 color = "red",
                 rot.per=0 )
dev.off()

png('C:/Users/Rubinzone/Desktop/F11_3.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df3$word,40) , head(freq_df3$n,40), 
                 scale = c(8,1),
                 color = "darkgreen",
                 rot.per=0 )
dev.off()

png('C:/Users/Rubinzone/Desktop/F11_4.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df4$word,40) , head(freq_df4$n,40), 
                 scale = c(8,1),
                 color = "blue",
                 rot.per=0 )
dev.off()

png('C:/Users/Rubinzone/Desktop/F11_5.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df5$word,40) , head(freq_df5$n,40), 
                 scale = c(8,1),
                 color = "yellow3",
                 rot.per=0 )
dev.off()



ffreq_df1a <- ffreq_df1
ffreq_df2a <- ffreq_df2
ffreq_df3a <- ffreq_df3
ffreq_df4a <- ffreq_df4
ffreq_df5a <- ffreq_df5

ffreq_df1c <- ffreq_df1
ffreq_df2c <- ffreq_df2
ffreq_df3c <- ffreq_df3
ffreq_df4c <- ffreq_df4
ffreq_df5c <- ffreq_df5



#1 Affective - Cognitive
ffreq_df1 <- ffreq_df1a[!ffreq_df1a$word %in% ffreq_df1c$word,]
ffreq_df2 <- ffreq_df2a[!ffreq_df2a$word %in% ffreq_df2c$word,]
ffreq_df3 <- ffreq_df3a[!ffreq_df3a$word %in% ffreq_df3c$word,]
ffreq_df4 <- ffreq_df4a[!ffreq_df4a$word %in% ffreq_df4c$word,]
ffreq_df5 <- ffreq_df5a[!ffreq_df5a$word %in% ffreq_df5c$word,]


#1 Cognitive - Affective
ffreq_df1 <- ffreq_df1c[!ffreq_df1c$word %in% ffreq_df1a$word,]
ffreq_df2 <- ffreq_df2c[!ffreq_df2c$word %in% ffreq_df2a$word,]
ffreq_df3 <- ffreq_df3c[!ffreq_df3c$word %in% ffreq_df3a$word,]
ffreq_df4 <- ffreq_df4c[!ffreq_df4c$word %in% ffreq_df4a$word,]
ffreq_df5 <- ffreq_df5c[!ffreq_df5c$word %in% ffreq_df5a$word,]


png('C:/Users/Rubinzone/Desktop/F11_1.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df1$word,40) , head(freq_df1$n,40), 
          scale = c(7,1),
          color = "orange",
          rot.per=0 )
dev.off()

png('C:/Users/Rubinzone/Desktop/F11_2.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df2$word,40) , head(freq_df2$n,40), 
          scale = c(8,1),
          color = "red",
          rot.per=0 )
dev.off()

png('C:/Users/Rubinzone/Desktop/F11_3.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df3$word,40) , head(freq_df3$n,40), 
          scale = c(8,1),
          color = "darkgreen",
          rot.per=0 )
dev.off()

png('C:/Users/Rubinzone/Desktop/F11_4.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df4$word,40) , head(freq_df4$n,40), 
          scale = c(8,1),
          color = "blue",
          rot.per=0 )
dev.off()

png('C:/Users/Rubinzone/Desktop/F11_5.png', width = 10, height = 10, units = 'in', res = 300)
wordcloud(head(ffreq_df5$word,40) , head(freq_df5$n,40), 
          scale = c(8,1),
          color = "yellow3",
          rot.per=0 )
dev.off()