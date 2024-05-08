model_data <- read.csv("model_data2_13_2024.csv")

# model from Report211.pptx, page #3, right
model1 <- lm(citation_count_n ~ 
                pType + 
                log(pAuthors_n) + 
                pDiversity +
                cDiversity +
                pSA1b + pSA2b + pSA3b + pSA4b + pSA5b, 
              data = model_data)

summary(model1)


# model from Report211.pptx, page #9 with cDiversity added
model_data <- subset(model_data, pType != "Intersect")

model2 <- lm(citation_count_n ~ 
               pType + 
               log(pAuthors_n) + 
               pDiversity +
               cDiversity,
             data = model_data)
summary(model2)