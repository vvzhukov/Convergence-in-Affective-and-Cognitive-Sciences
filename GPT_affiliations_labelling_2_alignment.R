
affective <- read.csv("E:/Research/Data_pubmed/model_data23/model_data_aff_CIP_v2.csv")
cognitive <- read.csv("E:/Research/Data_pubmed/model_data23/model_data_cog_CIP_v2.csv")

# same just for cognitive data:
affective <- cognitive


# Alignment adjustments (same # of dimensions)
#SA1 <> CIP2
#SA2 <> CIP1, 3, 7
#SA3 <> CIP0
#SA4 <> CIP4, 5, 6
#SA5 <> CIP8, 9

affective$CIPSA1 <- affective$CIP2
affective$CIPSA2 <- affective$CIP1 + affective$CIP3 + affective$CIP7
affective$CIPSA3 <- affective$CIP0
affective$CIPSA4 <- affective$CIP4 + affective$CIP5 + affective$CIP6
affective$CIPSA5 <- affective$CIP8 + affective$CIP9

norm.vec <- function(x) {x / sqrt(sum(x^2))}
stde <- function(x, na.rm = T) {sd(x, na.rm)/sqrt(length(x))}


affective$Af_CIP_SA <- 0

for (i in c(1:nrow(affective))){
  affective$Af_CIP_SA[i] <- norm.vec(as.numeric(affective[i,c("pSA1","pSA2","pSA3","pSA4","pSA5")])) %*% 
    norm.vec(as.numeric(affective[i,c("CIPSA1","CIPSA2","CIPSA3","CIPSA4","CIPSA5")]))  
}


library(ggplot2) 
library(dplyr)

affective %>% group_by(pYear) %>%
  summarise(mean_Af_CIP_SA=mean(Af_CIP_SA, na.rm=T),
            sd_Af_CIP_SA=stde(Af_CIP_SA)) -> affective_sum

            
            
ggplot(affective_sum, aes(x=pYear, y=mean_Af_CIP_SA)) + 
  geom_point() +  
  geom_errorbar(aes(ymin=mean_Af_CIP_SA-sd_Af_CIP_SA, ymax=mean_Af_CIP_SA+sd_Af_CIP_SA), width=.2) +
  xlim(c(1990,2022)) + 
  ylim(c(0.2,0.5)) +
  theme_bw() +
  xlab(label="") +
  ylab(label="")



# here you need to re-run top part of the script with proper switch at line 6
affective_sum_0 <- affective_sum
cognitive_sum_0 <- affective_sum



ggplot(affective_sum_0, aes(x=pYear, y=mean_Af_CIP_SA)) + 
  geom_point() +  
  geom_errorbar(aes(ymin=mean_Af_CIP_SA-sd_Af_CIP_SA, ymax=mean_Af_CIP_SA+sd_Af_CIP_SA), width=.2) +
  xlim(c(1990,2022)) + 
  ylim(c(0.2,0.45)) +
  theme_bw() +
  xlab(label="") +
  ylab(label="")


ggplot(cognitive_sum_0, aes(x=pYear, y=mean_Af_CIP_SA)) + 
  geom_point() +  
  geom_errorbar(aes(ymin=mean_Af_CIP_SA-sd_Af_CIP_SA, ymax=mean_Af_CIP_SA+sd_Af_CIP_SA), width=.2) +
  xlim(c(1990,2022)) + 
  ylim(c(0.2,0.45)) +
  theme_bw() +
  xlab(label="") +
  ylab(label="")