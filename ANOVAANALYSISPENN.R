#install.packages("ggpubr")
#install.packages("emmeans")
library(tidyverse)
library(ggpubr)
library(emmeans)
library(ggsci)
#visualize data
ggplot(masterDf, aes(x=Dose, y= Count, color= Genotype)) + geom_boxplot()+
  scale_y_log10() + scale_color_npg() + xlab ("Dose Dexmedetomidine (mg/kg)") + ylab ("Normalized Counts")
  s
#summary statistics
masterDfSum<- masterDf %>% group_by(Dose, Genotype)%>% summarize(n=n(), mean= mean(Count), sd=sd(Count))
####TWO WAVE ANOVA with interaction####
    
  masterDf<-masterDf%>% as.data.frame()
  model1 <- lm(Count ~ Genotype*Dose, data = masterDfSans)
  #standard 2 way anova
  anova(model1)
  #variety comparison (pairwise differences between each combination)
  emmeans(model1, pairwise~Genotype)
  emmeans(model1, pairwise~Dose)
  
  #store master comparisons as dataframe
  masterPairwise<-as.data.frame((emmeans(model1, pairwise~Dose*Genotype))[[2]])
  masterPairwise<- separate(masterPairwise, contrast, into=c("Contrast1", "Contrast2"), sep=" - ")
  masterPairwise<- masterPairwise[c(1,2,3,6,9,12,15,18,20,23,26,29,32,36,39,42,45,48,49,50,51,54,57,60,63,65,68,71,74,78,81,84,87,88,89,90,93,96,101,104, 107,111,114,117,118,119,120, 123,126,128,131,135,138,139,140,141,144,146,150,151,152,153), ]



  