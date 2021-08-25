library(ggsci)
#coerce dose to factor
masterDf[["Dose"]]<- as.character(masterDf[["Dose"]])

#make comparison dataframe
masterDfComp<- masterDf

#conditional dose changes
masterDfComp$Dose[masterDfComp$Dose==20]<- "High"
masterDfComp$Dose[masterDfComp$Dose==1]<- "High"
masterDfComp$Dose[masterDfComp$Dose==0]<- "Saline"
masterDfComp<- masterDfComp%>% filter(!(Dose==0.3))

#visualize 
ggplot(masterDfComp, aes(x=Dose, y= Count, color= Genotype)) + geom_boxplot()+
  scale_y_log10() + scale_color_npg() + xlab ("Dose (Dex or DCZ)") + ylab ("Counts")+
  scale_x_discrete(limits = c("Saline", "High"))
