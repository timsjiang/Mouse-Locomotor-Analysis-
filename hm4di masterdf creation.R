
library(tidyverse)

####create lists of each genotype, then combine all into one list#### 

list.CNO<- list(CNOr1, CNOr2)%>% set_names("CNOr1", "CNOr2")
masterListh<- list(list.CNO)
####Grabbing counts for all trials#### 

for(i in seq_along(masterListh)){
  for(j in seq_along(masterListh[[i]])){
    masterListh[[i]][[j]]<- masterListh[[i]][[j]]%>% group_by(Subject) %>% summarize(Count=n())
  }
}

####add column indicating what treatment####
#hm4di
c1<- c(20,20,20,20,20,20,20,0,20,20,0,0,0,0,0,0,0)
c2<- c(0,0,0,0,0,0,0,20,0,0,20,20,20,20,20,20,20)
list3.dose<-list(c1,c2)
for(i in 1:2){
  masterListh[[1]][[i]]$Dose<-list3.dose[[i]]
}
####normalize to Saline####
#standardize genotypes that don't have one saline trial
#hm4di
CNOobs<- rbind(masterListh[[1]][[1]], masterListh[[1]][[2]])
masterListh[[1]][[1]]<- filter(CNOobs, Dose==20)
masterListh[[1]][[2]]<- filter(CNOobs, Dose==0)


#Standardizing Loop
#for(i in seq_along(masterListh)){
  #for(j in seq_along(masterListh[[i]])){
    #masterListh[[i]][[j]]$Count<-(masterListh[[i]][[j]]$Count)/(masterListh[[i]][[length(masterListh[[i]])]]$Count)
    
 # }
#}
#combine all lists into one master list
masterListh<-do.call(c, masterListh)

#combine all dataframes in list into one master dataframe
masterDFh<-do.call(rbind, masterListh) 
#remove row names
rownames(masterDFh)<-NULL

#coerce dose and variable to a factor
masterDFh[["Dose"]]<-as.factor(masterDFh[["Dose"]])


#add genotype
masterDFh<- masterDFh%>% mutate(Genotype= "hM4Di")
masterDFh[["Genotype"]]<- as.factor(masterDFh[["Genotype"]])

#Combine all into masterDf
masterDf<- rbind(masterDf, masterDFh)
          



#export to folder
#write.csv(masterDFh, "dexBeamBreak.csv")

#remove constituent lists now that they are all in masterListh
rm(list=setdiff(ls(), "masterDf"))



