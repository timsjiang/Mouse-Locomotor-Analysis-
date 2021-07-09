
library(tidyverse)

####create lists of each genotype, then combine all into one list#### 
list.Snap25<-list(Snap25s, Snap25r1, Snap25r2) %>% set_names("Snap25r1", "Snap25r2","Snap25S")
list.Vglu2<-list(Vglu2s ,Vglu2r1, Vglu2r2, Vglu2r3, Vglu2r4) %>% set_names("Vglu2r1", "Vglu2r2", "Vglu2r3", "Vglu2r4","Vglu2S")
list.Vglu1<- list(Vglu1r1, Vglu1r2, Vglu1r3) %>% set_names( "Vglu1r1", "Vglu1r2", "Vglu1r3")
list.Vgat<-list(VgatS, Vgat0.3, Vgat1.0)%>% set_names("Vgat0.3", "Vgat1.0","VgatS")
list.Dbh<-list(DbhR1D1, DbhR1D2, DbhR2D1, DbhR2D2, DbhR3D1, DbhR3D2) %>% set_names("DbhR1D1", "DbhR1D2", "DbhR2D1", "DbhR2D2", "DbhR3D1", "DbhR3D2")
list.Control<-list(Control0.3, Control1.0,ControlSaline) %>% set_names("Control0.3", "Control1.0", "ControlS")
masterList<- list(list.Snap25, list.Vglu2, list.Vglu1, list.Vgat, list.Dbh, list.Control)%>% 
  set_names("list.Snap25", "list.Vglu2", "list.Vglu1", "list.Vgat", "list.Dbh", "list.Control") 

####Grabbing counts for all trials#### 
    
   for(i in seq_along(masterList)){
    for(j in seq_along(masterList[[i]])){
      masterList[[i]][[j]]<- masterList[[i]][[j]]%>% group_by(Subject) %>% summarize(Count=n())
    }
  }

#adding columns detailing what genotype 
for (i in seq_along(masterList)){
  for (j in seq_along(masterList[[i]])){

    names(masterList[[i]])[[j]]<-paste0(names(masterList[[i]])[[j]], "Obs")
    genoName<-substr(names(masterList)[[i]], 6, nchar(names(masterList)[[i]]))
    masterList[[i]][[j]]<-masterList[[i]][[j]]%>%mutate(Genotype=genoName)
    }
}

####add column indicating what treatment####
  #snap25
  masterList[[1]][[1]]$Dose<-c(1,1,1,1,1,0.3,0.3,0.3,0.3,0.3)
  masterList[[1]][[2]]$Dose<-c(0.3,0.3,0.3,0.3,0.3,1,1,1,1,1)
  masterList[[1]][[3]]<-mutate(masterList[[1]][[3]], Dose=0)
  
  
  #Vglu2
  masterList[[2]][[1]]$Dose<-c(rep(1,6), rep(0.3, 5))
  masterList[[2]][[2]]$Dose<-c(rep(0.3,6), rep(1,5))
  masterList[[2]][[3]]$Dose<-c(rep(0.1,6), rep(0.03, 5))
  masterList[[2]][[4]]$Dose<-c(rep(0.03,6), rep(0.1, 5))
  masterList[[2]][[5]]<-mutate(masterList[[2]][[5]], Dose=0)
  
  
  #Vglu1
  masterList[[3]][[1]]$Dose<- c(0, 0, 0.3, 0.3, 1, 0, 0, 0.3, 0.3, 1)
  masterList[[3]][[2]]$Dose<- c(0.3, 0.3, 1, 1, 0, 0.3, 0.3, 1, 1, 0)
  masterList[[3]][[3]]$Dose<- c(1,1,0,0,0.3,1,1,0,0,0.3)
  
  #Vgat
  masterList[[4]][[3]]<- mutate(masterList[[4]][[3]], Dose=0)
  masterList[[4]][[2]]<- mutate(masterList[[4]][[2]], Dose=0.3)
  masterList[[4]][[1]]<- mutate(masterList[[4]][[1]], Dose=1)
  
  #Dbh
  d1<-c(0,0,1,0,0,0.3,0.3,1)
  d2<-c(0,0,0.3,0.3,1,0,0,0.3,0.3,1)
  d3<-c(0.3,0.3,0, 0.3,0.3,1,1,0)
  d4<-c(0.3,0.3,1,1,0,0.3,0.3,1,1,0)
  d5<-c(1, 1, 0.3, 1, 1, 0, 0, 0.3)
  d6<-c(1,1,0,0,0.3,1,1,0,0,0.3)
  list.dose<-list(d1,d2,d3,d4,d5,d6)
  for(i in 1:6){
    masterList[[5]][[i]]$Dose<-list.dose[[i]]
  }
  
  #Control
  c1<-c(rep(0.3,5))
  c2<-c(rep(1.0,5))
  c3<-c(rep(0,5))
  list2.dose<-list(c1,c2,c3)
  for(i in 1:3){
    masterList[[6]][[i]]$Dose<-list2.dose[[i]]
  }
  
  
####normalize to Saline####
  #standardize genotypes that don't have one saline trial
    #Vglu1
      #combine df's
      Vglu1Obs<-rbind(masterList[[3]][[1]],masterList[[3]][[2]],masterList[[3]][[3]] )
      #split up into different treatments
      masterList[[3]][[3]]<- Vglu1Obs%>%filter(Dose==0)
        names(masterList[[3]])[[3]]<-"Vglu1SObs"
      masterList[[3]][[2]]<- Vglu1Obs%>%filter(Dose==0.3)
        names(masterList[[3]])[[2]]<-"Vglu10.3Obs"
      masterList[[3]][[1]]<-Vglu1Obs%>% filter(Dose==1.0)
        names(masterList[[3]])[[1]]<-"Vglu1.0Obs"
      
    #Dbh
      #combine df's
        masterList[[5]][[2]]$Subject<-masterList[[5]][[2]]$Subject+10
        masterList[[5]][[4]]$Subject<-masterList[[5]][[4]]$Subject+10
        masterList[[5]][[6]]$Subject<-masterList[[5]][[6]]$Subject+10
        
        bindList<-list()
        for(i in 1:6){
          bindList[[i]]<-masterList[[5]][[i]]
          DbhObs<-do.call(rbind, bindList)
        }
        
        for(i in 6:4){
          masterList[[5]][[i]]<-NULL
        }
      #Split up into different treatments
        masterList[[5]][[3]]<- filter(DbhObs, Dose==0)
          names(masterList[[5]])[[1]]<-"DbhSObs"
        masterList[[5]][[2]]<- filter(DbhObs, Dose==0.3)
          names(masterList[[5]])[[2]]<-"Dbh0.3Obs"
        masterList[[5]][[1]]<-filter(DbhObs, Dose==1)
          names(masterList[[5]])[[3]]<- "Dbh1.0Obs"
          
  #Standardizing Loop
  for(i in seq_along(masterList)){
    for(j in seq_along(masterList[[i]])){
    masterList[[i]][[j]]$Count<-(masterList[[i]][[j]]$Count)/(masterList[[i]][[length(masterList[[i]])]]$Count)

    }
  }

#combine all lists into one master list
masterList<-do.call(c, masterList)
  
#combine all dataframes in list into one master dataframe
masterDf<-do.call(rbind, masterList) 

#remove row names
rownames(masterDf)<-NULL

  
