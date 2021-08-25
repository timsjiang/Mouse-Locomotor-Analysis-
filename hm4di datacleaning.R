options(scipen=100)


library(tidyverse)
dataClean<-function(x){
  #Remove seconds from the Time section
  x<- read.csv(paste0("/Users/timsjiang/Documents/RProjects/Dex Exposures/hm4Di/", x,".csv"), header= TRUE, sep= ",")
  x$Time<- substr(x$Time,1, nchar(x$Time)-1)
  x$Time<- as.numeric(x$Time)
  
  #Change cage number to numeric
  x$Subject<- substr(x$Subject, 6, nchar(x$Subject))
  x$Subject<- as.numeric(x$Subject)
  
  #filter out when times=0
  x<-x%>%filter(Time>0)
  xfilter<- x %>% filter(Subject%in%c(1,2,3) & Sample%in%c(8,9,10,11,12,13))
  
  x<- x%>% filter(!(Subject%in%c(1,2,3) & Sample%in%c(8,9,10,11,12,13,14,15)))
  xfilter$Subject<- xfilter$Subject+14
  x<- rbind(x, xfilter)
  
  #generate lists of the first times and first samples per subject
  filterDf<-list()
  firstTime<-list()
  firstSample<-list()
  for(i in 1:max(x$Subject)){
    filterDf[[i]]<-filter(x,Subject==i) 
    firstTime[[i]]<-filterDf[[i]][1,6]
    firstSample[[i]]<-filterDf[[i]][1,5]
    
    filterDf[[i]]$Time<- filterDf[[i]]$Time+(filterDf[[i]]$Sample-firstSample[[i]])*600
    filterDf[[i]]$Time<- filterDf[[i]]$Time-firstTime[[i]]
  }
  
  finalDf<-do.call(rbind, filterDf)
  finalDf<-mutate(finalDf, time_minutes=Time/60)
  finalDf<-select(finalDf, Experiment, Subject, Time, time_minutes, Coordinate)
  finalDf<-filter(finalDf, time_minutes<=60)
  return(finalDf)
}

CNOr2<-dataClean("CNOr2")
CNOr1<- dataClean("CNOr1")
CNOr1<- CNOr1%>% filter(!(Subject==18))


