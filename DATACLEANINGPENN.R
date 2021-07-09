options(scipen=10)
rm(list=ls())

library(tidyverse)
#DATACLEANING FUNCTION
dataClean<-function(x){
  #load data
  x<- read.csv(paste0("/Users/timsjiang/Documents/RProjects/Dex Exposures/Dex Csv Data/", x,".csv"), header= TRUE, sep= ",")
  #Remove seconds from the Time section
  x$Time<- substr(x$Time,1, nchar(x$Time)-1)
  x$Time<- as.numeric(x$Time)
  
  #Change cage number to numeric
  x$Subject<- substr(x$Subject, 6, nchar(x$Subject))
  x$Subject<- as.numeric(x$Subject)
  
  #filter out when times=0
  x<-x%>%filter(Time>0)
  
  
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

#load data via loop
file_paths<- fs::dir_ls("Dex Csv Data") 
allData<-list()
for(i in seq_along(file_paths)){
  allData[[i]]<-read_csv(
    file=file_paths[[i]]
  )
}
allData<-set_names(allData, substr(file_paths, 14, nchar(file_paths)-4))

#assign output of function to the original file name 
for(i in 1:length(allData)){
  assign(names(allData)[[i]], dataClean(names(allData)[[i]]))
}

#special circumstance cleaning

#Dead Vgats
VgatS<-filter(VgatS, Subject>3)
Vgat0.3<- filter(Vgat0.3, Subject>2)
Vgat1.0<-filter(Vgat1.0, Subject>3)

#Dead Dbh's
DbhR1D1<-filter(DbhR2D1, Subject!=3, Subject!=4)
DbhR2D1<-filter(DbhR2D1, Subject!=3, Subject!=4)

#Dead Controls
Control0.3<-filter(Control0.3, Subject<6)
