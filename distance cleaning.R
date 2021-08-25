#requires non-count data ( so coordinates are still preserved)
CNOr1$Coordinate<-substr(CNOr1$Coordinate, 2, nchar(CNOr1$Coordinate)-3)
CNOr1$Coordinate<- as.numeric(CNOr1$Coordinate)
Sum<- 0
for (i in 1:length(CNOr1$Coordinate)){
  if((CNOr1$Coordinate[[i]]==CNOr1$Coordinate[[i+2]]) & (CNOr1$Coordinate[[i+1]]==CNOr1$Coordinate[[i+3]])){
    i=i+4
  }
   else{
     if(i<length(CNOr1$Coordinate)){
      Sum<- Sum + abs(CNOr1$Coordinate[[i]]- CNOr1$Coordinate[[i+1]])
    }
  }
}

for (i in 1:length(CNOr1$Coordinate)){

    if(i<length(CNOr1$Coordinate)){
      Sum<- Sum + abs(CNOr1$Coordinate[[i]]- CNOr1$Coordinate[[i+1]])
  }
}
