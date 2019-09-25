A1NewUseCode <- function(u1) {
  # Clean up and aggregate levels of usecode
  m1<-levels(u1)
  for (i1 in 1:length(m1)) {
    i2<-sum(as.character(u1)==m1[i1])
    if (i2>0 & i2<50) {
      m1[i1]<-substr(m1[i1],1,3)  
    }
  }
  levels(u1)<-m1
  u1<-droplevels(u1)

  m1<-levels(u1)
  for (i1 in 1:length(m1)) {
    i2<-sum(as.character(u1)==m1[i1])
    if (i2>0 & i2<50) {
      m1[i1]<-substr(m1[i1],1,2)  
    } 
  }
  levels(u1)<-m1
  u1<-droplevels(u1)

  m1<-levels(u1)
  for (i1 in 1:length(m1)) {
    i2<-sum(as.character(u1)==m1[i1]) 
    if (i2>0 & i2<50) {
      m1[i1]<-"ZZ"  
    }
  }
  levels(u1)<-m1
  A1NewUseCode<-droplevels(u1)
  return(A1NewUseCode)
}