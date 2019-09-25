attach(m2data)
modelflag<-5
if (modelflag==0) {
  m2<-m2ols0
} else if (modelflag==5) {
  m2<-m2ols5
}

# Plot price vs motorway
i1<-grep("motorway",names(m2$coefficients))
b1<-m2$coefficients[i1]
p_motorway<- b1[1] * (distance_motorway>=10) +
  (distance_motorway<10) *
  (b1[2]*log(distance_motorway)+b1[3]*log(distance_motorway)^2)
plot(distance_motorway,p_motorway,pch=".")

# Plot price vs aroad
  i1<-grep("distance_aroad",names(m2$coefficients))
  b1<-m2$coefficients[i1]
  p_aroad<- b1[1] * (distance_aroad>=10) +
            (distance_aroad<10) *
            (b1[2]*log(distance_aroad) + b1[3]*log(distance_aroad)^2)
  plot(distance_aroad,p_aroad,pch=".")


# Plot price vs AONB
AONBFlag<-0
if (AONBFlag==1) {
  i1<-grep("AONB",names(m2ols5$coefficients))
  b1<-m2ols5$coefficients[i1]
  p_AONB<- b1[1] * AONB + (b1[2] * log(1+drive_AONB) + b1[3]*log(1+drive_AONB)^2 ) * (drive_AONB<30)
  plot(drive_AONB,p_AONB,pch=".")
} else if (AONBFlag==2) {
  i1<-grep("AONB",names(m2ols0$coefficients))
  b1<-m2ols0$coefficients[i1]
  p_AONB<- b1[1]*m2data$AONB + 
          (drive_AONB<30) *
          (b1[2]*log(1+m2data$drive_AONB) + b1[3]*log(1+m2data$drive_AONB)^2)
  plot(m2data$drive_AONB,p_AONB,pch=".")
}

# Plot price vs natpark
natparkflag<-0
if (natparkflag==1) {
  i1<-grep("natpark",names(m2ols5$coefficients))
  b1<-m2ols5$coefficients[i1]
  p_natpark<- (b1[1] * log(1+drive_natpark) + b1[2]*log(1+drive_natpark)^2 ) * (as.character(natpark_ID)=="DARTMOOR")
  plot(drive_natpark,p_natpark,pch=".")
} else if (natparkflag==2) {
  i1<-grep("natpark",names(m2$coefficients))
  b1<-m2$coefficients[i1]
  p_natpark<- b1[1]*(natpark) + 
              (drive_natpark<30) *
              (b1[2]*log(1+drive_natpark) + b1[3]*log(1+drive_natpark)^2)
  plot(drive_natpark,p_natpark,pch=".")  
}

# Plot price vs drive_station
i1<-grep("drive_station",names(m2$coefficients))
b1<-m2$coefficients[i1]
p_station<- b1[1]*(drive_station>=30) +
           (drive_station<30) *
           (b1[2] * log(drive_station) + b1[3]*log(drive_station)^2 ) 
plot(drive_station,p_station,pch=".")

# drive town
# Plot price vs drive_town
i1<-grep("drive_town",names(m2$coefficients))
b1<-m2$coefficients[i1]
p_town<- b1[1]*(drive_town>=30)+
        (drive_town<30) *
          (b1[2] * log(drive_town) + b1[3]*log(drive_town)^2 )
plot(drive_town,p_town,pch=".")
summary(m2)
