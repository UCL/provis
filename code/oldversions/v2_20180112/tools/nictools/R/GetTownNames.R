#' Lookup coordinates and town names for each postcode
#' @param  m1data   
#' @return m1data
#' @examples 
#' m1data<-GetTownNames(m1data)
GetTownNames<-function(m1data) {
  postcode_left<-levels(m1data$postcode_left)
  
  lookupfile<-paste0(dirs$datadir,"/town_lookup.RData")
  if (file.exists(lookupfile)) {
    load(lookupfile)  
  } else {
    town_lookup<-data.frame(NULL)
    for (i1 in 1:length(postcode_left)) {
      if (postcode_left[i1]== "") {
        temp<-data.frame(id=1,postcode_left=postcode_left[i1],missing=1)  
      } else {
        for (i2 in 1:10) {
          temp<-geocode(paste0(as.character(postcode_left[i1])," UK"),
                        source="google",output="more")
          if (!is.na(temp[1])) {
            break  
          }
        }
        if (is.na(temp[1])) {
          temp<-data.frame(id=i1,postcode_left=postcode_left[i1],missing=1)  
        } else {
          temp<-data.frame(id=i1,postcode_left=postcode_left[i1],missing=0,temp)
        }
      }
      town_lookup<-rbind.fill(town_lookup,temp)
      print(c(i1,length(postcode_left),town_lookup$missing[i1]))
    }
    ilon<-grep("\\<lon",colnames(town_lookup))
    names(town_lookup)[ilon]<-"pc_longitude"
    ilat<-grep("\\<lat",colnames(town_lookup))
    names(town_lookup)[ilat]<-"pc_latitude"
    
    postal_town<-data.frame(levels(town_lookup$postal_town))
    town_lookup$town_lon<-NA
    town_lookup$town_lat<-NA
    for (i1 in levels(town_lookup$postal_town)) {
      for (i2 in 1:10) {
        temp<-geocode(paste0(i1," UK"),source="google") 
        if (!is.na(temp[1])) {
          break
        }
      }
      i3<-(as.character(town_lookup$postal_town)==i1 & !is.na(town_lookup$postal_town))
      town_lookup$town_lon[i3] <- temp$lon
      town_lookup$town_lat[i3] <- temp$lat
    }
  
    # Save town_lookup
    save(town_lookup,file=paste0(dirs$datadir,"/town_lookup.RData"))
  }
  
  # Merge with m1data merge based on postcode_left
  varlist<-c("postcode_left","postal_town","town_lon","town_lat")
  m1data$id<-c(1:nrow(m1data))
  m1data<-join(m1data,
           town_lookup[,varlist],
           by="postcode_left",type="left")
  itest<- (complete.cases(m1data[,c("longitude","latitude")]) &
           is.na(m1data$postal_town))
  itrain<- complete.cases(m1data[,c("longitude","latitude","postal_town")])
  b1<-knn(m1data[itrain,c("longitude","latitude")],
          m1data[itest,c("longitude","latitude")],m1data$postal_town[itrain],k=1)
  b1<-data.frame(b1)
  names(b1)<-"postal_town"
  b2<-join(b1,town_lookup[,varlist[2:4]],
           by="postal_town",type="left",match="first")
  m1data[itest,varlist[2:4]]<-b2
  return(m1data)
}  
