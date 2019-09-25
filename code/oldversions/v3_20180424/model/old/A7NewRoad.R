#' A7NewRoad(region_id,routefile,speed,dirs)
#' Predict travel times with new road and create dataframe with results
#'
#' @param region_id       region id number
#' @param routefile       filename containing route junction coordinates
#' @param speed           average speed on new route, miles per hour
#' @param dirs            list of directories
#' @return outdata        list containing (m1data_new,m2data_new)
#' @keywords new settlement
#' @export
#' @examples
#' newdata <- A7NewRoad(region_id,routefile,speed,dirs)

A7NewRoad<-function(region_id,routefile,speed,dirs) {

  if (missing(routefile) | is.null(routefile)) {
    routefile<-"road1.kml"
  }
  if (missing(speed)) { 
    speed<-80
  } else {
    speed<-1.609*speed  # convert to kilometers per hour
  }
 filetype<-file_ext(routefile)
  
  # Load m2 data
  m2data<-read.csv(paste0(dirs$datadir,"/m2data1.csv"))

  #4) load kml file in r
  if (filetype=="kml") {
    roadfile<-paste0(dirs$mapdir,"/",routefile)
    layer<-ogrListLayers(roadfile)
    road<-readOGR(roadfile,layer=layer)
  } else if (filetype=="csv") {
    print("csv file type in ComputeNewTravelTime. Not yet implemented.")
  }
  
  # Load destinations
  DestinationFile<-paste0(dirs$datadir,"/destinations",regino_id,".csv")
  dest<-read.csv(DestinationFile)  

  # Load travel time model
  load(paste0(dirs$traveldir,"/ttmodel.RData"))

  # Predict travel times:
  # ttime1:  travel time from each property to each road junction
  # ttime2: travel time from each road junction to each destination
  ttime1<-matrix(NA,nrow=nrow(m2data),ncol=nrow(road))
  ttime2<-matrix(NA,nrow=nrow(dest),ncol=nrow(road))
  m2temp<-data.frame(m2data[,c("longitude","latitude")],matrix(NA,nrow=nrow(m2data),ncol=2))
  tempdest<-data.frame(dest[,c("longitude","latitude")],matrix(NA,nrow=nrow(dest),ncol=2))
  names(m2temp)   <- c("res_x","res_y","work_x","work_y")
  names(tempdest) <- c("res_x","res_y","work_x","work_y")

  # Predict travel time: separately for each road junction
  for (i1 in (1:nrow(road))) {
    m2temp[,3:4]<-t(matrix(rep(road@coords[i1,1:2],nrow(m2temp)),2,nrow(m2temp)))
    tempdest[,3:4]<-t(matrix(rep(road@coords[i1,1:2],nrow(tempdest)),2,nrow(tempdest)))
    ttime1[,i1]<-predict(spline_drive_l1_s4_add,newdata=m2temp)
    ttime2[,i1]<-predict(spline_drive_l1_s4_add,newdata=tempdest)
    print(paste0("Travel time prediction for junction ",as.character(i1)," of ",as.character(nrow(road))," complete."))
  }

  # Compute minimum travel to/from road
  mintime_toroad<-apply(ttime1,1,min)
  mintime_todest<-apply(ttime2,1,min)

  # Find junction that corresponds to minimum travel time
  junction_toroad<-apply(ttime1,1,which.min)
  junction_todest<-apply(ttime2,1,which.min)

  # Compute distance travelled along road for each destimation
  start  <- as.data.frame(road[junction_toroad,])
  finish <- as.data.frame(road[junction_todest,])
  ilon<-grep("coords.x1",colnames(start))
  ilat<-grep("coords.x2",colnames(start))
  colnames(start)[c(ilon,ilat)]<-c("longitude","latitude")
  colnames(finish)[c(ilon,ilat)]<-c("longitude","latitude")
  finish$dest_name_short<-dest$dest_name_short
  roaddistance<-ComputeNICDistance(start,finish)
  
  #6) compute new travel times
  #       a) current travel times
  #       b) new travel time
  #           i) min travel time to junction on road and id of junction i)
  #          ii) min travel time from road to destinations and id of junction ii)
  #          iii) travel time to road + along road + from road to destination
  #       c) new time = min(a,b)

  idest<-(grepl("drive",colnames(m2data)) & !grepl("spline",colnames(m2data)))
  drivenames<-colnames(m2data)[idest]
  ndest<-length(drivenames)
  dest_name_short<-rep(NA,ndest)
  dest_name_list<-strsplit(drivenames,split="_")
  drivedistance<-rep(NA,ndest)
  for (i1 in 1:ndest) {
    dest_name_short[i1]<- dest_name_list[[i1]][2]  
    drivedistance[i1]<-paste0("distance_",dest_name_short[i1])
  }
  
#  drivedistance<-c("distance_OX",
#                   "distance_MK",
#                   "distance_CAM")
#  drivenames<-c("drive_OX","drive_MK","drive_CAM")
#  dest_name_short<-c("OX","MK","CAM")

#  speed<-80   # kilometers per hour
  for (i1 in 1:length(drivenames)) {  
    idistance<-grep(drivedistance[i1],colnames(roaddistance))
    im2<-grep(drivenames[i1],colnames(m2data),fixed=TRUE)
    idest<-grep(dest_name_short[i1],dest$dest_name_short)
    m2data$oldtime<-m2data[,im2[1]]
    ioldtime<-grep("oldtime",colnames(m2data))
    names(m2data)[ioldtime]<-paste0(drivenames[i1],"0")
    newtime <- mintime_toroad + 
                             60*roaddistance[,idistance]/speed + 
                             mintime_todest[idest]
    m2data[,drivenames[i1]]<-pmin(newtime,m2data[,drivenames[i1]])
  }
  #gain1<-m2data$drive_CAM0-m2data$drive_CAM
  #gain2<-m2data$drive_MK0-m2data$drive_MK
  #gain3<-m2data$drive_OX0-m2data$drive_OX

  for (i1 in 1:ndest) {
    gain<- m2data[,paste0(drivenames[i1],"0")] -
      m2data[,drivenames[i1]]
    heatmap1(m2data$longitude,m2data$latitude,gain,
             resolution=0.01,zlabel=paste0("gain ",drivenames[i1]),
             outfile=paste0(OutDir,"/gain_",drivenames[i1],".pdf"),
             places=dest[c(2:4),c(3:5)],
             route=as.data.frame(road)[,c(1,3,4)],routelabel = "new road")
  }
  return(m2data)
}

