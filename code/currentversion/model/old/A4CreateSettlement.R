#' A4CreateSettlement(filename,DataDir,NewDataDir,TravelModelDir,OutDir)
#' Create data frames for new settlement and predict prices
#' 
#' @param filename        .csv file containing details of settlement
#' @param DataDir         data directory
#' @param NewDataDir      new data directory (new settlement data)
#' @param TravelModelDir  directory with results from travel models
#' @param OutDir          output directory
#' @param varlist         list of variables in baseline model
#' @param title           title of table of summary stats
#' @param outfile         output file
#' @return newdata        list containing (m1data_new,m2data_new)
#' @keywords new settlement
#' @export
#' @examples
#' newdata <- A4CreateSettlement("Bedford.csv",DataDir,NewDataDir,TravelModelDir,OutDir,title,outfile)
A4CreateSettlement<-function(filename,DataDir,NewDataDir,TravelModelDir,OutDir,varlist,
                             title,outfile) {
  default_warn<-getOption("warn")
  options(warn=-1)
  
  # Load New settlement data
  #  example:  filename<-"Bedford.csv"  
  m2data_new<-read.csv(paste0(NewDataDir,"/",filename), header=TRUE)
  dest<-read.csv(paste0(DataDir,"/destinations.csv"))
  load(paste0(NewDataDir,"/","m1data_template.RData"))
  load(paste0(DataDir,"/","m2data.RData"))

  levels(m2data_new$tenure)<-c("F","L")

  # create dataset to estimate model 1
  if (nrow(m1data_template)>nrow(m2data_new)) {
    m1data_new<-m1data_template[1:nrow(m2data_new),]
  } else {
    m1data_new<-m1data_template[sample(c(1:nrow(m1data_template)),nrow(m2data_new),replace=TRUE),]  
  }  
  m1data_new$date<-as.Date("2017-12-31")
  m1data_new$year<-factor("2017",levels=levels(m1data_new$year))
  m1data_new$propertytype<-m2data_new$propertytype
  m1data_new$newbuild<-factor("Y",levels=levels(m1data_template$newbuild))
  m1data_new$tenure<-m2data_new$tenure
  m1data_new$total_floor_area<-m2data_new$total_floor_area
  m1data_new$latitude<-m2data_new$latitude
  m1data_new$longitude<-m2data_new$longitude

  # 1) Estimate travel times
  ilat<-grep("latitude",colnames(m2data_new))
  ilon<-grep("longitude",colnames(m2data_new))
  newdata<-predictTravelTime(m2data_new[,c(ilat,ilon)],TravelModelDir)
  m2data_new<-join(m2data_new,newdata,by=c("latitude","longitude"),match="first",type="left")  
  rm(newdata)
  m2data_new$train_LON <- with(m2data_new,
                               pmin(m2data_new$train_EUS,
                                    m2data_new$train_KGX,
                                    m2data_new$train_PAD,
                                    m2data_new$train_LIV,
                                    m2data_new$train_MAR))

  # 2) predict location_value for new bedford
  xmin<-summary(m2data_new$latitude)
  ymin<-summary(m2data_new$longitude)
  set.seed(100)
  iOldBedford<- (m2data$latitude>=xmin[1] & m2data$latitude<=xmin[6] 
               & m2data$longitude>=ymin[1] & m2data$longitude[6])
  tempdata<-m2data[iOldBedford,]
  if (nrow(tempdata)>1000) {
    isample<-sample(c(1:nrow(tempdata)),size=1000,replace=F)
  }else {
    isample<-c(1:nrow(tempdata))
  }
  
  np_location_value<-npreg(location_value~latitude+longitude,data=tempdata[isample,])
  m2data_new$location_value<-predict(np_location_value,newdata=m2data_new)

  # 3) Create oldbedford:  subsample of properties within area of New bedford
  nbins<-10
  xL <- xmin[1]+c(0:(nbins-1))*(xmin[6]-xmin[1])/nbins
  xH <- xmin[1]+c(1:nbins)*(xmin[6]-xmin[1])/nbins
  yL <- 0.0*c(1:nbins)
  yH <- yL
  for(i1 in 1:nbins) { 
    ix<-(m2data_new$latitude>=xL[i1] & m2data_new$latitude<xH[i1])
    nx<-summary(ix)
    tempy<-summary(m2data_new$longitude[ix]) 
    yL[i1]=tempy[1]
    yH[i1]=tempy[6]
    ixy<- (xL[i1]<=m2data$latitude & m2data$latitude<xH[i1] &
           yL[i1]<=m2data$longitude & m2data$longitude<yH[i1]) 
    if (i1==1) {
      oldbedford<-m2data[ixy,]
    }  else {  
      oldbedford<-rbind(oldbedford,m2data[ixy,])
    }
  }

  # 4) Restricted land percentage
  m2data_new$restrictedland_pct<-summary(oldbedford$restrictedland_pct)[4]

  # 5) Distance to destinations from new houses
  m2data_new<-ComputeNICDistance(m2data_new,dest) 

  # rename fz3share to floodzone3
  ifz3<-grep("fz3share",colnames(m2data_new))
  names(m2data_new)[ifz3]<-"floodzone3"  

  # summary statistics
  stargazer(oldbedford[,varlist],
            m2data_new[,varlist],title=title,
            align=TRUE, out.header=TRUE,digits=4,
            out=paste0(OutDir,"/",outfile))
  outdata<-list(m1data_new,m2data_new)
  names(outdata)<-c("m1data_new","m2data_new")
  options(warn=default_warn)
  return(outdata)
}
