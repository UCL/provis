# Construct commuting index
make_index<-function(flows_datafile,DataDir,TravelModelDir,m2data) {
  # flows_datafile <- "MSOA_OA_flows.csv"
  # Preload objects
  workflows<-read.csv(paste0(DataDir,"/"?n,flows_datafile), header=TRUE)
  load(paste0(TravelModelDir,"/ttmodel.RData"))

  # Merge with workflows and predict travel times
  workflows<-workflows[,6:13]
#  m2data<-read.csv(paste0(DataDir,"/CamOxMK_model2data.csv"),header=TRUE)
  
  m2temp<-m2data[m2data$msoa11cd=="",c("latitude","longitude","msoa11cd","postcode","laname","newbuild")]
  
  mat3<-join(m2temp,workflows,by="msoa11cd",match="all",type="left")
  mat3<-mat3[,-1]
  
  # Find column numbers for (longitude, latitude)
  ilat<-grep("latitude",colnames(mat3))
  ilon<-grep("longitude",colnames(mat3))
  
  # Find column numbers for (work_x,work_y)
  iwork_x<-grep("oacentroid_work_x",colnames(mat3))
  iwork_y<-grep("oacentroid_work_y",colnames(mat3))

  # rename columns  
  names(mat3)[c(ilon,ilat)]<-c("res_x","res_y")
  names(mat3)[c(iwork_x,iwork_y)]<-c("work_x","work_y")

#  nas<-which(is.na(mat3),arr.ind=TRUE)
#  attach(mat3)
#  mat3[nas]<-0

  ndata<-data.frame(mat3$res_x,mat3$res_y,mat3$work_x,mat3$work_y)
  names(ndata)<-c("res_x","res_y","work_x","work_y")
  ttime<-predict(spline_drive_l1_s4_add,newdata=ndata)
  ttime[nas[,1]]<-NA
  summary(ttime)
  ttime<-data.frame(ttime)
  ndata<-data.frame(mat3,ttime)
  ndata<-ndata[which(!is.na(ndata$ttime),arr.ind=TRUE),]

  unique_xy<-matrix(which(!duplicated.data.frame(data.frame(ndata$res_y,ndata$res_x)),arr.ind=TRUE))
  ind_xy<-matrix(0,dim(ndata)[1],1)

  # Set number of commuting destinations (max is 10)
  ndest<-10
  gam<-1.0/60.0

  # Construct index (choose which variable to use as weight)
  for (j in unique_xy) {
    for (i in 1:ndest) {  
      aux<-0
      k<-j
      aux<-aux+exp(-gam*ndata$ttime[k])*ndata$persons_msoa[k] 
      k<-k+i
    }
    ind_xy[j]<-aux
  }
  ind_xy<-data.frame(ind_xy)
  ind_xy[which(ind_xy==0,arr.ind=TRUE)]<-NA

  ndata<-data.frame(ndata,ind_xy)

  index_data<-data.frame(ndata$res_x,ndata$res_y,ndata$ind_xy)
  rm(ndata)
 
  index_data<-index_data[which(!is.na(index_data$ndata.ind_xy)),]

  write.csv(index_data,file=paste0(DataDir,"/index_data_commuters.csv"))
}
