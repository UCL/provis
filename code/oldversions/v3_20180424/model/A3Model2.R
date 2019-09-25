# Estimate model 2
# Author:  MM,LN
# Version: 2018.02.02

LondonFlag<-0   # 0 : Only Greater London
# 1 : London + some adjacent counties  
CRSFlag<-FALSE   # true to also estimate crs
datastub<-"m11" # "m11"   = domestic properties
                   # "nondom" = non-domestic properties

N0<-10000      # N0   = size of fullsample used for estimation
# N0   = 0 then use all data (excluding outliers)
plotflag<-0  # 0 = no plots.  1 = plot on screen.  2 = plot to device
nregs<-11
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", "NW", 
                                  "SE", "SW", "WestMid", 
                                  "YorkshireHumber"))
for (r in 1:11) {
  region_id<-regnames$region_id[r]    
  region_str<-as.character(regnames$region_str[r]) 
  dirs<-B2SetPath(RootDir,CodeDir,region_id,datastub)
  
  # Load data
  load(file=paste0(dirs$datadir,"/m2data2.RData"))
  
  # Define subsamples
  qnt <- quantile(m2data$location_value, probs=c(.01, .99),na.rm=TRUE)
  iFull <- m2data$location_value>qnt[1] & m2data$location_value<qnt[2]

  # Estimate base model: Model 0
  # vlist1:  land use and other amenties + travel times to (station,coast,aroad,motorway)
  # vlist2:  (drive,trans) variables
  # vlist3:  splines in (drive,trans)
  vlist1<-A3Model2_vlist1(r)
  
  # List of all variables starting with "drive_"
  # Create minimum drive time
  drivevars<-colnames(m2data)[grep("\\<drive_[^acmnst]",colnames(m2data))]
  iAONB<-grep("\\<drive_AONB",drivevars)
  if (length(iAONB)>0) drivevars<-drivevars[-iAONB]

  # List all variables with "trans_"
  # Create minimum transit time 
  transvars<-colnames(m2data)[grep("\\<trans_[^acmnst]",colnames(m2data))]

  # Get names of drive/trans and creates character strings for formula
  commutevars<-c(drivevars,transvars)
  drivesplines<-colnames(m2data)[grep("\\<spline_drive_",colnames(m2data))]
  transsplines<-colnames(m2data)[grep("\\<spline_trans_",colnames(m2data))]
  commute_splines<-c(drivesplines,transsplines)
  
  # Create vlist2:  to be included in formula
  logfunc<-function(x) paste0("log(1+",x,")")
  logcommutevars<-logfunc(commutevars)
  vlist2<-""
  for (i in 1:length(logcommutevars)) {vlist2<-paste(vlist2,logcommutevars[i], sep="+") }
  vlist2spline<-""
  for (i in 1:length(commute_splines)) {vlist2spline<-paste(vlist2spline,commute_splines[i], sep="+") }

  formula0<-as.formula(paste0(vlist1,vlist2))
  formula1<-as.formula(paste0(vlist1,vlist2spline))

  m2ols0<-lm(formula0,data=m2data,subset=iFull,na.action=na.exclude)
  m2ols1<-lm(formula1,data=m2data,subset=iFull,na.action=na.exclude)

  m2ols0$varlist<-B4GetVarList(names(m2ols0$model))
  m2ols1$varlist<-B4GetVarList(names(m2ols1$model))
  
  summary(m2ols0)
  
  if (r!=5) {
    formula5<-A3Model2_specification(r)
    m2ols5<-lm(formula5,data=m2data[iFull,],na.action=na.exclude)
    m2ols5$varlist<-B4GetVarList(names(m2ols5$model))
    summary(m2ols5)
  } else if (r==5) {
    m2ols5<-m2ols1
  }
  p5<-predict(m2ols5,na.action=na.exclude)
  e5<-m2data$location_value[iFull]-p5
  
  heatmap1(m2data$longitude[iFull],m2data$latitude[iFull],e5[iFull],filter=!is.na(e5[iFull]),resolution=0.01,zlabel="e5", 
           outfile=paste0(dirs$outdir,"/e5.pdf"))
#  heatmap2(m2data$longitude[iFull],m2data$latitude[iFull],e5[iFull],filter=!is.na(e5[iFull]),
#           resolution=0.01,zlabel="e5", 
#           outfile=paste0(dirs$outdir,"/e5.pdf"),
#           nlevels=5)
  
  p2ols0<-predict(m2ols0,na.action=na.exclude)
  e20<-m2data$location_value[iFull]-p2ols0
  p2ols1<-predict(m2ols1,na.action=na.exclude)
  e21<-m2data$location_value[iFull]-p2ols1[iFull]

if (plotflag>0) {
  if (plotflag==1) {
    map1<-heatmap1(m2data$latitude[iFull],m2data$longitude[iFull],e21,filter=!is.na(e21),
                   resolution=0.001,zlabel="residual ols1",outfile=paste0(dirs$outdir,"/errs1.pdf"))
    dev.off()
  }
}

  DestinationFile<-paste0(dirs$datadir,"/destinations",region_id,"B.csv")
  dest<-read.csv(DestinationFile)
  
if (plotflag>0) {
  # Plot vs travel times
  if (region_id!=5) {
    drivedest<-dest[dest$shortlist,]
    transdest<-dest[dest$raildest,]
    for (i1 in 1:nrow(drivedest)) { 
      n1<-paste0("drive_",drivedest[i1,"shortname"])
      ispline1<-grep(paste0("spline_",n1),names(coefficients(m2ols1)))
      ispline2<-grep(paste0("spline_",n1),colnames(m2data))
      i3<-grep(paste0("\\<",n1),colnames(m2data))
      b<- m2data[,ispline2] %*% coefficients(m2ols1)[ispline1]
      if (plotflag==2) {
        pdf(file=paste0(dirs$outdir,"/price_v_drive_",drivedest[i1,"shortname"],".pdf")) 
      }
      plot(m2data[,i3],b,pch=".",main=paste0("Log price vs ",n1))
      if (plotflag==2) {
        dev.off()
      }
    }
  }  
  n1<-"drive_town"
  ispline1<-grep(paste0("spline_",n1),names(coefficients(m2ols1)))
  ispline2<-grep(paste0("spline_",n1),colnames(m2data))
  i3<-grep(paste0("\\<",n1),colnames(m2data))
  b<- m2data[,ispline2] %*% coefficients(m2ols1)[ispline1]
  if (plotflag==2) {
    pdf(file=paste0(dirs$outdir,"/price_v_drive_town.pdf"))
  }
  plot(m2data[,i3],b,pch=".",main=paste0("Log price vs ",n1))
  if (plotflag==2) {
    dev.off()
  }

  if (region_id!=5) {    
    for (i1 in 1:nrow(transdest)) {
      n1<-paste0("trans_",transdest[i1,"shortname"])
      ispline1<-grep(paste0("spline_",n1),names(coefficients(m2ols1)))
      ispline2<-grep(paste0("spline_",n1),colnames(m2data))
      i3<-grep(paste0("\\<",n1),colnames(m2data))
      b<- m2data[,ispline2] %*% coefficients(m2ols1)[ispline1]
      if (plotflag==2) pdf(file=paste0(dirs$outdir,"/price_v_trans_",transdest[i1,"shortname"],".pdf"))
      plot(m2data[,i3],b,pch=".",main=paste0("Log price vs ",n1))
      if (plotflag==2) dev.off()
    }
  }
} # if (plotflag>0) 
  
  save(m2ols0,file=paste0(dirs$outdir,"/m2ols0.RData"))
  save(m2ols1,file=paste0(dirs$outdir,"/m2ols1.RData"))
  save(m2ols5,file=paste0(dirs$outdir,"/m2ols5.RData"))
  

}  # for (r in 1:nregs) {

