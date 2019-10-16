# Estimate model 2
# Author:  MM,LN
# Version: 2018.10.10

LondonFlag  <- 0     # 0 : Only Greater London
                     # 1 : London + some adjacent counties  
datastub    <- "nondom" # "m11"   = domestic properties
                        # "nondom" = non-domestic properties
N0          <- 10000 # N0   = size of fullsample used for estimation
                     # N0   = 0 then use all data (excluding outliers)
plotflag    <- 0     # 0 = no plots.  1 = plot on screen.  2 = plot to device
depvar      <- c("logvalue","value","boxcoxvalue")
avgtimeflag <- 1    # 1 = use average time to destinations. 0 = use (drive_time,trans_time)
nregs       <- 11
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", "NW", 
                                  "SE", "SW", "WestMid", 
                                  "YorkshireHumber"))
for (r in 1:11) {
  region_id<-regnames$region_id[r]    
  region_str<-as.character(regnames$region_str[r]) 
  dirs<-B2SetPath(RootDir,CodeDir,DataRoot,region_id,datastub)
  
  # Load data
  load(file=paste0(dirs$datadir,"/m2data2.RData"))
  
  # Convert prob_4band="" to prob_4band==NA
  i1<-(levels(m2data$prob_4band)=="")
  levels(m2data$prob_4band)[i1]<-NA
  
  # Estimate base model: Model 0 
  # vlist1:  land use and other amenties + travel times to (station,coast,aroad,motorway)
  # vlist2:  (drive,trans) variables
  # vlist3:  splines in (drive,trans)
  vlist1<-A3Model2_vlist1(r,datastub)
  
  if (avgtimeflag==0) {
    # List of all variables starting with "drive_"
    drivevars<-colnames(m2data)[grep("\\<drive_[^acmnst]",colnames(m2data))]
    iAONB<-grep("\\<drive_AONB",drivevars)
    if (length(iAONB)>0) drivevars<-drivevars[-iAONB]

    # List all variables with "trans_"
    transvars<-colnames(m2data)[grep("\\<trans_[^acmnst]",colnames(m2data))]

    # Get names of drive/trans and creates character strings for formula
    commutevars<-c(drivevars,transvars)
    drivesplines<-colnames(m2data)[grep("\\<spline_drive_",colnames(m2data))]
    transsplines<-colnames(m2data)[grep("\\<spline_trans_",colnames(m2data))]
    commute_splines<-c(drivesplines,transsplines)
  } else if (avgtimeflag==1) {
    # List of all variables starting with "avgtime_"
    commutevars<-colnames(m2data)[grep("\\<avgtime_[^acmnst]",colnames(m2data))]

    # Get names of drive/trans and creates character strings for formula
    commute_splines<-colnames(m2data)[grep("\\<spline_avgtime_",colnames(m2data))]
  }   
  # Create vlist2:  to be included in formula
  logfunc<-function(x) paste0("log(1+",x,")")
  logcommutevars<-logfunc(commutevars)
  vlist2<-""
  for (i in 1:length(logcommutevars)) {vlist2<-paste(vlist2,logcommutevars[i], sep="+") }
  vlist2spline<-""
  for (i in 1:length(commute_splines)) {vlist2spline<-paste(vlist2spline,commute_splines[i], sep="+") }

  formula0<-as.formula(paste0(vlist1,vlist2))
  formula1<-as.formula(paste0(vlist1,vlist2spline))

  for (y in depvar) {
    # dependent variables is one of c("logvalue","value","boxcoxvalue")
    m2data$location_value <- m2data[,grep(paste0("\\<",y),colnames(m2data))]
    # Define subsamples
    qnt   <- quantile(m2data$location_value, probs=c(.01, .99),na.rm=TRUE)
    iFull <- m2data$location_value>qnt[1] & m2data$location_value<qnt[2]
    
    m2ols0<-lm(formula0,data=m2data,subset=iFull,na.action=na.exclude)
    m2ols1<-lm(formula1,data=m2data,subset=iFull,na.action=na.exclude)

    m2ols0$varlist<-B4GetVarList(names(m2ols0$model))
    m2ols1$varlist<-B4GetVarList(names(m2ols1$model))
  
    summary(m2ols0)
    if (r!=5) {
      if (avgtimeflag==0) {
        # use (drive_time,trans_time)
        formula5<-A3Model2_specification0(r)
      } else if (avgtimeflag==1) { 
        # use (avgtime)
        formula5<-A3Model2_specification1(r,datastub)
      }
      m2ols5<-lm(formula5,data=m2data[iFull,],na.action=na.exclude)
      m2ols5$varlist<-B4GetVarList(names(m2ols5$model))
      summary(m2ols5)
    } else if (r==5) {
      m2ols5<-m2ols1
    }

    m2ols0$depvar <- y
    m2ols1$depvar <- y
    m2ols5$depvar <- y
    save(m2ols0,file=paste0(dirs$outdir,"/m2",y,"0.RData"))
    save(m2ols1,file=paste0(dirs$outdir,"/m2",y,"1.RData"))
    save(m2ols5,file=paste0(dirs$outdir,"/m2",y,"5.RData"))
  } # for (y in depvar)
}  # for (r in 1:nregs) {
  
