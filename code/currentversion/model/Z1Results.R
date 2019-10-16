# create table of model 1 results
plotflag<-2
nregions<-11
datastub<-"nondom"
m2flag<-5
depvar<-"logvalue"  # c("logvalue","value","boxcox")
outstr<-"output"
if (datastub=="nondom") {
  outstr<-"output/nondom"
}

regions<-data.frame(region_id=seq(1:nregions), 
                     region=c("CaMKOx", "CornwallDevon", 
                              "EastMid", "EastEng", 
                              "London", "NE", 
                              "NW", "SE",
                              "SW", "WestMid", "YorkshireHumber"))

OutDir<-paste0(CodeDir,"/",outstr,"/allregions")
if (!file.exists(OutDir)) {
  dir.create(OutDir)  
}

m1<-vector("list",nregions)
for (r in 1:nregions) {
  if (depvar=="logvalue") {
    load(paste0(CodeDir,"/",outstr,"/region",as.character(r),"/m1log0.RData"))
    m1[[r]]<-m1log0
  } else if (depvar=="vaue") {
    load(paste0(CodeDir,"/",outstr,"/region",as.character(r),"/m1linear0.RData"))
    m1[[r]]<-m1linear0  
  } else if (depvar=="boxcox") {
    load(paste0(CodeDir,"/",outstr,"/region",as.character(r),"/m1boxcox0.RData"))
    m1[[r]]<-m1boxcox0
  }
  names(m1)[r]<-paste0("region",as.character(r))
}

# Plot tables of parameter estimates
stargazer(m1$region1,m1$region2,m1$region3,m1$region4,m1$region5,m1$region6,
          type="latex",title="Model 1 results",
          out = paste0(OutDir,"/model1A.tex"),
          out.header=TRUE,
          digits=3,digits.extra=2,column.sep.width="0pt",
          font.size = "small",df=FALSE,
          keep = c("Intercept","year","propertytype","newbuild","tenure"))
stargazer(m1$region7,m1$region8,m1$region9,m1$region10,m1$region11,
          type="latex",title="Model 1 results",
          out = paste0(OutDir,"/model1B.tex"),
          out.header=TRUE,
          digits=3,digits.extra=2,column.sep.width="0pt",
          font.size = "normalsize",df=FALSE,
          keep = c("Intercept","year","propertytype","newbuild","tenure"))

# Create tables of spline parameters
segments<-matrix(0,nregions,5)
segments[,1]<-c(1:nregions)
polydegree<-segments
for (r in 1:nregions) {
  segments[r,2:5]  <- m1[[r]]$segments
  polydegree[r,2:5] <- m1[[r]]$degree
}
stargazer(segments,type="latex",title="Model 1: number of spline segments",
          out = paste0(OutDir,"/model1_segments.tex"),
          out.header=TRUE,
          column.sep.width="0pt")
stargazer(polydegree,type="latex",title="Model 1: degree of spline polynomials",
          out = paste0(OutDir,"/model1_degree.tex"),
          out.header=TRUE,
          column.sep.width="0pt")

if (datastub=="m11") {
  # average price per 
  nyears<-length(levels(m1[[1]]$model$year))
  year1<-as.numeric()

  for (r in 1:nregions) {
    tempdata<-regions[rep(r,nyears),]
    tempdata$year<-c(as.numeric(levels(m1[[r]]$model$year)))
    tempdata$logprice<-NA
    tempdata$price<-NA
    for (y in 1:nyears) {
      itemp <- m1[[r]]$model$year==levels(m1[[r]]$model$year)[y]
      tempdata$logprice[y] <- mean(m1[[r]]$model$logprice[itemp])
      tempdata$price[y]    <- mean(exp(m1[[r]]$model$logprice[itemp]))
    }
    if (r==1) {
      regiondata<-tempdata
    } else {
      regiondata<-rbind(regiondata,tempdata) 
    }
  }
  # plot time series of prices
    if (plotflag>0) {
      tempfile<-paste0(OutDir,"/price_timeseries.eps")
      if (plotflag==2)     postscript(tempfile)
      ggplot(data=regiondata,aes(year,price))+geom_line(aes(col=region)) +
        theme(legend.position = "bottom")
      if (plotflag==1) dev.copy(postscript,tempfile)
      dev.off()
    }
}

# create tables of model 2
m2<-vector("list",nregions)
for (r in c(1:11)) {
  if (m2flag==0) {
    load(paste0(CodeDir,"/",outstr,"/region",as.character(r),"/m2",depvar,"0.RData"))
    m2[[r]]<-m2ols0
  } else if (m2flag==1) {
    load(paste0(CodeDir,"/",outstr,"/region",as.character(r),"/m2",depvar,"1.RData"))
    m2[[r]]<-m2ols1
  } else if (m2flag==5) {
    load(paste0(CodeDir,"/",outstr,"/region",as.character(r),"/m2",depvar,"5.RData"))
    m2[[r]]<-m2ols5
  }
  names(m2)[r]<-paste0("region",as.character(r))
}

# Plot tables of parameter estimates
keeplist1<-c("pct","localplan","\\<lu_",
            "popdensity","imddec","prob_","noiseclass")
keeplist2<-c("airport","motorway","aroad",
             "AONB","natpark","coast",
             "station","town")

stargazer(m2$region1,m2$region2,m2$region3,m2$region4,m2$region5,m2$region6,
          type="latex",title="Model 2 results: 1",
          out = paste0(OutDir,"/model2A.tex"),
          out.header=TRUE,
          no.space=TRUE,
          digits=3,digits.extra=2,column.sep.width="0pt",
          font.size = "small",df=FALSE,
          keep = keeplist1)
stargazer(m2$region7,m2$region8,m2$region9,m2$region10,m2$region11,
          type="latex",title="Model 2 results 2",
          out = paste0(OutDir,"/model2B.tex"),
          out.header=TRUE,
          no.space=TRUE,
          digits=3,digits.extra=2,column.sep.width="0pt",
          font.size = "small",df=FALSE,
          keep = keeplist1)
stargazer(m2$region1,m2$region2,m2$region3,m2$region4,m2$region5,m2$region6,
          type="latex",title="Model 2 results 3",
          out = paste0(OutDir,"/model2C.tex"),
          out.header=TRUE,
          no.space=TRUE,
          digits=3,digits.extra=2,column.sep.width="0pt",
          font.size = "small",df=FALSE,
          keep = keeplist2)
stargazer(m2$region7,m2$region8,m2$region9,m2$region10,m2$region11,
          type="latex",title="Model 2 results 4",
          out = paste0(OutDir,"/model2D.tex"),
          out.header=TRUE,
          no.space=TRUE,
          digits=3,digits.extra=2,column.sep.width="0pt",
          font.size = "small",df=FALSE,
          keep = keeplist2)


# Create tables of summary stats
m2data3<-vector("list",nregions)

r<-1
dirs<-B2SetPath(RootDir,CodeDir,DataRoot,r,datastub)
# Load data
load(file=paste0(dirs$datadir,"/m2data2.RData"))
var1<-depvar
if (depvar=="boxcox") var1<-"boxcoxvalue"
if (datastub=="m11") {
  vlist1<-c(var1,"pricepaid","propertytype",
            "newbuild","tenure","total_floor_area",
            "builtuparea_pct","restrictedland_pct",
            "localplanrate","popdensityOA","imddecile",
            "prob_4band","roadnoise")
} else if (datastub=="nondom") {
  vlist1<-c(var1,"price",
            "total_floor_area",
            "builtuparea_pct","restrictedland_pct",
            "localplanrate","popdensityOA","imddecile",
            "prob_4band")
}
vlist2<-colnames(m2data[,grep("\\<lu_",colnames(m2data))])
vlist3<-c("drive_town","trans_town","distance_airport",
          "distance_AONB","AONB","distance_motorway",
          "distance_aroad","drive_motorway","drive_aroad",
          "distance_station","drive_station","trans_station")
vlist<-c(vlist1,vlist2,vlist3)
nvars<-length(vlist)
vtype<-rep(NA,nvars)
nvars1<-rep(1,nvars)
for (v in 1:nvars) {
 vtype<-class(m2data[,vlist[v]])
 if (is.factor(m2data[,vlist[v]])) {
   nvars1[v]<-length(levels(m2data[,vlist[v]]))   
 }
}
sumstats<-matrix(0,sum(nvars1),nregions)
rnames<-rep(NA,sum(nvars1))
colnames(sumstats)<-as.character(regions[,2])

for (r in c(1:11)) {
  dirs<-B2SetPath(RootDir,CodeDir,DataRoot,r,datastub)
  # Load data
  load(file=paste0(dirs$datadir,"/m2data2.RData"))
  if (datastub=="m11") {
    m2data$pricepaid<-m2data$pricepaid/1000
  }
  if (r==5) {
    m2data$AONB=FALSE
    m2data$distance_AONB<-NA
  }
  m2data3[[r]]<-m2data[,vlist]  
  names(m2data3)[r]<-paste0("region",as.character(r))
  v0<-1
  for (v in 1:nvars) {
    if (is.factor(m2data[,vlist[v]])) {
      lev<-levels(m2data[,vlist[v]])
      for (i1 in lev) {
        sumstats[v0,r]<-mean(m2data[,vlist[v]]==i1,na.rm=TRUE)
        rnames[v0]<-paste0(vlist[v],":",as.character(i1))
        v0<-v0+1
      }  
    } else {
      sumstats[v0,r]<-mean(m2data[,vlist[v]],na.rm=TRUE) 
      rnames[v0]<-vlist[v]
      v0<-v0+1
    }
  }
}
rownames(sumstats)<-rnames

stargazer(sumstats[,1:6],
          type="latex",title="Summary statistics 1",
          out = paste0(OutDir,"/summary1A.tex"),
          out.header=TRUE,
          digits=2,digits.extra=2,column.sep.width="0pt",
          font.size = "small")

stargazer(sumstats[,7:11],
          type="latex",title="Summary statistics 2",
          out = paste0(OutDir,"/summary1B.tex"),
          out.header=TRUE,
          digits=2,digits.extra=2,column.sep.width="0pt",
          font.size = "small")

# Table of destinations for each region
destlist  <- matrix(character(nregions*25),25,nregions)
for (r in 1:11) {
  dirs<-B2SetPath(RootDir,CodeDir,DataRoot,r)
  DestinationFile<-paste0(dirs$datadir,"/destinations",r,".csv")
  dest<-read.csv(DestinationFile)
  dest<-as.character(dest[dest$shortlist,1])
  ndest<-length(dest)
  for (i1 in 1:ndest) {
    destlist[i1,r]<-strsplit(dest[i1],",")[[1]][1]  
  }
}
colnames(destlist)<-as.character(regions$region)
stargazer(destlist[,1:6],type="latex",title="Destinations: regions 1-6",
          out = paste0(OutDir,"/destinations1.tex"),
          out.header=TRUE,
          rownames=FALSE,
          colnames=TRUE,
          summary=FALSE,
          column.sep.width="0pt")
stargazer(destlist[,7:11],type="latex",title="Destinations: regions 7-11",
          out = paste0(OutDir,"/destinations2.tex"),
          out.header=TRUE,
          rownames=FALSE,
          colnames=TRUE,
          summary=FALSE,
          column.sep.width="0pt")

