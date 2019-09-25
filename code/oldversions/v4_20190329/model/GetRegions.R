n1<-500
set.seed(345294)

for (r in 2:11) {
  dirs<-B2SetPath(RootDir,CodeDir,r)
  load(file=paste0(dirs$datadir,"/m2data1.RData"))
  i1<-sample(c(1:nrow(m2data)),size=n1,replace=FALSE)
  if (r==2) {
    regions<-m2data[i1,c("longitude","latitude")]
    regions$region<-r
  } else {
    tempdata<-m2data[i1,c("longitude","latitude")]
    tempdata$region<-r
    regions<-rbind(regions,tempdata)
  }
}  
save(regions,file=paste0(dirs$farmdir,"/regions.RData"))
