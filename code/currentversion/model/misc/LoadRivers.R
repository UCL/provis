library(rgdal)
RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
riverdir<-paste0(RootDir,"/data/rivers")

files<-dir(riverdir)
i1<-grep("shp",files)
files<-files[i1]


rivers<-readOGR(dsn=riverdir,layer=gsub("\\.shp","",files[2]))
