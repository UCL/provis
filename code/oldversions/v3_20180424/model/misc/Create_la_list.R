#get_la_list <- function(region_id,dirs) {
#  load(paste0(RootDir,"/data/region_la_list.RData"))
#  la_list <- region_data[region_id]$LA_list
#}

source(paste0(RootDir,"/code/model/B2SetPath.R"))
nregs<-11
region_data<-data.frame(region_id=seq(1:nregs), 
                       region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", 
                                  "NW", "SE",
                                  "SW", "WestMid", "YorkshireHumber"))

region_la_list<-vector("list",nregs)

for (r in 1:nregs) {
  dirs<-B2SetPath(RootDir,r)
  load(paste0(dirs$datadir,"/m1data.RData"))
  region_la_list[[r]]<-levels(m1data$lad15nm)
}
save(region_la_list,file=paste0(RootDir,"/data/region_la_list.RData"))

