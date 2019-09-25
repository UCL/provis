# The input file geodatabase
library(rgdal)
library(ggplot2)
library(plyr)

AONBdir<-"/Users/larsnesheim/Documents/research/hedonic/NIC/data/AONB.gdb"
outdir<-"/Users/larsnesheim/Documents/research/hedonic/NIC/code/output"
dirs<-list(AONBdir,outdir)
names(dirs)<-c("AONBdir","outdir")
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
AONBlayer <- ogrListLayers(dirs$AONBdir)
# Read the feature class
AONB <- readOGR(dsn=dirs$AONBdir,layer=AONBlayer[[1]])

# Determine the AONB extent, projection, and attribute information
summary(AONB)

# View the feature class
AONB@data$id <- rownames(AONB@data)
AONB.points = fortify(AONB, region="id")
AONB.df = join(AONB.points, AONB@data, by="id")
ggplot(AONB.df[AONB.df$id=="1",]) + 
  aes(long,lat,alpha=0) + 
  geom_polygon()

pdf(file=paste0(dirs$outdir,"/AONBMap.pdf"))
    
ggplot(AONB.df) + 
 aes(long,lat,fill=NAME,alpha=0) + 
  geom_polygon()
dev.off()

# 1) intersection of aonb with region
# 2) houses that are in AONB
# 3) distance from AONB


#plot(AONB)
