library(dplyr)
library(data.table)
library(ggmap)
library(sp)
library(rgdal)
library(ggplot2)
library(RColorBrewer)


### Data set by the OP
positions <- farms3[,c("id","longitude","latitude","price_per_acre")]
positions$price_per_acre <- 1000*positions$price_per_acre
i1<- (positions$price_per_acre>0)
i1[is.na(i1)]<-FALSE
positions$logprice_per_acre[i1] <- log(positions$price_per_acre[i1])
positions<-positions[complete.cases(positions),]

### Data arrangement
nbreaks<-10
cutvalue<-quantile(positions$logprice_per_acre,c(seq(0,1,length.out=nbreaks+1)))
cutvalue[1]<-cutvalue[1]-.1
positions$logpricecuts <- cut(positions$logprice_per_acre, breaks=cutvalue)
positions$logpricecuts <- as.character(as.integer(positions$logpricecuts))

### Step 1: Get a map
#map <- get_map(location="United Kingdom", maptype='roadmap',zoom=6,col="bw")
map1 <- get_map(location="Great Britain", maptype='roadmap',zoom=6)
map2 <- get_map(location="Great Britain", source="stamen",maptype='watercolor',zoom=6)
map3 <- get_map(location="Cornwall, UK", maptype='roadmap',zoom=11)
ggmap(map3)+aes(lon,lat)

plot(map)

### Step 2: I need to create SpatialPolygonDataFrame using the original data.
### http://stackoverflow.com/questions/25606512/create-polygon-from-points-and-save-as-shapefile
### For each price zone, create a polygon, SpatialPolygonDataFrame, and convert it
### it data.frame for ggplot.

cats <- list()

for(i in unique(positions$logpricecuts)){
  
  foo <- positions %>%
    filter(logpricecuts == i) %>%
    select(longitude, latitude)
  
  ch <- chull(foo)
  coords <- foo[c(ch, ch[1]), ]
  
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  
  bob <- fortify(sp_poly)
  
  bob$area <- i
  
  cats[[i]] <- bob
}

cathy <- as.data.frame(rbindlist(cats))


### Step 3: Draw a map
### The key thing may be that you subet data for each price_cuts and draw
### polygons from outer side given the following link.
### This link was great. This is exactly what I was thinking.
### http://stackoverflow.com/questions/21748852/choropleth-map-in-ggplot-with-polygons-that-have-holes

ggmap(map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data = subset(cathy, area == 10))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data = subset(cathy, area == 9))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data = subset(cathy, area == 8))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data = subset(cathy, area == 7))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data = subset(cathy, area == 6))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data = subset(cathy, area == 5))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data =subset(cathy, area == 4))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data = subset(cathy, area == 3))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha = .3,
               data = subset(cathy, area == 2))+
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(area)),
               alpha= .3,
               data = subset(cathy, area == 1))+
  geom_point(data = positions, aes(x = longitude, y = latitude), size = 0.3) +                              
  scale_fill_gradientn(colours = brewer.pal(nbreaks,"Spectral")) +
  guides(fill = guide_legend(title = "Property price zone"))
